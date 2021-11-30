import {
  registerModule as registerModuleAPI,
  ComponentToRegister,
  ComponentInsertOption,
} from 'utopia-api'
import deepEqual from 'fast-deep-equal'
import { ProjectContentTreeRoot } from '../../components/assets'
import {
  ComponentDescriptorsForFile,
  ComponentDescriptorWithName,
  ComponentInfo,
  PropertyControlsInfo,
} from '../../components/custom-code/code-file'
import type { EditorDispatch } from '../../components/editor/action-types'
import { dependenciesFromPackageJson } from '../../components/editor/npm-dependency/npm-dependency'
import {
  EditorState,
  packageJsonFileFromProjectContents,
} from '../../components/editor/store/editor-state'
import { updatePropertyControlsInfo } from '../../components/editor/actions/action-creators'
import { ParsedPropertyControls, parsePropertyControls } from './property-controls-parser'
import { ParseResult } from '../../utils/value-parser-utils'
import { UtopiaTsWorkers } from '../workers/common/worker-types'
import { getCachedParseResultForUserStrings } from './property-controls-local-parser-bridge'
import { Either, isRight, mapEither, sequenceEither } from '../shared/either'
import { mapArrayToDictionary } from '../shared/array-utils'

async function parseInsertOption(
  insertOption: ComponentInsertOption,
  componentName: string,
  workers: UtopiaTsWorkers,
): Promise<Either<string, ComponentInfo>> {
  const parsedParams = await getCachedParseResultForUserStrings(
    workers,
    insertOption.additionalRequiredImports ?? '',
    insertOption.codeToInsert,
  )

  return mapEither(({ importsToAdd, elementToInsert }) => {
    return {
      insertMenuLabel: insertOption.menuLabel ?? componentName,
      elementToInsert: elementToInsert,
      importsToAdd: importsToAdd,
    }
  }, parsedParams)
}

async function componentDescriptorForComponentToRegister(
  componentToRegister: ComponentToRegister,
  componentName: string,
  workers: UtopiaTsWorkers,
): Promise<Either<string, ComponentDescriptorWithName>> {
  const parsedPropertyControls = parsePropertyControls(componentToRegister.controls)
  const parsedInsertOptionPromises = componentToRegister.insertOptions.map((insertOption) =>
    parseInsertOption(insertOption, componentName, workers),
  )

  const parsedInsertOptionsUnsequenced = await Promise.all(parsedInsertOptionPromises)
  const parsedInsertOptions = sequenceEither(parsedInsertOptionsUnsequenced)

  return mapEither((insertOptions) => {
    return {
      componentName: componentName,
      propertyControls: parsedPropertyControls,
      insertOptions: insertOptions,
    }
  }, parsedInsertOptions)
}

async function registerModuleInternal(
  dispatch: EditorDispatch,
  getEditorState: (() => EditorState) | null,
  workers: UtopiaTsWorkers,
  moduleNameOrPath: string,
  components: { [componentName: string]: ComponentToRegister },
) {
  const componentNames = Object.keys(components)
  const componentDescriptorPromises = componentNames.map((componentName) => {
    const componentToRegister = components[componentName]
    return componentDescriptorForComponentToRegister(componentToRegister, componentName, workers)
  })

  const componentDescriptorsUnsequenced = await Promise.all(componentDescriptorPromises)
  const componentDescriptors = sequenceEither(componentDescriptorsUnsequenced)
  if (isRight(componentDescriptors)) {
    // FIXME At what point should we be caching / memoising this?
    const newDescriptorsForFile: ComponentDescriptorsForFile = mapArrayToDictionary(
      componentDescriptors.value,
      (descriptorWithName) => descriptorWithName.componentName,
      (descriptorWithName) => {
        return {
          propertyControls: descriptorWithName.propertyControls,
          insertOptions: descriptorWithName.insertOptions,
        }
      },
    )

    const currentPropertyControlsInfo = getEditorState?.().propertyControlsInfo ?? {}
    const currentDescriptorsForFile = currentPropertyControlsInfo[moduleNameOrPath] ?? {}

    const descriptorsChanged = !deepEqual(currentDescriptorsForFile, newDescriptorsForFile)
    if (descriptorsChanged) {
      const updatedPropertyControlsInfo: PropertyControlsInfo = {
        [moduleNameOrPath]: newDescriptorsForFile,
      }
      dispatch([updatePropertyControlsInfo(updatedPropertyControlsInfo)])
    }
  } else {
    console.error(
      `There was a problem with 'registerModule' ${moduleNameOrPath}: ${componentDescriptors.value}`,
    )
  }
}

export function createRegisterModuleFunction(
  dispatch: EditorDispatch,
  getEditorState: (() => EditorState) | null,
  workers: UtopiaTsWorkers | null,
): typeof registerModuleAPI {
  // create a function with a signature that matches utopia-api/registerModule
  return function registerModule(
    moduleName: string,
    components: { [componentName: string]: ComponentToRegister },
  ): void {
    if (workers != null) {
      registerModuleInternal(dispatch, getEditorState, workers, moduleName, components)
    }
  }
}

export function getThirdPartyControlsIntrinsic(
  elementName: string,
  propertyControlsInfo: PropertyControlsInfo,
  projectContents: ProjectContentTreeRoot,
): ParseResult<ParsedPropertyControls> | null {
  const packageJsonFile = packageJsonFileFromProjectContents(projectContents)
  const dependencies = dependenciesFromPackageJson(packageJsonFile, 'combined')
  const foundPackageWithElement = Object.keys(propertyControlsInfo).find((key) => {
    return (
      propertyControlsInfo[key][elementName] != null &&
      dependencies.some((dependency) => dependency.name === key)
    )
  })
  if (foundPackageWithElement != null) {
    return propertyControlsInfo[foundPackageWithElement]?.[elementName]?.propertyControls
  }
  return null
}
