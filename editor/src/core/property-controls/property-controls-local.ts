import {
  registerModule as registerModuleAPI,
  ComponentToRegister,
  ComponentInsertOption,
  PropertyControls,
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
import {
  parseControlDescription,
  ParsedPropertyControls,
  parsePropertyControls,
} from './property-controls-parser'
import {
  getParseErrorDetails,
  objectKeyParser,
  optionalObjectKeyParser,
  parseArray,
  parseObject,
  ParseResult,
  parseString,
} from '../../utils/value-parser-utils'
import { UtopiaTsWorkers } from '../workers/common/worker-types'
import { getCachedParseResultForUserStrings } from './property-controls-local-parser-bridge'
import {
  applicative2Either,
  applicative3Either,
  Either,
  forEachRight,
  isLeft,
  isRight,
  mapEither,
  sequenceEither,
} from '../shared/either'
import { mapArrayToDictionary } from '../shared/array-utils'
import { setOptionalProp } from '../shared/object-utils'

async function parseInsertOption(
  insertOption: ComponentInsertOption,
  componentName: string,
  moduleName: string,
  workers: UtopiaTsWorkers,
): Promise<Either<string, ComponentInfo>> {
  const allRequiredImports = `import { ${componentName} } from '${moduleName}'; ${
    insertOption.additionalRequiredImports ?? ''
  }`

  const parsedParams = await getCachedParseResultForUserStrings(
    workers,
    allRequiredImports,
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

function insertOptionsForComponentToRegister(
  componentToRegister: ComponentToRegister,
  componentName: string,
): Array<ComponentInsertOption> {
  if (componentToRegister.insertOptions.length > 0) {
    return componentToRegister.insertOptions
  } else {
    // If none provided, fall back to a default insert option
    return [
      {
        menuLabel: componentName,
        codeToInsert: `<${componentName} />`,
      },
    ]
  }
}

async function componentDescriptorForComponentToRegister(
  componentToRegister: ComponentToRegister,
  componentName: string,
  moduleName: string,
  workers: UtopiaTsWorkers,
): Promise<Either<string, ComponentDescriptorWithName>> {
  const parsedPropertyControls = parsePropertyControls(componentToRegister.controls)
  const unparsedInsertOptions = insertOptionsForComponentToRegister(
    componentToRegister,
    componentName,
  )

  const parsedInsertOptionPromises = unparsedInsertOptions.map((insertOption) =>
    parseInsertOption(insertOption, componentName, moduleName, workers),
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
    return componentDescriptorForComponentToRegister(
      componentToRegister,
      componentName,
      moduleNameOrPath,
      workers,
    )
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

export function fullyParsePropertyControls(value: unknown): ParseResult<PropertyControls> {
  return parseObject(parseControlDescription)(value)
}

export function parseComponentInsertOption(value: unknown): ParseResult<ComponentInsertOption> {
  return applicative3Either(
    (codeToInsert, additionalRequiredImports, menuLabel) => {
      let insertOption: ComponentInsertOption = {
        codeToInsert: codeToInsert,
      }

      setOptionalProp(insertOption, 'additionalRequiredImports', additionalRequiredImports)
      setOptionalProp(insertOption, 'menuLabel', menuLabel)

      return insertOption
    },
    objectKeyParser(parseString, 'codeToInsert')(value),
    optionalObjectKeyParser(parseString, 'additionalRequiredImports')(value),
    optionalObjectKeyParser(parseString, 'menuLabel')(value),
  )
}

export function parseComponentToRegister(value: unknown): ParseResult<ComponentToRegister> {
  return applicative2Either(
    (controls, insertOptions) => {
      return {
        controls: controls,
        insertOptions: insertOptions,
      }
    },
    objectKeyParser(fullyParsePropertyControls, 'controls')(value),
    objectKeyParser(parseArray(parseComponentInsertOption), 'insertOptions')(value),
  )
}

export const parseComponents: (
  value: unknown,
) => ParseResult<{ [componentName: string]: ComponentToRegister }> = parseObject(
  parseComponentToRegister,
)

export function createRegisterModuleFunction(
  dispatch: EditorDispatch,
  getEditorState: (() => EditorState) | null,
  workers: UtopiaTsWorkers | null,
): typeof registerModuleAPI {
  // create a function with a signature that matches utopia-api/registerModule
  return function registerModule(
    unparsedModuleName: string,
    unparsedComponents: { [componentName: string]: ComponentToRegister },
  ): void {
    const parsedModuleName = parseString(unparsedModuleName)
    const parsedComponents = parseComponents(unparsedComponents)

    const parsedParams = applicative2Either(
      (moduleName, components) => {
        return { moduleName: moduleName, components: components }
      },
      parsedModuleName,
      parsedComponents,
    )

    if (isLeft(parsedModuleName)) {
      const errorDetails = getParseErrorDetails(parsedModuleName.value)
      throw new Error(`registerModule first param (moduleName): ${errorDetails.description}`)
    }

    if (isLeft(parsedComponents)) {
      const errorDetails = getParseErrorDetails(parsedComponents.value)
      throw new Error(
        `registerModule second param (components): ${errorDetails.description} [${errorDetails.path}]`,
      )
    }

    forEachRight(parsedParams, ({ moduleName, components }) => {
      if (workers != null) {
        registerModuleInternal(dispatch, getEditorState, workers, moduleName, components)
      }
    })
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
