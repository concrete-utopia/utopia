import {
  ImportType,
  PropertyControls,
  registerComponent as registerComponentAPI,
  RegisterComponentEntry,
} from 'utopia-api'
import deepEqual from 'fast-deep-equal'

import { ProjectContentTreeRoot } from '../../components/assets'
import { ComponentDescriptor, PropertyControlsInfo } from '../../components/custom-code/code-file'
import type { EditorDispatch } from '../../components/editor/action-types'
import { dependenciesFromPackageJson } from '../../components/editor/npm-dependency/npm-dependency'
import {
  EditorState,
  packageJsonFileFromProjectContents,
} from '../../components/editor/store/editor-state'
import {
  showToast,
  updatePropertyControlsInfo,
} from '../../components/editor/actions/action-creators'
import {
  parseControlDescription,
  ParsedPropertyControls,
  parsePropertyControls,
} from './property-controls-parser'
import {
  getParseErrorDetails,
  objectKeyParser,
  parseObject,
  ParseResult,
  parseString,
} from '../../utils/value-parser-utils'
import { UtopiaTsWorkers } from '../workers/common/worker-types'
import { getCachedParseResultForUserStrings } from './property-controls-local-parser-bridge'
import { applicative5Either, forEachLeft, forEachRight, isLeft, isRight } from '../shared/either'
import { notice } from '../../components/common/notice'

async function registerComponentInternal(
  dispatch: EditorDispatch,
  getEditorState: (() => EditorState) | null,
  workers: UtopiaTsWorkers,
  componentName: string,
  moduleNameOrPath: string,
  propertyControls: PropertyControls,
  elementToInsert: string,
  importsToAdd: string,
) {
  const parsedParams = await getCachedParseResultForUserStrings(
    workers,
    importsToAdd,
    elementToInsert,
  )
  if (isRight(parsedParams)) {
    const parsedPropertyControls = parsePropertyControls(propertyControls)
    const currentPropertyControlsInfo = getEditorState?.().propertyControlsInfo
    if (currentPropertyControlsInfo != null) {
      const currentInfo: ComponentDescriptor | null =
        currentPropertyControlsInfo[moduleNameOrPath]?.[componentName]

      const newInfo: ComponentDescriptor = {
        propertyControls: parsedPropertyControls,
        componentInfo: {
          importsToAdd: parsedParams.value.importsToAdd,
          elementToInsert: parsedParams.value.elementToInsert,
        },
      }
      const currentControlsAreTheSame = deepEqual(currentInfo, newInfo)
      const updatedPropertyControlsInfo: PropertyControlsInfo = {
        [moduleNameOrPath]: {
          ...currentPropertyControlsInfo[moduleNameOrPath],
          [componentName]: newInfo,
        },
      }
      if (!currentControlsAreTheSame) {
        // only dispatch if the control info is updated, to prevent a potential infinite loop of code re-evaluation
        dispatch([updatePropertyControlsInfo(updatedPropertyControlsInfo)])
      }
    }
  } else {
    console.error(
      `There was a problem with 'registerComponent' ${componentName}: ${parsedParams.value}`,
    )
  }
}

export function fullyParsePropertyControls(value: unknown): ParseResult<PropertyControls> {
  return parseObject(parseControlDescription)(value)
}

export function parseRegisterComponentEntry(value: unknown): ParseResult<RegisterComponentEntry> {
  return applicative5Either(
    (componentName, moduleName, controls, insert, requiredImports) => {
      return {
        name: componentName,
        moduleName: moduleName,
        controls: controls,
        insert: insert,
        requiredImports: requiredImports,
      }
    },
    objectKeyParser(parseString, 'name')(value),
    objectKeyParser(parseString, 'moduleName')(value),
    objectKeyParser(fullyParsePropertyControls, 'controls')(value),
    objectKeyParser(parseString, 'insert')(value),
    objectKeyParser(parseString, 'requiredImports')(value),
  )
}

export function createRegisterComponentFunction(
  dispatch: EditorDispatch,
  getEditorState: (() => EditorState) | null,
  workers: UtopiaTsWorkers | null,
): typeof registerComponentAPI {
  // create a function with a signature that matches utopia-api/registerComponent
  return function registerComponent(paramsObj: unknown): void {
    const stackFrames = new Error().stack
    const parseResult = parseRegisterComponentEntry(paramsObj)
    if (isLeft(parseResult)) {
      const errorDescription = getParseErrorDetails(parseResult.value).description
      throw new Error(`registerComponent error: ${errorDescription}`)
    }
    forEachRight(parseResult, (registerComponentEntry) => {
      const { name, moduleName, controls, insert, requiredImports } = registerComponentEntry
      if (workers != null) {
        registerComponentInternal(
          dispatch,
          getEditorState,
          workers,
          name,
          moduleName,
          controls,
          insert,
          requiredImports,
        )
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
