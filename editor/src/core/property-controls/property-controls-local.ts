import { ImportType, PropertyControls, registerComponent as registerComponentAPI } from 'utopia-api'
import deepEqual from 'fast-deep-equal'

import { ProjectContentTreeRoot } from '../../components/assets'
import { ComponentDescriptor, PropertyControlsInfo } from '../../components/custom-code/code-file'
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
import { bimapEither, Either, isRight, left, mapEither } from '../shared/either'
import { jsxSimpleAttributesToProps, jsxSimpleAttributeToValue } from '../shared/jsx-attributes'
import { JSXElement, JSXElementWithoutUID } from '../shared/element-template'
import { Imports } from '../shared/project-file-types'

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
  }
}

export function createRegisterComponentFunction(
  dispatch: EditorDispatch,
  getEditorState: (() => EditorState) | null,
  workers: UtopiaTsWorkers | null = null,
): typeof registerComponentAPI {
  // create a function with a signature that matches utopia-api/registerComponent
  return function registerComponent(paramsObj: {
    name: string
    moduleName: string
    controls: PropertyControls
    insert: string
    requiredImports: string
  }): void {
    const { name, moduleName, controls, insert, requiredImports } = paramsObj
    if (
      name == null ||
      moduleName == null ||
      typeof controls !== 'object' ||
      insert == null ||
      requiredImports == null
    ) {
      console.warn(
        'registerComponent has 5 parameters: component name, module name or path, property controls object, inserted element, required imports',
      )
    } else {
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
