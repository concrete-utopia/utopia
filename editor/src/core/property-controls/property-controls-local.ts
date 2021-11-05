import { ImportType, PropertyControls, registerControls } from 'utopia-api'
import deepEqual from 'fast-deep-equal'

import { ProjectContentTreeRoot } from '../../components/assets'
import { PropertyControlsInfo } from '../../components/custom-code/code-file'
import type { EditorDispatch } from '../../components/editor/action-types'
import { dependenciesFromPackageJson } from '../../components/editor/npm-dependency/npm-dependency'
import {
  EditorState,
  packageJsonFileFromProjectContents,
} from '../../components/editor/store/editor-state'
import { updatePropertyControlsInfo } from '../../components/editor/actions/action-creators'
import { ParsedPropertyControls, parsePropertyControls } from './property-controls-parser'
import { ParseResult } from '../../utils/value-parser-utils'

export function createRegisterControlsFunction(
  dispatch: EditorDispatch,
  getEditorState: (() => EditorState) | null,
): typeof registerControls {
  // create a function with a signature that matches utopia-api/registerControls
  return (
    componentName: string,
    packageName: string,
    propertyControls: PropertyControls,
    requiredImports?: Array<ImportType>,
  ): void => {
    if (componentName == null || packageName == null || typeof propertyControls !== 'object') {
      console.warn(
        'registerControls has 3 parameters: component name, package name, property controls object',
      )
    } else {
      const parsedPropertyControls = parsePropertyControls(propertyControls)
      const currentPropertyControlsInfo = getEditorState?.().propertyControlsInfo
      if (currentPropertyControlsInfo != null) {
        const currentParsedPropertyControls: ParseResult<ParsedPropertyControls> =
          currentPropertyControlsInfo[packageName]?.[componentName]?.propertyControls
        const currentControlsAreTheSame = deepEqual(
          currentParsedPropertyControls,
          parsedPropertyControls,
        )
        const updatedControls: PropertyControlsInfo = {
          [packageName]: {
            ...currentPropertyControlsInfo[packageName],
            [componentName]: {
              propertyControls: parsedPropertyControls,
              componentInfo: { requiredImports: requiredImports },
            },
          },
        }
        if (!currentControlsAreTheSame) {
          // only dispatch if the control info is updated, to prevent a potential infinite loop of code re-evaluation
          dispatch([updatePropertyControlsInfo(updatedControls)])
        }
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
