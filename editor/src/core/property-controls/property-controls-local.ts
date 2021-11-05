import { ImportType, PropertyControls, registerComponent as registerComponentAPI } from 'utopia-api'
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

export function createRegisterComponentFunction(
  dispatch: EditorDispatch,
  getEditorState: (() => EditorState) | null,
): typeof registerComponentAPI {
  // create a function with a signature that matches utopia-api/registerControls
  return function registerComponent(
    componentName: string,
    moduleNameOrPath: string,
    propertyControls: PropertyControls,
    optionalParameters?: { requiredImports?: Array<ImportType> },
  ): void {
    if (componentName == null || moduleNameOrPath == null || typeof propertyControls !== 'object') {
      console.warn(
        'registerControls has 3 parameters: component name, module name or path, property controls object',
      )
    } else {
      const currentPropertyControlsInfo = getEditorState?.().propertyControlsInfo
      if (currentPropertyControlsInfo != null) {
        const currentControlsAreTheSame = deepEqual(
          currentPropertyControlsInfo[moduleNameOrPath]?.[componentName],
          propertyControls,
        )
        const updatedControls: PropertyControlsInfo = {
          [moduleNameOrPath]: {
            ...currentPropertyControlsInfo[moduleNameOrPath],
            [componentName]: {
              propertyControls: propertyControls,
              componentInfo: optionalParameters ?? {},
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
): PropertyControls | null {
  const packageJsonFile = packageJsonFileFromProjectContents(projectContents)
  const dependencies = dependenciesFromPackageJson(packageJsonFile, 'combined')
  const foundPackageWithElement = Object.keys(propertyControlsInfo).find((key) => {
    return (
      propertyControlsInfo[key][elementName] != null &&
      dependencies.some((dependency) => dependency.name === key)
    )
  })
  if (foundPackageWithElement != null) {
    return propertyControlsInfo[foundPackageWithElement][elementName].propertyControls
  }
  return null
}
