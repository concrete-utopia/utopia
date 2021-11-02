import { PropertyControls, registerControls } from 'utopia-api'
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

export function createRegisterControlsFunction(
  dispatch: EditorDispatch,
  getEditorState: (() => EditorState) | null,
): typeof registerControls {
  // create a function with a signature that matches utopia-api/registerControls
  return (componentName: string, packageName: string, propertyControls: PropertyControls): void => {
    if (componentName == null || packageName == null || typeof propertyControls !== 'object') {
      console.warn(
        'registerControls has 3 parameters: component name, package name, property controls object',
      )
    } else {
      const currentPropertyControlsInfo = getEditorState?.().propertyControlsInfo
      if (currentPropertyControlsInfo != null) {
        const currentControlsAreTheSame = deepEqual(
          currentPropertyControlsInfo[packageName]?.[componentName],
          propertyControls,
        )
        const updatedControls: PropertyControlsInfo = {
          [packageName]: {
            ...currentPropertyControlsInfo[packageName],
            [componentName]: propertyControls,
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

export function getLocalThirdPartyControlsIntrinsic(
  elementName: string,
  propertyControlsInfo: PropertyControlsInfo,
  projectContents: ProjectContentTreeRoot,
): PropertyControls | null {
  const packageJsonFile = packageJsonFileFromProjectContents(projectContents)
  const dependencies = dependenciesFromPackageJson(packageJsonFile, 'combined')
  const foundPackageWithElement = Object.keys(propertyControlsInfo).find((key) => {
    return propertyControlsInfo[key][elementName] != null
  })
  if (
    foundPackageWithElement != null &&
    dependencies.some((dependency) => dependency.name === foundPackageWithElement)
  ) {
    return propertyControlsInfo[foundPackageWithElement][elementName]
  }
  return null
}
