import { PropertyControls } from 'utopia-api'
import { ProjectContentTreeRoot } from '../../components/assets'
import { PropertyControlsInfo } from '../../components/custom-code/code-file'
import type { EditorDispatch } from '../../components/editor/action-types'
import { dependenciesFromPackageJson } from '../../components/editor/npm-dependency/npm-dependency'
import {
  EditorState,
  packageJsonFileFromProjectContents,
} from '../../components/editor/store/editor-state'
import { ReactThreeFiberControls } from './third-party-property-controls/react-three-fiber-controls'

export let LocalThirdPartyControls: PropertyControlsInfo = {
  '@react-three/fiber': ReactThreeFiberControls,
}

export const createRegisterControlsFunction = (
  dispatch: EditorDispatch,
  getEditorState: (() => EditorState) | null,
) => (componentName: string, packageName: string, propertyControls: PropertyControls): void => {
  if (componentName == null || packageName == null || typeof propertyControls !== 'object') {
    console.warn(
      'registerControls has 3 parameters: component name, package name, property controls object',
    )
  } else {
    LocalThirdPartyControls[packageName] = {
      ...(LocalThirdPartyControls[packageName] ?? {}),
      [componentName]: propertyControls,
    }
  }
}

export function getAllRegisteredControls(): PropertyControlsInfo {
  return LocalThirdPartyControls
}

export function getLocalThirdPartyControls(
  componentName: string,
  packageName: string,
): PropertyControls | null {
  return LocalThirdPartyControls[packageName]?.[componentName]
}

export function getLocalThirdPartyControlsIntrinsic(
  elementName: string,
  propertyControlsInfo: PropertyControlsInfo,
  projectContents: ProjectContentTreeRoot,
): PropertyControls | null {
  const packageJsonFile = packageJsonFileFromProjectContents(projectContents)
  const dependencies = dependenciesFromPackageJson(packageJsonFile, 'combined')
  const foundPackageWithElement = Object.keys(LocalThirdPartyControls).find((key) => {
    return LocalThirdPartyControls[key][elementName] != null
  })
  if (
    foundPackageWithElement != null &&
    dependencies.some((dependency) => dependency.name === foundPackageWithElement)
  ) {
    return getLocalThirdPartyControls(elementName, foundPackageWithElement)
  }
  return null
}
