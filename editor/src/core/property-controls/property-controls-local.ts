import { PropertyControls } from 'utopia-api'
import { PropertyControlsInfo } from '../../components/custom-code/code-file'

export let LocalThirdPartyControls: PropertyControlsInfo = {}

export const registerControls = (
  componentName: string,
  packageName: string,
  description: PropertyControls,
): void => {
  if (componentName == null || packageName == null || typeof description !== 'object') {
    console.warn(
      'registerControls has 3 parameters: component name, package name, descriptor object',
    )
  } else {
    LocalThirdPartyControls[packageName] = {
      ...(LocalThirdPartyControls[packageName] ?? {}),
      [componentName]: description,
    }
  }
}

export function getLocalThirdPartyControls(
  componentName: string,
  packageName: string,
): PropertyControls | null {
  return LocalThirdPartyControls[packageName]?.[componentName]
}
