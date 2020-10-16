import * as localforage from 'localforage'
import { fastForEach, isBrowserEnvironment } from '../core/shared/utils'

export type FeatureName =
  | 'Dragging Reparents By Default'
  | 'Dragging Shows Overlay'
  | 'Invisible Element Controls'
  | 'Advanced Resize Box'
  | 'Wrapper Element Controls'
  | 'Layout Info Box'
  | 'Nearby Reparent Target Highlight'
  | 'Toolbar For Controls'
  | 'Flex Sibling Numbers'
  | 'Flex Container Tools'
  | 'Element Resize Menu'
  | 'Flex Properties (Timer)'
  | 'Mini Navigator'
  | 'Hierarchy View'
  | 'Reorder Shows Placeholder Line'
  | 'Flow Resize'
  | 'Highlight Containing Block'
  | 'Layouttype Outline'
  | 'Floating Menu Warning'
  | 'Show Pins'

export const AllFeatureNames: FeatureName[] = [
  // 'Dragging Reparents By Default', // Removing this option so that we can experiment on this later
  // 'Dragging Shows Overlay', // Removing this option so that we can experiment on this later
  'Invisible Element Controls',
  'Advanced Resize Box',
  'Wrapper Element Controls',
  'Layout Info Box',
  'Toolbar For Controls',
  'Nearby Reparent Target Highlight',
  'Flex Sibling Numbers',
  'Flex Container Tools',
  'Element Resize Menu',
  'Flex Properties (Timer)',
  'Mini Navigator',
  'Hierarchy View',
  'Reorder Shows Placeholder Line',
  'Flow Resize',
  'Highlight Containing Block',
  'Layouttype Outline',
  'Floating Menu Warning',
  'Show Pins',
]

let FeatureSwitches: { [feature: string]: boolean } = {
  'Dragging Reparents By Default': false,
  'Dragging Shows Overlay': false,
  'Invisible Element Controls': false,
  'Advanced Resize Box': false,
  'Layout Info Box': false,
  'Wrapper Element Controls': true,
  'Toolbar For Controls': false,
  'Nearby Reparent Target Highlight': true,
  'Flex Sibling Numbers': true,
  'Flex Container Tools': true,
  'Element Resize Menu': true,
  'Flex Properties (Timer)': true,
  'Mini Navigator': true,
  'Hierarchy View': true,
  'Reorder Shows Placeholder Line': true,
  'Flow Resize': true,
  'Highlight Containing Block': true,
  'Layouttype Outline': true,
  'Floating Menu Warning': true,
  'Show Pins': true,
}

function settingKeyForName(featureName: FeatureName): string {
  return `Feature-Switch-${featureName}`
}

async function loadStoredValue(featureName: FeatureName) {
  if (isBrowserEnvironment) {
    const existing = await localforage.getItem<boolean | null>(settingKeyForName(featureName))
    if (existing != null) {
      FeatureSwitches[featureName] = existing
    }
  }
}

// Load stored settings
fastForEach(AllFeatureNames, (name) => {
  loadStoredValue(name)
})

export function isFeatureEnabled(featureName: FeatureName): boolean {
  return FeatureSwitches[featureName] ?? false
}

export function toggleFeatureEnabled(featureName: FeatureName) {
  const newValue = !isFeatureEnabled(featureName)
  FeatureSwitches[featureName] = newValue
  if (isBrowserEnvironment) {
    localforage.setItem(settingKeyForName(featureName), newValue)
  }
}
