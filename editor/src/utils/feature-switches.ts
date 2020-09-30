import * as localforage from 'localforage'
import { fastForEach, isBrowserEnvironment } from '../core/shared/utils'

export type FeatureName =
  | 'Dragging Reparents By Default'
  | 'Dragging Shows Overlay'
  | 'Invisible Element Controls'
  | 'Advanced Resize Box'
  | 'Layout Info Box'
  | 'Nearby Reparent Target Highlight'
  | 'Flex Sibling Numbers'
  | 'Flex Container Tools'
  | 'Mini Navigator'
export const AllFeatureNames: FeatureName[] = [
  // 'Dragging Reparents By Default', // Removing this option so that we can experiment on this later
  // 'Dragging Shows Overlay', // Removing this option so that we can experiment on this later
  'Invisible Element Controls',
  'Advanced Resize Box',
  'Layout Info Box',
  'Nearby Reparent Target Highlight',
  'Flex Sibling Numbers',
  'Flex Container Tools',
  'Mini Navigator',
]

let FeatureSwitches: { [feature: string]: boolean } = {
  'Dragging Reparents By Default': false,
  'Dragging Shows Overlay': false,
  'Invisible Element Controls': false,
  'Advanced Resize Box': false,
  'Layout Info Box': true,
  'Nearby Reparent Target Highlight': true,
  'Flex Sibling Numbers': true,
  'Flex Container Tools': true,
  'Mini Navigator': true,
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
