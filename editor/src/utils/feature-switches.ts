import * as localforage from 'localforage'
import { fastForEach, isBrowserEnvironment } from '../core/shared/utils'

export type FeatureName =
  | 'Dragging Reparents By Default'
  | 'Dragging Shows Overlay'
  | 'Invisible Element Controls'
  | 'Advanced Resize Box'
  | 'Component Isolation Mode'
  | 'Component Navigator'
  | 'Component Navigator Component Outline Title'
  | 'Component Navigator Nearest Ancestor'
  | 'Re-parse Project Button'
  | 'iFrame Code Editor'
export const AllFeatureNames: FeatureName[] = [
  // 'Dragging Reparents By Default', // Removing this option so that we can experiment on this later
  // 'Dragging Shows Overlay', // Removing this option so that we can experiment on this later
  'Invisible Element Controls',
  'Advanced Resize Box',
  'Component Navigator',
  'Component Navigator Component Outline Title',
  'Component Navigator Nearest Ancestor',
  'Re-parse Project Button',
  'iFrame Code Editor',
]

let FeatureSwitches: { [feature: string]: boolean } = {
  'Dragging Reparents By Default': false,
  'Dragging Shows Overlay': false,
  'Invisible Element Controls': false,
  'Advanced Resize Box': false,
  'Component Isolation Mode': false,
  'Component Navigator': true,
  'Component Navigator Component Outline Title': true,
  'Component Navigator Nearest Ancestor': true,
  'Re-parse Project Button': false,
  'iFrame Code Editor': false,
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
