import * as localforage from 'localforage'
import { PRODUCTION_CONFIG } from '../common/env-vars'
import { fastForEach, isBrowserEnvironment } from '../core/shared/utils'

export type FeatureName =
  | 'Dragging Reparents By Default'
  | 'Dragging Shows Overlay'
  | 'Invisible Element Controls'
  | 'Advanced Resize Box'
  | 'Re-parse Project Button'
  | 'Performance Test Triggers'
  | 'TopMenu ClassNames'
export const AllFeatureNames: FeatureName[] = [
  // 'Dragging Reparents By Default', // Removing this option so that we can experiment on this later
  // 'Dragging Shows Overlay', // Removing this option so that we can experiment on this later
  'Invisible Element Controls',
  'Advanced Resize Box',
  'Re-parse Project Button',
  'Performance Test Triggers',
  'TopMenu ClassNames',
]

let FeatureSwitches: { [feature: string]: boolean } = {
  'Dragging Reparents By Default': false,
  'Dragging Shows Overlay': false,
  'Invisible Element Controls': false,
  'Advanced Resize Box': false,
  'Re-parse Project Button': !(PRODUCTION_CONFIG as boolean),
  'Performance Test Triggers': !(PRODUCTION_CONFIG as boolean),
  'TopMenu ClassNames': false,
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

export function toggleFeatureEnabled(featureName: FeatureName): void {
  const newValue = !isFeatureEnabled(featureName)
  FeatureSwitches[featureName] = newValue
  if (isBrowserEnvironment) {
    localforage.setItem(settingKeyForName(featureName), newValue)
  }
}
