import * as localforage from 'localforage'
import { fastForEach, isBrowserEnvironment } from '../core/shared/utils'

export type FeatureName = 'Dragging Reparents By Default' | 'Dragging Shows Overlay'
export const AllFeatureNames: FeatureName[] = [
  'Dragging Reparents By Default',
  'Dragging Shows Overlay',
]

let FeatureSwitches: { [feature: string]: boolean } = {
  'Dragging Reparents By Default': false,
  'Dragging Shows Overlay': false,
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
