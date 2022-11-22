import localforage from 'localforage'
import { IS_TEST_ENVIRONMENT, PRODUCTION_CONFIG } from '../common/env-vars'
import { fastForEach, isBrowserEnvironment } from '../core/shared/utils'

export type FeatureName =
  | 'Debug mode – Redux Devtools'
  | 'Debug mode – Performance Marks'
  | 'Dragging Reparents By Default'
  | 'Dragging Shows Overlay'
  | 'Advanced Resize Box'
  | 'Re-parse Project Button'
  | 'Performance Test Triggers'
  | 'Canvas Strategies'
  | 'Canvas Strategies Debug Panel'
  | 'Single child, contiguous parent: move parent'
  | 'Single child, zero sized parent: move parent'

export const AllFeatureNames: FeatureName[] = [
  // 'Dragging Reparents By Default', // Removing this option so that we can experiment on this later
  // 'Dragging Shows Overlay', // Removing this option so that we can experiment on this later
  'Debug mode – Redux Devtools',
  'Debug mode – Performance Marks',
  'Advanced Resize Box',
  'Re-parse Project Button',
  'Performance Test Triggers',
  'Canvas Strategies',
  'Canvas Strategies Debug Panel',
  'Single child, contiguous parent: move parent',
  'Single child, zero sized parent: move parent',
]

let FeatureSwitches: { [feature in FeatureName]: boolean } = {
  'Debug mode – Redux Devtools': false,
  'Debug mode – Performance Marks': false,
  'Dragging Reparents By Default': false,
  'Dragging Shows Overlay': false,
  'Advanced Resize Box': false,
  'Re-parse Project Button': !(PRODUCTION_CONFIG as boolean),
  'Performance Test Triggers': !(PRODUCTION_CONFIG as boolean),
  'Canvas Strategies': true,
  'Canvas Strategies Debug Panel': false,
  'Single child, contiguous parent: move parent': true,
  'Single child, zero sized parent: move parent': true,
}

function settingKeyForName(featureName: FeatureName): string {
  return `Feature-Switch-${featureName}`
}

async function loadStoredValue(featureName: FeatureName) {
  if (isBrowserEnvironment && !IS_TEST_ENVIRONMENT) {
    const existing = await localforage.getItem<boolean | null>(settingKeyForName(featureName))
    if (existing != null) {
      FeatureSwitches[featureName] = existing
    }
  }
}

// Load stored settings
fastForEach(AllFeatureNames, (name) => {
  void loadStoredValue(name)
})

export function isFeatureEnabled(featureName: FeatureName): boolean {
  return FeatureSwitches[featureName] ?? false
}

export function toggleFeatureEnabled(featureName: FeatureName): void {
  const newValue = !isFeatureEnabled(featureName)
  FeatureSwitches[featureName] = newValue
  if (isBrowserEnvironment) {
    void localforage.setItem(settingKeyForName(featureName), newValue)
  }
}

export function setFeatureEnabled(featureName: FeatureName, newValue: boolean): void {
  FeatureSwitches[featureName] = newValue
  if (isBrowserEnvironment) {
    void localforage.setItem(settingKeyForName(featureName), newValue)
  }
}
