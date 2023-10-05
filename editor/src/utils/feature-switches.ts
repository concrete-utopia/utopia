import localforage from 'localforage'
import { IS_TEST_ENVIRONMENT, PRODUCTION_CONFIG } from '../common/env-vars'
import { fastForEach, isBrowserEnvironment } from '../core/shared/utils'

export type FeatureName =
  | 'Debug – Redux Devtools'
  | 'Debug – Performance Marks (Slow)'
  | 'Debug – Performance Marks (Fast)'
  | 'Debug – Measure Selectors'
  | 'Dragging Reparents By Default'
  | 'Re-parse Project Button'
  | 'Performance Test Triggers'
  | 'Canvas Strategies Debug Panel'
  | 'Project Thumbnail Generation'
  | 'Draggable Floating Panels'
  | 'Debug - Print UIDs'

export const AllFeatureNames: FeatureName[] = [
  // 'Dragging Reparents By Default', // Removing this option so that we can experiment on this later
  // 'Dragging Shows Overlay', // Removing this option so that we can experiment on this later
  'Debug – Redux Devtools',
  'Debug – Performance Marks (Slow)',
  'Debug – Performance Marks (Fast)',
  'Debug – Measure Selectors',
  'Re-parse Project Button',
  'Performance Test Triggers',
  'Canvas Strategies Debug Panel',
  'Project Thumbnail Generation',
  'Draggable Floating Panels',
  'Debug - Print UIDs',
]

let FeatureSwitches: { [feature in FeatureName]: boolean } = {
  'Debug – Redux Devtools': false,
  'Debug – Performance Marks (Slow)': false,
  'Debug – Performance Marks (Fast)': false,
  'Debug – Measure Selectors': false,
  'Dragging Reparents By Default': false,
  'Re-parse Project Button': !(PRODUCTION_CONFIG as boolean),
  'Performance Test Triggers': !(PRODUCTION_CONFIG as boolean),
  'Canvas Strategies Debug Panel': false,
  'Project Thumbnail Generation': false,
  'Draggable Floating Panels': false,
  'Debug - Print UIDs': false,
}

let FeatureSwitchLoaded: { [feature in FeatureName]?: boolean } = {}

function settingKeyForName(featureName: FeatureName): string {
  return `Feature-Switch-${featureName}`
}

async function loadStoredValue(featureName: FeatureName) {
  if (isBrowserEnvironment && !IS_TEST_ENVIRONMENT) {
    const existing = await localforage.getItem<boolean | null>(settingKeyForName(featureName))
    FeatureSwitchLoaded[featureName] = true
    if (existing != null) {
      FeatureSwitches[featureName] = existing
    }
    return true
  } else {
    return true
  }
}

// Load stored settings
export async function loadFeatureSwitches(): Promise<void> {
  if (IS_TEST_ENVIRONMENT) {
    // in test environments, we actually don't want to load from localforage, ever
    return
  }
  await Promise.all(
    AllFeatureNames.map((name) => {
      return loadStoredValue(name)
    }),
  )
}

export function isFeatureEnabled(featureName: FeatureName): boolean {
  if (FeatureSwitchLoaded[featureName] !== true) {
    if (!IS_TEST_ENVIRONMENT) {
      // in test environments, we actually don't want to load from localforage, ever
      throw new Error(`Reading FS before it was loaded! ${featureName}`)
    }
  }
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
