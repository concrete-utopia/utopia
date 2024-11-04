import localforage from 'localforage'
import { IS_TEST_ENVIRONMENT, PRODUCTION_CONFIG } from '../common/env-vars'
import { isBrowserEnvironment } from '../core/shared/utils'

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
  | 'Debug - Print UIDs'
  | 'Debug – Connections'
  | 'Parallel Parsing'
  | 'Debug - Log Parse Timings'
  | 'Condensed Navigator Entries'
  | 'Use Parsing Cache'
  | 'Debug - Verbose Log Cache'
  | 'Debug - Arbitrary Code Cache'
  | 'Canvas Fast Selection Hack'
  | 'Tailwind'
  | 'Import Wizard'
  | 'Show Debug Features'

export const AllFeatureNames: FeatureName[] = [
  // 'Dragging Reparents By Default', // Removing this option so that we can experiment on this later
  // 'Dragging Shows Overlay', // Removing this option so that we can experiment on this later
  'Show Debug Features',
  'Debug – Redux Devtools',
  'Debug – Performance Marks (Slow)',
  'Debug – Performance Marks (Fast)',
  'Debug – Measure Selectors',
  'Debug – Connections',
  'Debug - Log Parse Timings',
  'Debug - Verbose Log Cache',
  'Debug - Arbitrary Code Cache',
  'Debug - Print UIDs',
  'Re-parse Project Button',
  'Performance Test Triggers',
  'Canvas Strategies Debug Panel',
  'Project Thumbnail Generation',
  'Parallel Parsing',
  'Use Parsing Cache',
  'Condensed Navigator Entries',
  'Canvas Fast Selection Hack',
  'Tailwind',
  'Import Wizard',
]

let FeatureSwitches: { [feature in FeatureName]: boolean } = {
  'Debug – Redux Devtools': false,
  'Debug – Performance Marks (Slow)': false,
  'Debug – Performance Marks (Fast)': false,
  'Debug – Measure Selectors': false,
  'Debug – Connections': false,
  'Debug - Verbose Log Cache': false,
  'Debug - Arbitrary Code Cache': false,
  'Dragging Reparents By Default': false,
  'Re-parse Project Button': !(PRODUCTION_CONFIG as boolean),
  'Performance Test Triggers': !(PRODUCTION_CONFIG as boolean),
  'Canvas Strategies Debug Panel': false,
  'Project Thumbnail Generation': false,
  'Debug - Print UIDs': false,
  'Parallel Parsing': false,
  'Debug - Log Parse Timings': false,
  Tailwind: false,
  'Condensed Navigator Entries': !IS_TEST_ENVIRONMENT,
  'Use Parsing Cache': false,
  'Canvas Fast Selection Hack': true,
  'Import Wizard': true,
  'Show Debug Features': false,
}

export const STEGANOGRAPHY_ENABLED = false

let FeatureSwitchLoaded: { [feature in FeatureName]?: boolean } = {}

function settingKeyForName(featureName: FeatureName): string {
  return `Feature-Switch-${featureName}`
}

async function getFromLocalForage(featureName: FeatureName): Promise<boolean | null> {
  return localforage.getItem<boolean | null>(settingKeyForName(featureName))
}

async function getFromUrl(featureName: FeatureName): Promise<boolean | null> {
  const search = window?.location?.search
  if (search == null) {
    return null
  }
  const params = new URLSearchParams(search)
  const value = params.get(featureName)
  if (value === 'true') {
    return true
  }

  return null
}

async function loadStoredValue(featureName: FeatureName) {
  if (isBrowserEnvironment && !IS_TEST_ENVIRONMENT) {
    const existing = (await getFromUrl(featureName)) ?? (await getFromLocalForage(featureName))
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

export function toggleFeatureEnabled(featureName: FeatureName): boolean {
  const newValue = !isFeatureEnabled(featureName)
  FeatureSwitches[featureName] = newValue
  if (isBrowserEnvironment) {
    void localforage.setItem(settingKeyForName(featureName), newValue)
  }
  return newValue
}

export function setFeatureEnabled(featureName: FeatureName, newValue: boolean): void {
  FeatureSwitches[featureName] = newValue
  if (isBrowserEnvironment) {
    void localforage.setItem(settingKeyForName(featureName), newValue)
  }
}

export function getFeaturesToDisplay(): FeatureName[] {
  if (isFeatureEnabled('Show Debug Features')) {
    return AllFeatureNames
  } else {
    return AllFeatureNames.filter((name) => !name.startsWith('Debug'))
  }
}
