import localforage from 'localforage'
import type { StoredLayout } from '../canvas/stored-layout'
import { GridMenuDefaultPanels } from '../canvas/stored-layout'

export type UserPreferences = {
  storedLayout: StoredLayout
}

export const USER_PREFERENCES_KEY = 'utopia.userPreferences'

export function defaultUserPreferences(): UserPreferences {
  return {
    storedLayout: GridMenuDefaultPanels,
  }
}

export async function saveUserPreferences(prefs: Partial<UserPreferences>): Promise<void> {
  const currentPrefs = await loadUserPreferences()
  const newPrefs = {
    ...currentPrefs,
    ...prefs,
  }
  await localforage.setItem(USER_PREFERENCES_KEY, newPrefs)
}

export async function loadUserPreferences(): Promise<UserPreferences> {
  const stored = await localforage.getItem<UserPreferences | null>(USER_PREFERENCES_KEY)
  return stored ?? defaultUserPreferences()
}
