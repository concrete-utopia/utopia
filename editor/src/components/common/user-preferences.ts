import localforage from 'localforage'
import type { StoredLayout } from '../canvas/stored-layout'
import { gridMenuDefaultPanels } from '../canvas/stored-layout'

export type UserPreferences = {
  panelsLayout: PanelsLayout
}

type PanelsLayout = {
  // the default layout for new projects
  default: StoredLayout
  // per-project layouts
  project: {
    [key: string]: StoredLayout
  }
}

export function getProjectStoredLayoutOrDefault(
  layout: PanelsLayout,
  projectId: string,
): StoredLayout {
  const projectLayout = layout.project[projectId]
  return projectLayout ?? layout.default
}

export const USER_PREFERENCES_KEY = 'utopia.userPreferences'

export function defaultUserPreferences(): UserPreferences {
  return {
    panelsLayout: {
      default: gridMenuDefaultPanels(),
      project: {},
    },
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

export async function saveUserPreferencesDefaultLayout(layout: StoredLayout): Promise<void> {
  const prefs = await loadUserPreferences()
  prefs.panelsLayout.default = layout
  await localforage.setItem(USER_PREFERENCES_KEY, prefs)
}

export async function saveUserPreferencesProjectLayout(
  projectId: string,
  layout: StoredLayout,
): Promise<void> {
  const prefs = await loadUserPreferences()
  prefs.panelsLayout.project[projectId] = layout
  return saveUserPreferences(prefs)
}

export async function loadUserPreferences(): Promise<UserPreferences> {
  const stored = await localforage.getItem<UserPreferences | null>(USER_PREFERENCES_KEY)
  return stored ?? defaultUserPreferences()
}
