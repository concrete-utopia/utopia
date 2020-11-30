export interface OpenFileTab {
  type: 'OPEN_FILE_TAB'
  filename: string
}

export function openFileTab(filename: string): OpenFileTab {
  return {
    type: 'OPEN_FILE_TAB',
    filename: filename,
  }
}

export interface ReleaseNotesTab {
  type: 'RELEASE_NOTES_TAB'
}

export function releaseNotesTab(): ReleaseNotesTab {
  return {
    type: 'RELEASE_NOTES_TAB',
  }
}

export interface UserConfigurationTab {
  type: 'USER_CONFIGURATION_TAB'
}

export function userConfigurationTab(): UserConfigurationTab {
  return {
    type: 'USER_CONFIGURATION_TAB',
  }
}

export type EditorTab = OpenFileTab | ReleaseNotesTab | UserConfigurationTab

export function isOpenFileTab(editorTab: EditorTab): editorTab is OpenFileTab {
  return editorTab.type === 'OPEN_FILE_TAB'
}

export function isReleaseNotesTab(editorTab: EditorTab): editorTab is ReleaseNotesTab {
  return editorTab.type === 'RELEASE_NOTES_TAB'
}

export function isUserConfigurationTab(editorTab: EditorTab): editorTab is UserConfigurationTab {
  return editorTab.type === 'USER_CONFIGURATION_TAB'
}
