import type { UtopiaVSCodeConfig } from './utopia-vscode-config'

export interface ProjectDirectory {
  type: 'PROJECT_DIRECTORY'
  filePath: string
}

export function projectDirectory(filePath: string): ProjectDirectory {
  return {
    type: 'PROJECT_DIRECTORY',
    filePath: filePath,
  }
}

export interface ProjectTextFile {
  type: 'PROJECT_TEXT_FILE'
  filePath: string
  savedContent: string
  unsavedContent: string | null
}

export function projectTextFile(
  filePath: string,
  savedContent: string,
  unsavedContent: string | null,
): ProjectTextFile {
  return {
    type: 'PROJECT_TEXT_FILE',
    filePath: filePath,
    savedContent: savedContent,
    unsavedContent: unsavedContent,
  }
}

export type ProjectFile = ProjectDirectory | ProjectTextFile

export interface InitProject {
  type: 'INIT_PROJECT'
  projectContents: Array<ProjectFile>
  openFilePath: string | null
}

export function initProject(
  projectContents: Array<ProjectFile>,
  openFilePath: string | null,
): InitProject {
  return {
    type: 'INIT_PROJECT',
    projectContents: projectContents,
    openFilePath: openFilePath,
  }
}

export function isInitProject(messageData: unknown): messageData is InitProject {
  return typeof messageData === 'object' && (messageData as any)?.['type'] === 'INIT_PROJECT'
}

export interface WriteProjectFileChange {
  type: 'WRITE_PROJECT_FILE'
  projectFile: ProjectFile
}

export function writeProjectFileChange(projectFile: ProjectFile): WriteProjectFileChange {
  return {
    type: 'WRITE_PROJECT_FILE',
    projectFile: projectFile,
  }
}

export function isWriteProjectFileChange(
  messageData: unknown,
): messageData is WriteProjectFileChange {
  return typeof messageData === 'object' && (messageData as any)?.['type'] === 'WRITE_PROJECT_FILE'
}

export interface DeletePathChange {
  type: 'DELETE_PATH'
  fullPath: string
  recursive: boolean
}

export function deletePathChange(fullPath: string, recursive: boolean): DeletePathChange {
  return {
    type: 'DELETE_PATH',
    fullPath: fullPath,
    recursive: recursive,
  }
}

export function isDeletePathChange(messageData: unknown): messageData is DeletePathChange {
  return typeof messageData === 'object' && (messageData as any)?.['type'] === 'DELETE_PATH'
}

export interface EnsureDirectoryExistsChange {
  type: 'ENSURE_DIRECTORY_EXISTS'
  fullPath: string
}

export function ensureDirectoryExistsChange(fullPath: string): EnsureDirectoryExistsChange {
  return {
    type: 'ENSURE_DIRECTORY_EXISTS',
    fullPath: fullPath,
  }
}

export function isEnsureDirectoryExistsChange(
  messageData: unknown,
): messageData is EnsureDirectoryExistsChange {
  return (
    typeof messageData === 'object' && (messageData as any)?.['type'] === 'ENSURE_DIRECTORY_EXISTS'
  )
}

export interface OpenFileMessage {
  type: 'OPEN_FILE'
  filePath: string
  bounds: Bounds | null
}

export function openFileMessage(filePath: string, bounds: Bounds | null): OpenFileMessage {
  return {
    type: 'OPEN_FILE',
    filePath: filePath,
    bounds: bounds,
  }
}

export function isOpenFileMessage(messageData: unknown): messageData is OpenFileMessage {
  return typeof messageData === 'object' && (messageData as any)?.['type'] === 'OPEN_FILE'
}

export type DecorationRangeType = 'selection' | 'highlight'

export interface Bounds {
  startLine: number
  startCol: number
  endLine: number
  endCol: number
}

export interface BoundsInFile extends Bounds {
  filePath: string
}

export function boundsInFile(
  filePath: string,
  startLine: number,
  startCol: number,
  endLine: number,
  endCol: number,
): BoundsInFile {
  return {
    filePath: filePath,
    startLine: startLine,
    startCol: startCol,
    endLine: endLine,
    endCol: endCol,
  }
}

export interface DecorationRange extends BoundsInFile {
  rangeType: DecorationRangeType
}

export function decorationRange(
  rangeType: DecorationRangeType,
  filePath: string,
  startLine: number,
  startCol: number,
  endLine: number,
  endCol: number,
): DecorationRange {
  return {
    rangeType: rangeType,
    filePath: filePath,
    startLine: startLine,
    startCol: startCol,
    endLine: endLine,
    endCol: endCol,
  }
}

export interface UpdateDecorationsMessage {
  type: 'UPDATE_DECORATIONS'
  decorations: Array<DecorationRange>
}

export function updateDecorationsMessage(
  decorations: Array<DecorationRange>,
): UpdateDecorationsMessage {
  return {
    type: 'UPDATE_DECORATIONS',
    decorations: decorations,
  }
}

export function isUpdateDecorationsMessage(
  messageData: unknown,
): messageData is UpdateDecorationsMessage {
  return typeof messageData === 'object' && (messageData as any)?.['type'] === 'UPDATE_DECORATIONS'
}

export type ForceNavigation = 'do-not-force-navigation' | 'force-navigation'

export interface SelectedElementChanged {
  type: 'SELECTED_ELEMENT_CHANGED'
  boundsInFile: BoundsInFile
  forceNavigation: ForceNavigation
}

export function selectedElementChanged(
  bounds: BoundsInFile,
  forceNavigation: ForceNavigation,
): SelectedElementChanged {
  return {
    type: 'SELECTED_ELEMENT_CHANGED',
    boundsInFile: bounds,
    forceNavigation: forceNavigation,
  }
}

export function isSelectedElementChanged(
  messageData: unknown,
): messageData is SelectedElementChanged {
  return (
    typeof messageData === 'object' && (messageData as any)?.['type'] === 'SELECTED_ELEMENT_CHANGED'
  )
}

export interface GetUtopiaVSCodeConfig {
  type: 'GET_UTOPIA_VSCODE_CONFIG'
}

export function getUtopiaVSCodeConfig(): GetUtopiaVSCodeConfig {
  return {
    type: 'GET_UTOPIA_VSCODE_CONFIG',
  }
}

export function isGetUtopiaVSCodeConfig(
  messageData: unknown,
): messageData is GetUtopiaVSCodeConfig {
  return (
    typeof messageData === 'object' && (messageData as any)?.['type'] === 'GET_UTOPIA_VSCODE_CONFIG'
  )
}

export interface SetFollowSelectionConfig {
  type: 'SET_FOLLOW_SELECTION_CONFIG'
  enabled: boolean
}

export function setFollowSelectionConfig(enabled: boolean): SetFollowSelectionConfig {
  return {
    type: 'SET_FOLLOW_SELECTION_CONFIG',
    enabled: enabled,
  }
}

export function isSetFollowSelectionConfig(
  messageData: unknown,
): messageData is SetFollowSelectionConfig {
  return (
    typeof messageData === 'object' &&
    (messageData as any)?.['type'] === 'SET_FOLLOW_SELECTION_CONFIG'
  )
}

export interface SetVSCodeTheme {
  type: 'SET_VSCODE_THEME'
  theme: string
}

export function setVSCodeTheme(theme: string): SetVSCodeTheme {
  return {
    type: 'SET_VSCODE_THEME',
    theme: theme,
  }
}

export function isSetVSCodeTheme(messageData: unknown): messageData is SetVSCodeTheme {
  return typeof messageData === 'object' && (messageData as any)?.['type'] === 'SET_VSCODE_THEME'
}

export interface UtopiaReady {
  type: 'UTOPIA_READY'
}

export function utopiaReady(): UtopiaReady {
  return {
    type: 'UTOPIA_READY',
  }
}

export function isUtopiaReady(messageData: unknown): messageData is UtopiaReady {
  return typeof messageData === 'object' && (messageData as any)?.['type'] === 'UTOPIA_READY'
}

export function isFromUtopiaToVSCodeMessage(
  messageData: unknown,
): messageData is FromUtopiaToVSCodeMessage {
  return (
    isInitProject(messageData) ||
    isWriteProjectFileChange(messageData) ||
    isDeletePathChange(messageData) ||
    isEnsureDirectoryExistsChange(messageData) ||
    isOpenFileMessage(messageData) ||
    isUpdateDecorationsMessage(messageData) ||
    isSelectedElementChanged(messageData) ||
    isGetUtopiaVSCodeConfig(messageData) ||
    isSetFollowSelectionConfig(messageData) ||
    isSetVSCodeTheme(messageData) ||
    isUtopiaReady(messageData)
  )
}

export type FromUtopiaToVSCodeMessage =
  | InitProject
  | WriteProjectFileChange
  | DeletePathChange
  | EnsureDirectoryExistsChange
  | OpenFileMessage
  | UpdateDecorationsMessage
  | SelectedElementChanged
  | GetUtopiaVSCodeConfig
  | SetFollowSelectionConfig
  | SetVSCodeTheme
  | UtopiaReady
