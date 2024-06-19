import type { UtopiaVSCodeConfig } from './utopia-vscode-config'

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

export interface GetUtopiaVSCodeConfig {
  type: 'GET_UTOPIA_VSCODE_CONFIG'
}

export function getUtopiaVSCodeConfig(): GetUtopiaVSCodeConfig {
  return {
    type: 'GET_UTOPIA_VSCODE_CONFIG',
  }
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

export interface UtopiaReady {
  type: 'UTOPIA_READY'
}

export function utopiaReady(): UtopiaReady {
  return {
    type: 'UTOPIA_READY',
  }
}

export type ToVSCodeMessageNoAccumulated =
  | OpenFileMessage
  | UpdateDecorationsMessage
  | SelectedElementChanged
  | GetUtopiaVSCodeConfig
  | SetFollowSelectionConfig
  | SetVSCodeTheme
  | UtopiaReady

export interface AccumulatedToVSCodeMessage {
  type: 'ACCUMULATED_TO_VSCODE_MESSAGE'
  messages: Array<ToVSCodeMessageNoAccumulated>
}

export function accumulatedToVSCodeMessage(
  messages: Array<ToVSCodeMessageNoAccumulated>,
): AccumulatedToVSCodeMessage {
  return {
    type: 'ACCUMULATED_TO_VSCODE_MESSAGE',
    messages: messages,
  }
}

export type ToVSCodeMessage = ToVSCodeMessageNoAccumulated | AccumulatedToVSCodeMessage

export function isOpenFileMessage(message: unknown): message is OpenFileMessage {
  return (
    typeof message === 'object' &&
    !Array.isArray(message) &&
    (message as OpenFileMessage).type === 'OPEN_FILE'
  )
}

export function isUpdateDecorationsMessage(message: unknown): message is UpdateDecorationsMessage {
  return (
    typeof message === 'object' &&
    !Array.isArray(message) &&
    (message as UpdateDecorationsMessage).type === 'UPDATE_DECORATIONS'
  )
}

export function isSelectedElementChanged(message: unknown): message is SelectedElementChanged {
  return (
    typeof message === 'object' &&
    !Array.isArray(message) &&
    (message as SelectedElementChanged).type === 'SELECTED_ELEMENT_CHANGED'
  )
}

export function isGetUtopiaVSCodeConfig(message: unknown): message is GetUtopiaVSCodeConfig {
  return (
    typeof message === 'object' &&
    !Array.isArray(message) &&
    (message as GetUtopiaVSCodeConfig).type === 'GET_UTOPIA_VSCODE_CONFIG'
  )
}

export function isSetFollowSelectionConfig(message: unknown): message is SetFollowSelectionConfig {
  return (
    typeof message === 'object' &&
    !Array.isArray(message) &&
    (message as SetFollowSelectionConfig).type === 'SET_FOLLOW_SELECTION_CONFIG'
  )
}

export function isSetVSCodeTheme(message: unknown): message is SetVSCodeTheme {
  return (
    typeof message === 'object' &&
    !Array.isArray(message) &&
    (message as SetVSCodeTheme).type === 'SET_VSCODE_THEME'
  )
}

export function isUtopiaReadyMessage(message: unknown): message is UtopiaReady {
  return (
    typeof message === 'object' &&
    !Array.isArray(message) &&
    (message as UtopiaReady).type === 'UTOPIA_READY'
  )
}

export function isAccumulatedToVSCodeMessage(
  message: unknown,
): message is AccumulatedToVSCodeMessage {
  return (
    typeof message === 'object' &&
    !Array.isArray(message) &&
    (message as AccumulatedToVSCodeMessage).type === 'ACCUMULATED_TO_VSCODE_MESSAGE'
  )
}

export function parseToVSCodeMessage(unparsed: string): ToVSCodeMessage {
  const message = JSON.parse(unparsed)
  if (
    isOpenFileMessage(message) ||
    isUpdateDecorationsMessage(message) ||
    isSelectedElementChanged(message) ||
    isGetUtopiaVSCodeConfig(message) ||
    isSetFollowSelectionConfig(message) ||
    isSetVSCodeTheme(message) ||
    isUtopiaReadyMessage(message) ||
    isAccumulatedToVSCodeMessage(message)
  ) {
    return message
  } else {
    // FIXME This should return an Either
    throw new Error(`Invalid message type ${JSON.stringify(message)}`)
  }
}

export interface EditorCursorPositionChanged {
  type: 'EDITOR_CURSOR_POSITION_CHANGED'
  filePath: string
  line: number
  column: number
}

export function editorCursorPositionChanged(
  filePath: string,
  line: number,
  column: number,
): EditorCursorPositionChanged {
  return {
    type: 'EDITOR_CURSOR_POSITION_CHANGED',
    filePath: filePath,
    line: line,
    column: column,
  }
}

export interface UtopiaVSCodeConfigValues {
  type: 'UTOPIA_VSCODE_CONFIG_VALUES'
  config: UtopiaVSCodeConfig
}

export function utopiaVSCodeConfigValues(config: UtopiaVSCodeConfig): UtopiaVSCodeConfigValues {
  return {
    type: 'UTOPIA_VSCODE_CONFIG_VALUES',
    config: config,
  }
}

export interface VSCodeReady {
  type: 'VSCODE_READY'
}

export function vsCodeReady(): VSCodeReady {
  return {
    type: 'VSCODE_READY',
  }
}

export interface ClearLoadingScreen {
  type: 'CLEAR_LOADING_SCREEN'
}

export function clearLoadingScreen(): ClearLoadingScreen {
  return {
    type: 'CLEAR_LOADING_SCREEN',
  }
}

export type FromVSCodeMessage =
  | EditorCursorPositionChanged
  | UtopiaVSCodeConfigValues
  | VSCodeReady
  | ClearLoadingScreen

export function isEditorCursorPositionChanged(
  message: unknown,
): message is EditorCursorPositionChanged {
  return (
    typeof message === 'object' &&
    !Array.isArray(message) &&
    (message as EditorCursorPositionChanged).type === 'EDITOR_CURSOR_POSITION_CHANGED'
  )
}

export function isUtopiaVSCodeConfigValues(message: unknown): message is UtopiaVSCodeConfigValues {
  return (
    typeof message === 'object' &&
    !Array.isArray(message) &&
    (message as UtopiaVSCodeConfigValues).type === 'UTOPIA_VSCODE_CONFIG_VALUES'
  )
}

export function isVSCodeReady(message: unknown): message is VSCodeReady {
  return (
    typeof message === 'object' &&
    !Array.isArray(message) &&
    (message as VSCodeReady).type === 'VSCODE_READY'
  )
}

export function isClearLoadingScreen(message: unknown): message is ClearLoadingScreen {
  return (
    typeof message === 'object' &&
    !Array.isArray(message) &&
    (message as ClearLoadingScreen).type === 'CLEAR_LOADING_SCREEN'
  )
}

export function parseFromVSCodeMessage(unparsed: string): FromVSCodeMessage {
  const message = JSON.parse(unparsed)
  if (
    isEditorCursorPositionChanged(message) ||
    isUtopiaVSCodeConfigValues(message) ||
    isVSCodeReady(message) ||
    isClearLoadingScreen(message)
  ) {
    return message
  } else {
    // FIXME This should return an Either
    throw new Error(`Invalid message type ${JSON.stringify(message)}`)
  }
}
