import { UtopiaVSCodeConfig } from './utopia-vscode-config'

export interface OpenFileMessage {
  type: 'OPEN_FILE'
  filePath: string
}

export function openFileMessage(filePath: string): OpenFileMessage {
  return {
    type: 'OPEN_FILE',
    filePath: filePath,
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

export interface SelectedElementChanged {
  type: 'SELECTED_ELEMENT_CHANGED'
  boundsInFile: BoundsInFile
}

export function selectedElementChanged(boundsInFile: BoundsInFile): SelectedElementChanged {
  return {
    type: 'SELECTED_ELEMENT_CHANGED',
    boundsInFile: boundsInFile,
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

export type ToVSCodeMessage =
  | OpenFileMessage
  | UpdateDecorationsMessage
  | SelectedElementChanged
  | GetUtopiaVSCodeConfig
  | SetFollowSelectionConfig

export function isOpenFileMessage(message: unknown): message is OpenFileMessage {
  return (
    typeof message === 'object' && !Array.isArray(message) && (message as any).type === 'OPEN_FILE'
  )
}

export function isUpdateDecorationsMessage(message: unknown): message is UpdateDecorationsMessage {
  return (
    typeof message === 'object' &&
    !Array.isArray(message) &&
    (message as any).type === 'UPDATE_DECORATIONS'
  )
}

export function isSelectedElementChanged(message: unknown): message is SelectedElementChanged {
  return (
    typeof message === 'object' &&
    !Array.isArray(message) &&
    (message as any).type === 'SELECTED_ELEMENT_CHANGED'
  )
}

export function isGetUtopiaVSCodeConfig(message: unknown): message is GetUtopiaVSCodeConfig {
  return (
    typeof message === 'object' &&
    !Array.isArray(message) &&
    (message as any).type === 'GET_UTOPIA_VSCODE_CONFIG'
  )
}

export function isSetFollowSelectionConfig(message: unknown): message is SetFollowSelectionConfig {
  return (
    typeof message === 'object' &&
    !Array.isArray(message) &&
    (message as any).type === 'SET_FOLLOW_SELECTION_CONFIG'
  )
}

export function parseToVSCodeMessage(unparsed: string): ToVSCodeMessage {
  const message = JSON.parse(unparsed)
  if (
    isOpenFileMessage(message) ||
    isUpdateDecorationsMessage(message) ||
    isSelectedElementChanged(message) ||
    isGetUtopiaVSCodeConfig(message) ||
    isSetFollowSelectionConfig(message)
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

export interface SendInitialData {
  type: 'SEND_INITIAL_DATA'
}

export function sendInitialData(): SendInitialData {
  return {
    type: 'SEND_INITIAL_DATA',
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

export type FromVSCodeMessage =
  | EditorCursorPositionChanged
  | SendInitialData
  | UtopiaVSCodeConfigValues

export function isEditorCursorPositionChanged(
  message: unknown,
): message is EditorCursorPositionChanged {
  return (
    typeof message === 'object' &&
    !Array.isArray(message) &&
    (message as any).type === 'EDITOR_CURSOR_POSITION_CHANGED'
  )
}

export function isSendInitialData(message: unknown): message is SendInitialData {
  return (
    typeof message === 'object' &&
    !Array.isArray(message) &&
    (message as any).type === 'SEND_INITIAL_DATA'
  )
}

export function isUtopiaVSCodeConfigValues(message: unknown): message is UtopiaVSCodeConfigValues {
  return (
    typeof message === 'object' &&
    !Array.isArray(message) &&
    (message as any).type === 'UTOPIA_VSCODE_CONFIG_VALUES'
  )
}

export function parseFromVSCodeMessage(unparsed: string): FromVSCodeMessage {
  const message = JSON.parse(unparsed)
  if (
    isEditorCursorPositionChanged(message) ||
    isSendInitialData(message) ||
    isUtopiaVSCodeConfigValues(message)
  ) {
    return message
  } else {
    // FIXME This should return an Either
    throw new Error(`Invalid message type ${JSON.stringify(message)}`)
  }
}
