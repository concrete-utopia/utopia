import type { UtopiaVSCodeConfig } from './utopia-vscode-config'

export interface MessageListenersReady {
  type: 'MESSAGE_LISTENERS_READY'
}

export function messageListenersReady(): MessageListenersReady {
  return {
    type: 'MESSAGE_LISTENERS_READY',
  }
}

export function isMessageListenersReady(
  messageData: unknown,
): messageData is MessageListenersReady {
  return (
    typeof messageData === 'object' && (messageData as any)?.['type'] === 'MESSAGE_LISTENERS_READY'
  )
}

interface StoredFile {
  content: string
  unsavedContent: string | null
}

export interface VSCodeFileChange {
  type: 'VSCODE_FILE_CHANGE'
  filePath: string
  fileContent: StoredFile
}

export function vsCodeFileChange(filePath: string, fileContent: StoredFile): VSCodeFileChange {
  return {
    type: 'VSCODE_FILE_CHANGE',
    filePath: filePath,
    fileContent: fileContent,
  }
}

export function isVSCodeFileChange(messageData: unknown): messageData is VSCodeFileChange {
  return typeof messageData === 'object' && (messageData as any)?.['type'] === 'VSCODE_FILE_CHANGE'
}

export interface VSCodeFileDelete {
  type: 'VSCODE_FILE_DELETE'
  filePath: string
}

export function vsCodeFileDelete(filePath: string): VSCodeFileDelete {
  return {
    type: 'VSCODE_FILE_DELETE',
    filePath: filePath,
  }
}

export function isVSCodeFileDelete(messageData: unknown): messageData is VSCodeFileDelete {
  return typeof messageData === 'object' && (messageData as any)?.['type'] === 'VSCODE_FILE_DELETE'
}

export interface VSCodeBridgeReady {
  type: 'VSCODE_BRIDGE_READY'
}

export function vsCodeBridgeReady(): VSCodeBridgeReady {
  return {
    type: 'VSCODE_BRIDGE_READY',
  }
}

export function isVSCodeBridgeReady(messageData: unknown): messageData is VSCodeBridgeReady {
  return typeof messageData === 'object' && (messageData as any)?.['type'] === 'VSCODE_BRIDGE_READY'
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

export function isEditorCursorPositionChanged(
  messageData: unknown,
): messageData is EditorCursorPositionChanged {
  return (
    typeof messageData === 'object' &&
    (messageData as any)?.['type'] === 'EDITOR_CURSOR_POSITION_CHANGED'
  )
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

export function isUtopiaVSCodeConfigValues(
  messageData: unknown,
): messageData is UtopiaVSCodeConfigValues {
  return (
    typeof messageData === 'object' &&
    (messageData as any)?.['type'] === 'UTOPIA_VSCODE_CONFIG_VALUES'
  )
}

export interface VSCodeReady {
  type: 'VSCODE_READY'
}

export function vsCodeReady(): VSCodeReady {
  return {
    type: 'VSCODE_READY',
  }
}

export function isVSCodeReady(messageData: unknown): messageData is VSCodeReady {
  return typeof messageData === 'object' && (messageData as any)?.['type'] === 'VSCODE_READY'
}

export interface ClearLoadingScreen {
  type: 'CLEAR_LOADING_SCREEN'
}

export function clearLoadingScreen(): ClearLoadingScreen {
  return {
    type: 'CLEAR_LOADING_SCREEN',
  }
}

export function isClearLoadingScreen(messageData: unknown): messageData is ClearLoadingScreen {
  return (
    typeof messageData === 'object' && (messageData as any)?.['type'] === 'CLEAR_LOADING_SCREEN'
  )
}

export type FromVSCodeToUtopiaMessage =
  | MessageListenersReady
  | VSCodeFileChange
  | VSCodeFileDelete
  | VSCodeBridgeReady
  | EditorCursorPositionChanged
  | UtopiaVSCodeConfigValues
  | VSCodeReady
  | ClearLoadingScreen
