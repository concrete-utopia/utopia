import type { UtopiaVSCodeConfig } from 'utopia-vscode-common'

export interface DeleteFileFromVSCode {
  // Exactly the same as the regular DeleteFile action, but signifies this came from the code editor
  action: 'DELETE_FILE_FROM_VSCODE'
  filename: string
}

export function deleteFileFromVSCode(filename: string): DeleteFileFromVSCode {
  return {
    action: 'DELETE_FILE_FROM_VSCODE',
    filename: filename,
  }
}

export interface HideVSCodeLoadingScreen {
  action: 'HIDE_VSCODE_LOADING_SCREEN'
}

export function hideVSCodeLoadingScreen(): HideVSCodeLoadingScreen {
  return {
    action: 'HIDE_VSCODE_LOADING_SCREEN',
  }
}

export interface MarkVSCodeBridgeReady {
  action: 'MARK_VSCODE_BRIDGE_READY'
  ready: boolean
}

export function markVSCodeBridgeReady(ready: boolean): MarkVSCodeBridgeReady {
  return {
    action: 'MARK_VSCODE_BRIDGE_READY',
    ready: ready,
  }
}

export interface SelectFromFileAndPosition {
  action: 'SELECT_FROM_FILE_AND_POSITION'
  filePath: string
  line: number
  column: number
}

export function selectFromFileAndPosition(
  filePath: string,
  line: number,
  column: number,
): SelectFromFileAndPosition {
  return {
    action: 'SELECT_FROM_FILE_AND_POSITION',
    filePath: filePath,
    line: line,
    column: column,
  }
}

export interface SendCodeEditorInitialisation {
  action: 'SEND_CODE_EDITOR_INITIALISATION'
}

export function sendCodeEditorInitialisation(): SendCodeEditorInitialisation {
  return {
    action: 'SEND_CODE_EDITOR_INITIALISATION',
  }
}

export interface SendLinterRequestMessage {
  action: 'SEND_LINTER_REQUEST_MESSAGE'
  filePath: string
  content: string
}

export function sendLinterRequestMessage(
  filePath: string,
  content: string,
): SendLinterRequestMessage {
  return {
    action: 'SEND_LINTER_REQUEST_MESSAGE',
    filePath: filePath,
    content: content,
  }
}

export interface SetIndexedDBFailed {
  action: 'SET_INDEXED_DB_FAILED'
  indexedDBFailed: boolean
}

export function setIndexedDBFailed(indexedDBFailed: boolean): SetIndexedDBFailed {
  return {
    action: 'SET_INDEXED_DB_FAILED',
    indexedDBFailed: indexedDBFailed,
  }
}

export interface UpdateConfigFromVSCode {
  action: 'UPDATE_CONFIG_FROM_VSCODE'
  config: UtopiaVSCodeConfig
}

export function updateConfigFromVSCode(config: UtopiaVSCodeConfig): UpdateConfigFromVSCode {
  return {
    action: 'UPDATE_CONFIG_FROM_VSCODE',
    config: config,
  }
}

export interface UpdateFromCodeEditor {
  action: 'UPDATE_FROM_CODE_EDITOR'
  filePath: string
  savedContent: string
  unsavedContent: string | null
}

export function updateFromCodeEditor(
  filePath: string,
  savedContent: string,
  unsavedContent: string | null,
): UpdateFromCodeEditor {
  return {
    action: 'UPDATE_FROM_CODE_EDITOR',
    filePath: filePath,
    savedContent: savedContent,
    unsavedContent: unsavedContent,
  }
}

export type FromVSCodeAction =
  | DeleteFileFromVSCode
  | HideVSCodeLoadingScreen
  | MarkVSCodeBridgeReady
  | SelectFromFileAndPosition
  | SendCodeEditorInitialisation
  | SendLinterRequestMessage
  | SetIndexedDBFailed
  | UpdateConfigFromVSCode
  | UpdateFromCodeEditor

export function isFromVSCodeAction(
  action: { action: string } & unknown,
): action is FromVSCodeAction {
  switch (action.action) {
    case 'DELETE_FILE_FROM_VSCODE':
    case 'HIDE_VSCODE_LOADING_SCREEN':
    case 'MARK_VSCODE_BRIDGE_READY':
    case 'SELECT_FROM_FILE_AND_POSITION':
    case 'SEND_CODE_EDITOR_INITIALISATION':
    case 'SEND_LINTER_REQUEST_MESSAGE':
    case 'SET_INDEXED_DB_FAILED':
    case 'UPDATE_CONFIG_FROM_VSCODE':
    case 'UPDATE_FROM_CODE_EDITOR':
      return true
    default:
      return false
  }
}
