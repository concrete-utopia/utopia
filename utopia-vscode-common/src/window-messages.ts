import type { StoredFile } from './fs/fs-utils'
import type { FromVSCodeMessage, ToVSCodeMessage } from './messages'

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

// Message Types To VS Code
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

export interface ToVSCodeExtensionMessage {
  type: 'TO_VSCODE_EXTENSION_MESSAGE'
  message: ToVSCodeMessage
}

export function toVSCodeExtensionMessage(message: ToVSCodeMessage): ToVSCodeExtensionMessage {
  return {
    type: 'TO_VSCODE_EXTENSION_MESSAGE',
    message: message,
  }
}

export function isToVSCodeExtensionMessage(
  messageData: unknown,
): messageData is ToVSCodeExtensionMessage {
  return (
    typeof messageData === 'object' &&
    (messageData as any)?.['type'] === 'TO_VSCODE_EXTENSION_MESSAGE'
  )
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

export type FromUtopiaToVSCodeMessage =
  | InitProject
  | ToVSCodeExtensionMessage
  | WriteProjectFileChange
  | DeletePathChange
  | EnsureDirectoryExistsChange

// Message Types To Utopia
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

export interface FromVSCodeExtensionMessage {
  type: 'FROM_VSCODE_EXTENSION_MESSAGE'
  message: FromVSCodeMessage
}

export function fromVSCodeExtensionMessage(message: FromVSCodeMessage): FromVSCodeExtensionMessage {
  return {
    type: 'FROM_VSCODE_EXTENSION_MESSAGE',
    message: message,
  }
}

export function isFromVSCodeExtensionMessage(
  messageData: unknown,
): messageData is FromVSCodeExtensionMessage {
  return (
    typeof messageData === 'object' &&
    (messageData as any)?.['type'] === 'FROM_VSCODE_EXTENSION_MESSAGE'
  )
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

export interface IndexedDBFailure {
  type: 'INDEXED_DB_FAILURE'
}

export function indexedDBFailure(): IndexedDBFailure {
  return {
    type: 'INDEXED_DB_FAILURE',
  }
}

export function isIndexedDBFailure(messageData: unknown): messageData is IndexedDBFailure {
  return typeof messageData === 'object' && (messageData as any)?.['type'] === 'INDEXED_DB_FAILURE'
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

export type FromVSCodeToUtopiaMessage =
  | MessageListenersReady
  | FromVSCodeExtensionMessage
  | VSCodeFileChange
  | VSCodeFileDelete
  | IndexedDBFailure
  | VSCodeBridgeReady
