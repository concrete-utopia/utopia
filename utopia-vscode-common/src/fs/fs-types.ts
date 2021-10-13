type FSNodeType = 'FILE' | 'DIRECTORY'
export type FSUser = 'UTOPIA' | 'VSCODE'

export interface FSNode {
  type: FSNodeType
  ctime: number
  mtime: number
  lastSavedTime: number
  sourceOfLastChange: FSUser
}

export interface FSNodeWithPath {
  path: string
  node: FSNode
}

export interface FSStat extends FSNode {
  size: number
}

export interface FileContent {
  content: Uint8Array
  unsavedContent: Uint8Array | null
}

export interface FSFile extends FSNode, FileContent {
  type: 'FILE'
}

export function fsFile(
  content: Uint8Array,
  unsavedContent: Uint8Array | null,
  ctime: number,
  mtime: number,
  lastSavedTime: number,
  sourceOfLastChange: FSUser,
): FSFile {
  return {
    type: 'FILE',
    ctime: ctime,
    mtime: mtime,
    lastSavedTime: lastSavedTime,
    content: content,
    unsavedContent: unsavedContent,
    sourceOfLastChange: sourceOfLastChange,
  }
}

export interface FSDirectory extends FSNode {
  type: 'DIRECTORY'
}

export function fsDirectory(ctime: number, mtime: number, sourceOfLastChange: FSUser): FSDirectory {
  return {
    type: 'DIRECTORY',
    ctime: ctime,
    mtime: mtime,
    lastSavedTime: mtime,
    sourceOfLastChange: sourceOfLastChange,
  }
}

export function newFSDirectory(sourceOfLastChange: FSUser): FSDirectory {
  const now = Date.now()
  return {
    type: 'DIRECTORY',
    ctime: now,
    mtime: now,
    lastSavedTime: now,
    sourceOfLastChange: sourceOfLastChange,
  }
}

export function isFile(node: FSNode): node is FSFile {
  return node.type === 'FILE'
}

export function isDirectory(node: FSNode): node is FSDirectory {
  return node.type === 'DIRECTORY'
}

export type FSErrorCode = 'ENOENT' | 'EEXIST' | 'EISDIR' | 'ENOTDIR' | 'FS_UNAVAILABLE'
export interface FSError {
  code: FSErrorCode
  path: string
}

export type FSErrorHandler = (e: FSError) => Error

function fsError(code: FSErrorCode, path: string): FSError {
  return {
    code: code,
    path: path,
  }
}

export const enoent = (path: string) => fsError('ENOENT', path)
export const eexist = (path: string) => fsError('EEXIST', path)
export const eisdir = (path: string) => fsError('EISDIR', path)
export const enotdir = (path: string) => fsError('ENOTDIR', path)
export const fsUnavailable = (path: string) => fsError('FS_UNAVAILABLE', path)
