type FSNodeType = 'FILE' | 'DIRECTORY'

export interface FSNode {
  type: FSNodeType
  ctime: number
  mtime: number
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

export function fsFile(content: Uint8Array, unsavedContent: Uint8Array | null, ctime: number, mtime: number): FSFile {
  return {
    type: 'FILE',
    ctime: ctime,
    mtime: mtime,
    content: content,
    unsavedContent: unsavedContent,
  }
}

export function newFSFile(content: Uint8Array, unsavedContent: Uint8Array | null): FSFile {
  const now = Date.now()
  return {
    type: 'FILE',
    ctime: now,
    mtime: now,
    content: content,
    unsavedContent: unsavedContent,
  }
}

export interface FSDirectory extends FSNode {
  type: 'DIRECTORY'
}

export function fsDirectory(ctime: number, mtime: number): FSDirectory {
  return {
    type: 'DIRECTORY',
    ctime: ctime,
    mtime: mtime,
  }
}

export function newFSDirectory(): FSDirectory {
  const now = Date.now()
  return {
    type: 'DIRECTORY',
    ctime: now,
    mtime: now,
  }
}

export function isFile(node: FSNode): node is FSFile {
  return node.type === 'FILE'
}

export function isDirectory(node: FSNode): node is FSDirectory {
  return node.type === 'DIRECTORY'
}

export type FSErrorCode = 'ENOENT' | 'EEXIST' | 'EISDIR' | 'ENOTDIR'
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
