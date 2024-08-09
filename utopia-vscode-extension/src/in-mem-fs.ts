import { stripTrailingSlash } from 'utopia-vscode-common'
import { getParentPath } from './path-utils'

type FSNodeType = 'FILE' | 'DIRECTORY'

interface FSNode {
  type: FSNodeType
  ctime: number
  mtime: number
  lastSavedTime: number
}

interface FSNodeWithPath {
  path: string
  node: FSNode
}

interface FSStat extends FSNode {
  size: number
}

interface FileContent {
  content: Uint8Array
  unsavedContent: Uint8Array | null
}

interface FSFile extends FSNode, FileContent {
  type: 'FILE'
}

function fsFile(
  content: Uint8Array,
  unsavedContent: Uint8Array | null,
  ctime: number,
  mtime: number,
  lastSavedTime: number,
): FSFile {
  return {
    type: 'FILE',
    ctime: ctime,
    mtime: mtime,
    lastSavedTime: lastSavedTime,
    content: content,
    unsavedContent: unsavedContent,
  }
}

interface FSDirectory extends FSNode {
  type: 'DIRECTORY'
}

function fsDirectory(ctime: number, mtime: number): FSDirectory {
  return {
    type: 'DIRECTORY',
    ctime: ctime,
    mtime: mtime,
    lastSavedTime: mtime,
  }
}

export function newFSDirectory(): FSDirectory {
  const now = Date.now()
  return {
    type: 'DIRECTORY',
    ctime: now,
    mtime: now,
    lastSavedTime: now,
  }
}

export function isFile(node: FSNode): node is FSFile {
  return node.type === 'FILE'
}

export function isDirectory(node: FSNode): node is FSDirectory {
  return node.type === 'DIRECTORY'
}

type FSErrorCode = 'ENOENT' | 'EEXIST' | 'EISDIR' | 'ENOTDIR'
export interface FSError {
  code: FSErrorCode
  path: string
}

type FSErrorHandler = (e: FSError) => Error

function fsError(code: FSErrorCode, path: string): FSError {
  return {
    code: code,
    path: path,
  }
}

const encoder = new TextEncoder()
const decoder = new TextDecoder()

const Store = new Map<string, FSNode>()
Store.set('', newFSDirectory())

export function keys(): IterableIterator<string> {
  return Store.keys()
}

export function getItem(path: string): FSNode | undefined {
  return Store.get(stripTrailingSlash(path))
}

export function setItem(path: string, value: FSNode) {
  Store.set(stripTrailingSlash(path), value)
}

export function removeItem(path: string) {
  Store.delete(stripTrailingSlash(path))
}

let handleError: FSErrorHandler = (e: FSError) => {
  let error = Error(`FS Error: ${JSON.stringify(e)}`)
  error.name = e.code
  return error
}

export function setErrorHandler(handler: FSErrorHandler): void {
  handleError = handler
}

const missingFileError = (path: string) => handleError(fsError('ENOENT', path))
const existingFileError = (path: string) => handleError(fsError('EEXIST', path))
const isDirectoryError = (path: string) => handleError(fsError('EISDIR', path))
export const isNotDirectoryError = (path: string) => handleError(fsError('ENOTDIR', path))

export function exists(path: string): boolean {
  const value = getItem(path)
  return value != null
}

export function pathIsDirectory(path: string): boolean {
  const node = getItem(path)
  return node != null && isDirectory(node)
}

export function pathIsFile(path: string): boolean {
  const node = getItem(path)
  return node != null && isFile(node)
}

export function pathIsFileWithUnsavedContent(path: string): boolean {
  const node = getItem(path)
  return node != null && isFile(node) && node.unsavedContent != null
}

function getNode(path: string): FSNode {
  const node = getItem(path)
  if (node == null) {
    throw missingFileError(path)
  } else {
    return node
  }
}

function getFile(path: string): FSFile {
  const node = getNode(path)
  if (isFile(node)) {
    return node
  } else {
    throw isDirectoryError(path)
  }
}

export function readFile(path: string): FileContent {
  return getFile(path)
}

export function readFileSavedContent(path: string): Uint8Array {
  const fileNode = getFile(path)
  return fileNode.content
}

export function readFileUnsavedContent(path: string): Uint8Array | null {
  const fileNode = getFile(path)
  return fileNode.unsavedContent
}

export interface StoredFile {
  content: string
  unsavedContent: string | null
}

export function readFileAsUTF8(path: string): StoredFile {
  const { content, unsavedContent } = getFile(path)
  return {
    content: decoder.decode(content),
    unsavedContent: unsavedContent == null ? null : decoder.decode(unsavedContent),
  }
}

export function readFileSavedContentAsUTF8(path: string): string {
  const { content } = readFileAsUTF8(path)
  return content
}

export function readFileUnsavedContentAsUTF8(path: string): string | null {
  const { unsavedContent } = readFileAsUTF8(path)
  return unsavedContent
}

function fsStatForNode(node: FSNode): FSStat {
  return {
    type: node.type,
    ctime: node.ctime,
    mtime: node.mtime,
    lastSavedTime: node.lastSavedTime,
    size: isFile(node) ? node.content.length : 0,
  }
}

export function stat(path: string): FSStat {
  const node = getNode(path)
  return fsStatForNode(node)
}

export function getDescendentPathsWithAllPaths(
  path: string,
  allPaths: Array<string>,
): Array<string> {
  return allPaths.filter((k) => k != path && k.startsWith(path))
}

export function getDescendentPaths(path: string): string[] {
  const allPaths = keys()
  return getDescendentPathsWithAllPaths(path, Array.from(allPaths))
}

function targetsForOperation(path: string, recursive: boolean): string[] {
  if (recursive) {
    const allDescendents = getDescendentPaths(path)
    let result = [path, ...allDescendents]
    result.sort()
    result.reverse()
    return result
  } else {
    return [path]
  }
}

function filenameOfPath(path: string): string {
  const target = path.endsWith('/') ? path.slice(0, -1) : path
  const lastSlashIndex = target.lastIndexOf('/')
  return lastSlashIndex >= 0 ? path.slice(lastSlashIndex + 1) : path
}

export function childPaths(path: string): Array<string> {
  const allDescendents = getDescendentPaths(path)
  const pathAsDir = stripTrailingSlash(path)
  return allDescendents.filter((k) => getParentPath(k) === pathAsDir)
}

function getDirectory(path: string): FSDirectory {
  const node = getNode(path)
  if (isDirectory(node)) {
    return node
  } else {
    throw isNotDirectoryError(path)
  }
}

function getParent(path: string): FSNodeWithPath | null {
  // null signifies we're already at the root
  const parentPath = getParentPath(path)
  if (parentPath == null) {
    return null
  } else {
    const parentDir = getDirectory(parentPath)
    return {
      path: parentPath,
      node: parentDir,
    }
  }
}

export function readDirectory(path: string): Array<string> {
  getDirectory(path) // Ensure the path exists and is a directory
  const children = childPaths(path)
  return children.map(filenameOfPath)
}

export function createDirectory(path: string) {
  if (exists(path)) {
    throw existingFileError(path)
  }

  createDirectoryWithoutError(path)
}

export function createDirectoryWithoutError(path: string) {
  setItem(path, newFSDirectory())

  const parent = getParent(path)
  if (parent != null) {
    markModified(parent)
  }
}

export function writeFile(path: string, content: Uint8Array, unsavedContent: Uint8Array | null) {
  const parent = getParent(path)
  const maybeExistingFile = getItem(path)
  if (maybeExistingFile != null && isDirectory(maybeExistingFile)) {
    throw isDirectoryError(path)
  }

  const now = Date.now()
  const fileCTime = maybeExistingFile == null ? now : maybeExistingFile.ctime
  const lastSavedTime =
    unsavedContent == null || maybeExistingFile == null ? now : maybeExistingFile.lastSavedTime
  const fileToWrite = fsFile(content, unsavedContent, fileCTime, now, lastSavedTime)
  setItem(path, fileToWrite)
  if (parent != null) {
    markModified(parent)
  }
}

export function writeFileSavedContent(path: string, content: Uint8Array) {
  writeFile(path, content, null)
}

export function writeFileUnsavedContent(path: string, unsavedContent: Uint8Array) {
  const savedContent = readFileSavedContent(path)
  writeFile(path, savedContent, unsavedContent)
}

export function writeFileAsUTF8(path: string, content: string, unsavedContent: string | null) {
  writeFile(
    path,
    encoder.encode(content),
    unsavedContent == null ? null : encoder.encode(unsavedContent),
  )
}

export function writeFileSavedContentAsUTF8(path: string, savedContent: string) {
  writeFileAsUTF8(path, savedContent, null)
}

export function writeFileUnsavedContentAsUTF8(path: string, unsavedContent: string) {
  writeFileUnsavedContent(path, encoder.encode(unsavedContent))
}

export function clearFileUnsavedContent(path: string) {
  const savedContent = readFileSavedContent(path)
  writeFileSavedContent(path, savedContent)
}

function updateMTime(node: FSNode): FSNode {
  const now = Date.now()
  if (isFile(node)) {
    const lastSavedTime = node.unsavedContent == null ? now : node.lastSavedTime
    return fsFile(node.content, node.unsavedContent, node.ctime, now, lastSavedTime)
  } else {
    return fsDirectory(node.ctime, now)
  }
}

function markModified(nodeWithPath: FSNodeWithPath) {
  setItem(nodeWithPath.path, updateMTime(nodeWithPath.node))
}

function uncheckedMove(oldPath: string, newPath: string) {
  const node = getNode(oldPath)
  setItem(newPath, updateMTime(node))
  removeItem(oldPath)
}

export function rename(oldPath: string, newPath: string) {
  const oldParent = getParent(oldPath)
  const newParent = getParent(newPath)

  const pathsToMove = targetsForOperation(oldPath, true)
  const toNewPath = (p: string) => `${newPath}${p.slice(0, oldPath.length)}`
  pathsToMove.forEach((pathToMove) => uncheckedMove(pathToMove, toNewPath(pathToMove)))
  if (oldParent != null) {
    markModified(oldParent)
  }
  if (newParent != null) {
    markModified(newParent)
  }
}

export function deletePath(path: string, recursive: boolean) {
  const parent = getParent(path)
  const targets = targetsForOperation(path, recursive)

  // Really this should fail if recursive isn't set to true when trying to delete a
  // non-empty directory, but for some reason VSCode doesn't provide an error suitable for that
  for (const target of targets) {
    removeItem(target)
  }

  if (parent != null) {
    markModified(parent)
  }
  return targets
}
