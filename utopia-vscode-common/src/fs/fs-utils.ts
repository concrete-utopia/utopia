import * as localforage from 'localforage'
import {
  FSError,
  FSErrorHandler,
  FSNode,
  isDirectory,
  isFile,
  enoent,
  eexist,
  eisdir,
  enotdir,
  FSStat,
  FSDirectory,
  FSFile,
  fsFile,
  fsDirectory,
  newFSDirectory,
} from './fs-types'

const encoder = new TextEncoder()
const decoder = new TextDecoder()

let handleError: FSErrorHandler = (e: FSError) => {
  return Error(`FS Error: ${e}`)
}

export function setErrorHandler(handler: FSErrorHandler): void {
  handleError = handler
}

const missingFileError = (path: string) => handleError(enoent(path))
const existingFileError = (path: string) => handleError(eexist(path))
const isDirectoryError = (path: string) => handleError(eisdir(path))
const isNotDirectoryError = (path: string) => handleError(enotdir(path))

// FIXME Should this be wrapped in an init function?
// FIXME This needs to ensure the root directory exists
const store = localforage.createInstance({
  name: 'utopia',
  storeName: 'utopia-vscode',
  driver: localforage.INDEXEDDB,
})

export async function exists(path: string): Promise<boolean> {
  const value = await store.getItem(path)
  return value != null
}

export async function pathIsDirectory(path: string): Promise<boolean> {
  const node = await store.getItem<FSNode>(path)
  return node != null && isDirectory(node)
}

async function getNode(path: string): Promise<FSNode> {
  const node = await store.getItem<FSNode>(path)
  if (node == null) {
    return Promise.reject(missingFileError(path))
  } else {
    return node
  }
}

export async function readFile(path: string): Promise<Uint8Array> {
  const node = await getNode(path)
  if (isFile(node)) {
    return node.content
  } else {
    return Promise.reject(isDirectoryError(path))
  }
}

export async function readFileAsUTF8(path: string): Promise<string> {
  const rawData = await readFile(path)
  return decoder.decode(rawData)
}

function fsStatForNode(node: FSNode): FSStat {
  return {
    type: node.type,
    ctime: node.ctime,
    mtime: node.mtime,
    size: isFile(node) ? node.content.length : 0,
  }
}

export async function stat(path: string): Promise<FSStat> {
  const node = await getNode(path)
  return fsStatForNode(node)
}

export async function getDescendentPaths(path: string): Promise<string[]> {
  const allPaths = await store.keys()
  return allPaths.filter((k) => k != path && k.startsWith(path))
}

function directoryOfPath(path: string): string {
  const target = path.endsWith('/') ? path.slice(0, -1) : path
  const lastSlashIndex = target.lastIndexOf('/')
  return lastSlashIndex >= 0 ? path.slice(0, lastSlashIndex + 1) : '/'
}

function filenameOfPath(path: string): string {
  const target = path.endsWith('/') ? path.slice(0, -1) : path
  const lastSlashIndex = target.lastIndexOf('/')
  return lastSlashIndex >= 0 ? path.slice(lastSlashIndex + 1) : path
}

async function childPaths(path: string): Promise<string[]> {
  const allDescendents = await getDescendentPaths(path)
  const pathAsDir = path.endsWith('/') ? path : `${path}/`
  return allDescendents.filter((k) => directoryOfPath(k) === pathAsDir)
}

async function getDirectory(path: string): Promise<FSDirectory> {
  const node = await getNode(path)
  if (isDirectory(node)) {
    return node
  } else {
    return Promise.reject(isNotDirectoryError(path))
  }
}

export async function readDirectory(path: string): Promise<string[]> {
  await getDirectory(path) // Ensure the directory exists
  const children = await childPaths(path)
  return children.map(filenameOfPath)
}

export async function createDirectory(path: string): Promise<void> {
  const parentPath = directoryOfPath(path)
  const parent = await getDirectory(parentPath)
  const pathExists = await store.getItem<FSNode>(path)
  if (pathExists != null) {
    return Promise.reject(existingFileError(path))
  }

  await store.setItem(path, newFSDirectory())
  await store.setItem(parentPath, markModified(parent))
}

// export async function ensureDirectoryExists(path: string): Promise<void>

export async function writeFile(path: string, content: Uint8Array): Promise<void> {
  const parentPath = directoryOfPath(path)
  const parent = await getDirectory(parentPath)
  const maybeExistingFile = await store.getItem<FSNode>(path)
  if (maybeExistingFile != null && isDirectory(maybeExistingFile)) {
    return Promise.reject(isDirectoryError(path))
  }

  const now = Date.now()
  const fileCTime = maybeExistingFile == null ? now : maybeExistingFile.ctime
  const fileToWrite = fsFile(content, fileCTime, now)
  await store.setItem(path, fileToWrite)
  await store.setItem(parentPath, markModified(parent))
}

export async function writeFileAsUTF8(path: string, content: string): Promise<void> {
  return writeFile(path, encoder.encode(content))
}

function markModified(node: FSNode): FSNode {
  return {
    ...node,
    mtime: Date.now(),
  }
}

async function uncheckedMove(oldPath: string, newPath: string): Promise<void> {
  const node = await getNode(oldPath)
  await store.setItem(newPath, markModified(node))
  await store.removeItem(oldPath)
}

export async function rename(oldPath: string, newPath: string): Promise<void> {
  const oldParentPath = directoryOfPath(oldPath)
  const newParentPath = directoryOfPath(newPath)
  const oldParent = await getDirectory(oldParentPath)
  const newParent = await getDirectory(newParentPath)

  const allDescendents = await getDescendentPaths(oldPath)
  const pathsToMove = [oldPath, ...allDescendents]
  const toNewPath = (p: string) => `${newPath}${p.slice(0, oldPath.length)}`
  await Promise.all(
    pathsToMove.map((pathToMove) => uncheckedMove(pathToMove, toNewPath(pathToMove))),
  )
  await store.setItem(oldParentPath, markModified(oldParent))
  await store.setItem(newParentPath, markModified(newParent))
}

// export async function deletePath(path: string, recursive: boolean): Promise<string[]>
// export async function watch(target: string, recursive: boolean, onCreated: (path: string) => void, onModified: (path: string) => void, onDeleted: (path: string) => void): Promise<void>
// export async function stopWatching(target: string, recursive: boolean): Promise<void>
