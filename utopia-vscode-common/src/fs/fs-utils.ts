import { INDEXEDDB } from 'localforage'
import { appendToPath } from '../path-utils'
import { getItem, initializeStore, keys, removeItem, setItem } from './fs-core'
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
  fsFile,
  newFSDirectory,
  FSNodeWithPath,
} from './fs-types'

const encoder = new TextEncoder()
const decoder = new TextDecoder()

let handleError: FSErrorHandler = (e: FSError) => {
  return Error(`FS Error: ${JSON.stringify(e)}`)
}

export function setErrorHandler(handler: FSErrorHandler): void {
  handleError = handler
}

const missingFileError = (path: string) => handleError(enoent(path))
const existingFileError = (path: string) => handleError(eexist(path))
const isDirectoryError = (path: string) => handleError(eisdir(path))
const isNotDirectoryError = (path: string) => handleError(enotdir(path))

export async function initializeFS(storeName: string, driver: string = INDEXEDDB): Promise<void> {
  await initializeStore(storeName, driver)
  await simpleCreateDirectoryIfMissing('/')
}

export async function exists(path: string): Promise<boolean> {
  const value = await getItem(path)
  return value != null
}

export async function pathIsDirectory(path: string): Promise<boolean> {
  const node = await getItem(path)
  return node != null && isDirectory(node)
}

async function getNode(path: string): Promise<FSNode> {
  const node = await getItem(path)
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
  const allPaths = await keys()
  return allPaths.filter((k) => k != path && k.startsWith(path))
}

async function targetsForOperation(path: string, recursive: boolean): Promise<string[]> {
  if (recursive) {
    const allDescendents = await getDescendentPaths(path)
    return [path, ...allDescendents]
  } else {
    return [path]
  }
}

function directoryOfPath(path: string): string | null {
  const target = path.endsWith('/') ? path.slice(0, -1) : path
  const lastSlashIndex = target.lastIndexOf('/')
  return lastSlashIndex >= 0 ? path.slice(0, lastSlashIndex + 1) : null
}

function filenameOfPath(path: string): string {
  const target = path.endsWith('/') ? path.slice(0, -1) : path
  const lastSlashIndex = target.lastIndexOf('/')
  return lastSlashIndex >= 0 ? path.slice(lastSlashIndex + 1) : path
}

export async function childPaths(path: string): Promise<string[]> {
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

async function getParent(path: string): Promise<FSNodeWithPath | null> {
  // null signifies we're already at the root
  const parentPath = directoryOfPath(path)
  if (parentPath == null) {
    return null
  } else {
    const parentDir = await getDirectory(parentPath)
    return {
      path: parentPath,
      node: parentDir,
    }
  }
}

export async function readDirectory(path: string): Promise<string[]> {
  await getDirectory(path) // Ensure the path exists and is a directory
  const children = await childPaths(path)
  return children.map(filenameOfPath)
}

export async function createDirectory(path: string): Promise<void> {
  const parent = await getParent(path)
  const pathExists = await getItem(path)
  if (pathExists != null) {
    return Promise.reject(existingFileError(path))
  }

  await setItem(path, newFSDirectory())
  if (parent != null) {
    await markModified(parent)
  }
}

function allPathsUpToPath(path: string): string[] {
  const directories = path.split('/')
  const { paths } = directories.reduce(
    ({ paths, workingPath }, next) => {
      const nextPath = appendToPath(workingPath, next)
      return {
        paths: paths.concat(nextPath),
        workingPath: nextPath,
      }
    },
    { paths: ['/'], workingPath: '/' },
  )
  return paths
}

async function simpleCreateDirectoryIfMissing(path: string): Promise<void> {
  const existingNode = await getItem(path)
  if (existingNode == null) {
    await setItem(path, newFSDirectory())

    // Attempt to mark the parent as modified, but don't fail if it doesn't exist
    // since it might not have been created yet
    const parentPath = directoryOfPath(path)
    if (parentPath != null) {
      const parentNode = await getItem(parentPath)
      if (parentNode != null) {
        await markModified({ path: parentPath, node: parentNode })
      }
    }
  } else if (isFile(existingNode)) {
    return Promise.reject(isNotDirectoryError(path))
  }
}

export async function ensureDirectoryExists(path: string): Promise<void> {
  const allPaths = allPathsUpToPath(path)
  await Promise.all(allPaths.map(simpleCreateDirectoryIfMissing))
}

export async function writeFile(path: string, content: Uint8Array): Promise<void> {
  const parent = await getParent(path)
  const maybeExistingFile = await getItem(path)
  if (maybeExistingFile != null && isDirectory(maybeExistingFile)) {
    return Promise.reject(isDirectoryError(path))
  }

  const now = Date.now()
  const fileCTime = maybeExistingFile == null ? now : maybeExistingFile.ctime
  const fileToWrite = fsFile(content, fileCTime, now)
  await setItem(path, fileToWrite)
  if (parent != null) {
    await markModified(parent)
  }
}

export async function writeFileAsUTF8(path: string, content: string): Promise<void> {
  return writeFile(path, encoder.encode(content))
}

function updateMTime(node: FSNode): FSNode {
  return {
    ...node,
    mtime: Date.now(),
  }
}

async function markModified(nodeWithPath: FSNodeWithPath): Promise<void> {
  await setItem(nodeWithPath.path, updateMTime(nodeWithPath.node))
}

async function uncheckedMove(oldPath: string, newPath: string): Promise<void> {
  const node = await getNode(oldPath)
  await setItem(newPath, updateMTime(node))
  await removeItem(oldPath)
}

export async function rename(oldPath: string, newPath: string): Promise<void> {
  const oldParent = await getParent(oldPath)
  const newParent = await getParent(newPath)

  const pathsToMove = await targetsForOperation(oldPath, true)
  const toNewPath = (p: string) => `${newPath}${p.slice(0, oldPath.length)}`
  await Promise.all(
    pathsToMove.map((pathToMove) => uncheckedMove(pathToMove, toNewPath(pathToMove))),
  )
  if (oldParent != null) {
    await markModified(oldParent)
  }
  if (newParent != null) {
    await markModified(newParent)
  }
}

export async function deletePath(path: string, recursive: boolean): Promise<string[]> {
  const parent = await getParent(path)
  const targets = await targetsForOperation(path, recursive)

  // Really this should fail if recursive isn't set to true when trying to delete a
  // non-empty directory, but for some reason VSCode doesn't provide an error suitable for that
  await Promise.all(targets.map((p) => removeItem(p)))

  if (parent != null) {
    await markModified(parent)
  }
  return targets
}

interface WatchConfig {
  recursive: boolean
  onCreated: (path: string) => void
  onModified: (path: string) => void
  onDeleted: (path: string) => void
}

let watchTimeout: number | null = null
let watchedPaths: Map<string, WatchConfig> = new Map()
let lastModifiedTSs: Map<string, number> = new Map()
const POLLING_TIMEOUT = 100

function watchPath(path: string, config: WatchConfig) {
  watchedPaths.set(path, config)
  lastModifiedTSs.set(path, Date.now())
}

async function onPolledWatch(path: string, config: WatchConfig): Promise<void> {
  const { recursive, onCreated, onModified, onDeleted } = config

  try {
    const node = await getItem(path)
    if (node == null) {
      watchedPaths.delete(path)
      lastModifiedTSs.delete(path)
      onDeleted(path)
    } else {
      const stats = fsStatForNode(node)

      const modifiedTS = stats.mtime
      const wasModified = modifiedTS > (lastModifiedTSs.get(path) ?? 0)

      if (recursive && isDirectory(node) && wasModified) {
        const children = await childPaths(path)
        const unsupervisedChildren = children.filter((p) => !watchedPaths.has(p))
        unsupervisedChildren.forEach((childPath) => {
          watchPath(childPath, config)
          onCreated(childPath)
        })
      }

      if (wasModified) {
        lastModifiedTSs.set(path, modifiedTS)
        onModified(path)
      }
    }
  } catch (e) {
    // Something was changed mid-poll, likely the file or its parent was deleted. We'll catch it on the next poll.
  }
}

async function polledWatch(): Promise<void> {
  let promises: Array<Promise<void>> = []
  watchedPaths.forEach((config, path) => {
    promises.push(onPolledWatch(path, config))
  })

  await Promise.all(promises)
  watchTimeout = setTimeout(polledWatch, POLLING_TIMEOUT) as any
}

export async function watch(
  target: string,
  recursive: boolean,
  onCreated: (path: string) => void,
  onModified: (path: string) => void,
  onDeleted: (path: string) => void,
): Promise<void> {
  const fileExists = await exists(target)
  if (fileExists) {
    // This has the limitation that calling `watch` on a path will replace any existing subscriber
    const startWatchingPath = (path: string) =>
      watchPath(path, {
        recursive: recursive,
        onCreated: onCreated,
        onModified: onModified,
        onDeleted: onDeleted,
      })

    const targets = await targetsForOperation(target, recursive)
    targets.forEach(startWatchingPath)

    if (watchTimeout == null) {
      watchTimeout = setTimeout(polledWatch, POLLING_TIMEOUT) as any
    }
  }
}

export async function stopWatching(target: string, recursive: boolean) {
  const stopWatchingPath = (path: string) => {
    watchedPaths.delete(path)
  }

  const targets = await targetsForOperation(target, recursive)
  targets.forEach(stopWatchingPath)
}
