import { INDEXEDDB } from 'localforage'
import { appendToPath, stripLeadingSlash, stripTrailingSlash } from '../path-utils'
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
  FSFile,
  FileContent,
  FSUser,
  fsDirectory,
} from './fs-types'

const encoder = new TextEncoder()
const decoder = new TextDecoder()

let fsUser: FSUser // Used to determine if changes came from this user or another

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

export async function initializeFS(
  storeName: string,
  user: FSUser,
  driver: string = INDEXEDDB,
): Promise<void> {
  fsUser = user
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

export async function pathIsFile(path: string): Promise<boolean> {
  const node = await getItem(path)
  return node != null && isFile(node)
}

export async function pathIsFileWithUnsavedContent(path: string): Promise<boolean> {
  const node = await getItem(path)
  return node != null && isFile(node) && node.unsavedContent != null
}

async function getNode(path: string): Promise<FSNode> {
  const node = await getItem(path)
  if (node == null) {
    return Promise.reject(missingFileError(path))
  } else {
    return node
  }
}

async function getFile(path: string): Promise<FSFile> {
  const node = await getNode(path)
  if (isFile(node)) {
    return node
  } else {
    return Promise.reject(isDirectoryError(path))
  }
}

export async function readFile(path: string): Promise<FileContent> {
  return getFile(path)
}

export async function readFileSavedContent(path: string): Promise<Uint8Array> {
  const fileNode = await getFile(path)
  return fileNode.content
}

export async function readFileUnsavedContent(path: string): Promise<Uint8Array | null> {
  const fileNode = await getFile(path)
  return fileNode.unsavedContent
}

export async function readFileAsUTF8(
  path: string,
): Promise<{ content: string; unsavedContent: string | null }> {
  const { content, unsavedContent } = await getFile(path)
  return {
    content: decoder.decode(content),
    unsavedContent: unsavedContent == null ? null : decoder.decode(unsavedContent),
  }
}

export async function readFileSavedContentAsUTF8(path: string): Promise<string> {
  const { content } = await readFileAsUTF8(path)
  return content
}

export async function readFileUnsavedContentAsUTF8(path: string): Promise<string | null> {
  const { unsavedContent } = await readFileAsUTF8(path)
  return unsavedContent
}

function fsStatForNode(node: FSNode): FSStat {
  return {
    type: node.type,
    ctime: node.ctime,
    mtime: node.mtime,
    lastSavedTime: node.lastSavedTime,
    size: isFile(node) ? node.content.length : 0,
    sourceOfLastChange: node.sourceOfLastChange,
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
    let result = [path, ...allDescendents]
    result.sort()
    result.reverse()
    return result
  } else {
    return [path]
  }
}

function getParentPath(path: string): string | null {
  const withoutLeadingOrTrailingSlash = stripLeadingSlash(stripTrailingSlash(path))
  const pathElems = withoutLeadingOrTrailingSlash.split('/')
  if (pathElems.length <= 1) {
    return null
  } else {
    return `/${pathElems.slice(0, -1).join('/')}`
  }
}

function filenameOfPath(path: string): string {
  const target = path.endsWith('/') ? path.slice(0, -1) : path
  const lastSlashIndex = target.lastIndexOf('/')
  return lastSlashIndex >= 0 ? path.slice(lastSlashIndex + 1) : path
}

export async function childPaths(path: string): Promise<string[]> {
  const allDescendents = await getDescendentPaths(path)
  const pathAsDir = stripTrailingSlash(path)
  return allDescendents.filter((k) => getParentPath(k) === pathAsDir)
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
  const parentPath = getParentPath(path)
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

  await setItem(path, newFSDirectory(fsUser))
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
    await setItem(path, newFSDirectory(fsUser))

    // Attempt to mark the parent as modified, but don't fail if it doesn't exist
    // since it might not have been created yet
    const parentPath = getParentPath(path)
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
  for (const pathToCreate of allPaths) {
    await simpleCreateDirectoryIfMissing(pathToCreate)
  }
}

export async function writeFile(
  path: string,
  content: Uint8Array,
  unsavedContent: Uint8Array | null,
): Promise<void> {
  const parent = await getParent(path)
  const maybeExistingFile = await getItem(path)
  if (maybeExistingFile != null && isDirectory(maybeExistingFile)) {
    return Promise.reject(isDirectoryError(path))
  }

  const now = Date.now()
  const fileCTime = maybeExistingFile == null ? now : maybeExistingFile.ctime
  const lastSavedTime =
    unsavedContent == null || maybeExistingFile == null ? now : maybeExistingFile.lastSavedTime
  const fileToWrite = fsFile(content, unsavedContent, fileCTime, now, lastSavedTime, fsUser)
  await setItem(path, fileToWrite)
  if (parent != null) {
    await markModified(parent)
  }
}

export async function writeFileSavedContent(path: string, content: Uint8Array): Promise<void> {
  return writeFile(path, content, null)
}

export async function writeFileUnsavedContent(
  path: string,
  unsavedContent: Uint8Array,
): Promise<void> {
  const savedContent = await readFileSavedContent(path)
  return writeFile(path, savedContent, unsavedContent)
}

export async function writeFileAsUTF8(
  path: string,
  content: string,
  unsavedContent: string | null,
): Promise<void> {
  return writeFile(
    path,
    encoder.encode(content),
    unsavedContent == null ? null : encoder.encode(unsavedContent),
  )
}

export async function writeFileSavedContentAsUTF8(
  path: string,
  savedContent: string,
): Promise<void> {
  return writeFileAsUTF8(path, savedContent, null)
}

export async function writeFileUnsavedContentAsUTF8(
  path: string,
  unsavedContent: string,
): Promise<void> {
  return writeFileUnsavedContent(path, encoder.encode(unsavedContent))
}

export async function clearFileUnsavedContent(path: string): Promise<void> {
  const savedContent = await readFileSavedContent(path)
  return writeFileSavedContent(path, savedContent)
}

function updateMTime(node: FSNode): FSNode {
  const now = Date.now()
  if (isFile(node)) {
    const lastSavedTime = node.unsavedContent == null ? now : node.lastSavedTime
    return fsFile(node.content, node.unsavedContent, node.ctime, now, lastSavedTime, fsUser)
  } else {
    return fsDirectory(node.ctime, now, fsUser)
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
  for (const target of targets) {
    await removeItem(target)
  }

  if (parent != null) {
    await markModified(parent)
  }
  return targets
}

interface WatchConfig {
  recursive: boolean
  onCreated: (path: string) => void
  onModified: (path: string, modifiedBySelf: boolean) => void
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
      const modifiedBySelf = stats.sourceOfLastChange === fsUser

      if (wasModified) {
        if (isDirectory(node)) {
          if (recursive) {
            const children = await childPaths(path)
            const unsupervisedChildren = children.filter((p) => !watchedPaths.has(p))
            unsupervisedChildren.forEach((childPath) => {
              watchPath(childPath, config)
              onCreated(childPath)
            })
            if (unsupervisedChildren.length > 0) {
              onModified(path, modifiedBySelf)
            }
          }
        } else {
          onModified(path, modifiedBySelf)
        }

        lastModifiedTSs.set(path, modifiedTS)
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
}

export async function watch(
  target: string,
  recursive: boolean,
  onCreated: (path: string) => void,
  onModified: (path: string, modifiedBySelf: boolean) => void,
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
      watchTimeout = setInterval(polledWatch, POLLING_TIMEOUT) as any
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

export function stopWatchingAll() {
  if (watchTimeout != null) {
    clearTimeout(watchTimeout)
    watchTimeout = null
  }
  watchedPaths = new Map()
  lastModifiedTSs = new Map()
}
