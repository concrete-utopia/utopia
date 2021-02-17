import * as BrowserFS from 'browserfs'
import { ApiError } from 'browserfs/dist/node/core/api_error'
import { BFSCallback, BFSOneArgCallback } from 'browserfs/dist/node/core/file_system'
import { FSModule } from 'browserfs/dist/node/core/FS'
import Stats from 'browserfs/dist/node/core/node_fs_stats'
import { uint8Array2Buffer } from 'browserfs/dist/node/core/util'
import { appendToPath } from './path-utils'

let fs: FSModule

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
let errorHandler: (e?: ApiError | null) => Error = (e?: ApiError | null) => {
  return Error(`Error with file handling: ${e}`)
}

export function setErrorHandler(handler: (e?: ApiError | null) => Error): void {
  errorHandler = handler
}

export function initializeBrowserFS(): Promise<void> {
  return new Promise<void>((resolve, reject) => {
    try {
      BrowserFS.getFileSystem({ fs: 'IndexedDB', options: { storeName: 'utopia' } }, (e, v) => {
        if (e) {
          console.error(`Failed to start BrowserFS`)
          throw e
        }

        if (v == null) {
          throw new Error('No filesystem was available.')
        }

        BrowserFS.initialize(v)
        fs = BrowserFS.BFSRequire('fs')
        resolve()
      })
    } catch (e) {
      reject(e)
    }
  })
}

// Used to chain promises and BrowserFS started on import.
let currentPromise: Promise<unknown> = initializeBrowserFS()

function chainOntoCurrent<T>(promise: () => Promise<T>): Promise<T> {
  const newPromise = currentPromise.then(promise)
  currentPromise = newPromise
  return newPromise
}

export async function readFile(path: string): Promise<Uint8Array> {
  return chainOntoCurrent(
    () =>
      new Promise<Uint8Array>((resolve, reject) => {
        fs.readFile(path, wrappedCallback(resolve, reject))
      }),
  )
}

export async function readFileWithEncoding(path: string, encoding: string): Promise<string> {
  return chainOntoCurrent(
    () =>
      new Promise<string>((resolve, reject) => {
        fs.readFile(path, encoding, wrappedCallback(resolve, reject))
      }),
  )
}

export async function exists(path: string): Promise<boolean> {
  return chainOntoCurrent(
    () =>
      new Promise<boolean>((resolve) => {
        fs.exists(path, (exists) => resolve(exists))
      }),
  )
}

export async function writeFile(path: string, content: Uint8Array): Promise<void> {
  return chainOntoCurrent(
    () =>
      new Promise<void>((resolve, reject) => {
        fs.writeFile(path, uint8Array2Buffer(content), wrappedOneArgCallback(resolve, reject))
      }),
  )
}

export async function rename(oldPath: string, newPath: string): Promise<void> {
  return chainOntoCurrent(
    () =>
      new Promise<void>((resolve, reject) => {
        fs.rename(oldPath, newPath, wrappedOneArgCallback(resolve, reject))
      }),
  )
}

export async function stat(path: string): Promise<Stats> {
  return chainOntoCurrent(
    () =>
      new Promise<Stats>((resolve, reject) => {
        fs.stat(path, wrappedCallback(resolve, reject))
      }),
  )
}

export async function pathIsDirectory(path: string): Promise<boolean> {
  return chainOntoCurrent(() => stat(path)).then((stats) => {
    return stats.isDirectory()
  })
}

export async function deletePath(path: string, recursive: boolean): Promise<string[]> {
  const targetPaths = recursive ? await getDescendentPaths(path, true) : [path]
  const pathsToDelete = [...targetPaths].reverse() // Delete all paths in reverse order

  for (const pathToDelete of pathsToDelete) {
    await _deletePath(pathToDelete)
  }

  return targetPaths
}

async function _deletePath(path: string): Promise<void> {
  const isDirectory = await pathIsDirectory(path)
  if (isDirectory) {
    return rmdir(path)
  } else {
    return unlink(path)
  }
}

async function rmdir(path: string): Promise<void> {
  return chainOntoCurrent(
    () =>
      new Promise<void>((resolve, reject) => {
        fs.rmdir(path, wrappedOneArgCallback(resolve, reject))
      }),
  )
}

async function unlink(path: string): Promise<void> {
  return chainOntoCurrent(
    () =>
      new Promise<void>((resolve, reject) => {
        fs.unlink(path, wrappedOneArgCallback(resolve, reject))
      }),
  )
}

export async function ensureDirectoryExists(path: string): Promise<void> {
  const fileExists = await exists(path)
  if (!fileExists) {
    await createDirectory(path)
  }
}

export async function createDirectory(path: string): Promise<void> {
  return chainOntoCurrent(
    () =>
      new Promise<void>((resolve, reject) => {
        fs.mkdir(path, 777, wrappedOneArgCallback(resolve, reject))
      }),
  )
}

export async function readdir(path: string): Promise<string[]> {
  return chainOntoCurrent(
    () =>
      new Promise<string[]>((resolve, reject) => {
        fs.readdir(path, wrappedCallback(resolve, reject))
      }),
  )
}

export async function childPaths(path: string): Promise<string[]> {
  const childrenFileNames = await readdir(path)
  return childrenFileNames.map((name) => appendToPath(path, name))
}

export async function getDescendentPaths(
  path: string,
  includeDirectories: boolean,
): Promise<string[]> {
  const isDirectory = await pathIsDirectory(path)
  if (isDirectory) {
    const children = await childPaths(path)
    const descendentPaths = await Promise.all(
      children.map((p) => getDescendentPaths(p, includeDirectories)),
    )
    let result = includeDirectories ? [path] : []
    descendentPaths.forEach((paths) => result.push(...paths))
    return result
  } else {
    return [path]
  }
}

function watchPath(path: string, config: WatchConfig) {
  watchedPaths.set(path, config)
  lastModifiedTSs.set(path, Date.now())
}

async function onPolledWatch(path: string, config: WatchConfig): Promise<void> {
  const { recursive, onCreated, onModified, onDeleted } = config

  try {
    const stillExists = await exists(path)
    if (stillExists) {
      const stats = await stat(path)

      if (recursive && stats.isDirectory()) {
        // This sucks, but it seems the only way to watch a directory is to check if its children are being watched
        const children = await childPaths(path)
        const unsupervisedChildren = children.filter((p) => !watchedPaths.has(p))
        unsupervisedChildren.forEach((childPath) => {
          watchPath(childPath, config)
          onCreated(childPath)
        })
      }

      const modifiedTS = stats.mtime.valueOf()
      if (modifiedTS > (lastModifiedTSs.get(path) ?? 0)) {
        lastModifiedTSs.set(path, modifiedTS)
        onModified(path)
      }
    } else {
      watchedPaths.delete(path)
      lastModifiedTSs.delete(path)
      onDeleted(path)
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
  watchTimeout = globalThis.setTimeout(polledWatch, POLLING_TIMEOUT) as any
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

    if (recursive) {
      const allPaths = await getDescendentPaths(target, true)
      allPaths.forEach(startWatchingPath)
    } else {
      startWatchingPath(target)
    }

    if (watchTimeout == null) {
      watchTimeout = globalThis.setTimeout(polledWatch, POLLING_TIMEOUT) as any
    }
  }
}

export async function stopWatching(target: string, recursive: boolean) {
  const stopWatchingPath = (path: string) => {
    watchedPaths.delete(path)
  }

  if (recursive) {
    const allPaths = await getDescendentPaths(target, true)
    allPaths.forEach(stopWatchingPath)
  } else {
    stopWatchingPath(target)
  }
}

function wrappedCallback<T>(
  resolve: (t: T | undefined) => void,
  reject: (e: Error) => void,
): BFSCallback<T> {
  return (e, v) => {
    if (e == null) {
      resolve(v)
    } else {
      reject(errorHandler(e))
    }
  }
}

function wrappedOneArgCallback(resolve: () => void, reject: (e: Error) => void): BFSOneArgCallback {
  return (e) => {
    if (e == null) {
      resolve()
    } else {
      reject(errorHandler(e))
    }
  }
}
