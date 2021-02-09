import * as BrowserFS from 'browserfs'
import { FSModule } from 'browserfs/dist/node/core/FS'
import { BFSCallback, BFSOneArgCallback, FileSystem } from 'browserfs/dist/node/core/file_system'
import Stats from 'browserfs/dist/node/core/node_fs_stats'
import { appendToPath, toUtopiaPath } from './path-utils'
import { ApiError } from 'browserfs/dist/node/core/api_error'
import { FileSystemError } from 'vscode'
import { uint8Array2Buffer } from 'browserfs/dist/node/core/util'

let fs: FSModule

// Immediately attempt to initialise BrowserFS
new Promise<void>((resolve, reject) => {
  try {
    BrowserFS.getFileSystem({ fs: 'IndexedDB', options: { storeName: 'utopia' } }, (e, v) => {
      if (e) {
        console.error(`Failed to start BrowserFS`)
        throw e
      }

      BrowserFS.initialize(v)
      fs = BrowserFS.BFSRequire('fs')
      resolve()
    })
  } catch (e) {
    reject(e)
  }
})

export async function waitUntilFSReady(): Promise<void> {
  // All exported functions must call this because otherwise VS Code will attempt to read files before BrowserFS
  // has finished initialising
  if (fs == undefined) {
    return new Promise((resolve, reject) =>
      setTimeout(() => waitUntilFSReady().then(resolve).catch(reject), 100),
    )
  }
}

export async function readFile(path: string): Promise<Uint8Array> {
  await waitUntilFSReady()

  return new Promise<Uint8Array>((resolve, reject) => {
    fs.readFile(path, wrappedCallback(resolve, reject))
  })
}

export async function readFileWithEncoding(path: string, encoding: string): Promise<string> {
  await waitUntilFSReady()

  return new Promise<string>((resolve, reject) => {
    fs.readFile(path, encoding, wrappedCallback(resolve, reject))
  })
}

export async function exists(path: string): Promise<boolean> {
  await waitUntilFSReady()

  return new Promise<boolean>((resolve) => {
    fs.exists(path, (exists) => resolve(exists))
  })
}

export async function writeFile(path: string, content: Uint8Array): Promise<void> {
  await waitUntilFSReady()

  await new Promise<void>((resolve, reject) => {
    fs.writeFile(path, uint8Array2Buffer(content), wrappedOneArgCallback(resolve, reject))
  })
}

export async function rename(oldPath: string, newPath: string): Promise<void> {
  await waitUntilFSReady()

  await new Promise<void>((resolve, reject) => {
    fs.rename(oldPath, newPath, wrappedOneArgCallback(resolve, reject))
  })
}

export async function stat(path: string): Promise<Stats> {
  await waitUntilFSReady()

  return new Promise<Stats>((resolve, reject) => {
    fs.stat(path, wrappedCallback(resolve, reject))
  })
}

export async function pathIsDirectory(path: string): Promise<boolean> {
  await waitUntilFSReady()

  const stats = await stat(path)
  return stats.isDirectory()
}

export async function deleteFile(path: string, recursive: boolean): Promise<string[]> {
  await waitUntilFSReady()

  const descendentsToDelete = recursive ? await getDescendentPaths(path) : []
  const targetPaths = [path, ...descendentsToDelete]
  const pathsToDelete = [...targetPaths].reverse() // Delete all paths in reverse order
  await Promise.all(pathsToDelete.map(_deleteFile))
  return targetPaths
}

async function _deleteFile(path: string): Promise<void> {
  const isDirectory = await pathIsDirectory(path)
  if (isDirectory) {
    return rmdir(path)
  } else {
    return unlink(path)
  }
}

async function rmdir(path: string): Promise<void> {
  return new Promise<void>((resolve, reject) => {
    fs.rmdir(path, wrappedOneArgCallback(resolve, reject))
  })
}

async function unlink(path: string): Promise<void> {
  return new Promise<void>((resolve, reject) => {
    fs.unlink(path, wrappedOneArgCallback(resolve, reject))
  })
}

export async function ensureDirectoryExists(path: string): Promise<void> {
  await waitUntilFSReady()

  const fileExists = await exists(path)
  if (!fileExists) {
    await createDirectory(path)
  }
}

export async function createDirectory(path: string): Promise<void> {
  await waitUntilFSReady()

  return new Promise<void>((resolve, reject) => {
    fs.mkdir(path, 777, wrappedOneArgCallback(resolve, reject))
  })
}

export async function readdir(path: string): Promise<string[]> {
  await waitUntilFSReady()

  return new Promise<string[]>((resolve, reject) => {
    fs.readdir(path, wrappedCallback(resolve, reject))
  })
}

export async function childPaths(path: string): Promise<string[]> {
  await waitUntilFSReady()

  const childrenFileNames = await readdir(path)
  return childrenFileNames.map((name) => appendToPath(path, name))
}

export async function getDescendentPaths(path: string): Promise<string[]> {
  await waitUntilFSReady()

  const isDirectory = await pathIsDirectory(path)
  if (isDirectory) {
    const children = await childPaths(path)
    const descendentPaths = await Promise.all(children.map(getDescendentPaths))
    let result = []
    descendentPaths.forEach((paths) => result.push(...paths))
    return result
  } else {
    return [path]
  }
}

function wrappedCallback<T>(resolve: (t: T) => void, reject: (e: Error) => void): BFSCallback<T> {
  return (e, v) => {
    if (e == null) {
      resolve(v)
    } else {
      reject(toFileSystemProviderError(e))
    }
  }
}

function wrappedOneArgCallback(resolve: () => void, reject: (e: Error) => void): BFSOneArgCallback {
  return (e) => {
    if (e == null) {
      resolve()
    } else {
      reject(toFileSystemProviderError(e))
    }
  }
}

function toFileSystemProviderError(error: ApiError): FileSystemError {
  const { path: unadjustedPath, code } = error
  const path = toUtopiaPath(unadjustedPath)
  switch (code) {
    case 'ENOENT':
      return FileSystemError.FileNotFound(path)
    case 'EISDIR':
      return FileSystemError.FileIsADirectory(path)
    case 'ENOTDIR':
      return FileSystemError.FileNotADirectory(path)
    case 'EEXIST':
      return FileSystemError.FileExists(path)
    case 'EPERM':
    case 'EACCES':
      return FileSystemError.NoPermissions(path)
    default:
      return new FileSystemError(error.message)
  }
}
