import type {
  CancellationToken,
  Event,
  FileChangeEvent,
  FileSearchOptions,
  FileSearchProvider,
  FileSearchQuery,
  FileStat,
  FileSystemProvider,
  Progress,
  TextSearchComplete,
  TextSearchOptions,
  TextSearchProvider,
  TextSearchQuery,
  TextSearchResult,
  Uri,
} from 'vscode'
import {
  Disposable,
  EventEmitter,
  FileChangeType,
  FileType,
  FileSystemError,
  Position,
  Range,
  workspace,
  commands,
} from 'vscode'
import {
  stat,
  pathIsDirectory,
  createDirectory,
  readFile,
  exists,
  writeFile,
  deletePath,
  rename,
  getDescendentPaths,
  isDirectory,
  readDirectory,
  readFileSavedContent,
  writeFileSavedContent,
  readFileSavedContentAsUTF8,
  pathIsFileWithUnsavedContent,
  readFileAsUTF8,
  writeFileAsUTF8,
  getItem,
  createDirectoryWithoutError,
  isFile,
  isNotDirectoryError,
  pathIsFile,
} from './in-mem-fs'
import { appendToPath, dirname, vsCodeFileDelete } from 'utopia-vscode-common'
import type { ProjectFile } from 'utopia-vscode-common'
import { addSchemeToPath, allPathsUpToPath } from './path-utils'

interface EventQueue<T> {
  queue: T[]
  emitter: EventEmitter<T[]>
  handle: number | null
}

function newEventQueue<T>(): EventQueue<T> {
  return {
    queue: [],
    emitter: new EventEmitter<T[]>(),
    handle: null,
  }
}

export class UtopiaFSExtension
  implements FileSystemProvider, FileSearchProvider, TextSearchProvider, Disposable
{
  private disposable: Disposable

  private fileChangeEventQueue = newEventQueue<FileChangeEvent>()

  private allFilePaths: string[] | null = null

  constructor(private projectID: string) {
    this.disposable = Disposable.from(
      workspace.registerFileSystemProvider(projectID, this, { isCaseSensitive: true }),
      workspace.registerFileSearchProvider(projectID, this),
      workspace.registerTextSearchProvider(projectID, this),
    )
  }

  dispose() {
    this.disposable?.dispose()
  }

  // FileSystemProvider
  readonly onDidChangeFile: Event<FileChangeEvent[]> = this.fileChangeEventQueue.emitter.event

  private queueEvents<T>(events: Array<T>, eventQueue: EventQueue<T>): void {
    eventQueue.queue.push(...events)

    if (eventQueue.handle != null) {
      clearTimeout(eventQueue.handle)
    }

    eventQueue.handle = setTimeout(() => {
      eventQueue.emitter.fire(eventQueue.queue)
      eventQueue.queue = []
    }, 5)
  }

  private queueFileChangeEvents(events: Array<FileChangeEvent>): void {
    this.clearCachedFiles()
    this.queueEvents(events, this.fileChangeEventQueue)
  }

  private notifyFileChanged(path: string) {
    const uri = addSchemeToPath(this.projectID, path)
    const hasUnsavedContent = pathIsFileWithUnsavedContent(path)
    const fileWasSaved = !hasUnsavedContent

    if (fileWasSaved) {
      // Notify VS Code of updates to the saved content
      this.queueFileChangeEvents([
        {
          type: FileChangeType.Changed,
          uri: uri,
        },
      ])
    }
  }

  private notifyFileCreated(path: string) {
    const parentDirectory = dirname(path)
    this.queueFileChangeEvents([
      {
        type: FileChangeType.Created,
        uri: addSchemeToPath(this.projectID, path),
      },
      {
        type: FileChangeType.Changed,
        uri: addSchemeToPath(this.projectID, parentDirectory),
      },
    ])
  }

  private notifyFileDeleted(path: string) {
    const parentDirectory = dirname(path)
    this.queueFileChangeEvents([
      {
        type: FileChangeType.Deleted,
        uri: addSchemeToPath(this.projectID, path),
      },
      {
        type: FileChangeType.Changed,
        uri: addSchemeToPath(this.projectID, parentDirectory),
      },
    ])
  }

  private notifyFileRenamed(oldPath: string, newPath: string) {
    const oldParentDirectory = dirname(oldPath)
    const newParentDirectory = dirname(newPath)
    const parentChanged = oldParentDirectory !== newParentDirectory
    this.queueFileChangeEvents([
      {
        type: FileChangeType.Deleted,
        uri: addSchemeToPath(this.projectID, oldPath),
      },
      {
        type: FileChangeType.Created,
        uri: addSchemeToPath(this.projectID, newPath),
      },
      {
        type: FileChangeType.Changed,
        uri: addSchemeToPath(this.projectID, oldParentDirectory),
      },
      ...(parentChanged
        ? [
            {
              type: FileChangeType.Changed,
              uri: addSchemeToPath(this.projectID, newParentDirectory),
            },
          ]
        : []),
    ])
  }

  watch(): Disposable {
    // No need for this since all events are manually fired
    return new Disposable(() => {})
  }

  exists(uri: Uri): boolean {
    const path = uri.path
    return exists(path)
  }

  stat(uri: Uri): FileStat {
    const path = uri.path
    const stats = stat(path)
    const fileType = isDirectory(stats) ? FileType.Directory : FileType.File

    return {
      type: fileType,
      ctime: stats.ctime.valueOf(),
      mtime: stats.lastSavedTime.valueOf(), // VS Code is only interested in changes to the saved content
      size: stats.size,
    }
  }

  readDirectory(uri: Uri): Array<[string, FileType]> {
    const path = uri.path
    const children = readDirectory(path)
    return children.map((childName) => {
      const resultIsDirectory = pathIsDirectory(appendToPath(path, childName))
      return [childName, resultIsDirectory ? FileType.Directory : FileType.File]
    })
  }

  createDirectory(uri: Uri) {
    const path = uri.path
    createDirectory(path)
    this.notifyFileCreated(path)
  }

  readFile(uri: Uri): Uint8Array {
    const path = uri.path
    return readFileSavedContent(path)
  }

  writeFile(uri: Uri, content: Uint8Array, options: { create: boolean; overwrite: boolean }) {
    const path = uri.path
    const fileExists = exists(path)
    if (!options.create || !options.overwrite) {
      if (!fileExists && !options.create) {
        throw FileSystemError.FileNotFound(uri)
      } else if (fileExists && !options.overwrite) {
        throw FileSystemError.FileExists(uri)
      }
    }

    writeFileSavedContent(path, content)
    if (fileExists) {
      this.notifyFileChanged(path)
    } else {
      this.notifyFileCreated(path)
    }
  }

  ensureDirectoryExists(pathToEnsure: string) {
    const allPaths = allPathsUpToPath(pathToEnsure)
    let createdDirectories: Array<string> = []
    for (const pathToCreate of allPaths) {
      const existingNode = getItem(pathToCreate)
      if (existingNode == null) {
        createDirectoryWithoutError(pathToCreate)
        createdDirectories.push(pathToCreate)
      } else if (isFile(existingNode)) {
        throw isNotDirectoryError(pathToCreate)
      }
    }

    createdDirectories.forEach((createdDirectory) => this.notifyFileCreated(createdDirectory))
  }

  writeProjectFile(projectFile: ProjectFile) {
    switch (projectFile.type) {
      case 'PROJECT_DIRECTORY': {
        const { filePath } = projectFile
        this.ensureDirectoryExists(filePath)
        break
      }
      case 'PROJECT_TEXT_FILE': {
        const { filePath, savedContent, unsavedContent } = projectFile
        const fileExists = exists(filePath)
        const alreadyExistingFile = fileExists ? readFileAsUTF8(filePath) : null
        const fileDiffers =
          alreadyExistingFile == null ||
          alreadyExistingFile.content !== savedContent ||
          alreadyExistingFile.unsavedContent !== unsavedContent
        if (fileDiffers) {
          // Avoid pushing a file to the file system if the content hasn't changed.
          writeFileAsUTF8(filePath, savedContent, unsavedContent)

          if (fileExists) {
            this.notifyFileChanged(filePath)
          } else {
            this.notifyFileCreated(filePath)
          }
        }
        break
      }
      default:
        const _exhaustiveCheck: never = projectFile
        throw new Error(`Invalid file projectFile type ${projectFile}`)
    }
  }

  delete(uri: Uri, options: { recursive: boolean }) {
    this.silentDelete(uri.path, options)
    commands.executeCommand('utopia.toUtopiaMessage', vsCodeFileDelete(uri.path))
  }

  silentDelete(path: string, options: { recursive: boolean }) {
    deletePath(path, options.recursive)
    this.notifyFileDeleted(path)
  }

  rename(oldUri: Uri, newUri: Uri, options: { overwrite: boolean }) {
    const oldPath = oldUri.path
    const newPath = newUri.path

    if (!options.overwrite) {
      const fileExists = exists(newPath)
      if (fileExists) {
        throw FileSystemError.FileExists(newUri)
      }
    }

    rename(oldPath, newPath)
    this.notifyFileRenamed(oldPath, newPath)
  }

  copy(source: Uri, destination: Uri, options: { overwrite: boolean }) {
    // It's not clear where this will ever be called from, but it seems to be from the side bar
    // that isn't available in Utopia, so this implementation is "just in case"
    const sourcePath = source.path
    const destinationPath = destination.path
    const destinationParentDir = dirname(destinationPath)
    const destinationParentDirExists = exists(destinationParentDir)

    if (!destinationParentDirExists) {
      throw FileSystemError.FileNotFound(addSchemeToPath(this.projectID, destinationParentDir))
    }

    if (!options.overwrite) {
      const destinationExists = exists(destinationPath)
      if (destinationExists && !options.overwrite) {
        throw FileSystemError.FileExists(destination)
      }
    }

    const { content, unsavedContent } = readFile(sourcePath)
    writeFile(destinationPath, content, unsavedContent)
    this.notifyFileCreated(destinationPath)
  }

  // FileSearchProvider

  provideFileSearchResults(
    query: FileSearchQuery,
    options: FileSearchOptions,
    _token: CancellationToken,
  ): Array<Uri> {
    // TODO Support all search options
    const lowerCaseQuery = query.pattern.toLocaleLowerCase()
    const filePaths = this.getAllFilePaths()
    const foundPaths = filePaths.filter((p) => p.toLocaleLowerCase().includes(lowerCaseQuery))
    return foundPaths.map((p) => addSchemeToPath(this.projectID, p))
  }

  // TextSearchProvider

  provideTextSearchResults(
    query: TextSearchQuery,
    options: TextSearchOptions,
    progress: Progress<TextSearchResult>,
    token: CancellationToken,
  ): TextSearchComplete {
    // This appears to only be callable from the side bar that isn't available in Utopia
    // TODO Support all search options
    const filePaths = this.filterFilePaths(options.includes[0])

    if (filePaths.length > 0) {
      const isCaseSensitive = query.isCaseSensitive ?? false
      const lowerCaseQuery = query.pattern.toLocaleLowerCase()

      for (const filePath of filePaths) {
        if (token.isCancellationRequested) {
          break
        }

        const content = readFileSavedContentAsUTF8(filePath)

        const lines = splitIntoLines(content)
        for (let i = 0; i < lines.length; i++) {
          const line = lines[i]
          const index = isCaseSensitive
            ? line.indexOf(query.pattern)
            : line.toLocaleLowerCase().indexOf(lowerCaseQuery)
          if (index !== -1) {
            progress.report({
              uri: addSchemeToPath(this.projectID, filePath),
              ranges: new Range(
                new Position(i, index),
                new Position(i, index + query.pattern.length),
              ),
              preview: {
                text: line,
                matches: new Range(
                  new Position(0, index),
                  new Position(0, index + query.pattern.length),
                ),
              },
            })
          }
        }
      }
    }

    return { limitHit: false }
  }

  // Common

  private filterFilePaths(query: string | undefined): Array<string> {
    const filePaths = this.getAllFilePaths()
    if (query == null) {
      return filePaths
    }

    let result: string[] = []
    const pattern = new RegExp(convertSimple2RegExpPattern(query))

    for (const path of filePaths) {
      if (!pattern || pattern.exec(path)) {
        result.push(path)
      }
    }

    return result
  }

  getAllFilePaths(): Array<string> {
    if (this.allFilePaths == null) {
      const allPaths = getDescendentPaths('')
      const allFilePaths = allPaths.filter((p) => pathIsFile(p))
      this.allFilePaths = allFilePaths
      return allFilePaths
    } else {
      return this.allFilePaths
    }
  }

  private clearCachedFiles() {
    this.allFilePaths = null
  }
}

function convertSimple2RegExpPattern(pattern: string): string {
  return pattern.replace(/[\-\\\{\}\+\?\|\^\$\.\,\[\]\(\)\#\s]/g, '\\$&').replace(/[\*]/g, '.*')
}

function splitIntoLines(s: string): Array<string> {
  return s.split(/[\r\n]/)
}
