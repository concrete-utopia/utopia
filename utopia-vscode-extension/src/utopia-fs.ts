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
} from 'vscode'
import {
  watch,
  stopWatching,
  stat,
  pathIsDirectory,
  createDirectory,
  readFile,
  exists,
  writeFile,
  appendToPath,
  dirname,
  stripRootPrefix,
  deletePath,
  rename,
  getDescendentPaths,
  isDirectory,
  readDirectory,
  RootDir,
  readFileSavedContent,
  writeFileSavedContent,
  readFileSavedContentAsUTF8,
  pathIsFileWithUnsavedContent,
} from 'utopia-vscode-common'
import { fromUtopiaURI, toUtopiaURI } from './path-utils'

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
  private utopiaSavedChangeEventQueue = newEventQueue<Uri>()
  private utopiaUnsavedChangeEventQueue = newEventQueue<Uri>()

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
  readonly onUtopiaDidChangeSavedContent: Event<Uri[]> =
    this.utopiaSavedChangeEventQueue.emitter.event
  readonly onUtopiaDidChangeUnsavedContent: Event<Uri[]> =
    this.utopiaUnsavedChangeEventQueue.emitter.event

  private queueEvent<T>(event: T, eventQueue: EventQueue<T>): void {
    eventQueue.queue.push(event)

    if (eventQueue.handle != null) {
      clearTimeout(eventQueue.handle)
    }

    eventQueue.handle = setTimeout(() => {
      eventQueue.emitter.fire(eventQueue.queue)
      eventQueue.queue = []
    }, 5)
  }

  private queueFileChangeEvent(event: FileChangeEvent): void {
    this.clearCachedFiles()
    this.queueEvent(event, this.fileChangeEventQueue)
  }

  private queueUtopiaSavedChangeEvent(resource: Uri): void {
    this.queueEvent(resource, this.utopiaSavedChangeEventQueue)
  }

  private queueUtopiaUnsavedChangeEvent(resource: Uri): void {
    this.queueEvent(resource, this.utopiaUnsavedChangeEventQueue)
  }

  private async notifyFileChanged(path: string, modifiedBySelf: boolean): Promise<void> {
    const uri = toUtopiaURI(this.projectID, path)
    const hasUnsavedContent = await pathIsFileWithUnsavedContent(path)
    const fileWasSaved = !hasUnsavedContent

    if (fileWasSaved) {
      // Notify VS Code of updates to the saved content
      this.queueFileChangeEvent({
        type: FileChangeType.Changed,
        uri: uri,
      })
    }

    if (!modifiedBySelf) {
      // Notify our extension of changes coming from Utopia only
      if (fileWasSaved) {
        this.queueUtopiaSavedChangeEvent(uri)
      } else {
        this.queueUtopiaUnsavedChangeEvent(uri)
      }
    }
  }

  private notifyFileCreated(path: string): void {
    this.queueFileChangeEvent({
      type: FileChangeType.Created,
      uri: toUtopiaURI(this.projectID, path),
    })
  }

  private notifyFileDeleted(path: string): void {
    this.queueFileChangeEvent({
      type: FileChangeType.Deleted,
      uri: toUtopiaURI(this.projectID, path),
    })
  }

  watch(uri: Uri, options: { recursive: boolean; excludes: string[] }): Disposable {
    const path = fromUtopiaURI(uri)
    watch(
      path,
      options.recursive,
      this.notifyFileCreated.bind(this),
      this.notifyFileChanged.bind(this),
      this.notifyFileDeleted.bind(this),
      () => {
        /* no op */
      },
    )

    return new Disposable(() => {
      stopWatching(path, options.recursive)
    })
  }

  async exists(uri: Uri): Promise<boolean> {
    const path = fromUtopiaURI(uri)
    return exists(path)
  }

  async stat(uri: Uri): Promise<FileStat> {
    const path = fromUtopiaURI(uri)
    const stats = await stat(path)
    const fileType = isDirectory(stats) ? FileType.Directory : FileType.File

    return {
      type: fileType,
      ctime: stats.ctime.valueOf(),
      mtime: stats.lastSavedTime.valueOf(), // VS Code is only interested in changes to the saved content
      size: stats.size,
    }
  }

  async readDirectory(uri: Uri): Promise<[string, FileType][]> {
    const path = fromUtopiaURI(uri)
    const children = await readDirectory(path)
    const result: Promise<[string, FileType]>[] = children.map((childName) =>
      pathIsDirectory(appendToPath(path, childName)).then((resultIsDirectory) => [
        childName,
        resultIsDirectory ? FileType.Directory : FileType.File,
      ]),
    )
    return Promise.all(result)
  }

  async createDirectory(uri: Uri): Promise<void> {
    const path = fromUtopiaURI(uri)
    await createDirectory(path)
  }

  async readFile(uri: Uri): Promise<Uint8Array> {
    const path = fromUtopiaURI(uri)
    return readFileSavedContent(path)
  }

  async writeFile(
    uri: Uri,
    content: Uint8Array,
    options: { create: boolean; overwrite: boolean },
  ): Promise<void> {
    const path = fromUtopiaURI(uri)
    if (!options.create || !options.overwrite) {
      const fileExists = await exists(path)
      if (!fileExists && !options.create) {
        throw FileSystemError.FileNotFound(uri)
      } else if (fileExists && !options.overwrite) {
        throw FileSystemError.FileExists(uri)
      }
    }

    await writeFileSavedContent(path, content)
  }

  async delete(uri: Uri, options: { recursive: boolean }): Promise<void> {
    const path = fromUtopiaURI(uri)
    await deletePath(path, options.recursive)
  }

  async rename(oldUri: Uri, newUri: Uri, options: { overwrite: boolean }): Promise<void> {
    const oldPath = fromUtopiaURI(oldUri)
    const newPath = fromUtopiaURI(newUri)

    if (!options.overwrite) {
      const fileExists = await exists(newPath)
      if (fileExists) {
        throw FileSystemError.FileExists(newUri)
      }
    }

    await rename(oldPath, newPath)
  }

  async copy(source: Uri, destination: Uri, options: { overwrite: boolean }): Promise<void> {
    // It's not clear where this will ever be called from, but it seems to be from the side bar
    // that isn't available in Utopia, so this implementation is "just in case"
    const sourcePath = fromUtopiaURI(source)
    const destinationPath = fromUtopiaURI(destination)
    const destinationParentDir = dirname(destinationPath)
    const destinationParentDirExists = await exists(destinationParentDir)

    if (!destinationParentDirExists) {
      throw FileSystemError.FileNotFound(toUtopiaURI(this.projectID, destinationParentDir))
    }

    if (!options.overwrite) {
      const destinationExists = await exists(destinationPath)
      if (destinationExists && !options.overwrite) {
        throw FileSystemError.FileExists(destination)
      }
    }

    const { content, unsavedContent } = await readFile(sourcePath)
    await writeFile(destinationPath, content, unsavedContent)
  }

  // FileSearchProvider

  async provideFileSearchResults(
    query: FileSearchQuery,
    options: FileSearchOptions,
    _token: CancellationToken,
  ): Promise<Uri[]> {
    // TODO Support all search options
    const { result: foundPaths } = await this.filterFilePaths(query.pattern, options.maxResults)
    return foundPaths.map((p) => toUtopiaURI(this.projectID, p))
  }

  // TextSearchProvider

  async provideTextSearchResults(
    query: TextSearchQuery,
    options: TextSearchOptions,
    progress: Progress<TextSearchResult>,
    token: CancellationToken,
  ): Promise<TextSearchComplete> {
    // This appears to only be callable from the side bar that isn't available in Utopia
    // TODO Support all search options
    const { result: filePaths, limitHit } = await this.filterFilePaths(options.includes[0])

    if (filePaths.length > 0) {
      for (const filePath of filePaths) {
        if (token.isCancellationRequested) {
          break
        }

        const content = await readFileSavedContentAsUTF8(filePath)

        const lines = splitIntoLines(content)
        for (let i = 0; i < lines.length; i++) {
          const line = lines[i]
          const index = line.indexOf(query.pattern)
          if (index !== -1) {
            progress.report({
              uri: toUtopiaURI(this.projectID, filePath),
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

    return { limitHit: limitHit }
  }

  // Common

  private async filterFilePaths(
    query: string | undefined,
    maxResults?: number,
  ): Promise<{ result: string[]; limitHit: boolean }> {
    const filePaths = await this.getAllPaths()
    let result: string[] = []
    let limitHit = false
    let remainingCount = maxResults == null ? Infinity : maxResults

    const pattern = query ? new RegExp(convertSimple2RegExpPattern(query)) : null

    for (const path of filePaths) {
      if (remainingCount < 0) {
        break
      }

      if (!pattern || pattern.exec(stripRootPrefix(path))) {
        if (remainingCount === 0) {
          // We've already found the max number of results, but we want to flag that there are more
          limitHit = true
        } else {
          result.push(path)
        }
        remainingCount--
      }
    }

    return {
      result: result,
      limitHit: limitHit,
    }
  }

  async getAllPaths(): Promise<string[]> {
    if (this.allFilePaths == null) {
      const result = await getDescendentPaths(RootDir)
      this.allFilePaths = result
      return result
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
