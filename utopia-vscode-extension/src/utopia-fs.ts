import {
  CancellationToken,
  Disposable,
  Event,
  EventEmitter,
  FileChangeEvent,
  FileChangeType,
  FileSearchOptions,
  FileSearchProvider,
  FileSearchQuery,
  FileStat,
  FileSystemError,
  FileSystemProvider,
  FileType,
  Position,
  Progress,
  Range,
  TextSearchComplete,
  TextSearchOptions,
  TextSearchProvider,
  TextSearchQuery,
  TextSearchResult,
  Uri,
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
} from 'utopia-vscode-common'
import { fromUtopiaURI, toUtopiaURI } from './path-utils'

export class UtopiaFSExtension
  implements FileSystemProvider, FileSearchProvider, TextSearchProvider, Disposable {
  private disposable: Disposable
  private emitter = new EventEmitter<FileChangeEvent[]>()
  private eventQueue: FileChangeEvent[] = []
  private emitEventHandle: number | null = null
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

  readonly onDidChangeFile: Event<FileChangeEvent[]> = this.emitter.event

  private queueFileChangeEvent(event: FileChangeEvent): void {
    this.clearCachedFiles()
    this.eventQueue.push(event)

    if (this.emitEventHandle != null) {
      clearTimeout(this.emitEventHandle)
    }

    this.emitEventHandle = setTimeout(() => {
      this.emitter.fire(this.eventQueue)
      this.eventQueue = []
    }, 5)
  }

  private notifyFileChanged(path: string): void {
    this.queueFileChangeEvent({
      type: FileChangeType.Changed,
      uri: toUtopiaURI(this.projectID, path),
    })
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
      mtime: stats.mtime.valueOf(),
      size: stats.size,
    }
  }

  async readDirectory(uri: Uri): Promise<[string, FileType][]> {
    const path = fromUtopiaURI(uri)
    const children = await readDirectory(path)
    const result: Promise<[string, FileType]>[] = children.map((childName) =>
      pathIsDirectory(appendToPath(path, childName)).then((isDirectory) => [
        childName,
        isDirectory ? FileType.Directory : FileType.File,
      ]),
    )
    return Promise.all(result)
  }

  async createDirectory(uri: Uri): Promise<void> {
    const path = fromUtopiaURI(uri)
    await createDirectory(path)

    const parentDir = dirname(path)
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
