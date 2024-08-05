import type {
  ProjectContents,
  ImageFile,
  ProjectFile,
  Directory,
  TextFile,
  AssetFile,
  ParseSuccess,
} from '../core/shared/project-file-types'
import {
  directory,
  isDirectory,
  isImageFile,
  RevisionsState,
} from '../core/shared/project-file-types'
import { isTextFile, isParseSuccess, isAssetFile } from '../core/shared/project-file-types'
import Utils from '../utils/utils'
import { dropLeadingSlash } from './filebrowser/filepath-utils'
import { assertNever, fastForEach } from '../core/shared/utils'
import { mapValues, propOrNull } from '../core/shared/object-utils'
import { emptySet } from '../core/shared/set-utils'
import { sha1 } from 'sha.js'
import type { GithubFileChanges, TreeConflicts } from '../core/shared/github/helpers'
import type { FileChecksumsWithFile } from './editor/store/editor-state'
import { memoize } from '../core/shared/memoize'
import { makeOptic, type Optic } from '../core/shared/optics/optics'
import type {
  AssetFileWithFileName,
  ProjectContentTreeRoot,
  ProjectContentDirectory,
  ProjectContentFile,
  ProjectContentsTree,
  PathAndFileEntry,
} from 'utopia-shared/src/types/assets'
import { filtered, fromField, fromTypeGuard } from '../core/shared/optics/optic-creators'
import { anyBy, toArrayOf } from '../core/shared/optics/optic-utilities'
export type {
  AssetFileWithFileName,
  ProjectContentTreeRoot,
  ProjectContentDirectory,
  ProjectContentFile,
  ProjectContentsTree,
  PathAndFileEntry,
}

export function getAllProjectAssetFiles(
  projectContents: ProjectContentTreeRoot,
): Array<AssetFileWithFileName> {
  let allProjectAssets: Array<AssetFileWithFileName> = []

  walkContentsTree(projectContents, (fullPath, file) => {
    if (isImageFile(file) || isAssetFile(file)) {
      allProjectAssets.push({
        fileName: fullPath,
        file: file,
      })
    }
  })

  return allProjectAssets
}

function getSHA1ChecksumInner(contents: string | Buffer): string {
  return new sha1().update(contents).digest('hex')
}

// Memoized because it can be called for the same piece of code more than once before the
// checksum gets cached. For example in the canvas strategies and the regular dispatch flow, which don't share
// those cached checksum objects.
export const getSHA1Checksum = memoize(getSHA1ChecksumInner, {
  maxSize: 10,
  matchesArg: (first, second) => first === second,
})

export function gitBlobChecksumFromBase64(base64: string): string {
  return gitBlobChecksumFromBuffer(Buffer.from(base64, 'base64'))
}

export function gitBlobChecksumFromText(text: string): string {
  return gitBlobChecksumFromBuffer(Buffer.from(text, 'utf8'))
}

export function gitBlobChecksumFromBuffer(buffer: Buffer): string {
  // This function returns the same SHA1 checksum string that git would return for the same contents.
  // Given the contents in the buffer variable, the final checksum is calculated by hashing
  // a string built as "<prefix><contents>". The prefix looks like "blob <contents_length_in_bytes><null_character>".
  // Ref: https://git-scm.com/book/en/v2/Git-Internals-Git-Objects
  const prefix = Buffer.from(`blob ${buffer.byteLength}\0`)
  const wrapped = Buffer.concat([prefix, buffer])
  return getSHA1Checksum(wrapped)
}

export function checkFilesHaveSameContent(first: ProjectFile, second: ProjectFile): boolean {
  switch (first.type) {
    case 'DIRECTORY':
      return first.type === second.type
    case 'TEXT_FILE':
      if (first.type === second.type) {
        return first.fileContents.code === second.fileContents.code
      } else {
        return false
      }
    case 'IMAGE_FILE':
    case 'ASSET_FILE':
      if (first.type === second.type) {
        if (first.gitBlobSha != null && second.gitBlobSha != null) {
          return first.gitBlobSha === second.gitBlobSha
        } else if (first.base64 != null && second.base64 != null) {
          return first.base64 === second.base64
        } else {
          return false
        }
      } else {
        return false
      }
    default:
      assertNever(first)
  }
}

export function getProjectContentsChecksums(
  tree: ProjectContentTreeRoot,
  previousChecksums: FileChecksumsWithFile,
): FileChecksumsWithFile {
  const updatedChecksums: FileChecksumsWithFile = {}
  walkContentsTree(tree, (filename, file) => {
    if (file.type !== 'DIRECTORY') {
      let usedPreviousChecksum: boolean = false
      if (filename in previousChecksums) {
        const previousChecksum = previousChecksums[filename]
        if (checkFilesHaveSameContent(previousChecksum.file, file)) {
          updatedChecksums[filename] = previousChecksum
          usedPreviousChecksum = true
        }
      }
      if (!usedPreviousChecksum) {
        switch (file.type) {
          case 'TEXT_FILE':
            updatedChecksums[filename] = {
              file: file,
              checksum: gitBlobChecksumFromText(file.fileContents.code),
            }
            break
          case 'ASSET_FILE':
          case 'IMAGE_FILE':
            if (file.gitBlobSha != null) {
              updatedChecksums[filename] = {
                file: file,
                checksum: file.gitBlobSha,
              }
            } else if (file.base64 != undefined) {
              updatedChecksums[filename] = {
                file: file,
                checksum: gitBlobChecksumFromBase64(file.base64),
              }
            }
            break
          default:
            assertNever(file)
        }
      }
    }
  })

  return updatedChecksums
}

export function deriveGithubFileChanges(
  previousChecksums: FileChecksumsWithFile | null,
  currentChecksums: FileChecksumsWithFile,
  treeConflicts: TreeConflicts,
): GithubFileChanges | null {
  if (previousChecksums == null || currentChecksums == null) {
    return null
  }

  const previousFiles = new Set(Object.keys(previousChecksums))
  const currentFiles = new Set(Object.keys(currentChecksums))

  let untracked: Array<string> = []
  let modified: Array<string> = []
  let deleted: Array<string> = []
  const conflicted: Array<string> = Object.keys(treeConflicts)
  const conflictedSet = new Set(conflicted)

  previousFiles.forEach((f) => {
    if (!conflictedSet.has(f)) {
      if (!currentFiles.has(f)) {
        deleted.push(f)
      } else if (currentChecksums[f].checksum !== previousChecksums[f].checksum) {
        modified.push(f)
      }
    }
  })

  currentFiles.forEach((f) => {
    if (!conflictedSet.has(f)) {
      if (!previousFiles.has(f)) {
        untracked.push(f)
      }
    }
  })

  return {
    untracked,
    modified,
    deleted,
    conflicted,
  }
}

export function projectContentDirectory(
  fullPath: string,
  dir: Directory,
  children: ProjectContentTreeRoot,
): ProjectContentDirectory {
  return {
    type: 'PROJECT_CONTENT_DIRECTORY',
    fullPath: fullPath,
    directory: dir,
    children: children,
  }
}

export function projectContentFile(
  fullPath: string,
  content: TextFile | ImageFile | AssetFile,
): ProjectContentFile {
  return {
    type: 'PROJECT_CONTENT_FILE',
    fullPath: fullPath,
    content: content,
  }
}

export function isProjectContentDirectory(
  projectContentsTree: ProjectContentsTree | null,
): projectContentsTree is ProjectContentDirectory {
  return projectContentsTree != null && projectContentsTree.type === 'PROJECT_CONTENT_DIRECTORY'
}

export function isProjectContentFile(
  projectContentsTree: ProjectContentsTree | null,
): projectContentsTree is ProjectContentFile {
  return projectContentsTree != null && projectContentsTree.type === 'PROJECT_CONTENT_FILE'
}

export function getProjectFileFromTree(tree: ProjectContentsTree): ProjectFile {
  switch (tree.type) {
    case 'PROJECT_CONTENT_FILE':
      return tree.content
    case 'PROJECT_CONTENT_DIRECTORY':
      return tree.directory
    default:
      const _exhaustiveCheck: never = tree
      throw new Error(`Unhandled tree ${JSON.stringify(tree)}`)
  }
}

export function getProjectContentKeyPathElements(projectContentKey: string): Array<string> {
  return dropLeadingSlash(projectContentKey)
    .split('/')
    .filter((s) => s.length > 0)
}

export function pathElementsToProjectContentKey(
  pathElements: Array<string> | ReadonlyArray<string>,
  pathIndex: number,
): string {
  return '/' + pathElements.slice(0, pathIndex + 1).join('/')
}

function addToProjectContentTreeRoot(
  treeRoot: ProjectContentTreeRoot,
  pathElements: ReadonlyArray<string>,
  file: ProjectFile,
): void {
  let workingTreeRoot: ProjectContentTreeRoot = treeRoot
  for (let pathIndex = 0; pathIndex < pathElements.length; pathIndex++) {
    const pathPart = pathElements[pathIndex]
    const isLastElement = pathIndex >= pathElements.length - 1

    function addNewFile(innerValue: ProjectContentsTree | null | undefined): void {
      const pathString = pathElementsToProjectContentKey(pathElements, pathIndex)
      if (isDirectory(file) || !isLastElement) {
        let newTreeRoot: ProjectContentTreeRoot = {}
        if (innerValue != null && innerValue.type === 'PROJECT_CONTENT_DIRECTORY') {
          newTreeRoot = innerValue.children
        } else {
          workingTreeRoot[pathPart] = projectContentDirectory(
            pathString,
            isDirectory(file) ? file : directory(),
            newTreeRoot,
          )
        }
        workingTreeRoot = newTreeRoot
      } else {
        workingTreeRoot[pathPart] = projectContentFile(pathString, file)
      }
    }

    const innerValue = workingTreeRoot[pathPart]
    if (innerValue == null) {
      addNewFile(innerValue)
    } else {
      if (innerValue.type === 'PROJECT_CONTENT_DIRECTORY') {
        addNewFile(innerValue)
      } else {
        if (isLastElement) {
          if (isDirectory(file)) {
            throw new Error(`Cannot replace directory with file at ${pathElements}.`)
          } else {
            innerValue.content = file
          }
        } else {
          addNewFile(innerValue)
        }
      }
    }
  }
}

export function contentsToTree(projectContents: ProjectContents): ProjectContentTreeRoot {
  const contentKeys = Object.keys(projectContents)
  let treeRoot: ProjectContentTreeRoot = {}
  fastForEach(contentKeys, (contentKey) => {
    const pathElements = getProjectContentKeyPathElements(contentKey)
    const file = projectContents[contentKey]

    addToProjectContentTreeRoot(treeRoot, pathElements, file)
  })
  return treeRoot
}

function treeToContentsInner(tree: ProjectContentTreeRoot): ProjectContents {
  const treeKeys = Object.keys(tree)
  return treeKeys.reduce((working, treeKey) => {
    const treePart = tree[treeKey]
    switch (treePart.type) {
      case 'PROJECT_CONTENT_FILE':
        return {
          ...working,
          [treePart.fullPath]: treePart.content,
        }
      case 'PROJECT_CONTENT_DIRECTORY':
        return {
          ...working,
          [treePart.fullPath]: treePart.directory,
          ...treeToContentsInner(treePart.children),
        }
      default:
        const _exhaustiveCheck: never = treePart
        throw new Error(`Unhandled tree part ${JSON.stringify(treePart)}`)
    }
  }, {})
}
export const treeToContents = memoize(treeToContentsInner)

export function walkContentsTree(
  tree: ProjectContentTreeRoot,
  onElement: (fullPath: string, file: ProjectFile) => void,
): void {
  const treeKeys = Object.keys(tree)
  treeKeys.sort()
  Utils.fastForEach(treeKeys, (treeKey) => {
    const treeElement = tree[treeKey]
    switch (treeElement.type) {
      case 'PROJECT_CONTENT_FILE':
        onElement(treeElement.fullPath, treeElement.content)
        break
      case 'PROJECT_CONTENT_DIRECTORY':
        onElement(treeElement.fullPath, treeElement.directory)
        walkContentsTree(treeElement.children, onElement)
        break
      default:
        const _exhaustiveCheck: never = treeElement
        throw new Error(`Unhandled tree element ${JSON.stringify(treeElement)}`)
    }
  })
}

export const contentsTreeOptic: Optic<ProjectContentTreeRoot, PathAndFileEntry> = makeOptic(
  (tree, callback) => {
    walkContentsTree(tree, (fullPath, file) => {
      callback({ fullPath: fullPath, file: file })
    })
  },
  (tree: ProjectContentTreeRoot, modify: (entry: PathAndFileEntry) => PathAndFileEntry) => {
    let result: ProjectContentTreeRoot = {}
    walkContentsTree(tree, (fullPath, file) => {
      const modified: PathAndFileEntry = modify({ fullPath: fullPath, file: file })
      result = addFileToProjectContents(result, modified.fullPath, modified.file)
    })
    return result
  },
)

export function anyCodeAhead(tree: ProjectContentTreeRoot): boolean {
  const revisionsStateOptic = contentsTreeOptic
    .compose(fromField('file'))
    .compose(fromTypeGuard(isTextFile))
    .compose(fromField('fileContents'))
    .compose(filtered((f) => f.parsed.type === 'PARSE_SUCCESS'))
    .compose(fromField('revisionsState'))

  return anyBy(
    revisionsStateOptic,
    (revisionsState) => {
      switch (revisionsState) {
        case 'BOTH_MATCH':
        case 'PARSED_AHEAD':
          return false
        case 'CODE_AHEAD':
        case 'CODE_AHEAD_BUT_PLEASE_TELL_VSCODE_ABOUT_IT':
          return true
        default:
          assertNever(revisionsState)
      }
    },
    tree,
  )
}

export function walkContentsTreeForParseSuccess(
  tree: ProjectContentTreeRoot,
  onElement: (fullPath: string, parseSuccess: ParseSuccess) => void,
): void {
  walkContentsTree(tree, (fullPath, file) => {
    if (isTextFile(file) && isParseSuccess(file.fileContents.parsed)) {
      onElement(fullPath, file.fileContents.parsed)
    }
  })
}

export async function walkContentsTreeAsync(
  tree: ProjectContentTreeRoot,
  onElement: (fullPath: string, file: ProjectFile) => Promise<void>,
): Promise<void> {
  const treeKeys = Object.keys(tree)
  treeKeys.sort()
  for (const treeKey of treeKeys) {
    const treeElement = tree[treeKey]
    switch (treeElement.type) {
      case 'PROJECT_CONTENT_FILE':
        // eslint-disable-next-line no-await-in-loop
        await onElement(treeElement.fullPath, treeElement.content)
        break
      case 'PROJECT_CONTENT_DIRECTORY':
        // eslint-disable-next-line no-await-in-loop
        await onElement(treeElement.fullPath, treeElement.directory)
        // eslint-disable-next-line no-await-in-loop
        await walkContentsTreeAsync(treeElement.children, onElement)
        break
      default:
        const _exhaustiveCheck: never = treeElement
        throw new Error(`Unhandled tree element ${JSON.stringify(treeElement)}`)
    }
  }
}

export function zipContentsTree(
  firstTree: ProjectContentTreeRoot,
  secondTree: ProjectContentTreeRoot,
  onElement: (
    fullPath: string,
    firstContents: ProjectContentsTree | null,
    secondContents: ProjectContentsTree | null,
  ) => boolean,
): void {
  let keys: Set<string> = emptySet()
  Object.keys(firstTree).forEach(keys.add, keys)
  Object.keys(secondTree).forEach(keys.add, keys)
  for (const key of keys) {
    const firstContents = propOrNull(key, firstTree)
    const secondContents = propOrNull(key, secondTree)
    const fullPath = firstContents?.fullPath ?? secondContents?.fullPath
    if (fullPath == null) {
      throw new Error(`Invalid state of both elements being false reached.`)
    } else {
      const shouldContinueDeeper = onElement(fullPath, firstContents, secondContents)
      if (isProjectContentDirectory(secondContents) && shouldContinueDeeper) {
        zipContentsTree(
          isProjectContentDirectory(firstContents) ? firstContents.children : {},
          secondContents.children,
          onElement,
        )
      }
    }
  }
}

export async function zipContentsTreeAsync(
  firstTree: ProjectContentTreeRoot,
  secondTree: ProjectContentTreeRoot,
  onElement: (
    fullPath: string,
    firstContents: ProjectContentsTree | null,
    secondContents: ProjectContentsTree | null,
  ) => Promise<boolean>,
): Promise<void> {
  if (firstTree == secondTree) {
    return Promise.resolve()
  } else {
    let keys: Set<string> = emptySet()
    Object.keys(firstTree).forEach(keys.add, keys)
    Object.keys(secondTree).forEach(keys.add, keys)
    for (const key of keys) {
      const firstContents = propOrNull(key, firstTree)
      const secondContents = propOrNull(key, secondTree)
      const fullPath = firstContents?.fullPath ?? secondContents?.fullPath
      if (fullPath == null) {
        throw new Error(`Invalid state of both elements being false reached.`)
      } else {
        // eslint-disable-next-line no-await-in-loop
        const shouldContinueDeeper = await onElement(fullPath, firstContents, secondContents)
        if (isProjectContentDirectory(secondContents) && shouldContinueDeeper) {
          // eslint-disable-next-line no-await-in-loop
          await zipContentsTreeAsync(
            isProjectContentDirectory(firstContents) ? firstContents.children : {},
            secondContents.children,
            onElement,
          )
        }
      }
    }
  }
}

// FIXME A lot of these files should be moved to a more relevant file
export function getContentsTreeFileFromElements(
  tree: ProjectContentTreeRoot,
  pathElements: ReadonlyArray<string>,
): ProjectFile | null {
  const projectContentsTree = getContentsTreeFromElements(tree, pathElements)
  if (projectContentsTree == null) {
    return null
  }
  return getProjectFileFromTree(projectContentsTree)
}

export function getContentsTreeFromElements(
  tree: ProjectContentTreeRoot,
  pathElements: ReadonlyArray<string>,
): ProjectContentsTree | null {
  if (pathElements.length === 0) {
    throw new Error(`Invalid pathElements.`)
  } else {
    let workingTree: ProjectContentTreeRoot = tree
    for (let index = 0; index < pathElements.length; index++) {
      const pathPart = pathElements[index]
      const treePart = workingTree[pathPart]
      if (treePart == null) {
        return null
      } else {
        if (index === pathElements.length - 1) {
          return treePart
        } else {
          if (treePart.type === 'PROJECT_CONTENT_DIRECTORY') {
            workingTree = treePart.children
          } else {
            return null
          }
        }
      }
    }
    return null
  }
}

export function getProjectFileByFilePath(
  tree: ProjectContentTreeRoot,
  path: string,
): ProjectFile | null {
  return getContentsTreeFileFromElements(tree, getProjectContentKeyPathElements(path))
}

export function getTextFileByPath(projectContents: ProjectContentTreeRoot, path: string): TextFile {
  const possibleResult = getProjectFileByFilePath(projectContents, path)
  if (possibleResult != null && isTextFile(possibleResult)) {
    return possibleResult
  } else {
    throw new Error(`Unable to find a text file at path ${path}.`)
  }
}

export function packageJsonFileFromProjectContents(
  projectContents: ProjectContentTreeRoot,
): ProjectFile | null {
  return getProjectFileByFilePath(projectContents, '/package.json')
}

export function getContentsTreeFromPath(
  tree: ProjectContentTreeRoot,
  path: string,
): ProjectContentsTree | null {
  return getContentsTreeFromElements(tree, getProjectContentKeyPathElements(path))
}

export function addFileToProjectContents(
  tree: ProjectContentTreeRoot,
  path: string,
  file: ProjectFile,
): ProjectContentTreeRoot {
  const pathElements = getProjectContentKeyPathElements(path)

  function createProjectContent(): ProjectContentsTree {
    if (isDirectory(file)) {
      return projectContentDirectory(path, file, {})
    } else {
      return projectContentFile(path, file)
    }
  }

  function addAtCurrentIndex(
    workingTreeRoot: ProjectContentTreeRoot,
    index: number,
  ): ProjectContentTreeRoot {
    const isLastElement = index === pathElements.length - 1
    const pathPart = pathElements[index]
    const treePart = workingTreeRoot[pathPart]
    if (treePart == null) {
      if (isLastElement) {
        return {
          ...workingTreeRoot,
          [pathPart]: createProjectContent(),
        }
      } else {
        return {
          ...workingTreeRoot,
          [pathPart]: projectContentDirectory(
            pathElementsToProjectContentKey(pathElements, index),
            directory(),
            addAtCurrentIndex({}, index + 1),
          ),
        }
      }
    } else {
      if (isLastElement) {
        return {
          ...workingTreeRoot,
          [pathPart]: createProjectContent(),
        }
      } else {
        if (treePart.type === 'PROJECT_CONTENT_FILE') {
          throw new Error(
            `Attempting to add something at ${path} which is below a pre-existing file at ${treePart.fullPath}`,
          )
        } else {
          return {
            ...workingTreeRoot,
            [pathPart]: projectContentDirectory(
              treePart.fullPath,
              treePart.directory,
              addAtCurrentIndex(treePart.children, index + 1),
            ),
          }
        }
      }
    }
  }

  return addAtCurrentIndex(tree, 0)
}

export function getProjectFileFromContents(contents: ProjectContentsTree): ProjectFile {
  switch (contents.type) {
    case 'PROJECT_CONTENT_FILE':
      return contents.content
    case 'PROJECT_CONTENT_DIRECTORY':
      return contents.directory
    default:
      const _exhaustiveCheck: never = contents
      throw new Error(`Unhandled contents ${JSON.stringify(contents)}`)
  }
}

export function removeFromProjectContents(
  projectContents: ProjectContentTreeRoot,
  path: string,
): ProjectContentTreeRoot {
  const pathElements = getProjectContentKeyPathElements(path)

  function removeWithIndex(
    workingTreeRoot: ProjectContentTreeRoot,
    pathIndex: number,
  ): 'NO_CHANGE' | ProjectContentTreeRoot {
    const isLastElement = pathIndex === pathElements.length - 1
    const pathPart = pathElements[pathIndex]
    const treePart = workingTreeRoot[pathPart]

    if (treePart == null) {
      // Reached a part of the tree that doesn't exist, therefore the target
      // doesn't exist.
      return 'NO_CHANGE'
    } else {
      if (isLastElement) {
        let updatedWorkingTreeRoot: ProjectContentTreeRoot = {
          ...workingTreeRoot,
        }
        delete updatedWorkingTreeRoot[pathPart]
        return updatedWorkingTreeRoot
      } else {
        if (treePart.type === 'PROJECT_CONTENT_FILE') {
          // Attempting to delete something which is hierarchically below a file.
          // Which means it can't possibly exist.
          return 'NO_CHANGE'
        } else {
          const innerResult = removeWithIndex(treePart.children, pathIndex + 1)
          if (innerResult === 'NO_CHANGE') {
            return 'NO_CHANGE'
          } else {
            return {
              ...workingTreeRoot,
              [pathPart]: projectContentDirectory(
                treePart.fullPath,
                treePart.directory,
                innerResult,
              ),
            }
          }
        }
      }
    }
  }

  const result = removeWithIndex(projectContents, 0)
  if (result === 'NO_CHANGE') {
    return projectContents
  } else {
    return result
  }
}

export function transformContentsTree(
  projectContents: ProjectContentTreeRoot,
  transform: (tree: ProjectContentsTree) => ProjectContentsTree,
): ProjectContentTreeRoot {
  return mapValues((tree: ProjectContentsTree) => {
    if (tree.type === 'PROJECT_CONTENT_DIRECTORY') {
      const transformedChildren = transformContentsTree(tree.children, transform)
      return transform(projectContentDirectory(tree.fullPath, tree.directory, transformedChildren))
    } else {
      return transform(tree)
    }
  }, projectContents)
}

export function ensureDirectoriesExist(projectContents: ProjectContents): ProjectContents {
  let directoriesUsed: Set<string> = Utils.emptySet()
  let directoriesFound: Set<string> = Utils.emptySet()
  Utils.fastForEach(Object.keys(projectContents), (contentKey) => {
    // Determine what directories are used.
    const pathElements = getProjectContentKeyPathElements(contentKey)
    for (let pathLength = 1; pathLength < pathElements.length; pathLength++) {
      const pathForDir = pathElementsToProjectContentKey(pathElements, pathLength - 1)
      directoriesUsed.add(pathForDir)
    }
    // Determine which directories already exist.
    const content = projectContents[contentKey]
    if (isDirectory(content)) {
      directoriesFound.add(contentKey)
    }
  })

  // Calculate the difference.
  directoriesFound.forEach((directoryPath) => {
    directoriesUsed.delete(directoryPath)
  })

  if (directoriesUsed.size === 0) {
    return projectContents
  } else {
    let result: ProjectContents = { ...projectContents }
    directoriesUsed.forEach((directoryPath) => {
      result[directoryPath] = directory()
    })
    return result
  }
}
