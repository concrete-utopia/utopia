import {
  ProjectContents,
  ImageFile,
  ProjectFile,
  Directory,
  TextFile,
  AssetFile,
  ParseSuccess,
  isTextFile,
  isParseSuccess,
  isAssetFile,
} from '../core/shared/project-file-types'
import { isDirectory, directory, isImageFile } from '../core/model/project-file-utils'
import Utils from '../utils/utils'
import { dropLeadingSlash } from './filebrowser/filepath-utils'
import { fastForEach } from '../core/shared/utils'
import { mapValues, propOrNull } from '../core/shared/object-utils'
import { emptySet } from '../core/shared/set-utils'
import { sha1 } from 'sha.js'
import { GithubFileChanges, TreeConflicts } from '../core/shared/github'
import { GithubChecksums } from './editor/store/editor-state'

export interface AssetFileWithFileName {
  fileName: string
  file: ImageFile | AssetFile
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

function getSHA1Checksum(contents: string): string {
  return new sha1().update(contents).digest('hex')
}

export function getProjectContentsChecksums(tree: ProjectContentTreeRoot): GithubChecksums {
  const contents = treeToContents(tree)

  const checksums: GithubChecksums = {}
  Object.keys(contents).forEach((filename) => {
    const file = contents[filename]
    if (isTextFile(file)) {
      checksums[filename] = getSHA1Checksum(file.fileContents.code)
    } else if (isAssetFile(file) && file.base64 != undefined) {
      checksums[filename] = getSHA1Checksum(file.base64)
    }
  })

  return checksums
}

export function deriveGithubFileChanges(
  projectChecksums: GithubChecksums,
  githubChecksums: GithubChecksums | null,
  treeConflicts: TreeConflicts,
): GithubFileChanges | null {
  if (githubChecksums == null || Object.keys(githubChecksums).length === 0) {
    return null
  }

  const projectFiles = new Set(Object.keys(projectChecksums))
  const githubFiles = new Set(Object.keys(githubChecksums))

  let untracked: Array<string> = []
  let modified: Array<string> = []
  let deleted: Array<string> = []
  const conflicted: Array<string> = Object.keys(treeConflicts)
  const conflictedSet = new Set(conflicted)

  projectFiles.forEach((f) => {
    if (!conflictedSet.has(f)) {
      if (!githubFiles.has(f)) {
        untracked.push(f)
      } else if (githubChecksums[f] !== projectChecksums[f]) {
        modified.push(f)
      }
    }
  })

  githubFiles.forEach((f) => {
    if (!conflictedSet.has(f)) {
      if (!projectFiles.has(f)) {
        deleted.push(f)
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

// Ensure this is kept up to date with clientmodel/lib/src/Utopia/ClientModel.hs.
export type ProjectContentTreeRoot = { [key: string]: ProjectContentsTree }

// Ensure this is kept up to date with clientmodel/lib/src/Utopia/ClientModel.hs
export interface ProjectContentDirectory {
  type: 'PROJECT_CONTENT_DIRECTORY'
  fullPath: string
  directory: Directory
  children: ProjectContentTreeRoot
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

// Ensure this is kept up to date with clientmodel/lib/src/Utopia/ClientModel.hs.
export interface ProjectContentFile {
  type: 'PROJECT_CONTENT_FILE'
  fullPath: string
  content: TextFile | ImageFile | AssetFile
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

// Ensure this is kept up to date with clientmodel/lib/src/Utopia/ClientModel.hs.
export type ProjectContentsTree = ProjectContentDirectory | ProjectContentFile

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

export function treeToContents(tree: ProjectContentTreeRoot): ProjectContents {
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
          ...treeToContents(treePart.children),
        }
      default:
        const _exhaustiveCheck: never = treePart
        throw new Error(`Unhandled tree part ${JSON.stringify(treePart)}`)
    }
  }, {})
}

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
          return getProjectFileFromTree(treePart)
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

export function getContentsTreeFileFromString(
  tree: ProjectContentTreeRoot,
  path: string,
): ProjectFile | null {
  return getContentsTreeFileFromElements(tree, getProjectContentKeyPathElements(path))
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
