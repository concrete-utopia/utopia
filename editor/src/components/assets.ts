import {
  ProjectContents,
  ImageFile,
  ProjectFile,
  Directory,
  CodeFile,
  UIJSFile,
  AssetFile,
} from '../core/shared/project-file-types'
import { isDirectory, directory, isImageFile } from '../core/model/project-file-utils'
import Utils from '../utils/utils'
import { dropLeadingSlash } from './filebrowser/filepath-utils'
import { fastForEach } from '../core/shared/utils'
import { mapValues } from '../core/shared/object-utils'

interface ImageAsset {
  assetPath: string
  asset: ImageFile
}

function imageAsset(assetPath: string, asset: ImageFile): ImageAsset {
  return {
    assetPath: assetPath,
    asset: asset,
  }
}

export function getImageAssets(projectContents: ProjectContents): Array<ImageAsset> {
  const result: Array<ImageAsset> = []
  Utils.fastForEach(Object.keys(projectContents), (projectContentKey) => {
    const projectContent = projectContents[projectContentKey]
    if (isImageFile(projectContent)) {
      result.push(imageAsset(projectContentKey, projectContent))
    }
  })
  return result
}

export type ProjectContentTreeRoot = { [key: string]: ProjectContentsTree }

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

export interface ProjectContentFile {
  type: 'PROJECT_CONTENT_FILE'
  fullPath: string
  content: UIJSFile | CodeFile | ImageFile | AssetFile
}

export function projectContentFile(
  fullPath: string,
  content: UIJSFile | CodeFile | ImageFile | AssetFile,
): ProjectContentFile {
  return {
    type: 'PROJECT_CONTENT_FILE',
    fullPath: fullPath,
    content: content,
  }
}

export type ProjectContentsTree = ProjectContentDirectory | ProjectContentFile

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

// FIXME A lot of these files should be moved to a more relevant file
export function getContentsTreeFileFromElements(
  tree: ProjectContentTreeRoot,
  pathElements: ReadonlyArray<string>,
): ProjectFile | null {
  if (pathElements.length === 0) {
    throw new Error(`Invalid pathElements.`)
  } else {
    function getFileWithIndex(
      currentTree: ProjectContentTreeRoot,
      index: number,
    ): ProjectFile | null {
      const pathPart = pathElements[index]
      const treePart = currentTree[pathPart]
      if (treePart == null) {
        return null
      } else {
        if (index === pathElements.length - 1) {
          return getProjectFileFromTree(treePart)
        } else {
          if (treePart.type === 'PROJECT_CONTENT_DIRECTORY') {
            return getFileWithIndex(treePart.children, index + 1)
          } else {
            return null
          }
        }
      }
    }
    return getFileWithIndex(tree, 0)
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
