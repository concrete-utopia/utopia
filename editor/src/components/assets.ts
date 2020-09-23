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
import * as R from 'ramda'
import { dropLeadingSlash } from './filebrowser/filepath-utils'
import { fastForEach } from '../core/shared/utils'

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
  type: 'DIRECTORY'
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
    type: 'DIRECTORY',
    fullPath: fullPath,
    directory: dir,
    children: children,
  }
}

export interface ProjectContentFile {
  type: 'FILE'
  fullPath: string
  content: UIJSFile | CodeFile | ImageFile | AssetFile
}

export function projectContentFile(
  fullPath: string,
  content: UIJSFile | CodeFile | ImageFile | AssetFile,
): ProjectContentFile {
  return {
    type: 'FILE',
    fullPath: fullPath,
    content: content,
  }
}

export type ProjectContentsTree = ProjectContentDirectory | ProjectContentFile

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

export function addToProjectContentTreeRoot(
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
        if (innerValue != null && innerValue.type === 'DIRECTORY') {
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
      if (innerValue.type === 'DIRECTORY') {
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
      case 'FILE':
        return {
          ...working,
          [treePart.fullPath]: treePart.content,
        }
      case 'DIRECTORY':
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
      case 'FILE':
        onElement(treeElement.fullPath, treeElement.content)
        break
      case 'DIRECTORY':
        onElement(treeElement.fullPath, treeElement.directory)
        walkContentsTree(treeElement.children, onElement)
        break
      default:
        const _exhaustiveCheck: never = treeElement
        throw new Error(`Unhandled tree element ${JSON.stringify(treeElement)}`)
    }
  })
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
