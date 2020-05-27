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

export function mergeTrees(
  first: ProjectContentTreeRoot,
  second: ProjectContentTreeRoot,
): ProjectContentTreeRoot {
  let merged: ProjectContentTreeRoot = {}
  Utils.fastForEach(Object.keys(first), (firstKey) => {
    const fromFirst = first[firstKey]
    if (firstKey in second) {
      const fromSecond = second[firstKey]
      if (fromFirst.type === 'DIRECTORY') {
        if (fromSecond.type === 'DIRECTORY') {
          merged[firstKey] = projectContentDirectory(
            fromSecond.fullPath,
            fromSecond.directory,
            mergeTrees(fromFirst.children, fromSecond.children),
          )
        } else {
          throw new Error(
            `Unable to merge ${JSON.stringify(fromSecond)} into ${JSON.stringify(fromFirst)}`,
          )
        }
      } else {
        if (fromSecond.type === 'FILE') {
          merged[firstKey] = fromSecond
        } else {
          throw new Error(
            `Unable to merge ${JSON.stringify(fromSecond)} into ${JSON.stringify(fromFirst)}`,
          )
        }
      }
    } else {
      merged[firstKey] = fromFirst
    }
  })
  Utils.fastForEach(Object.keys(second), (secondKey) => {
    if (!(secondKey in merged)) {
      merged[secondKey] = second[secondKey]
    }
  })
  return merged
}

export function getProjectContentKeyPathElements(projectContentKey: string): Array<string> {
  return dropLeadingSlash(projectContentKey).split('/')
}

export function pathElementsToProjectContentKey(elements: Array<string>): string {
  return '/' + elements.join('/')
}

export function contentFileToTreeRoot(
  fullPath: string,
  projectFile: ProjectFile,
): ProjectContentTreeRoot {
  const pathElements = getProjectContentKeyPathElements(fullPath)
  const leaf = isDirectory(projectFile)
    ? projectContentDirectory(fullPath, projectFile, {})
    : projectContentFile(fullPath, projectFile)
  const rootDirectory = R.range(1, pathElements.length).reduceRight(
    (previous, pathElementLength) => {
      const pathForDir = pathElementsToProjectContentKey(pathElements.slice(0, pathElementLength))
      return projectContentDirectory(pathForDir, directory(), {
        [pathElements[pathElementLength]]: previous,
      })
    },
    leaf,
  )
  const treeRoot = {
    [pathElements[0]]: rootDirectory,
  }
  return treeRoot
}

export function contentsToTree(projectContents: ProjectContents): ProjectContentTreeRoot {
  const contentKeys = Object.keys(projectContents)
  return contentKeys.reduce((working, contentKey) => {
    const tree = contentFileToTreeRoot(contentKey, projectContents[contentKey])
    return mergeTrees(working, tree)
  }, {})
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
      const pathForDir = pathElementsToProjectContentKey(pathElements.slice(0, pathLength))
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
