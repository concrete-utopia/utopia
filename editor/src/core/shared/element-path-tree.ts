import { isFeatureEnabled } from '../../utils/feature-switches'
import { MetadataUtils } from '../model/element-metadata-utils'
import { forEachRight, isLeft } from './either'
import * as EP from './element-path'
import type { ElementInstanceMetadataMap, JSXElementChild } from './element-template'
import { isJSXConditionalExpression, isJSXElement, isJSXFragment } from './element-template'
import type { ElementPath } from './project-file-types'
import { fastForEach } from './utils'

export interface ElementPathTree {
  path: ElementPath
  pathString: string
  children: Array<ElementPathTree>
}

export function elementPathTree(
  path: ElementPath,
  pathString: string,
  children: Array<ElementPathTree>,
): ElementPathTree {
  return {
    path: path,
    pathString: pathString,
    children: children,
  }
}

export type ElementPathTrees = { [key: string]: ElementPathTree }

export function buildTree(...metadatas: Array<ElementInstanceMetadataMap>): ElementPathTrees {
  let tree: ElementPathTrees = {}
  for (const metadata of metadatas) {
    const elementPaths = Object.values(metadata).map((m) => m.elementPath)
    if (elementPaths.length === 0) {
      return {}
    }
    const root = EP.getStoryboardPathFromPath(elementPaths[0])
    if (root == null) {
      return {}
    }

    const missingParents = getMissingParentPaths(elementPaths, metadata)
    const paths = getReorderedPaths(elementPaths, metadata, missingParents)

    buildTree_MUTATE(root, tree, paths, metadata)
  }

  return tree
}

function buildTree_MUTATE(
  rootPath: ElementPath,
  trees: ElementPathTrees,
  originalPaths: ElementPath[],
  metadata: ElementInstanceMetadataMap,
): void {
  const rootPathString = EP.toString(rootPath)
  trees[rootPathString] = elementPathTree(rootPath, rootPathString, [])

  function addPathToTree(path: ElementPath): void {
    const pathString = EP.toString(path)
    const alreadyAdded = pathString in trees
    if (!alreadyAdded) {
      const parentPath = EP.parentPath(path)
      const subTree = elementPathTree(path, pathString, [])
      trees[pathString] = subTree
      trees[EP.toString(parentPath)].children.push(subTree)
    }
  }

  let workingPaths = [...originalPaths]
  workingPaths.sort((a, b) => {
    return EP.fullDepth(a) - EP.fullDepth(b)
  })
  for (const workingPath of workingPaths) {
    addPathToTree(workingPath)
  }

  for (const elementMetadata of Object.values(metadata)) {
    forEachRight(elementMetadata.element, (element) => {
      switch (element.type) {
        case 'JSX_ELEMENT':
        case 'JSX_FRAGMENT':
          {
            let elementChildren: Array<JSXElementChild>
            if (isFeatureEnabled('Condensed Navigator Entries')) {
              elementChildren = element.children
            } else {
              elementChildren = element.children.filter((child) => {
                switch (child.type) {
                  case 'JSX_TEXT_BLOCK':
                  case 'ATTRIBUTE_OTHER_JAVASCRIPT':
                  case 'JSX_MAP_EXPRESSION':
                  case 'JS_IDENTIFIER':
                  case 'JS_PROPERTY_ACCESS':
                  case 'JS_ELEMENT_ACCESS':
                    return false
                  default:
                    return true
                }
              })
            }
            for (const child of elementChildren) {
              const childPath = EP.appendToPath(elementMetadata.elementPath, child.uid)
              addPathToTree(childPath)
            }
          }
          break
        default:
          break
      }
    })
  }

  function walkTreeSortingChildren(tree: ElementPathTree): void {
    tree.children.sort(
      (a, b) =>
        originalPaths.findIndex((p) => EP.pathsEqual(p, a.path)) -
        originalPaths.findIndex((p) => EP.pathsEqual(p, b.path)),
    )
    for (const child of tree.children) {
      walkTreeSortingChildren(child)
    }
  }

  walkTreeSortingChildren(trees[rootPathString])
}

function getMissingParentPaths(
  paths: ElementPath[],
  metadata: ElementInstanceMetadataMap,
): ElementPath[] {
  const missingParentPaths = new Set<ElementPath>()
  for (const path of paths) {
    let parent = EP.parentPath(path)
    while (parent.parts.length > 0) {
      if (metadata[EP.toString(parent)] == null) {
        missingParentPaths.add(parent)
      }
      parent = EP.parentPath(parent)
    }
  }
  return Array.from(missingParentPaths)
}

function getReorderedPaths(
  original: ElementPath[],
  metadata: ElementInstanceMetadataMap,
  missingParents: ElementPath[],
): ElementPath[] {
  const elementsToReorder = original.filter((path) => {
    const elementMetadata = MetadataUtils.findElementByElementPath(metadata, path)
    if (elementMetadata == null || isLeft(elementMetadata.element)) {
      return false
    }
    const element = elementMetadata.element.value
    switch (element.type) {
      case 'JSX_CONDITIONAL_EXPRESSION':
      case 'ATTRIBUTE_OTHER_JAVASCRIPT':
      case 'JSX_MAP_EXPRESSION':
      case 'JS_IDENTIFIER':
      case 'JS_ELEMENT_ACCESS':
      case 'JS_PROPERTY_ACCESS':
        return true
      default:
        return false
    }
  })
  const pathsToBeReordered = original.filter(
    (path) =>
      !elementsToReorder.some((other) => EP.pathsEqual(other, path)) && // omit conditionals, that will be reordered
      !missingParents.some((parentPath) => EP.isDescendantOf(path, parentPath)), // omit elements that have a missing parent
  )
  return elementsToReorder.reduce((paths, path) => {
    const index = getReorderedIndexInPaths(paths, metadata, path)
    if (index === 'do-not-reorder') {
      return paths
    }
    const before = paths.slice(0, index)
    const after = paths.slice(index)
    return [...before, path, ...after]
  }, pathsToBeReordered)
}

function getReorderedIndexInPaths(
  paths: ElementPath[],
  metadata: ElementInstanceMetadataMap,
  conditionalPath: ElementPath,
): number | 'do-not-reorder' {
  const index = paths.findIndex((path) => EP.isDescendantOf(path, conditionalPath))
  if (index >= 0) {
    return index
  }

  const parent = MetadataUtils.getParent(metadata, conditionalPath)
  if (parent == null || isLeft(parent.element)) {
    return 'do-not-reorder'
  }

  if (isJSXElement(parent.element.value) || isJSXFragment(parent.element.value)) {
    const innerIndex = parent.element.value.children.findIndex((child) => {
      const childPath = EP.appendToPath(parent.elementPath, child.uid)
      return EP.pathsEqual(childPath, conditionalPath)
    })
    const parentIndex = paths.findIndex((path) => EP.pathsEqual(parent.elementPath, path))
    if (parentIndex < 0) {
      return 'do-not-reorder'
    }
    return parentIndex + innerIndex
  } else if (isJSXConditionalExpression(parent.element.value)) {
    const parentIndex = paths.findIndex((path) => EP.pathsEqual(parent.elementPath, path))
    if (parentIndex < 0) {
      return 'do-not-reorder'
    }
    return parentIndex
  } else {
    return 'do-not-reorder'
  }
}

export function getStoryboardRoot(trees: ElementPathTrees): ElementPathTree | null {
  const storyboardTree = Object.values(trees).find((e) => EP.isStoryboardPath(e.path))
  return storyboardTree ?? null
}

export function getCanvasRoots(trees: ElementPathTrees): Array<ElementPathTree> {
  const storyboardTree = getStoryboardRoot(trees)
  if (storyboardTree == null) {
    return []
  }

  return storyboardTree.children
}

export function printTree(treeRoot: ElementPathTrees): string {
  let outputText: string = ''
  function printElement(element: ElementPathTree, depth: number): void {
    for (let index = 0; index < depth; index++) {
      outputText += '  '
    }
    outputText += EP.toString(element.path)
    outputText += '\n'
    for (const child of element.children) {
      printElement(child, depth + 1)
    }
  }
  const storyboardRoot = getStoryboardRoot(treeRoot)
  if (storyboardRoot != null) {
    printElement(storyboardRoot, 0)
  }
  return outputText
}

export function forEachChildOfTarget(
  treeRoot: ElementPathTrees,
  target: ElementPath,
  handler: (elementPath: ElementPath) => void,
): void {
  const foundTree = getSubTree(treeRoot, target)
  if (foundTree != null) {
    fastForEach(Object.values(foundTree.children), (subTree) => {
      handler(subTree.path)
    })
  }
}

export function getSubTree(
  treeRoot: ElementPathTrees,
  target: ElementPath,
): ElementPathTree | null {
  const subTree = treeRoot[EP.toString(target)]
  return subTree ?? null
}
