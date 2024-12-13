import { isFeatureEnabled } from '../../utils/feature-switches'
import { MetadataUtils } from '../model/element-metadata-utils'
import { forEachRight, isLeft } from './either'
import * as EP from './element-path'
import type { ElementInstanceMetadataMap, JSXElementChild } from './element-template'
import {
  isJSXConditionalExpression,
  isJSXElement,
  isJSXElementLike,
  isJSXFragment,
} from './element-template'
import type { ElementPath } from './project-file-types'
import { assertNever } from './utils'

export interface ElementPathTree {
  path: ElementPath
  pathString: string
  innerChildren: Array<ElementPathTree>
  propsChildren: Array<ElementPathTree>
}

export function elementPathTree(
  path: ElementPath,
  pathString: string,
  innerChildren: Array<ElementPathTree>,
  propsChildren: Array<ElementPathTree>,
): ElementPathTree {
  return {
    path: path,
    pathString: pathString,
    innerChildren: innerChildren,
    propsChildren: propsChildren,
  }
}

export function getElementPathTreeChildren(tree: ElementPathTree): Array<ElementPathTree> {
  return [...tree.innerChildren, ...tree.propsChildren]
}

export function forEachElementPathTreeChild(
  tree: ElementPathTree,
  handler: (elementPathTree: ElementPathTree) => void,
): void {
  tree.innerChildren.forEach(handler)
  tree.propsChildren.forEach(handler)
}

export type ElementPathTrees = { [key: string]: ElementPathTree }

export function buildTree(...metadatas: Array<ElementInstanceMetadataMap>): ElementPathTrees {
  let tree: ElementPathTrees = {}
  for (const metadata of metadatas) {
    // The tree should maintain the ordering of the statically defined elements,
    // But the metadata itself may not match that ordering.
    const elementPaths = Object.values(metadata).map((m) => m.elementPath)
    const firstElementPath = elementPaths.at(0)
    if (firstElementPath == null) {
      continue
    }
    const root = EP.getStoryboardPathFromPath(firstElementPath)
    if (root == null) {
      continue
    }

    const missingParents = getMissingParentPaths(elementPaths, metadata)
    const paths = getReorderedPaths(elementPaths, metadata, missingParents)

    buildTree_MUTATE(root, tree, paths, metadata)
  }

  return tree
}

function addRootToTree(rootPath: ElementPath, trees: ElementPathTrees): void {
  const rootPathString = EP.toString(rootPath)
  if (!(rootPathString in trees)) {
    trees[rootPathString] = elementPathTree(rootPath, rootPathString, [], [])
  }
}

function buildTree_MUTATE(
  rootPath: ElementPath,
  trees: ElementPathTrees,
  originalPaths: ElementPath[],
  metadata: ElementInstanceMetadataMap,
): void {
  addRootToTree(rootPath, trees)

  function addPathToTree(path: ElementPath): void {
    const pathString = EP.toString(path)
    const alreadyAdded = pathString in trees
    if (!alreadyAdded) {
      const parentPath = EP.parentPath(path)
      const subTree = elementPathTree(path, pathString, [], [])
      trees[pathString] = subTree
      const pathLastPart = EP.lastElementPathForPath(path)

      if (pathLastPart?.length === 1) {
        // If the last part of the path is a single element, it should be added to the inner children.
        // This would be a path like `a/b/c:d`.
        trees[EP.toString(parentPath)].innerChildren.push(subTree)
      } else {
        // Otherwise this should be added to the props children.
        trees[EP.toString(parentPath)].propsChildren.push(subTree)
      }
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
      if (isJSXElementLike(element)) {
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
              case 'JSX_ELEMENT':
              case 'JSX_CONDITIONAL_EXPRESSION':
              case 'JSX_FRAGMENT':
              case 'ATTRIBUTE_FUNCTION_CALL':
              case 'ATTRIBUTE_NESTED_ARRAY':
              case 'ATTRIBUTE_NESTED_OBJECT':
              case 'ATTRIBUTE_VALUE':
                return true
              default:
                assertNever(child)
            }
          })
        }
        for (const child of elementChildren) {
          const childPath = EP.appendToPath(elementMetadata.elementPath, child.uid)
          addPathToTree(childPath)
        }
      }
    })
  }
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
  const pathsToBeReordered = original.filter((path) => {
    // Omit elements that have a missing ancestor.
    return !missingParents.some((parentPath) => EP.isDescendantOf(path, parentPath))
  })

  return original.reduce((paths, path) => {
    const index = getReorderedIndexInPaths(paths, metadata, path)
    if (index === 'do-not-reorder') {
      return paths
    }
    const pathsWithout = paths.filter((pathEntry) => !EP.pathsEqual(pathEntry, path))
    const before = pathsWithout.slice(0, index)
    const after = pathsWithout.slice(index)
    return [...before, path, ...after]
  }, pathsToBeReordered)
}

function getReorderedIndexInPaths(
  paths: ElementPath[],
  metadata: ElementInstanceMetadataMap,
  pathToReorder: ElementPath,
): number | 'do-not-reorder' {
  const parent = MetadataUtils.getParent(metadata, pathToReorder)
  if (parent == null || isLeft(parent.element)) {
    return 'do-not-reorder'
  }
  const parentElement = parent.element.value

  if (isJSXElement(parentElement) || isJSXFragment(parentElement)) {
    let innerIndex: number | null = null
    let priorSiblings: Array<JSXElementChild> = []
    for (let childIndex = 0; childIndex < parentElement.children.length; childIndex++) {
      const child = parentElement.children[childIndex]
      const childPath = EP.appendToPath(parent.elementPath, child.uid)
      if (EP.pathsEqual(childPath, pathToReorder)) {
        // We've found the item that we're trying to reorder, so record the index
        // and stop searching.
        innerIndex = childIndex
        break
      } else {
        // As we haven't reached the element of interest, record
        // this prior sibling for later use.
        priorSiblings.push(child)
      }
    }
    if (innerIndex == null) {
      return 'do-not-reorder'
    }

    const parentIndex = paths.findIndex((path) => EP.pathsEqual(parent.elementPath, path))

    if (innerIndex === 0) {
      if (parentIndex < 0) {
        return 'do-not-reorder'
      } else {
        // As this is the first item, shift the index past the parent.
        return parentIndex + 1
      }
    } else {
      const priorSiblingPaths = priorSiblings.map((sibling) => {
        return EP.appendToPath(parent.elementPath, sibling.uid)
      })
      // As there are prior siblings, we need to count those and their descendants,
      // so as to shift this element past them.
      const priorSiblingDescendants = paths.reduce((workingCount, path) => {
        if (
          priorSiblingPaths.some((priorSiblingPath) => {
            return EP.isDescendantOfOrEqualTo(path, priorSiblingPath)
          })
        ) {
          return workingCount + 1
        } else {
          return workingCount
        }
      }, 0)

      // Shift by the parent position, add 1 to put it past the parent and then shift it by the
      // count of the prior siblings and their descendants.
      return parentIndex + 1 + priorSiblingDescendants
    }
  } else if (isJSXConditionalExpression(parentElement)) {
    const parentIndex = paths.findIndex((path) => EP.pathsEqual(parent.elementPath, path))
    if (parentIndex < 0) {
      return 'do-not-reorder'
    } else {
      return parentIndex
    }
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

  return getElementPathTreeChildren(storyboardTree)
}

export function printTree(treeRoot: ElementPathTrees): string {
  let outputText: string = ''
  function printElement(element: ElementPathTree, depth: number): void {
    for (let index = 0; index < depth; index++) {
      outputText += '  '
    }
    outputText += EP.toString(element.path)
    outputText += '\n'
    for (const child of getElementPathTreeChildren(element)) {
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
    forEachElementPathTreeChild(foundTree, (subTree) => {
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
