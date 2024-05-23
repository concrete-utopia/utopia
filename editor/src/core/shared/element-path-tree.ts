import { isFeatureEnabled } from '../../utils/feature-switches'
import { MetadataUtils } from '../model/element-metadata-utils'
import { isLeft, isRight } from './either'
import * as EP from './element-path'
import type { ElementInstanceMetadataMap } from './element-template'
import {
  isJSElementAccess,
  isJSExpressionMapOrOtherJavaScript,
  isJSIdentifier,
  isJSPropertyAccess,
  isJSXConditionalExpression,
  isJSXElement,
  isJSXFragment,
  isJSXTextBlock,
} from './element-template'
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

export function buildTree(metadata: ElementInstanceMetadataMap): ElementPathTrees {
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

  let tree: ElementPathTrees = {}
  buildTreeRecursive_MUTATE(root, tree, paths, metadata)

  return tree
}

function buildTreeRecursive_MUTATE(
  rootPath: ElementPath,
  trees: ElementPathTrees,
  originalPaths: ElementPath[],
  metadata: ElementInstanceMetadataMap,
): ElementPathTree[] {
  const rootPathString = EP.toString(rootPath)
  trees[rootPathString] = elementPathTree(rootPath, rootPathString, [])

  const childrenPaths = getChildrenPaths(metadata, rootPath, originalPaths)

  let children: ElementPathTree[] = []
  for (const path of childrenPaths) {
    const pathString = EP.toString(path)
    const subTree = elementPathTree(
      path,
      pathString,
      buildTreeRecursive_MUTATE(path, trees, originalPaths, metadata),
    )
    trees[rootPathString].children.push(subTree)
    children.push(subTree)
  }

  return children
}

function getChildrenPaths(
  metadata: ElementInstanceMetadataMap,
  rootPath: ElementPath,
  paths: ElementPath[],
): ElementPath[] {
  const element = metadata[EP.toString(rootPath)]

  // Grab the child elements from the element metadata, if available.
  let childrenFromElement: ElementPath[] = []
  if (
    element != null &&
    isRight(element.element) &&
    (isJSXElement(element.element.value) || isJSXFragment(element.element.value)) &&
    element.element.value.children.length > 0
  ) {
    childrenFromElement = element.element.value.children
      .filter((child) => {
        if (isFeatureEnabled('Condensed Navigator Entries')) {
          // if Data Entries are enabled, we should always show them in the Navigator
          return true
        } else {
          return (
            !isJSXTextBlock(child) &&
            !isJSExpressionMapOrOtherJavaScript(child) &&
            !isJSIdentifier(child) &&
            !isJSPropertyAccess(child) &&
            !isJSElementAccess(child)
          )
        }
      })
      .map((child) => EP.appendToPath(rootPath, child.uid))
  }

  // Then, grab any other child from the paths array, which is not included in the
  // elements from the metadata.
  const otherChildrenFromPaths = paths.filter(
    (path) =>
      EP.isChildOf(path, rootPath) &&
      !childrenFromElement.some((other) => EP.pathsEqual(other, path)),
  )

  // If there are children outside the element metadata, need to merge the two and sort them.
  // Otherwise, return the children from the meta.
  return otherChildrenFromPaths.length > 0
    ? childrenFromElement
        .concat(otherChildrenFromPaths)
        .sort(
          (a, b) =>
            paths.findIndex((p) => EP.pathsEqual(p, a)) -
            paths.findIndex((p) => EP.pathsEqual(p, b)),
        )
    : childrenFromElement
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
    const element = MetadataUtils.findElementByElementPath(metadata, path)
    if (element == null) {
      return false
    }
    return (
      MetadataUtils.isConditionalFromMetadata(element) ||
      MetadataUtils.isExpressionOtherJavascriptFromMetadata(element) ||
      MetadataUtils.isJSXMapExpressionFromMetadata(element) ||
      MetadataUtils.isIdentifierFromMetadata(element) ||
      MetadataUtils.isPropertyAccessFromMetadata(element) ||
      MetadataUtils.isElementAccessFromMetadata(element)
    )
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
