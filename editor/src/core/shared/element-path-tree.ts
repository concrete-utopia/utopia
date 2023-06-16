import { findMaybeConditionalExpression } from '../model/conditionals'
import { MetadataUtils } from '../model/element-metadata-utils'
import { isRight } from './either'
import * as EP from './element-path'
import {
  ElementInstanceMetadataMap,
  isJSExpressionOtherJavaScript,
  isJSXConditionalExpression,
  isJSXElement,
  isJSXFragment,
  isJSXTextBlock,
} from './element-template'
import { ElementPath } from './project-file-types'
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
  if (
    elementPaths.length === 0 ||
    elementPaths[0].parts.length < 1 ||
    elementPaths[0].parts[0].length < 1
  ) {
    return {}
  }

  const root = EP.fromString(elementPaths[0].parts[0][0])
  const missingParents = getMissingParentPaths(elementPaths, metadata)
  const paths = getReorderedPaths(elementPaths, metadata, missingParents)

  let tree: ElementPathTrees = {}
  buildTreeRecursive(root, tree, paths, metadata)

  return tree
}

function buildTreeRecursive(
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
      buildTreeRecursive(path, trees, originalPaths, metadata),
    )
    trees[rootPathString].children.push(subTree)
    children.push(subTree)
  }

  return children
}

function getChildrenPaths(
  metadata: ElementInstanceMetadataMap,
  rootPath: ElementPath,
  originalPaths: ElementPath[],
): ElementPath[] {
  const rootPathString = EP.toString(rootPath)
  const element = metadata[rootPathString]
  if (
    element != null &&
    isRight(element.element) &&
    (isJSXElement(element.element.value) || isJSXFragment(element.element.value)) &&
    element.element.value.children.length > 0
  ) {
    const elementChildren = element.element.value.children
      .filter((child) => !isJSXTextBlock(child) && !isJSExpressionOtherJavaScript(child))
      .map((child) => EP.appendToPath(rootPath, child.uid))

    const dynamicChildrenFromPaths = originalPaths.filter(
      (path) =>
        EP.isChildOf(path, rootPath) &&
        EP.hasDynamicUid(path) &&
        !elementChildren.some((childPath) => EP.pathsEqual(childPath, path)),
    )

    return dynamicChildrenFromPaths.length > 0
      ? elementChildren.concat(dynamicChildrenFromPaths).sort((a, b) => {
          return (
            originalPaths.findIndex((p) => EP.pathsEqual(p, a)) -
            originalPaths.findIndex((p) => EP.pathsEqual(p, b))
          )
        })
      : elementChildren
  } else {
    return originalPaths.filter((path) => EP.isChildOf(path, rootPath))
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
  return [...missingParentPaths.values()]
}

function getReorderedPaths(
  original: ElementPath[],
  metadata: ElementInstanceMetadataMap,
  missingParents: ElementPath[],
): ElementPath[] {
  let elementsToReorderStack = original.filter(
    (path) => findMaybeConditionalExpression(path, metadata) != null,
  )
  let paths = original.filter(
    (path) =>
      !elementsToReorderStack.some((other) => EP.pathsEqual(other, path)) && // omit conditionals, that will be reordered
      !missingParents.some((parentPath) => EP.isDescendantOf(path, parentPath)), // omit elements that have a missing parent
  )

  while (elementsToReorderStack.length > 0) {
    const conditional = elementsToReorderStack.shift()
    if (conditional == null) {
      break
    }

    let index = paths.findIndex((path) => EP.isDescendantOf(path, conditional))
    if (index < 0) {
      // If the index is not found, it may be that it's a conditional with null branches, so
      // we need to find it inside its parent and use the sum index for the reordering.
      const parent = MetadataUtils.getParent(metadata, conditional)
      if (parent != null && isRight(parent.element)) {
        if (isJSXElement(parent.element.value) || isJSXFragment(parent.element.value)) {
          const innerIndex = parent.element.value.children.findIndex((child) =>
            EP.pathsEqual(EP.appendToPath(parent.elementPath, child.uid), conditional),
          )
          const parentIndex = paths.findIndex((path) => EP.pathsEqual(parent.elementPath, path))
          if (parentIndex >= 0) {
            index = innerIndex + parentIndex
          }
        } else if (isJSXConditionalExpression(parent.element.value)) {
          index = paths.findIndex((path) => EP.pathsEqual(parent.elementPath, path))
        }
      }
    }

    if (index >= 0) {
      const before = paths.slice(0, index)
      const after = paths.slice(index)
      paths = [...before, conditional, ...after]
    }
  }

  return paths
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
