import { ElementPath, ElementPathPart } from './project-file-types'
import * as EP from './element-path'
import { fastForEach } from './utils'
import { ElementInstanceMetadataMap, isUtopiaElement } from './element-template'
import { MetadataUtils } from '../model/element-metadata-utils'
import { foldEither } from './either'
import { move } from './array-utils'
import { getUtopiaID } from './uid-utils'

export interface ElementPathTree {
  path: ElementPath
  children: ElementPathTreeRoot
}

function emptyElementPathTree(path: ElementPath): ElementPathTree {
  return {
    path: path,
    children: {},
  }
}

export type ElementPathTreeRoot = { [key: string]: ElementPathTree }

export function buildTree(elementPaths: Array<ElementPath>): ElementPathTreeRoot {
  let result: ElementPathTreeRoot = {}
  let workingRoot: ElementPathTreeRoot = result
  let currentPathParts: Array<ElementPathPart> = []
  // Loop over each and every path that needs to be added.
  for (let elementPathIndex = 0; elementPathIndex < elementPaths.length; elementPathIndex++) {
    const elementPath = elementPaths[elementPathIndex]
    workingRoot = result
    currentPathParts = []
    // Loop over the array segments of the path that is being added.
    for (let pathPartIndex = 0; pathPartIndex < elementPath.parts.length; pathPartIndex++) {
      const pathPart = elementPath.parts[pathPartIndex]
      currentPathParts[pathPartIndex] = []
      let currentPathPart = currentPathParts[pathPartIndex]
      // Loop over the parts of the of the path segment.
      for (
        let pathPartElementIndex = 0;
        pathPartElementIndex < pathPart.length;
        pathPartElementIndex++
      ) {
        const pathPartElement = pathPart[pathPartElementIndex]
        currentPathPart.push(pathPartElement)
        const subElementPath = EP.elementPath(currentPathParts)

        // See if there's an already existing part of the tree with this path.
        const subElementPathString = EP.toString(subElementPath)
        const foundTree = workingRoot[subElementPathString]
        let treeToTarget = foundTree ?? emptyElementPathTree(subElementPath)
        // Add this if it doesn't already exist.
        if (foundTree == null) {
          workingRoot[subElementPathString] = treeToTarget
        }
        workingRoot = treeToTarget.children
      }
    }
  }
  return result
}

export function reorderTree(
  tree: ElementPathTree,
  metadata: ElementInstanceMetadataMap,
): ElementPathTree {
  const element = MetadataUtils.findElementByElementPath(metadata, tree.path)
  if (element == null) {
    return tree
  } else {
    return foldEither(
      () => {
        return tree
      },
      (elementChild) => {
        switch (elementChild.type) {
          case 'JSX_ELEMENT': {
            const allChildrenAreElements = elementChild.children.every(isUtopiaElement)
            if (allChildrenAreElements) {
              let updatedChildrenArray: Array<{ key: string; value: ElementPathTree }> =
                Object.keys(tree.children).map((childKey) => {
                  return { key: childKey, value: tree.children[childKey] }
                })
              elementChild.children.forEach((child, childIndex) => {
                const uid = getUtopiaID(child)
                const workingTreeIndex = updatedChildrenArray.findIndex((workingTreeChild) => {
                  return EP.toUid(workingTreeChild.value.path) === uid
                })
                if (workingTreeIndex !== childIndex) {
                  updatedChildrenArray = move(workingTreeIndex, childIndex, updatedChildrenArray)
                }
              })
              let updatedChildren: ElementPathTreeRoot = {}
              for (const { key, value } of updatedChildrenArray) {
                updatedChildren[key] = reorderTree(value, metadata)
              }
              return {
                ...tree,
                children: updatedChildren,
              }
            } else {
              return tree
            }
          }
          default:
            return tree
        }
      },
      element.element,
    )
  }
}

export function printTree(treeRoot: ElementPathTreeRoot): string {
  let outputText: string = ''
  function printElement(element: ElementPathTree, depth: number): void {
    for (let index = 0; index < depth; index++) {
      outputText += '  '
    }
    outputText += EP.toString(element.path)
    outputText += '\n'
    printTreeWithDepth(element.children, depth + 1)
  }
  function printTreeWithDepth(subTreeRoot: ElementPathTreeRoot, depth: number): void {
    fastForEach(Object.values(subTreeRoot), (elem) => {
      printElement(elem, depth)
    })
  }
  printTreeWithDepth(treeRoot, 0)
  return outputText
}

export function forEachChildOfTarget(
  treeRoot: ElementPathTreeRoot,
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
  treeRoot: ElementPathTreeRoot,
  target: ElementPath,
): ElementPathTree | null {
  let workingRoot: ElementPathTreeRoot = treeRoot
  const totalParts = target.parts.reduce((workingCount, elem) => {
    return workingCount + elem.length
  }, 0)
  for (let pathSize = 1; pathSize <= totalParts; pathSize++) {
    let subElementPathParts: Array<ElementPathPart> = []
    let workingCount = pathSize
    fastForEach(target.parts, (pathPart) => {
      if (workingCount >= pathPart.length) {
        subElementPathParts.push(pathPart)
        workingCount = workingCount - pathPart.length
      } else if (workingCount > 0) {
        subElementPathParts.push(pathPart.slice(0, workingCount))
        workingCount = 0
      }
    })
    const subElementPath = EP.elementPath(subElementPathParts)

    // See if there's an already existing part of the tree with this path.
    const foundTree = workingRoot[EP.toString(subElementPath)]
    // Should there not be something at this point of the tree, bail out.
    if (foundTree == null) {
      return null
    } else {
      if (pathSize === totalParts) {
        // When this is true, we're at the target element.
        return foundTree
      } else {
        // Otherwise continue working down the tree.
        workingRoot = foundTree.children
      }
    }
  }

  return null
}
