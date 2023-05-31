import { ElementPath, ElementPathPart } from './project-file-types'
import * as EP from './element-path'
import { fastForEach } from './utils'
import { ElementInstanceMetadataMap, isJSXElement } from './element-template'
import { MetadataUtils } from '../model/element-metadata-utils'
import { forEachRight } from './either'
import { move } from './array-utils'
import { getUtopiaID } from './uid-utils'

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

function emptyElementPathTree(path: ElementPath, pathString: string): ElementPathTree {
  return elementPathTree(path, pathString, [])
}

export type ElementPathTreeRoot = { [key: string]: ElementPathTree }

export function buildTree(elementPaths: Array<ElementPath>): ElementPathTreeRoot {
  let result: ElementPathTreeRoot = {}
  let currentPathParts: Array<ElementPathPart> = []
  let priorPathString: string | null = null
  // Loop over each and every path that needs to be added.
  for (let elementPathIndex = 0; elementPathIndex < elementPaths.length; elementPathIndex++) {
    const elementPath = elementPaths[elementPathIndex]
    currentPathParts = []
    priorPathString = null
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
        const foundTree = result[subElementPathString]
        let treeToTarget = foundTree ?? emptyElementPathTree(subElementPath, subElementPathString)

        // Add this if it doesn't already exist.
        if (foundTree == null) {
          result[subElementPathString] = treeToTarget
          // Handle adding this entry to its parent as a child.
          if (priorPathString != null) {
            const childrenOfPriorPath = result[priorPathString].children
            childrenOfPriorPath.push(treeToTarget)
          }
        }

        // Set the prior path, so that it can be used on the next call around.
        priorPathString = subElementPathString
      }
    }
  }
  return result
}

export function getStoryboardRoot(trees: ElementPathTreeRoot): ElementPathTree | null {
  const storyboardTree = Object.values(trees).find((e) => EP.isStoryboardPath(e.path))
  return storyboardTree ?? null
}

export function getCanvasRoots(trees: ElementPathTreeRoot): Array<ElementPathTree> {
  const storyboardTree = getStoryboardRoot(trees)
  if (storyboardTree == null) {
    return []
  }

  return storyboardTree.children
}

// Mutates the value of `tree`.
export function reorderTree(tree: ElementPathTree, metadata: ElementInstanceMetadataMap): void {
  const element = MetadataUtils.findElementByElementPath(metadata, tree.path)
  if (element != null) {
    forEachRight(element.element, (elementChild) => {
      switch (elementChild.type) {
        case 'JSX_ELEMENT':
        case 'JSX_FRAGMENT': {
          let updatedChildrenArray: Array<ElementPathTree> = [...tree.children]

          // We want to explicitly keep the root element of an instance first.
          const firstChild = updatedChildrenArray[0]
          const hasRootElement = firstChild != null && EP.isRootElementOfInstance(firstChild.path)
          elementChild.children.forEach((child, childIndex) => {
            const uid = getUtopiaID(child)
            const workingTreeIndex = updatedChildrenArray.findIndex((workingTreeChild) => {
              return EP.toUid(workingTreeChild.path) === uid
            })
            const adjustedChildIndex = hasRootElement ? childIndex + 1 : childIndex
            if (workingTreeIndex !== adjustedChildIndex) {
              updatedChildrenArray = move(
                workingTreeIndex,
                adjustedChildIndex,
                updatedChildrenArray,
              )
            }
          })

          tree.children = updatedChildrenArray
          break
        }
        // TODO Add in handling of the various attribute types once those become selectable.
        default:
          break
      }
    })
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
  const subTree = treeRoot[EP.toString(target)]
  return subTree ?? null
}
