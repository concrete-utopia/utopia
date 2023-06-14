import { MetadataUtils } from '../model/element-metadata-utils'
import { mapDropNulls, move } from './array-utils'
import { forEachRight } from './either'
import * as EP from './element-path'
import { ElementInstanceMetadataMap } from './element-template'
import { ElementPath, ElementPathPart } from './project-file-types'
import { getUtopiaID } from './uid-utils'
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

function emptyElementPathTree(path: ElementPath, pathString: string): ElementPathTree {
  return elementPathTree(path, pathString, [])
}

export type ElementPathTrees = { [key: string]: ElementPathTree }

export function buildTree(elementPaths: Array<ElementPath>): ElementPathTrees {
  let result: ElementPathTrees = {}
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

// Mutates the value of `trees`.
export function reorderTree(trees: ElementPathTrees, metadata: ElementInstanceMetadataMap): void {
  const dynamicPathsGroups = getDynamicPathsGroups(metadata)

  for (const tree of Object.values(trees)) {
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

            tree.children = maybeReorderDynamicChildren(updatedChildrenArray, dynamicPathsGroups)

            break
          }
          // TODO Add in handling of the various attribute types once those become selectable.
          default:
            break
        }
      })
    }
  }
}

function getDynamicPathsGroups(metadata: ElementInstanceMetadataMap): ElementPath[][] {
  // This function takes the metadata and returns a stack of contiguous dynamic paths,
  // splitting when either an element is non-dynamic or it is under a new parent.

  const paths = Object.values(metadata).map((e) => e.elementPath)

  let stack: ElementPath[][] = []
  let currentGroup: ElementPath[] = []
  let lastParentPath: ElementPath | null = null

  for (const path of paths) {
    if (!EP.isDynamicPath(path)) {
      // It's not a dynamic path, so it's a group terminator.
      stack.push(currentGroup)
      currentGroup = []
      lastParentPath = null
      continue
    }

    const parentPath = EP.parentPath(path)
    if (lastParentPath != null && !EP.pathsEqual(parentPath, lastParentPath)) {
      // The parent changed, so it's a group terminator.
      stack.push(currentGroup)
      currentGroup = []
    }
    lastParentPath = parentPath

    currentGroup.push(path)
  }
  // Add the dangling group.
  stack.push(currentGroup)

  // Return only non-empty groups.
  return stack.filter((c) => c.length > 0)
}

function maybeReorderDynamicChildren(
  children: ElementPathTree[],
  dynamicPathsGroups: ElementPath[][],
): ElementPathTree[] {
  // If there are no dynamic paths, no need to continue.
  if (!children.some((child) => EP.isDynamicPath(child.path))) {
    return children
  }

  const childrenByElementPath = children.reduce(
    (acc: { [key: string]: ElementPathTree }, val: ElementPathTree) => {
      acc[EP.toString(val.path)] = val
      return acc
    },
    {},
  )

  // Replace the dynamic paths in the children with the popped groups from the stack.
  return children.flatMap((child): ElementPathTree[] => {
    if (EP.isDynamicPath(child.path)) {
      // Pop the first group from the groups stack.
      const dynamicGroup = dynamicPathsGroups.shift()
      if (dynamicGroup == null) {
        // Ignore trailing remnants.
        return []
      }
      return mapDropNulls(
        (dynamicPath) => childrenByElementPath[EP.toString(dynamicPath)],
        dynamicGroup,
      )
    }
    return [child]
  })
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
