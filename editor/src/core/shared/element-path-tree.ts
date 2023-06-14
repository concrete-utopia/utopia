import { MetadataUtils } from '../model/element-metadata-utils'
import { move, uniqBy } from './array-utils'
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

            tree.children = maybeReorderDynamicChildren(updatedChildrenArray, metadata)

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

function maybeReorderDynamicChildren(
  children: ElementPathTree[],
  metadata: ElementInstanceMetadataMap,
): ElementPathTree[] {
  // Get all the dynamic children and sort them
  // alphabetically, since their paths will have incremental indexes (~~~N).
  const dynamicChildren = children
    .filter((child) => EP.isDynamicPath(child.path))
    .sort((a, b) => a.pathString.localeCompare(b.pathString))

  // If there are no dynamic children, do nothing.
  if (dynamicChildren.length === 0) {
    return children
  }

  const metadataKeys = Object.keys(metadata)

  // Build a stack to reorder dynamic children
  const dynamicChildrenStack = uniqBy(
    dynamicChildren
      .sort((a, b) => {
        return metadataKeys.indexOf(a.pathString) - metadataKeys.indexOf(b.pathString)
      })
      .map((c) => EP.dynamicPathToStaticPath(c.path)),
    (a, b) => EP.pathsEqual(a, b),
  )

  // Group the children paths based on their non-dynamic paths.
  // E.g.:
  //  +--------------+     +-----------+
  //  | foo/bar      |     | foo/bar   |
  //  | foo/baz~~~1  |  →  | foo/baz   |
  //  | foo/baz~~~2  |     | foo/qux   |
  //  | foo/baz~~~3  |     | foo/corge |
  //  | foo/qux      |     +-----------+
  //  | foo/corge~~1 |
  //  +--------------+
  let groupedPaths: ElementPath[] = []
  for (const child of children) {
    if (EP.isDynamicPath(child.path)) {
      const nextDynamicChild = dynamicChildrenStack.shift()
      if (nextDynamicChild != null) {
        groupedPaths.push(nextDynamicChild)
      }
    } else {
      groupedPaths.push(child.path)
    }
  }

  // Now calculate the complete full children array,
  // expanding the grouped paths and replacing them with the
  // list of sorted dynamic paths calculated earlier.
  const expandedChildren = groupedPaths.flatMap((path): ElementPathTree[] => {
    const matchingDynamicChildren = dynamicChildren.filter((other) =>
      EP.pathsEqual(EP.dynamicPathToStaticPath(other.path), path),
    )
    if (matchingDynamicChildren.length > 0) {
      return matchingDynamicChildren
    }
    return children.filter((other) => EP.pathsEqual(other.path, path))
  })

  return expandedChildren
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
