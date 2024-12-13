import { isJSXElement, type ElementInstanceMetadataMap } from './element-template'
import type { ElementPath } from './project-file-types'
import * as EP from './element-path'
import type { LockedElements } from '../../components/editor/store/editor-state'
import type { ElementPathTrees } from './element-path-tree'
import { MetadataUtils } from '../model/element-metadata-utils'
import { isRight } from './either'

export function updateSimpleLocks(
  priorMetadata: ElementInstanceMetadataMap,
  newMetadata: ElementInstanceMetadataMap,
  pathTree: ElementPathTrees,
  currentSimpleLockedItems: Array<ElementPath>,
): Array<ElementPath> {
  let result: Array<ElementPath> = [...currentSimpleLockedItems]
  for (const [key, value] of Object.entries(newMetadata)) {
    // The entry isn't present in the previous metadata, which implies that it has been newly added.
    const isNewlyAdded = !(key in priorMetadata)

    // You rarely want to select a root element of a component instance, except when it is a leaf element
    const isNonLeafRootElement =
      EP.isRootElementOfInstance(value.elementPath) &&
      MetadataUtils.getChildrenPathsOrdered(pathTree, value.elementPath).length > 0

    // Remix Outlet are rarely needed to be selected
    const isRemixOutlet = MetadataUtils.isProbablyRemixOutlet(newMetadata, value.elementPath)

    // This entry is a `html` tag
    const element = MetadataUtils.findElementByElementPath(newMetadata, value.elementPath)
    const isHTMLTag =
      element != null &&
      isRight(element.element) &&
      isJSXElement(element.element.value) &&
      element.element.value.name.baseVariable === 'html'

    // This entry is a newly added non-leaf root element or a remix Outlet, it should be autolocked
    if (isNewlyAdded && (isNonLeafRootElement || isRemixOutlet || isHTMLTag)) {
      result.push(value.elementPath)
    }
  }
  return result
}

export function getAllLockedElementPaths(
  componentMetadata: ElementInstanceMetadataMap,
  elementPathTree: ElementPathTrees,
  lockedElements: LockedElements,
): Array<ElementPath> {
  const descendantsOfHierarchyLocked = MetadataUtils.getAllPaths(
    componentMetadata,
    elementPathTree,
  ).filter((path) => MetadataUtils.isDescendantOfHierarchyLockedElement(path, lockedElements))
  return [
    ...lockedElements.simpleLock,
    ...lockedElements.hierarchyLock,
    ...descendantsOfHierarchyLocked,
  ]
}

export function unlockedParent(
  componentMetadata: ElementInstanceMetadataMap,
  elementPathTree: ElementPathTrees,
  lockedElements: LockedElements,
  target: ElementPath,
): ElementPath | null {
  const allLockedElementPaths = getAllLockedElementPaths(
    componentMetadata,
    elementPathTree,
    lockedElements,
  )

  let workingPath: ElementPath = target
  while (!EP.isEmptyPath(workingPath)) {
    const parentPath = EP.parentPath(workingPath)
    if (allLockedElementPaths.some((lockedPath) => EP.pathsEqual(lockedPath, parentPath))) {
      workingPath = parentPath
    } else {
      return parentPath
    }
  }

  return null
}
