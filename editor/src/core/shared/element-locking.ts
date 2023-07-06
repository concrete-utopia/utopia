import { ElementInstanceMetadataMap } from './element-template'
import { ElementPath } from './project-file-types'
import * as EP from './element-path'
import { LockedElements } from '../../components/editor/store/editor-state'
import { ElementPathTrees } from './element-path-tree'
import { MetadataUtils } from '../model/element-metadata-utils'

export function updateSimpleLocks(
  priorMetadata: ElementInstanceMetadataMap,
  newMetadata: ElementInstanceMetadataMap,
  currentSimpleLockedItems: Array<ElementPath>,
): Array<ElementPath> {
  let result: Array<ElementPath> = [...currentSimpleLockedItems]
  for (const [key, value] of Object.entries(newMetadata)) {
    // This entry is the root element of an instance and it isn't present in the previous metadata,
    // which implies that it has been newly added.
    if (EP.isRootElementOfInstance(value.elementPath) && !(key in priorMetadata)) {
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
