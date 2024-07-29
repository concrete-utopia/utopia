import {
  type JSXAttributesEntry,
  type JSXAttributesPart,
  type JSXElementChild,
  emptyComments,
  isJSXAttributeValue,
  isJSXAttributesEntry,
  isJSXElement,
  jsExpressionValue,
  jsxAttributesEntry,
  type ElementInstanceMetadataMap,
} from './element-template'
import type { ElementPath } from './project-file-types'
import * as EP from './element-path'
import type { LockedElements } from '../../components/editor/store/editor-state'
import type { ElementPathTrees } from './element-path-tree'
import { MetadataUtils } from '../model/element-metadata-utils'
import { isRight } from './either'

export const DataSimpleLocked = 'data-simple-locked'
export const DataHierarchyLocked = 'data-hierarchy-locked'

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
  const allPaths = MetadataUtils.getAllPaths(componentMetadata, elementPathTree)
  const simpleLockedElementsFromMetadata = allPaths.filter(
    (p) =>
      dataSimpleLockedFromMetadata(componentMetadata, p) &&
      lockedElements.simpleLock.find((e) => EP.pathsEqual(e, p)) == null,
  )
  const hierarchyLockedElementsFromMetadata = allPaths.filter(
    (p) =>
      dataSimpleLockedFromMetadata(componentMetadata, p) &&
      lockedElements.hierarchyLock.find((e) => EP.pathsEqual(e, p)) == null,
  )
  const descendantsOfHierarchyLocked = MetadataUtils.getAllPaths(
    componentMetadata,
    elementPathTree,
  ).filter((path) =>
    MetadataUtils.isDescendantOfHierarchyLockedElement(path, [
      ...hierarchyLockedElementsFromMetadata,
      ...lockedElements.hierarchyLock,
    ]),
  )

  const foo = [
    ...lockedElements.simpleLock,
    ...simpleLockedElementsFromMetadata,
    ...lockedElements.hierarchyLock,
    ...hierarchyLockedElementsFromMetadata,
    ...descendantsOfHierarchyLocked,
  ].map(EP.toString)

  const fooSet = new Set(foo)
  return Array.from(fooSet).map(EP.fromString)
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

export function dataSimpleLockedFromMetadata(
  metadata: ElementInstanceMetadataMap,
  path: ElementPath,
): boolean {
  const target = MetadataUtils.findElementByElementPath(metadata, path)
  return (
    target != null &&
    isRight(target.element) &&
    isJSXElement(target.element.value) &&
    isSimpleLockedJSXElementChild(target.element.value)
  )
}

export function isSimpleLockedJSXElementChild(element: JSXElementChild) {
  return (
    isJSXElement(element) &&
    element.props.some(
      (prop) =>
        isDataSimpleLockedProp(prop) &&
        isJSXAttributeValue(prop.value) &&
        prop.value.value === true,
    )
  )
}

interface DataSimpleLockedProp extends JSXAttributesEntry {
  key: typeof DataSimpleLocked
}

export function isDataSimpleLockedProp(prop: JSXAttributesPart): prop is DataSimpleLockedProp {
  return isJSXAttributesEntry(prop) && (prop as DataSimpleLockedProp).key === DataSimpleLocked
}

export function dataSimpleLockedProp(value: boolean): JSXAttributesEntry {
  return jsxAttributesEntry(
    DataSimpleLocked,
    jsExpressionValue(value, emptyComments),
    emptyComments,
  )
}

export function dataHierarchyLockedFromMetadata(
  metadata: ElementInstanceMetadataMap,
  path: ElementPath,
): boolean {
  const target = MetadataUtils.findElementByElementPath(metadata, path)
  return (
    target != null &&
    isRight(target.element) &&
    isJSXElement(target.element.value) &&
    isHierarchyLockedJSXElementChild(target.element.value)
  )
}

export function isHierarchyLockedJSXElementChild(element: JSXElementChild) {
  return (
    isJSXElement(element) &&
    element.props.some(
      (prop) =>
        isDataHierarchyLockedProp(prop) &&
        isJSXAttributeValue(prop.value) &&
        prop.value.value === true,
    )
  )
}

interface DataHierarchyLockedProp extends JSXAttributesEntry {
  key: typeof DataHierarchyLocked
}

export function isDataHierarchyLockedProp(
  prop: JSXAttributesPart,
): prop is DataHierarchyLockedProp {
  return isJSXAttributesEntry(prop) && (prop as DataHierarchyLockedProp).key === DataHierarchyLocked
}

export function dataHierarchyLockedProp(value: boolean): JSXAttributesEntry {
  return jsxAttributesEntry(
    DataHierarchyLocked,
    jsExpressionValue(value, emptyComments),
    emptyComments,
  )
}
