import { stripNulls } from '../../core/shared/array-utils'
import { getDOMAttribute } from '../../core/shared/dom-utils'
import type { ElementInstanceMetadataMap } from '../../core/shared/element-template'
import type {
  CanvasPoint,
  CanvasRectangle,
  CanvasVector,
  WindowPoint,
} from '../../core/shared/math-utils'
import {
  boundingRectangleArray,
  canvasPoint,
  isInfinityRectangle,
  negate,
  offsetPoint,
  roundPointToNearestHalf,
  scaleVector,
  windowPoint,
} from '../../core/shared/math-utils'
import type { ElementPath } from '../../core/shared/project-file-types'
import * as EP from '../../core/shared/element-path'
import { getPathsOnDomElement } from '../../core/shared/uid-utils'
import Canvas, { TargetSearchType } from './canvas'
import type { CanvasPositions } from './canvas-types'
import type { AllElementProps, LockedElements } from '../editor/store/editor-state'
import Utils from '../../utils/utils'
import { memoize } from '../../core/shared/memoize'
import type { ElementPathTrees } from '../../core/shared/element-path-tree'
import { MetadataUtils } from '../../core/model/element-metadata-utils'

type FindParentSceneValidPathsCache = Map<Element, Array<ElementPath> | null>

export function findParentSceneValidPaths(
  target: Element,
  mutableCache: FindParentSceneValidPathsCache,
): Array<ElementPath> | null {
  const cacheResult = mutableCache.get(target)
  if (cacheResult === undefined) {
    const validPaths = getDOMAttribute(target, 'data-utopia-valid-paths')
    if (validPaths != null) {
      const result = validPaths.split(' ').map(EP.fromString)
      mutableCache.set(target, result)
      return result
    } else {
      if (target.parentElement != null) {
        const result = findParentSceneValidPaths(target.parentElement, mutableCache)
        mutableCache.set(target, result)
        return result
      } else {
        return null
      }
    }
  } else {
    return cacheResult
  }
}

type StaticAndDynamicElementPaths = Array<{
  static: string
  staticPath: ElementPath
  dynamic: ElementPath
}>

function getStaticAndDynamicElementPathsForDomElement(
  target: Element,
): StaticAndDynamicElementPaths {
  const dynamicElementPaths = getPathsOnDomElement(target)
  return dynamicElementPaths.map((p) => {
    return {
      static: EP.toString(EP.makeLastPartOfPathStatic(p)),
      staticPath: EP.makeLastPartOfPathStatic(p),
      dynamic: p,
    }
  })
}

function getValidStaticElementPathsForDomElement(
  target: Element,
  parentSceneValidPathsCache: FindParentSceneValidPathsCache,
  validDynamicElementPathsForLookup: Set<string> | 'no-filter',
): Set<string> {
  const validStaticElementPathsForScene: Set<string> = new Set()
  const parentSceneValidPaths = findParentSceneValidPaths(target, parentSceneValidPathsCache)
  if (parentSceneValidPaths != null) {
    for (const validPath of parentSceneValidPaths) {
      validStaticElementPathsForScene.add(EP.toString(validPath))
    }
  }

  let validStaticElementPaths: Set<string>
  if (validDynamicElementPathsForLookup === 'no-filter') {
    validStaticElementPaths = validStaticElementPathsForScene
  } else {
    validStaticElementPaths = new Set()
    for (const path of validDynamicElementPathsForLookup) {
      if (validStaticElementPathsForScene.has(path)) {
        validStaticElementPaths.add(path)
      }
    }
  }
  return validStaticElementPaths
}

// Take a DOM element, return that if it has a valid element path, or find the closest ancestor with a valid path
export function firstAncestorOrItselfWithValidElementPath(
  validDynamicElementPathsForLookup: Set<string> | 'no-filter',
  target: Element,
  parentSceneValidPathsCache: FindParentSceneValidPathsCache,
  metadata: ElementInstanceMetadataMap,
  point: CanvasPoint,
  lockedElements: LockedElements,
): { path: ElementPath; locked: boolean } | null {
  const staticAndDynamicTargetElementPaths = getStaticAndDynamicElementPathsForDomElement(target)

  if (staticAndDynamicTargetElementPaths.length === 0) {
    return null
  }

  const isLocked = staticAndDynamicTargetElementPaths.some((p) => {
    if (EP.containsPath(p.dynamic, lockedElements.simpleLock)) {
      return true
    }
    if (
      lockedElements.hierarchyLock.some((hierarchyLock) =>
        EP.isDescendantOfOrEqualTo(p.dynamic, hierarchyLock),
      )
    ) {
      return true
    }
    return false
  })

  const validStaticElementPaths = getValidStaticElementPathsForDomElement(
    target,
    parentSceneValidPathsCache,
    validDynamicElementPathsForLookup,
  )

  let resultPath: ElementPath | null = null
  let maxDepth = -1
  for (const validPath of validStaticElementPaths) {
    const validPathFromString = EP.fromString(validPath)
    // We try to find a valid dynamic path with the deepest possible element path.
    // We use two algorithms, and the one with the deeper result wins.

    for (const staticAndDynamic of staticAndDynamicTargetElementPaths) {
      // 1. Go through all element paths from DOM and find the closest ancestor of the static paths which are also a valid path.
      // When this ancestor is found, get the dynamic element path version of the static path, and step upwards
      // the same number of steps in the hierarchy from there: this dynamic path can be the a valid target.
      // Why is this necessary?
      // When a component has a root fragment, than neither the component, nor the root fragment is available in the dom.
      // So without this search it would be not possible to double click into that component.
      // This would make all tests in the describe block 'Select Mode Double Clicking With Fragments' in select-mode.spec.browser2.tsx fail.
      if (EP.isDescendantOfOrEqualTo(staticAndDynamic.staticPath, validPathFromString)) {
        const depthDiff =
          EP.fullDepth(staticAndDynamic.staticPath) - EP.fullDepth(validPathFromString)
        const pathToAdd = EP.nthParentPath(staticAndDynamic.dynamic, depthDiff)
        if (maxDepth < EP.fullDepth(pathToAdd)) {
          maxDepth = EP.fullDepth(pathToAdd)
          resultPath = pathToAdd
        }

        // 2. This algorithm is designed to find conditionals where the active branch is a js expression, but it is implemented in a more general way.
        // The goal is to be able to search downwards in hierarchy to find elements which are not in the dom, have a valid path, and the mouse point is
        // inside their globalFrame. This makes it possible to find leaf elements in the hierachy which are not in the dom, or elements which don't have
        // any descendant which appear in the dom.
        // This is necessary for the conditionals because if they only contain an expression in the active branch, then they don't appear in the dom at
        // all.
        // Without this search the test 'Double click can dive into single conditional inside element with an expression in the active branch' would be broken.
        // Update: It turned out this algorithm is also useful to select elements by clicking on their overflown text content.
      } else if (
        EP.isDescendantOf(validPathFromString, staticAndDynamic.dynamic) &&
        maxDepth < EP.fullDepth(validPathFromString) &&
        !staticAndDynamicTargetElementPaths.some((p) =>
          EP.pathsEqual(validPathFromString, p.dynamic),
        )
      ) {
        const frame = MetadataUtils.getFrameInCanvasCoords(validPathFromString, metadata)
        const contentFrame = MetadataUtils.getFrameWithContentInCanvasCoords(
          validPathFromString,
          metadata,
        )
        if (
          (frame != null && !isInfinityRectangle(frame) && Utils.rectContainsPoint(frame, point)) ||
          (contentFrame != null &&
            !isInfinityRectangle(contentFrame) &&
            Utils.rectContainsPoint(contentFrame, point))
        ) {
          maxDepth = EP.fullDepth(validPathFromString)
          resultPath = validPathFromString
        }
      }
    }

    // 3. Start to traverse the DOM elements upwards in the hierarchy.
    // When we find an element which is attached to a static path which is also in the valid path list,
    // the dynamic version of that path is a valid target.
    // This search is necessary so we can find generated components and focus in them.
    // So without this search the 'Single click and four double clicks will focus a generated Card' and the
    // 'Single click and four double clicks will focus a generated Card and select the Button inside' tests would fail
    let currentElement: Element | null = target
    let deeperResultPossible = true
    while (currentElement != null && deeperResultPossible) {
      const paths = getStaticAndDynamicElementPathsForDomElement(currentElement)

      const pathToAdd = paths.find(
        (staticAndDynamic) => staticAndDynamic.static === validPath,
      )?.dynamic

      if (pathToAdd != null) {
        if (maxDepth > EP.fullDepth(pathToAdd)) {
          deeperResultPossible = false
        } else {
          maxDepth = EP.fullDepth(pathToAdd)
          resultPath = pathToAdd
        }
      }

      // IMPORTANT: None of the 1-2-3 algorithms can find the contents of generated components which root fragments.
      // See disabled test which fails today:
      // 'Single click and four double clicks will focus a generated Card with a root fragment and select the Button inside'
      currentElement = currentElement.parentElement
    }
  }

  return resultPath == null ? null : { path: resultPath, locked: isLocked }
}

export function getValidTargetAtPoint(
  validElementPathsForLookup: Array<ElementPath> | 'no-filter',
  point: WindowPoint | null,
  canvasScale: number,
  canvasOffset: CanvasVector,
  metadata: ElementInstanceMetadataMap,
  lockedElements: LockedElements,
): ElementPath | null {
  if (point == null) {
    return null
  }
  const elementsUnderPoint = document.elementsFromPoint(point.x, point.y)
  const parentSceneValidPathsCache = new Map()
  const pointOnCanvas = windowToCanvasCoordinates(
    canvasScale,
    canvasOffset,
    point,
  ).canvasPositionRaw
  return findFirstValidParentForSingleElement(
    validElementPathsForLookup,
    elementsUnderPoint,
    parentSceneValidPathsCache,
    metadata,
    pointOnCanvas,
    lockedElements,
  )
}

export function getAllTargetsAtPoint(
  validElementPathsForLookup: Array<ElementPath> | 'no-filter',
  point: WindowPoint | null,
  canvasScale: number,
  canvasOffset: CanvasVector,
  metadata: ElementInstanceMetadataMap,
  lockedElements: LockedElements,
): Array<ElementPath> {
  if (point == null) {
    return []
  }
  const elementsUnderPoint = document.elementsFromPoint(point.x, point.y)
  const pointOnCanvas = windowToCanvasCoordinates(
    canvasScale,
    canvasOffset,
    point,
  ).canvasPositionRaw
  const parentSceneValidPathsCache = new Map()
  // TODO FIXME we should take the zero-sized elements from Canvas.getAllTargetsAtPoint, and insert them (in a correct-enough order) here. See PR for context https://github.com/concrete-utopia/utopia/pull/2345
  return findFirstValidParentsForAllElements(
    validElementPathsForLookup,
    elementsUnderPoint,
    parentSceneValidPathsCache,
    metadata,
    pointOnCanvas,
    lockedElements,
  )
}

const findFirstValidParentForSingleElement = memoize(findFirstValidParentForSingleElementUncached, {
  maxSize: 30,
})

function findFirstValidParentForSingleElementUncached(
  validElementPathsForLookup: Array<ElementPath> | 'no-filter',
  elementsUnderPoint: Array<Element>,
  parentSceneValidPathsCache: FindParentSceneValidPathsCache,
  metadata: ElementInstanceMetadataMap,
  point: CanvasPoint,
  lockedElements: LockedElements,
) {
  const validPathsSet =
    validElementPathsForLookup == 'no-filter'
      ? 'no-filter'
      : new Set(
          validElementPathsForLookup.map((path) => EP.toString(EP.makeLastPartOfPathStatic(path))),
        )

  let foundValidElementPath: ElementPath | null = null

  for (const element of elementsUnderPoint) {
    const p = firstAncestorOrItselfWithValidElementPath(
      validPathsSet,
      element,
      parentSceneValidPathsCache,
      metadata,
      point,
      lockedElements,
    )
    if (p != null) {
      foundValidElementPath = p.path
      if (!p.locked) {
        break
      }
    }
  }

  return foundValidElementPath
}

const findFirstValidParentsForAllElements = memoize(findFirstValidParentsForAllElementsUncached, {
  maxSize: 30,
})

function findFirstValidParentsForAllElementsUncached(
  validElementPathsForLookup: Array<ElementPath> | 'no-filter',
  elementsUnderPoint: Array<Element>,
  parentSceneValidPathsCache: FindParentSceneValidPathsCache,
  metadata: ElementInstanceMetadataMap,
  point: CanvasPoint,
  lockedElements: LockedElements,
): Array<ElementPath> {
  const validPathsSet =
    validElementPathsForLookup == 'no-filter'
      ? 'no-filter'
      : new Set(
          validElementPathsForLookup.map((path) => EP.toString(EP.makeLastPartOfPathStatic(path))),
        )
  const elementsFromDOM = stripNulls(
    elementsUnderPoint.map((element) => {
      const foundValidElementPath = firstAncestorOrItselfWithValidElementPath(
        validPathsSet,
        element,
        parentSceneValidPathsCache,
        metadata,
        point,
        lockedElements,
      )?.path
      if (foundValidElementPath != null) {
        return foundValidElementPath
      } else {
        return null
      }
    }),
  )
  return elementsFromDOM
}

export function getSelectionOrValidTargetAtPoint(
  componentMetadata: ElementInstanceMetadataMap,
  selectedViews: Array<ElementPath>,
  hiddenInstances: Array<ElementPath>,
  validElementPathsForLookup: Array<ElementPath> | 'no-filter',
  point: WindowPoint | null,
  canvasScale: number,
  canvasOffset: CanvasVector,
  elementPathTree: ElementPathTrees,
  allElementProps: AllElementProps,
  lockedElements: LockedElements,
): ElementPath | null {
  if (point == null) {
    return null
  }
  const target = getSelectionOrFirstTargetAtPoint(
    componentMetadata,
    selectedViews,
    hiddenInstances,
    validElementPathsForLookup,
    point,
    canvasScale,
    canvasOffset,
    elementPathTree,
    allElementProps,
    lockedElements,
  )
  if (target === 'selection') {
    return selectedViews[0] ?? null
  } else {
    return target
  }
}

function getSelectionOrFirstTargetAtPoint(
  componentMetadata: ElementInstanceMetadataMap,
  selectedViews: Array<ElementPath>,
  hiddenInstances: Array<ElementPath>,
  validElementPathsForLookup: Array<ElementPath> | 'no-filter',
  point: WindowPoint | null,
  canvasScale: number,
  canvasOffset: CanvasVector,
  elementPathTree: ElementPathTrees,
  allElementProps: AllElementProps,
  lockedElements: LockedElements,
): 'selection' | ElementPath | null {
  if (point == null) {
    return null
  }

  const inSelectionRectangle = isPointInSelectionRectangle(
    canvasScale,
    canvasOffset,
    point,
    elementPathTree,
    allElementProps,
    componentMetadata,
    selectedViews,
    hiddenInstances,
  )

  if (inSelectionRectangle) {
    return 'selection'
  }

  const parentSceneValidPathsCache = new Map()
  const elementsUnderPoint = document.elementsFromPoint(point.x, point.y)
  const validPathsSet =
    validElementPathsForLookup === 'no-filter'
      ? 'no-filter'
      : new Set(
          validElementPathsForLookup.map((path) => EP.toString(EP.makeLastPartOfPathStatic(path))),
        )
  let elementFromDOM: ElementPath | null = null
  const pointOnCanvas = windowToCanvasCoordinates(
    canvasScale,
    canvasOffset,
    point,
  ).canvasPositionRaw
  for (const element of elementsUnderPoint) {
    const foundValidElementPath = firstAncestorOrItselfWithValidElementPath(
      validPathsSet,
      element,
      parentSceneValidPathsCache,
      componentMetadata,
      pointOnCanvas,
      lockedElements,
    )
    if (foundValidElementPath != null) {
      elementFromDOM = foundValidElementPath.path
      if (!foundValidElementPath.locked) {
        break
      }
    }
  }

  return elementFromDOM
}

function isPointInSelectionRectangle(
  canvasScale: number,
  canvasOffset: CanvasVector,
  point: WindowPoint,
  elementPathTree: ElementPathTrees,
  allElementProps: AllElementProps,
  componentMetadata: ElementInstanceMetadataMap,
  selectedViews: ElementPath[],
  hiddenInstances: ElementPath[],
) {
  const pointOnCanvas = windowToCanvasCoordinates(canvasScale, canvasOffset, point)
  const framesWithPaths = Canvas.getFramesInCanvasContext(
    allElementProps,
    componentMetadata,
    elementPathTree,
    true,
  )
  const selectedFrames = framesWithPaths.filter(
    (f) =>
      selectedViews.some((v) => EP.pathsEqual(f.path, v)) &&
      !hiddenInstances.some((hidden) => EP.isDescendantOfOrEqualTo(f.path, hidden)),
  )
  const selectionRectangle = boundingRectangleArray(selectedFrames.map((f) => f.frame))
  return (
    selectionRectangle != null &&
    Utils.rectContainsPoint(selectionRectangle, pointOnCanvas.canvasPositionRaw)
  )
}

export function getAllTargetsAtPointAABB(
  componentMetadata: ElementInstanceMetadataMap,
  selectedViews: Array<ElementPath>,
  hiddenInstances: Array<ElementPath>,
  validElementPathsForLookup: Array<ElementPath> | 'no-filter',
  pointOnCanvas: CanvasPoint | null,
  elementPathTree: ElementPathTrees,
  allElementProps: AllElementProps,
  useBoundingFrames: boolean,
  targetSearchTypes: TargetSearchType[] = [TargetSearchType.All],
): Array<ElementPath> {
  return getAllTargetsUnderAreaAABB(
    componentMetadata,
    selectedViews,
    hiddenInstances,
    validElementPathsForLookup,
    Canvas.getMousePositionCanvasArea(pointOnCanvas),
    elementPathTree,
    allElementProps,
    useBoundingFrames,
    targetSearchTypes,
  )
}

export function getAllTargetsUnderAreaAABB(
  componentMetadata: ElementInstanceMetadataMap,
  selectedViews: Array<ElementPath>,
  hiddenInstances: Array<ElementPath>,
  validElementPathsForLookup: Array<ElementPath> | 'no-filter',
  canvasArea: CanvasRectangle | null,
  elementPathTree: ElementPathTrees,
  allElementProps: AllElementProps,
  useBoundingFrames: boolean,
  targetSearchTypes: TargetSearchType[] = [TargetSearchType.All],
): Array<ElementPath> {
  if (canvasArea == null) {
    return []
  }

  const getElementsUnderPointFromAABB = Canvas.getAllTargetsUnderArea(
    componentMetadata,
    selectedViews,
    hiddenInstances,
    canvasArea,
    targetSearchTypes,
    useBoundingFrames,
    'loose',
    elementPathTree,
    allElementProps,
  )

  const validPathsSet =
    validElementPathsForLookup == 'no-filter'
      ? 'no-filter'
      : new Set(
          validElementPathsForLookup.map((path) => EP.toString(EP.makeLastPartOfPathStatic(path))),
        )

  return getElementsUnderPointFromAABB
    .filter((foundElement) => {
      if (foundElement.canBeFilteredOut) {
        return (
          validPathsSet === 'no-filter' || validPathsSet.has(EP.toString(foundElement.elementPath))
        )
      } else {
        return true
      }
    })
    .map((e) => e.elementPath)
}

export function windowToCanvasCoordinates(
  canvasScale: number,
  canvasOffset: CanvasVector,
  screenPoint: WindowPoint,
): CanvasPositions {
  const canvasWrapper = document.getElementById('canvas-root')

  if (canvasWrapper != null) {
    const canvasWrapperRect = canvasWrapper.getBoundingClientRect()
    const canvasDivCoords = {
      x: screenPoint.x - canvasWrapperRect.left,
      y: screenPoint.y - canvasWrapperRect.top,
    } as WindowPoint
    const inverseOffset = negate(canvasOffset)
    const inverseScale = 1 / canvasScale
    const pagePosition = canvasPoint(scaleVector(canvasDivCoords, inverseScale))
    const canvasPositionRaw = offsetPoint(pagePosition, inverseOffset)
    return {
      windowPosition: windowPoint({ x: screenPoint.x, y: screenPoint.y }),
      canvasPositionRaw: canvasPositionRaw,
      canvasPositionRounded: roundPointToNearestHalf(canvasPositionRaw),
    }
  } else {
    throw new Error('calling screenToElementCoordinates() before being mounted')
  }
}

export function canvasPointToWindowPoint(
  point: CanvasPoint,
  canvasScale: number,
  canvasOffset: CanvasVector,
): WindowPoint {
  const canvasWrapper = document.getElementById('canvas-root')

  if (canvasWrapper != null) {
    const canvasWrapperRect = canvasWrapper.getBoundingClientRect()
    const withoutOffset = {
      x: point.x + canvasOffset.x,
      y: point.y + canvasOffset.y,
    } as WindowPoint
    const scaledBack = scaleVector(withoutOffset, canvasScale)
    const offsetByWrapper = {
      x: scaledBack.x + canvasWrapperRect.left,
      y: scaledBack.y + canvasWrapperRect.top,
    }
    return windowPoint(offsetByWrapper)
  } else {
    throw new Error('calling screenToElementCoordinates() before being mounted')
  }
}
