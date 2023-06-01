import { intersection, last, mapDropNulls, stripNulls } from '../../core/shared/array-utils'
import { getDOMAttribute } from '../../core/shared/dom-utils'
import { ElementInstanceMetadataMap } from '../../core/shared/element-template'
import {
  boundingRectangleArray,
  CanvasPoint,
  canvasPoint,
  CanvasVector,
  isInfinityRectangle,
  negate,
  offsetPoint,
  roundPointToNearestHalf,
  scaleVector,
  windowPoint,
  WindowPoint,
} from '../../core/shared/math-utils'
import { ElementPath } from '../../core/shared/project-file-types'
import * as EP from '../../core/shared/element-path'
import { getPathsOnDomElement } from '../../core/shared/uid-utils'
import Canvas, { TargetSearchType } from './canvas'
import { CanvasPositions } from './canvas-types'
import { AllElementProps } from '../editor/store/editor-state'
import Utils from '../../utils/utils'
import { memoize } from '../../core/shared/memoize'
import { ElementPathTrees } from '../../core/shared/element-path-tree'
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

// Take a DOM element, and try to find the nearest selectable path for it
export function findFirstParentWithValidElementPath(
  validDynamicElementPathsForLookup: Set<string> | 'no-filter',
  target: Element,
  metadata: ElementInstanceMetadataMap,
  point: CanvasPoint,
): ElementPath | null {
  const parentSceneValidPathsCache = new Map()

  const firstParentFromDom = findFirstParentWithValidElementPathInner(
    validDynamicElementPathsForLookup,
    target,
    'search-dom-only',
    parentSceneValidPathsCache,
    metadata,
    point,
  )

  const firstParentFromPath = findFirstParentWithValidElementPathInner(
    validDynamicElementPathsForLookup,
    target,
    'allow-descendants-from-path',
    parentSceneValidPathsCache,
    metadata,
    point,
  )

  if (firstParentFromDom == null || firstParentFromPath == null) {
    return firstParentFromDom ?? firstParentFromPath
  }

  return EP.fullDepth(firstParentFromDom) < EP.fullDepth(firstParentFromPath)
    ? firstParentFromPath
    : firstParentFromDom
}

// Take a DOM element, and try to find the nearest selectable path for it
export function findFirstParentWithValidElementPathInner(
  validDynamicElementPathsForLookup: Set<string> | 'no-filter',
  target: Element,
  allowDescendantsFromPath: 'allow-descendants-from-path' | 'search-dom-only',
  parentSceneValidPathsCache: FindParentSceneValidPathsCache,
  metadata: ElementInstanceMetadataMap,
  point: CanvasPoint,
): ElementPath | null {
  const dynamicElementPaths = getPathsOnDomElement(target)
  const staticAndDynamicTargetElementPaths = dynamicElementPaths.map((p) => {
    return {
      static: EP.toString(EP.makeLastPartOfPathStatic(p)),
      staticPath: EP.makeLastPartOfPathStatic(p),
      dynamic: p,
    }
  })
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

  let filteredValidPathsMappedToDynamic: Array<ElementPath> = []
  switch (allowDescendantsFromPath) {
    case 'allow-descendants-from-path':
      for (const validPath of validStaticElementPaths) {
        const validPathFromString = EP.fromString(validPath)
        for (const staticAndDynamic of staticAndDynamicTargetElementPaths) {
          const validNonDomParent = extendValidPathsWithNonDomParents(
            staticAndDynamic,
            validPathFromString,
          )
          if (validNonDomParent != null) {
            filteredValidPathsMappedToDynamic.push(validNonDomParent)
          }
          const validNonDomChild = extendValidPathsWithNonDomChildren(
            staticAndDynamic,
            validPathFromString,
            point,
            dynamicElementPaths,
            metadata,
          )
          if (validNonDomChild != null) {
            filteredValidPathsMappedToDynamic.push(validNonDomChild)
          }
        }
      }
      break
    case 'search-dom-only':
      for (const validPath of validStaticElementPaths) {
        const pathToAdd = staticAndDynamicTargetElementPaths.find(
          (staticAndDynamic) => staticAndDynamic.static === validPath,
        )?.dynamic
        if (pathToAdd != null) {
          filteredValidPathsMappedToDynamic.push(pathToAdd)
        }
      }
      break
    default:
      const _exhaustiveCheck: never = allowDescendantsFromPath
  }

  if (filteredValidPathsMappedToDynamic.length > 0) {
    let deepestDepth: number = -1
    let deepestResult: ElementPath | null = null
    for (const path of filteredValidPathsMappedToDynamic) {
      const latestDepth = EP.fullDepth(path)
      if (latestDepth > deepestDepth) {
        deepestDepth = latestDepth
        deepestResult = path
      }
    }

    return deepestResult
  } else {
    if (target.parentElement == null) {
      return null
    } else {
      return findFirstParentWithValidElementPathInner(
        validDynamicElementPathsForLookup,
        target.parentElement,
        allowDescendantsFromPath,
        parentSceneValidPathsCache,
        metadata,
        point,
      )
    }
  }
}

function extendValidPathsWithNonDomParents(
  domPath: {
    static: string
    staticPath: ElementPath
    dynamic: ElementPath
  },
  nonDomPath: ElementPath,
): ElementPath | null {
  // check if there is a dom path which is a descendant of a non-dom path: then the
  // non-dom path should be a valid target too
  if (EP.isDescendantOfOrEqualTo(domPath.staticPath, nonDomPath)) {
    const depthDiff = EP.fullDepth(domPath.staticPath) - EP.fullDepth(nonDomPath)
    return EP.nthParentPath(domPath.dynamic, depthDiff)
  }
  return null
}

function extendValidPathsWithNonDomChildren(
  domPath: {
    static: string
    staticPath: ElementPath
    dynamic: ElementPath
  },
  nonDomPath: ElementPath,
  point: CanvasPoint,
  dynamicElementPaths: ElementPath[],
  metadata: ElementInstanceMetadataMap,
): ElementPath | null {
  // check if there is a non-dom path which has a frame which contains the mouse point
  // and which is a direct child of a dom path
  const frame = MetadataUtils.getFrameInCanvasCoords(nonDomPath, metadata)
  if (
    frame != null &&
    !isInfinityRectangle(frame) &&
    EP.pathsEqual(EP.parentPath(nonDomPath), domPath.dynamic) &&
    !dynamicElementPaths.some((p) => EP.pathsEqual(nonDomPath, p)) &&
    Utils.rectContainsPoint(frame, point)
  ) {
    return nonDomPath
  }
  return null
}

export function getValidTargetAtPoint(
  validElementPathsForLookup: Array<ElementPath> | 'no-filter',
  point: WindowPoint | null,
  canvasScale: number,
  canvasOffset: CanvasVector,
  metadata: ElementInstanceMetadataMap,
): ElementPath | null {
  if (point == null) {
    return null
  }
  const elementsUnderPoint = document.elementsFromPoint(point.x, point.y)
  const pointOnCanvas = windowToCanvasCoordinates(
    canvasScale,
    canvasOffset,
    point,
  ).canvasPositionRaw
  return findFirstValidParentForSingleElement(
    validElementPathsForLookup,
    elementsUnderPoint,
    metadata,
    pointOnCanvas,
  )
}

export function getAllTargetsAtPoint(
  validElementPathsForLookup: Array<ElementPath> | 'no-filter',
  point: WindowPoint | null,
  canvasScale: number,
  canvasOffset: CanvasVector,
  metadata: ElementInstanceMetadataMap,
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
  // TODO FIXME we should take the zero-sized elements from Canvas.getAllTargetsAtPoint, and insert them (in a correct-enough order) here. See PR for context https://github.com/concrete-utopia/utopia/pull/2345
  return findFirstValidParentsForAllElements(
    validElementPathsForLookup,
    elementsUnderPoint,
    metadata,
    pointOnCanvas,
  )
}

const findFirstValidParentForSingleElement = memoize(findFirstValidParentForSingleElementUncached, {
  maxSize: 30,
})

function findFirstValidParentForSingleElementUncached(
  validElementPathsForLookup: Array<ElementPath> | 'no-filter',
  elementsUnderPoint: Array<Element>,
  metadata: ElementInstanceMetadataMap,
  point: CanvasPoint,
) {
  const validPathsSet =
    validElementPathsForLookup == 'no-filter'
      ? 'no-filter'
      : new Set(
          validElementPathsForLookup.map((path) => EP.toString(EP.makeLastPartOfPathStatic(path))),
        )
  for (const element of elementsUnderPoint) {
    const foundValidElementPath = findFirstParentWithValidElementPath(
      validPathsSet,
      element,
      metadata,
      point,
    )
    if (foundValidElementPath != null) {
      return foundValidElementPath
    }
  }
  return null
}

const findFirstValidParentsForAllElements = memoize(findFirstValidParentsForAllElementsUncached, {
  maxSize: 30,
})

function findFirstValidParentsForAllElementsUncached(
  validElementPathsForLookup: Array<ElementPath> | 'no-filter',
  elementsUnderPoint: Array<Element>,
  metadata: ElementInstanceMetadataMap,
  point: CanvasPoint,
) {
  const validPathsSet =
    validElementPathsForLookup == 'no-filter'
      ? 'no-filter'
      : new Set(
          validElementPathsForLookup.map((path) => EP.toString(EP.makeLastPartOfPathStatic(path))),
        )
  const elementsFromDOM = stripNulls(
    elementsUnderPoint.map((element) => {
      const foundValidElementPath = findFirstParentWithValidElementPath(
        validPathsSet,
        element,
        metadata,
        point,
      )
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
): ElementPath | null {
  if (point == null) {
    return null
  }
  const targets = getSelectionOrAllTargetsAtPoint(
    componentMetadata,
    selectedViews,
    hiddenInstances,
    validElementPathsForLookup,
    point,
    canvasScale,
    canvasOffset,
    elementPathTree,
    allElementProps,
  )
  if (targets === 'selection') {
    return selectedViews[0] ?? null
  } else {
    return targets[0] ?? null
  }
}

export function getSelectionOrAllTargetsAtPoint(
  componentMetadata: ElementInstanceMetadataMap,
  selectedViews: Array<ElementPath>,
  hiddenInstances: Array<ElementPath>,
  validElementPathsForLookup: Array<ElementPath> | 'no-filter',
  point: WindowPoint | null,
  canvasScale: number,
  canvasOffset: CanvasVector,
  elementPathTree: ElementPathTrees,
  allElementProps: AllElementProps,
): Array<ElementPath> | 'selection' {
  if (point == null) {
    return []
  }
  const elementsUnderPoint = document.elementsFromPoint(point.x, point.y)
  const validPathsSet =
    validElementPathsForLookup === 'no-filter'
      ? 'no-filter'
      : new Set(
          validElementPathsForLookup.map((path) => EP.toString(EP.makeLastPartOfPathStatic(path))),
        )
  const pointOnCanvas = windowToCanvasCoordinates(
    canvasScale,
    canvasOffset,
    point,
  ).canvasPositionRaw
  const elementsFromDOM: Array<ElementPath> = []
  for (const element of elementsUnderPoint) {
    const foundValidElementPath = findFirstParentWithValidElementPath(
      validPathsSet,
      element,
      componentMetadata,
      pointOnCanvas,
    )
    if (foundValidElementPath != null) {
      elementsFromDOM.push(foundValidElementPath)
    }
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

  return inSelectionRectangle ? 'selection' : elementsFromDOM
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
): Array<ElementPath> {
  if (pointOnCanvas == null) {
    return []
  }

  const canvasPositionRaw = pointOnCanvas
  const getElementsUnderPointFromAABB = Canvas.getAllTargetsAtPoint(
    componentMetadata,
    selectedViews,
    hiddenInstances,
    canvasPositionRaw,
    [TargetSearchType.All],
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
