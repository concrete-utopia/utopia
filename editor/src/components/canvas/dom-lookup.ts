import { intersection, last, mapDropNulls, stripNulls } from '../../core/shared/array-utils'
import { getDOMAttribute } from '../../core/shared/dom-utils'
import { ElementInstanceMetadataMap } from '../../core/shared/element-template'
import {
  boundingRectangleArray,
  CanvasPoint,
  canvasPoint,
  CanvasVector,
  negate,
  offsetPoint,
  roundPointToNearestHalf,
  scaleVector,
  windowPoint,
  WindowPoint,
} from '../../core/shared/math-utils'
import { ElementPath } from '../../core/shared/project-file-types'
import * as EP from '../../core/shared/element-path'
import { getPathsOnDomElementOrNode } from '../../core/shared/uid-utils'
import Canvas, { TargetSearchType } from './canvas'
import { CanvasPositions } from './canvas-types'
import { AllElementProps } from '../editor/store/editor-state'
import Utils from '../../utils/utils'
import { memoize } from '../../core/shared/memoize'

type FindParentSceneValidPathsCache = Map<Element | Node, Array<ElementPath> | null>

export function findParentSceneValidPaths(
  target: Element | Node,
  mutableCache: FindParentSceneValidPathsCache,
): Array<ElementPath> | null {
  const cacheResult = mutableCache.get(target)
  if (cacheResult === undefined) {
    const targetIsElement = 'attributes' in target // TODO clean this code up
    const validPaths = targetIsElement ? getDOMAttribute(target, 'data-utopia-valid-paths') : null
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
  target: Element | Node,
): ElementPath | null {
  const parentSceneValidPathsCache = new Map()

  const firstParentFromDom = findFirstParentWithValidElementPathInner(
    validDynamicElementPathsForLookup,
    target,
    'search-dom-only',
    parentSceneValidPathsCache,
  )

  const firstParentFromPath = findFirstParentWithValidElementPathInner(
    validDynamicElementPathsForLookup,
    target,
    'allow-descendants-from-path',
    parentSceneValidPathsCache,
  )

  if (firstParentFromDom == null || firstParentFromPath == null) {
    return firstParentFromDom ?? firstParentFromPath
  }

  return EP.fullDepth(firstParentFromDom) < EP.fullDepth(firstParentFromPath)
    ? firstParentFromPath
    : firstParentFromDom
}

function getElementsOrTextNodesUnderPoint(x: number, y: number): Array<Element | ChildNode> {
  const elements = document.elementsFromPoint(x, y)
  return elements.flatMap((element) => {
    return [...getTextNodeForElement(element, x, y), element]
  })
}

function getTextNodeForElement(element: Element, x: number, y: number): Array<ChildNode> {
  var nodes = element.childNodes
  for (const node of nodes) {
    if (node.nodeType === node.TEXT_NODE) {
      var range = document.createRange()
      range.selectNode(node)
      var rects = range.getClientRects()
      for (const rect of rects) {
        if (x > rect.left && x < rect.right && y > rect.top && y < rect.bottom) {
          return [node]
        }
      }
    }
  }

  return []
}

// Take a DOM element, and try to find the nearest selectable path for it
export function findFirstParentWithValidElementPathInner(
  validDynamicElementPathsForLookup: Set<string> | 'no-filter',
  target: Element | Node,
  allowDescendantsFromPath: 'allow-descendants-from-path' | 'search-dom-only',
  parentSceneValidPathsCache: FindParentSceneValidPathsCache,
): ElementPath | null {
  const dynamicElementPaths = getPathsOnDomElementOrNode(target)
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
          if (EP.isDescendantOfOrEqualTo(staticAndDynamic.staticPath, validPathFromString)) {
            const depthDiff =
              EP.fullDepth(staticAndDynamic.staticPath) - EP.fullDepth(validPathFromString)
            filteredValidPathsMappedToDynamic.push(
              EP.nthParentPath(staticAndDynamic.dynamic, depthDiff),
            )
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
      )
    }
  }
}

export function getValidTargetAtPoint(
  validElementPathsForLookup: Array<ElementPath> | 'no-filter',
  point: WindowPoint | null,
): ElementPath | null {
  if (point == null) {
    return null
  }
  const elementsUnderPoint = getElementsOrTextNodesUnderPoint(point.x, point.y)
  return findFirstValidParentForSingleElement(validElementPathsForLookup, elementsUnderPoint)
}

export function getAllTargetsAtPoint(
  validElementPathsForLookup: Array<ElementPath> | 'no-filter',
  point: WindowPoint | null,
): Array<ElementPath> {
  if (point == null) {
    return []
  }
  const elementsUnderPoint = getElementsOrTextNodesUnderPoint(point.x, point.y)
  // TODO FIXME we should take the zero-sized elements from Canvas.getAllTargetsAtPoint, and insert them (in a correct-enough order) here. See PR for context https://github.com/concrete-utopia/utopia/pull/2345
  return findFirstValidParentsForAllElements(validElementPathsForLookup, elementsUnderPoint)
}

const findFirstValidParentForSingleElement = memoize(findFirstValidParentForSingleElementUncached, {
  maxSize: 30,
})

function findFirstValidParentForSingleElementUncached(
  validElementPathsForLookup: Array<ElementPath> | 'no-filter',
  elementsUnderPoint: Array<Element | Node>,
) {
  const validPathsSet =
    validElementPathsForLookup == 'no-filter'
      ? 'no-filter'
      : new Set(
          validElementPathsForLookup.map((path) => EP.toString(EP.makeLastPartOfPathStatic(path))),
        )
  for (const element of elementsUnderPoint) {
    const foundValidElementPath = findFirstParentWithValidElementPath(validPathsSet, element)
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
  elementsUnderPoint: Array<Element | Node>,
) {
  const validPathsSet =
    validElementPathsForLookup == 'no-filter'
      ? 'no-filter'
      : new Set(
          validElementPathsForLookup.map((path) => EP.toString(EP.makeLastPartOfPathStatic(path))),
        )
  const elementsFromDOM = stripNulls(
    elementsUnderPoint.map((element) => {
      const foundValidElementPath = findFirstParentWithValidElementPath(validPathsSet, element)
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
  allElementProps: AllElementProps,
): Array<ElementPath> | 'selection' {
  if (point == null) {
    return []
  }
  const elementsUnderPoint = getElementsOrTextNodesUnderPoint(point.x, point.y)
  const validPathsSet =
    validElementPathsForLookup === 'no-filter'
      ? 'no-filter'
      : new Set(
          validElementPathsForLookup.map((path) => EP.toString(EP.makeLastPartOfPathStatic(path))),
        )
  const elementsFromDOM: Array<ElementPath> = []
  for (const element of elementsUnderPoint) {
    const foundValidElementPath = findFirstParentWithValidElementPath(validPathsSet, element)
    if (foundValidElementPath != null) {
      elementsFromDOM.push(foundValidElementPath)
    }
  }

  const inSelectionRectangle = isPointInSelectionRectangle(
    canvasScale,
    canvasOffset,
    point,
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
  allElementProps: AllElementProps,
  componentMetadata: ElementInstanceMetadataMap,
  selectedViews: ElementPath[],
  hiddenInstances: ElementPath[],
) {
  const pointOnCanvas = windowToCanvasCoordinates(canvasScale, canvasOffset, point)
  const framesWithPaths = Canvas.getFramesInCanvasContext(allElementProps, componentMetadata, true)
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
