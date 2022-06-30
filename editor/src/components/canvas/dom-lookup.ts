import { intersection, last, mapDropNulls, stripNulls } from '../../core/shared/array-utils'
import { getDOMAttribute } from '../../core/shared/dom-utils'
import { ElementInstanceMetadataMap } from '../../core/shared/element-template'
import {
  boundingRectangleArray,
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
import { getPathsOnDomElement } from '../../core/shared/uid-utils'
import Canvas, { TargetSearchType } from './canvas'
import { CanvasPositions } from './canvas-types'
import { CanvasScale, CanvasScrollOffset } from '../../utils/global-positions'
import { AllElementProps } from '../editor/store/editor-state'
import { CanvasControlsContainerID } from './controls/new-canvas-controls'
import { Utils } from '../../uuiui-deps'

export function findParentSceneValidPaths(target: Element): Array<ElementPath> | null {
  const validPaths = getDOMAttribute(target, 'data-utopia-valid-paths')
  if (validPaths != null) {
    return validPaths.split(' ').map(EP.fromString)
  } else {
    if (target.parentElement != null) {
      return findParentSceneValidPaths(target.parentElement)
    } else {
      return null
    }
  }
}

// Take a DOM element, and try to find the nearest selectable path for it
export function findFirstParentWithValidElementPath(
  validDynamicElementPathsForLookup: Set<string> | 'no-filter',
  target: Element,
): ElementPath | null {
  const dynamicElementPaths = getPathsOnDomElement(target)
  const staticAndDynamicTargetElementPaths = dynamicElementPaths.map((p) => {
    return {
      static: EP.toString(EP.makeLastPartOfPathStatic(p)),
      dynamic: p,
    }
  })
  const validStaticElementPathsForSceneArray =
    findParentSceneValidPaths(target)?.map(EP.toString) ?? []
  const validStaticElementPathsForScene = new Set(validStaticElementPathsForSceneArray)

  const validStaticElementPaths =
    validDynamicElementPathsForLookup === 'no-filter'
      ? validStaticElementPathsForScene
      : new Set(
          [...validDynamicElementPathsForLookup].filter((p) =>
            validStaticElementPathsForScene.has(p),
          ),
        )

  const filteredValidPathsMappedToDynamic = mapDropNulls(
    (validPath: string) => {
      return staticAndDynamicTargetElementPaths.find(
        (staticAndDynamic) => staticAndDynamic.static === validPath,
      )?.dynamic
    },
    [...validStaticElementPaths],
  )

  if (filteredValidPathsMappedToDynamic.length > 0) {
    const sortedFilteredPaths = filteredValidPathsMappedToDynamic.sort(
      (l, r) => EP.depth(l) - EP.depth(r),
    )
    return last(sortedFilteredPaths) ?? null
  } else {
    if (target.parentElement == null) {
      return null
    } else {
      return findFirstParentWithValidElementPath(
        validDynamicElementPathsForLookup,
        target.parentElement,
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
  return getAllTargetsAtPoint(validElementPathsForLookup, point)[0] ?? null
}

export function getAllTargetsAtPoint(
  validElementPathsForLookup: Array<ElementPath> | 'no-filter',
  point: WindowPoint | null,
): Array<ElementPath> {
  if (point == null) {
    return []
  }
  const elementsUnderPoint = document.elementsFromPoint(point.x, point.y)
  const validPathsSet =
    validElementPathsForLookup == 'no-filter'
      ? 'no-filter'
      : new Set(
          validElementPathsForLookup.map((path) => EP.toString(EP.makeLastPartOfPathStatic(path))),
        )
  const elementsFromDOM = stripNulls(
    elementsUnderPoint.map((element) => {
      const foundValidelementPath = findFirstParentWithValidElementPath(validPathsSet, element)
      if (foundValidelementPath != null) {
        return foundValidelementPath
      } else {
        return null
      }
    }),
  )

  // TODO FIXME we should take the zero-sized elements from Canvas.getAllTargetsAtPoint, and insert them (in a correct-enough order) here. See PR for context https://github.com/concrete-utopia/utopia/pull/2345
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
  const elementsUnderPoint = document.elementsFromPoint(point.x, point.y)
  const validPathsSet =
    validElementPathsForLookup == 'no-filter'
      ? 'no-filter'
      : new Set(
          validElementPathsForLookup.map((path) => EP.toString(EP.makeLastPartOfPathStatic(path))),
        )
  const elementsFromDOM = stripNulls(
    elementsUnderPoint.map((element) => {
      const foundValidelementPath = findFirstParentWithValidElementPath(validPathsSet, element)
      if (foundValidelementPath != null) {
        return foundValidelementPath
      } else {
        return null
      }
    }),
  )

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

// export function isSelectionContainerElement(element: Element): boolean {
//   const testidAttr = getDOMAttribute(element, 'data-selection')
//   console.log('testidAttr', testidAttr)
//   return testidAttr === 'true'
// }

export function getAllTargetsAtPointAABB(
  componentMetadata: ElementInstanceMetadataMap,
  selectedViews: Array<ElementPath>,
  hiddenInstances: Array<ElementPath>,
  validElementPathsForLookup: Array<ElementPath> | 'no-filter',
  point: WindowPoint | null,
  canvasScale: number,
  canvasOffset: CanvasVector,
  allElementProps: AllElementProps,
): Array<ElementPath> {
  if (point == null) {
    return []
  }

  const pointOnCanvas = windowToCanvasCoordinates(canvasScale, canvasOffset, point)
  const getElementsUnderPointFromAABB = Canvas.getAllTargetsAtPoint(
    componentMetadata,
    selectedViews,
    hiddenInstances,
    pointOnCanvas.canvasPositionRaw,
    [TargetSearchType.All],
    true,
    'loose',
    allElementProps,
  )

  const elementsUnderPoint = document.elementsFromPoint(point.x, point.y)
  const validPathsSet =
    validElementPathsForLookup == 'no-filter'
      ? 'no-filter'
      : new Set(
          validElementPathsForLookup.map((path) => EP.toString(EP.makeLastPartOfPathStatic(path))),
        )
  const elementsFromDOM = stripNulls(
    elementsUnderPoint.map((element) => {
      const foundValidelementPath = findFirstParentWithValidElementPath(validPathsSet, element)
      if (foundValidelementPath != null) {
        return foundValidelementPath
      } else {
        return null
      }
    }),
  )

  return getElementsUnderPointFromAABB
    .filter((foundElement) => {
      if (!foundElement.canBeFilteredOut) {
        return true
      } else {
        return elementsFromDOM.some((e) => EP.pathsEqual(e, foundElement.elementPath))
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
