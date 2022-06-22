import { intersection, last, mapDropNulls, stripNulls } from '../../core/shared/array-utils'
import { getDOMAttribute } from '../../core/shared/dom-utils'
import { ElementInstanceMetadataMap } from '../../core/shared/element-template'
import {
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
  return (
    getAllTargetsAtPoint(
      componentMetadata,
      selectedViews,
      hiddenInstances,
      validElementPathsForLookup,
      point,
      canvasScale,
      canvasOffset,
      allElementProps,
    )[0] ?? null
  )
}

export function getAllTargetsAtPoint(
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

  // return getElementsUnderPointFromAABB
  //   .filter((foundElement) => {
  //     if (!foundElement.canBeFilteredOut) {
  //       return true
  //     } else {
  //       return elementsFromDOM.some((e) => EP.pathsEqual(e, foundElement.elementPath))
  //     }
  //   })
  //   .map((e) => e.elementPath)

  return elementsFromDOM
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
