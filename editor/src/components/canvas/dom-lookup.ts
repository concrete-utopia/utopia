import * as R from 'ramda'
import { last, mapDropNulls, stripNulls } from '../../core/shared/array-utils'
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

export function findFirstParentWithValidElementPath(
  validDynamicElementPathsForLookup: Array<ElementPath> | 'no-filter',
  target: Element,
): ElementPath | null {
  const dynamicElementPaths = getPathsOnDomElement(target)
  const validStaticElementPathsForScene = findParentSceneValidPaths(target) ?? []
  const validStaticElementPaths =
    validDynamicElementPathsForLookup === 'no-filter'
      ? validStaticElementPathsForScene
      : R.intersection(
          validDynamicElementPathsForLookup.map(EP.makeLastPartOfPathStatic),
          validStaticElementPathsForScene,
        )

  const filteredValidPathsMappedToDynamic = mapDropNulls((validPath: ElementPath) => {
    return dynamicElementPaths.find((tp) => {
      const elementPathWithStaticElementPart = EP.makeLastPartOfPathStatic(tp)
      return EP.pathsEqual(validPath, elementPathWithStaticElementPart)
    })
  }, validStaticElementPaths)

  if (filteredValidPathsMappedToDynamic.length > 0) {
    return last(filteredValidPathsMappedToDynamic) ?? null
  } else {
    if (target.parentElement != null) {
      return findFirstParentWithValidElementPath(validStaticElementPaths, target.parentElement)
    } else {
      return null
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
  )
  const elementsUnderPoint = document.elementsFromPoint(point.x, point.y)
  const elementsFromDOM = stripNulls(
    elementsUnderPoint.map((element) => {
      const foundValidelementPath = findFirstParentWithValidElementPath(
        validElementPathsForLookup,
        element,
      )
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
