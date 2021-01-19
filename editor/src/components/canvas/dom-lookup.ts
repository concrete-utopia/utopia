import * as R from 'ramda'
import { last, stripNulls } from '../../core/shared/array-utils'
import { getDOMAttribute } from '../../core/shared/dom-utils'
import { JSXMetadata } from '../../core/shared/element-template'
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
import { TemplatePath } from '../../core/shared/project-file-types'
import * as TP from '../../core/shared/template-path'
import Canvas, { TargetSearchType } from './canvas'
import { CanvasPositions } from './canvas-types'

export function findParentSceneValidPaths(target: Element): Array<string> | null {
  const validPaths = getDOMAttribute(target, 'data-utopia-valid-paths')
  if (validPaths != null) {
    return validPaths.split(' ')
  } else {
    if (target.parentElement != null) {
      return findParentSceneValidPaths(target.parentElement)
    } else {
      return null
    }
  }
}

export function findFirstParentWithValidUID(
  validTemplatePathsForLookup: Array<string> | 'no-filter',
  target: Element,
): string | null {
  const uid = getDOMAttribute(target, 'data-uid')
  const originalUid = getDOMAttribute(target, 'data-utopia-original-uid')
  const validTemplatePathsForScene = findParentSceneValidPaths(target) ?? []
  const validTemplatePaths =
    validTemplatePathsForLookup === 'no-filter'
      ? validTemplatePathsForScene
      : R.intersection(validTemplatePathsForLookup, validTemplatePathsForScene)
  if (originalUid != null && validTemplatePaths.find((tp) => tp.endsWith(originalUid))) {
    return last(validTemplatePaths.filter((tp) => tp.endsWith(originalUid))) ?? null
  } else if (uid != null && validTemplatePaths.find((tp) => tp.endsWith(uid))) {
    return last(validTemplatePaths.filter((tp) => tp.endsWith(uid))) ?? null
  } else {
    if (target.parentElement != null) {
      return findFirstParentWithValidUID(validTemplatePaths, target.parentElement)
    } else {
      return null
    }
  }
}

export function getValidTargetAtPoint(
  componentMetadata: JSXMetadata,
  selectedViews: Array<TemplatePath>,
  hiddenInstances: Array<TemplatePath>,
  validTemplatePathsForLookup: Array<string> | 'no-filter',
  point: WindowPoint | null,
  canvasScale: number,
  canvasOffset: CanvasVector,
): TemplatePath | null {
  if (point == null) {
    return null
  }
  return (
    getAllTargetsAtPoint(
      componentMetadata,
      selectedViews,
      hiddenInstances,
      validTemplatePathsForLookup,
      point,
      canvasScale,
      canvasOffset,
    )[0] ?? null
  )
}

export function getAllTargetsAtPoint(
  componentMetadata: JSXMetadata,
  selectedViews: Array<TemplatePath>,
  hiddenInstances: Array<TemplatePath>,
  validTemplatePathsForLookup: Array<string> | 'no-filter',
  point: WindowPoint | null,
  canvasScale: number,
  canvasOffset: CanvasVector,
): Array<TemplatePath> {
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
      const foundValidtemplatePath = findFirstParentWithValidUID(
        validTemplatePathsForLookup,
        element,
      )
      if (foundValidtemplatePath != null) {
        return TP.fromString(foundValidtemplatePath)
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
        return elementsFromDOM.includes(foundElement.templatePath)
      }
    })
    .map((e) => e.templatePath)
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
