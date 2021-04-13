import * as R from 'ramda'
import { last, stripNulls } from '../../core/shared/array-utils'
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
import { InstancePath, ScenePath, TemplatePath } from '../../core/shared/project-file-types'
import * as TP from '../../core/shared/template-path'
import { getPathsOnDomElement, getUIDsOnDomELement } from '../../core/shared/uid-utils'
import Canvas, { TargetSearchType } from './canvas'
import { CanvasPositions } from './canvas-types'

export function findParentSceneValidPaths(target: Element): Array<TemplatePath> | null {
  const validPaths = getDOMAttribute(target, 'data-utopia-valid-paths')
  if (validPaths != null) {
    return validPaths.split(' ').map(TP.fromString)
  } else {
    if (target.parentElement != null) {
      return findParentSceneValidPaths(target.parentElement)
    } else {
      return null
    }
  }
}

export function findFirstParentWithValidTemplatePath(
  validTemplatePathsForLookup: Array<TemplatePath> | 'no-filter',
  target: Element,
): InstancePath | null {
  const templatePaths = getPathsOnDomElement(target)
  const validTemplatePathsForScene = findParentSceneValidPaths(target) ?? []
  const validTemplatePaths =
    validTemplatePathsForLookup === 'no-filter'
      ? validTemplatePathsForScene
      : R.intersection(validTemplatePathsForLookup, validTemplatePathsForScene)

  const filteredValidPaths = templatePaths.filter((tp) =>
    validTemplatePaths.some((validPath) => TP.pathsEqual(validPath, tp)),
  )

  if (filteredValidPaths.length > 0) {
    return last(filteredValidPaths) ?? null
  } else {
    if (target.parentElement != null) {
      return findFirstParentWithValidTemplatePath(validTemplatePaths, target.parentElement)
    } else {
      return null
    }
  }
}

export function getValidTargetAtPoint(
  componentMetadata: ElementInstanceMetadataMap,
  selectedViews: Array<TemplatePath>,
  hiddenInstances: Array<TemplatePath>,
  focusedElementPath: ScenePath | null,
  validTemplatePathsForLookup: Array<TemplatePath> | 'no-filter',
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
      focusedElementPath,
      validTemplatePathsForLookup,
      point,
      canvasScale,
      canvasOffset,
    )[0] ?? null
  )
}

export function getAllTargetsAtPoint(
  componentMetadata: ElementInstanceMetadataMap,
  selectedViews: Array<TemplatePath>,
  hiddenInstances: Array<TemplatePath>,
  focusedElementPath: ScenePath | null,
  validTemplatePathsForLookup: Array<TemplatePath> | 'no-filter',
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
    focusedElementPath,
    pointOnCanvas.canvasPositionRaw,
    [TargetSearchType.All],
    true,
    'loose',
  )
  const elementsUnderPoint = document.elementsFromPoint(point.x, point.y)
  const elementsFromDOM = stripNulls(
    elementsUnderPoint.map((element) => {
      const foundValidtemplatePath = findFirstParentWithValidTemplatePath(
        validTemplatePathsForLookup,
        element,
      )
      if (foundValidtemplatePath != null) {
        return foundValidtemplatePath
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
        return elementsFromDOM.some((e) => TP.pathsEqual(e, foundElement.templatePath))
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
