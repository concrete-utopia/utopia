import Utils from '../../../utils/utils'
import {
  CanvasPoint,
  CanvasRectangle,
  isFiniteRectangle,
  offsetPoint,
  offsetRect,
  zeroRectangle,
} from '../../../core/shared/math-utils'
import { ElementPath } from '../../../core/shared/project-file-types'
import * as EP from '../../../core/shared/element-path'
import {
  ConstrainedDragAxis,
  Guideline,
  Guidelines,
  GuidelineWithRelevantPoints,
  GuidelineWithSnappingVectorAndPointsOfRelevance,
} from '../guideline'
import { ElementInstanceMetadataMap } from '../../../core/shared/element-template'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { EdgePosition } from '../canvas-types'
import { defaultIfNull } from '../../../core/shared/optional-utils'
import { AllElementProps } from '../../editor/store/editor-state'
import {
  replaceFragmentLikePathsWithTheirChildrenRecursive,
  treatElementAsFragmentLike,
} from '../canvas-strategies/strategies/fragment-like-helpers'
import { fastForEach } from '../../../core/shared/utils'
import { ElementPathTrees } from '../../../core/shared/element-path-tree'

export const SnappingThreshold = 5

function getSnapTargetsForElementPath(
  componentMetadata: ElementInstanceMetadataMap,
  allElementProps: AllElementProps,
  pathTrees: ElementPathTrees,
  elementPath: ElementPath,
): Array<ElementPath> {
  const parent = getFirstNonFragmentLikeParent(
    componentMetadata,
    allElementProps,
    pathTrees,
    elementPath,
  )

  const siblings = replaceFragmentLikePathsWithTheirChildrenRecursive(
    componentMetadata,
    allElementProps,
    pathTrees,
    MetadataUtils.getChildrenPathsOrdered(componentMetadata, pathTrees, parent),
  ).filter((path) => !EP.isDescendantOfOrEqualTo(path, elementPath))

  return [parent, ...siblings]
}

export function collectParentAndSiblingGuidelines(
  componentMetadata: ElementInstanceMetadataMap,
  allElementProps: AllElementProps,
  pathTrees: ElementPathTrees,
  targets: Array<ElementPath>,
): Array<GuidelineWithRelevantPoints> {
  const result: Array<GuidelineWithRelevantPoints> = []
  Utils.fastForEach(targets, (target) => {
    const pinnedAndNotAbsolutePositioned = MetadataUtils.isPinnedAndNotAbsolutePositioned(
      componentMetadata,
      target,
    )

    const isElementFragmentLike = treatElementAsFragmentLike(
      componentMetadata,
      allElementProps,
      pathTrees,
      target,
    )

    if (isElementFragmentLike || !pinnedAndNotAbsolutePositioned) {
      const snapTargets = getSnapTargetsForElementPath(
        componentMetadata,
        allElementProps,
        pathTrees,
        target,
      ).filter((snapTarget) => targets.every((t) => !EP.pathsEqual(snapTarget, t)))
      fastForEach(snapTargets, (snapTarget) => {
        const frame = MetadataUtils.getFrameInCanvasCoords(snapTarget, componentMetadata)
        if (frame != null && isFiniteRectangle(frame)) {
          result.push(...Guidelines.guidelinesWithRelevantPointsForFrame(frame, 'include'))
        }
      })
    }
  })
  return result
}

function getFirstNonFragmentLikeParent(
  componentMetadata: ElementInstanceMetadataMap,
  allElementProps: AllElementProps,
  pathTrees: ElementPathTrees,
  elementPath: ElementPath,
): ElementPath {
  const parentPath = EP.parentPath(elementPath)
  if (!treatElementAsFragmentLike(componentMetadata, allElementProps, pathTrees, parentPath)) {
    return parentPath
  }
  return getFirstNonFragmentLikeParent(componentMetadata, allElementProps, pathTrees, parentPath)
}

export function getSnappedGuidelines(
  guidelines: Array<GuidelineWithRelevantPoints>,
  constrainedDragAxis: ConstrainedDragAxis | null,
  draggedFrame: CanvasRectangle,
  scale: number,
): Array<GuidelineWithSnappingVectorAndPointsOfRelevance> {
  const { horizontalPoints, verticalPoints } = Utils.getRectPointsAlongAxes(draggedFrame)

  // TODO constrained drag axis
  return Guidelines.getClosestGuidelinesAndOffsets(
    horizontalPoints,
    verticalPoints,
    Utils.rectangleToPoints(draggedFrame),
    guidelines,
    constrainedDragAxis,
    SnappingThreshold,
    scale,
  )
}

export function oneGuidelinePerDimension(
  guidelines: Array<GuidelineWithSnappingVectorAndPointsOfRelevance>,
): Array<GuidelineWithSnappingVectorAndPointsOfRelevance> {
  let xAxisGuideline: GuidelineWithSnappingVectorAndPointsOfRelevance | null = null
  let yAxisGuideline: GuidelineWithSnappingVectorAndPointsOfRelevance | null = null

  for (const guideline of guidelines) {
    if (guideline.guideline.type === 'CornerGuideline') {
      return [guideline]
    } else if (guideline.guideline.type === 'XAxisGuideline' && xAxisGuideline == null) {
      xAxisGuideline = guideline
    } else if (guideline.guideline.type === 'YAxisGuideline' && yAxisGuideline == null) {
      yAxisGuideline = guideline
    }
  }

  return Utils.stripNulls<GuidelineWithSnappingVectorAndPointsOfRelevance>([
    xAxisGuideline,
    yAxisGuideline,
  ])
}

export function getSnapDelta(
  guidelines: Array<GuidelineWithRelevantPoints>,
  constrainedDragAxis: ConstrainedDragAxis | null,
  draggedFrame: CanvasRectangle,
  scale: number,
): {
  delta: CanvasPoint
  guidelinesWithSnappingVector: Array<GuidelineWithSnappingVectorAndPointsOfRelevance>
} {
  const closestGuideLines = getSnappedGuidelines(
    guidelines,
    constrainedDragAxis,
    draggedFrame,
    scale,
  )
  const winningGuidelines = oneGuidelinePerDimension(closestGuideLines)
  const delta = winningGuidelines.reduce((working, guideline) => {
    return Utils.offsetPoint(working, guideline.snappingVector)
  }, Utils.zeroPoint as CanvasPoint)
  return {
    delta: Utils.roundPointToNearestHalf(delta),
    guidelinesWithSnappingVector: winningGuidelines,
  }
}

export function pointGuidelineToBoundsEdge(
  guidelinesWithSnappingVector: Array<GuidelineWithSnappingVectorAndPointsOfRelevance>,
  multiselectBounds: CanvasRectangle,
): Array<GuidelineWithSnappingVectorAndPointsOfRelevance> {
  return guidelinesWithSnappingVector.map((guidelineWithSnappingVector) => {
    const guideline = guidelineWithSnappingVector.guideline
    switch (guideline.type) {
      case 'XAxisGuideline':
        return {
          ...guidelineWithSnappingVector,
          guideline: {
            ...guideline,
            yTop: Math.min(guideline.yTop, multiselectBounds.y),
            yBottom: Math.max(guideline.yBottom, multiselectBounds.y + multiselectBounds.height),
          },
        }
      case 'YAxisGuideline':
        return {
          ...guidelineWithSnappingVector,
          guideline: {
            ...guideline,
            xLeft: Math.min(guideline.xLeft, multiselectBounds.x),
            xRight: Math.max(guideline.xRight, multiselectBounds.x + multiselectBounds.width),
          },
        }
      case 'CornerGuideline':
        throw new Error('CornerGuidelines are not updated to frame length')
      default:
        const _exhaustiveCheck: never = guideline
        throw 'Unexpected value for guideline: ' + guideline
    }
  })
}

export function runLegacyAbsoluteMoveSnapping(
  drag: CanvasPoint,
  constrainedDragAxis: ConstrainedDragAxis | null,
  moveGuidelines: Array<GuidelineWithRelevantPoints>,
  canvasScale: number,
  multiselectBounds: CanvasRectangle | null,
): {
  snappedDragVector: CanvasPoint
  guidelinesWithSnappingVector: Array<GuidelineWithSnappingVectorAndPointsOfRelevance>
} {
  const { delta, guidelinesWithSnappingVector } = getSnapDelta(
    moveGuidelines,
    constrainedDragAxis,
    offsetRect(defaultIfNull(zeroRectangle as CanvasRectangle, multiselectBounds), drag),
    canvasScale,
  )

  const snappedDragVector = offsetPoint(drag, delta)
  const directionConstrainedDragVector = Guidelines.applyDirectionConstraint(
    constrainedDragAxis,
    snappedDragVector,
  )

  if (multiselectBounds != null) {
    const draggedBounds = offsetRect(multiselectBounds, directionConstrainedDragVector)
    const updatedGuidelinesWithSnapping = pointGuidelineToBoundsEdge(
      guidelinesWithSnappingVector,
      draggedBounds,
    )
    return {
      snappedDragVector: directionConstrainedDragVector,
      guidelinesWithSnappingVector: updatedGuidelinesWithSnapping,
    }
  } else {
    return { snappedDragVector: directionConstrainedDragVector, guidelinesWithSnappingVector }
  }
}

export function filterGuidelinesStaticAxis<T>(
  fn: (t: T) => Guideline,
  guidelineLikes: Array<T>,
  resizingFromPosition: EdgePosition,
): Array<T> {
  // when resizing on vertical side horizontal guidelines are not visible
  return guidelineLikes.filter((guideline) => {
    return !(
      (resizingFromPosition.x === 0.5 && fn(guideline).type === 'XAxisGuideline') ||
      (resizingFromPosition.y === 0.5 && fn(guideline).type === 'YAxisGuideline')
    )
  })
}
