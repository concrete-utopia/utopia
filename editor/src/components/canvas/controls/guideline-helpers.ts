import Utils from '../../../utils/utils'
import { CanvasPoint, CanvasRectangle } from '../../../core/shared/math-utils'
import { ElementPath } from '../../../core/shared/project-file-types'
import * as EP from '../../../core/shared/element-path'
import {
  ConstrainedDragAxis,
  Guideline,
  Guidelines,
  GuidelineWithSnappingVector,
} from '../guideline'
import { ElementInstanceMetadataMap } from '../../../core/shared/element-template'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { EdgePosition } from '../canvas-types'

export const SnappingThreshold = 5

export function collectParentAndSiblingGuidelines(
  componentMetadata: ElementInstanceMetadataMap,
  targets: Array<ElementPath>,
): Array<Guideline> {
  const allPaths = MetadataUtils.getAllPaths(componentMetadata)
  const result: Array<Guideline> = []
  Utils.fastForEach(targets, (target) => {
    const pinnedAndNotAbsolutePositioned = MetadataUtils.isPinnedAndNotAbsolutePositioned(
      componentMetadata,
      target,
    )
    if (!pinnedAndNotAbsolutePositioned) {
      const parent = EP.parentPath(target)
      Utils.fastForEach(allPaths, (maybeTarget) => {
        // for now we only snap to parents and sibligns and not us or our descendants
        const isSibling = EP.isSiblingOf(maybeTarget, target)
        const isParent = EP.pathsEqual(parent, maybeTarget)
        const notSelectedOrDescendantOfSelected = targets.every(
          (view) => !EP.isDescendantOfOrEqualTo(maybeTarget, view),
        )
        if ((isSibling || isParent) && notSelectedOrDescendantOfSelected) {
          const frame = MetadataUtils.getFrameInCanvasCoords(maybeTarget, componentMetadata)
          if (frame != null) {
            result.push(...Guidelines.guidelinesForFrame(frame, true))
          }
        }
      })
    }
  })
  return result
}

export function collectSelfAndChildrenGuidelines(
  componentMetadata: ElementInstanceMetadataMap,
  targets: Array<ElementPath>,
  insertingElementId: string,
): Array<Guideline> {
  const allPaths = MetadataUtils.getAllPaths(componentMetadata)
  const result: Array<Guideline> = []
  Utils.fastForEach(targets, (target) => {
    const pinnedAndNotAbsolutePositioned = MetadataUtils.isPinnedAndNotAbsolutePositioned(
      componentMetadata,
      target,
    )
    if (!pinnedAndNotAbsolutePositioned) {
      if (EP.toUid(target) !== insertingElementId) {
        const frame = MetadataUtils.getFrameInCanvasCoords(target, componentMetadata)
        if (frame != null) {
          result.push(...Guidelines.guidelinesForFrame(frame, true))
        }
      }

      Utils.fastForEach(allPaths, (maybeTarget) => {
        if (EP.isChildOf(maybeTarget, target) && EP.toUid(maybeTarget) !== insertingElementId) {
          const frame = MetadataUtils.getFrameInCanvasCoords(maybeTarget, componentMetadata)
          if (frame != null) {
            result.push(...Guidelines.guidelinesForFrame(frame, true))
          }
        }
      })
    }
  })
  return result
}

export function getSnappedGuidelines(
  guidelines: Array<Guideline>,
  constrainedDragAxis: ConstrainedDragAxis | null,
  draggedFrame: CanvasRectangle,
  scale: number,
) {
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

export function getSnappedGuidelinesForPoint(
  guidelines: Array<Guideline>,
  constrainedDragAxis: ConstrainedDragAxis | null,
  point: CanvasPoint,
  scale: number,
) {
  return Guidelines.getClosestGuidelinesAndOffsets(
    [point.x],
    [point.y],
    [point],
    guidelines,
    constrainedDragAxis,
    SnappingThreshold,
    scale,
  )
}

export function oneGuidelinePerDimension(
  guidelines: Array<GuidelineWithSnappingVector>,
): Array<GuidelineWithSnappingVector> {
  let xAxisGuideline: GuidelineWithSnappingVector | null = null
  let yAxisGuideline: GuidelineWithSnappingVector | null = null

  for (const guideline of guidelines) {
    if (guideline.guideline.type === 'CornerGuideline') {
      return [guideline]
    } else if (guideline.guideline.type === 'XAxisGuideline' && xAxisGuideline == null) {
      xAxisGuideline = guideline
    } else if (guideline.guideline.type === 'YAxisGuideline' && yAxisGuideline == null) {
      yAxisGuideline = guideline
    }
  }

  return Utils.stripNulls<GuidelineWithSnappingVector>([xAxisGuideline, yAxisGuideline])
}

export function getSnapDelta(
  guidelines: Array<Guideline>,
  constrainedDragAxis: ConstrainedDragAxis | null,
  draggedFrame: CanvasRectangle,
  scale: number,
): CanvasPoint {
  const closestGuideLines = getSnappedGuidelines(
    guidelines,
    constrainedDragAxis,
    draggedFrame,
    scale,
  )
  const delta = oneGuidelinePerDimension(closestGuideLines).reduce((working, guideline) => {
    if (guideline.activateSnap) {
      return Utils.offsetPoint(working, guideline.snappingVector)
    } else {
      return working
    }
  }, Utils.zeroPoint as CanvasPoint)
  return Utils.roundPointTo(delta, 0)
}

export function filterGuidelinesStaticAxis(
  guidelines: Array<Guideline>,
  resizingFromPosition: EdgePosition,
) {
  // when resizing on vertical side horizontal guidelines are not visible
  return guidelines.filter((guideline) => {
    return !(
      (resizingFromPosition.x === 0.5 && guideline.type === 'XAxisGuideline') ||
      (resizingFromPosition.y === 0.5 && guideline.type === 'YAxisGuideline')
    )
  })
}

export function applySnappingToPoint(
  point: CanvasPoint,
  guidelines: Array<GuidelineWithSnappingVector>,
): CanvasPoint {
  return oneGuidelinePerDimension(guidelines).reduce((p, guidelineResult) => {
    if (guidelineResult.activateSnap) {
      return Utils.offsetPoint(p, guidelineResult.snappingVector)
    } else {
      return p
    }
  }, point)
}
