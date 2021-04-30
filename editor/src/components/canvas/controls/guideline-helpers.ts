import Utils from '../../../utils/utils'
import { CanvasPoint, CanvasRectangle } from '../../../core/shared/math-utils'
import { TemplatePath } from '../../../core/shared/project-file-types'
import * as TP from '../../../core/shared/template-path'
import {
  ConstrainedDragAxis,
  Guideline,
  Guidelines,
  GuidelineWithSnappingVector,
} from '../guideline'
import { SnappingThreshold } from './select-mode-control-container'
import { ElementInstanceMetadataMap } from '../../../core/shared/element-template'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { EdgePosition } from '../canvas-types'

export function collectParentAndSiblingGuidelines(
  componentMetadata: ElementInstanceMetadataMap,
  targets: Array<TemplatePath>,
): Array<Guideline> {
  const allPaths = MetadataUtils.getAllPaths(componentMetadata)
  const result: Array<Guideline> = []
  Utils.fastForEach(targets, (target) => {
    const pinnedAndNotAbsolutePositioned = MetadataUtils.isPinnedAndNotAbsolutePositioned(
      componentMetadata,
      target,
    )
    if (!pinnedAndNotAbsolutePositioned) {
      const parent = TP.parentPath(target)
      Utils.fastForEach(allPaths, (maybeTarget) => {
        // for now we only snap to parents and sibligns and not us or our descendants
        const isSibling = TP.isSiblingOf(maybeTarget, target)
        const isParent = TP.pathsEqual(parent, maybeTarget)
        const notSelectedOrDescendantOfSelected = targets.every(
          (view) => !TP.isAncestorOf(maybeTarget, view),
        )
        const isGroup = MetadataUtils.isAutoSizingViewFromComponents(componentMetadata, parent)
        if ((isSibling || (isParent && !isGroup)) && notSelectedOrDescendantOfSelected) {
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
  targets: Array<TemplatePath>,
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
      if (TP.isInstancePath(target) && TP.toUid(target) !== insertingElementId) {
        const frame = MetadataUtils.getFrameInCanvasCoords(target, componentMetadata)
        if (frame != null) {
          result.push(...Guidelines.guidelinesForFrame(frame, true))
        }
      }

      Utils.fastForEach(allPaths, (maybeTarget) => {
        if (
          TP.isInstancePath(maybeTarget) &&
          TP.isChildOf(maybeTarget, target) &&
          TP.toUid(maybeTarget) !== insertingElementId
        ) {
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
