import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import { ElementInstanceMetadataMap } from '../../../../core/shared/element-template'
import { ElementPath } from '../../../../core/shared/project-file-types'
import Utils from '../../../../utils/utils'
import {
  CanvasPoint,
  CanvasRectangle,
  CanvasVector,
  isInfinityRectangle,
} from '../../../../core/shared/math-utils'
import { EditorAction, EditorDispatch } from '../../../editor/action-types'
import * as EditorActions from '../../../editor/actions/action-creators'
import { setCanvasFrames } from '../../../editor/actions/action-creators'
import { EditorState } from '../../../editor/store/editor-state'
import * as EP from '../../../../core/shared/element-path'
import {
  CanvasFrameAndTarget,
  flexMoveChange,
  FlexMoveChange,
  flexResizeChange,
  pinFrameChange,
  PinMoveChange,
  pinMoveChange,
} from '../../canvas-types'
import {
  ConstrainedDragAxis,
  Guideline,
  Guidelines,
  GuidelineWithRelevantPoints,
} from '../../guideline'
import { getSnapDelta } from '../guideline-helpers'
import { getNewIndex } from './yoga-utils'
import { mapDropNulls } from '../../../../core/shared/array-utils'
import { ElementPathTrees } from '../../../../core/shared/element-path-tree'

export function determineConstrainedDragAxis(dragDelta: CanvasVector): 'x' | 'y' {
  if (Math.abs(dragDelta.x) > Math.abs(dragDelta.y)) {
    return 'x'
  } else {
    return 'y'
  }
}

export function extendSelectedViewsForInteraction(
  selectedViews: Array<ElementPath>,
  componentMetadata: ElementInstanceMetadataMap,
): Array<ElementPath> {
  return Utils.flatMapArray((view) => {
    const frame = MetadataUtils.getFrameInCanvasCoords(view, componentMetadata)
    if (frame == null) {
      // What's the deal here? Why are we checking if the thing has a global frame but then not using it?
      return []
    } else {
      return [view]
    }
  }, selectedViews)
}

export function determineElementsToOperateOnForDragging(
  selectedViews: Array<ElementPath>,
  componentMetadata: ElementInstanceMetadataMap,
  isMoving: boolean,
  isAnchor: boolean,
): Array<ElementPath> {
  if (isMoving) {
    // Moving.
    return extendSelectedViewsForInteraction(
      selectedViews.filter((view) =>
        selectedViews.every((otherView) => {
          return EP.pathsEqual(view, otherView) && EP.isDescendantOfOrEqualTo(view, otherView)
        }),
      ),
      componentMetadata,
    )
  } else {
    // Resizing.
    return extendSelectedViewsForInteraction(selectedViews, componentMetadata)
  }
}

export function dragComponent(
  componentsMetadata: ElementInstanceMetadataMap,
  elementPathTree: ElementPathTrees,
  selectedViews: Array<ElementPath>,
  originalFrames: Array<CanvasFrameAndTarget>,
  moveGuidelines: Array<Guideline>,
  dragSelectionBoundingBox: CanvasRectangle | null,
  furthestDragDelta: CanvasVector | null,
  dragDelta: CanvasVector,
  enableSnapping: boolean,
  constrainDragAxis: boolean,
  scale: number,
): Array<PinMoveChange | FlexMoveChange> {
  const roundedDragDelta = Utils.roundPointTo(dragDelta, 0)
  // TODO: Probably makes more sense to pull this out.
  const viewsToOperateOn = determineElementsToOperateOnForDragging(
    selectedViews,
    componentsMetadata,
    true,
    false,
  )
  let dragChanges: Array<PinMoveChange | FlexMoveChange> = []
  Utils.fastForEach(viewsToOperateOn, (view) => {
    const parentPath = EP.parentPath(view)
    const isFlexContainer =
      MetadataUtils.isParentYogaLayoutedContainerAndElementParticipatesInLayout(
        view,
        componentsMetadata,
      )
    const originalFrame = originalFrames.find((frame) => EP.pathsEqual(frame.target, view))
    if (originalFrame == null) {
      // found a target with no original frame
      return
    }
    if (isFlexContainer) {
      if (originalFrame.frame != null) {
        const flexDirection = MetadataUtils.getFlexDirection(
          MetadataUtils.getParent(componentsMetadata, view),
        )
        const draggedFrame = Utils.offsetRect(originalFrame.frame, dragDelta)
        const newIndex = getNewIndex(
          componentsMetadata,
          elementPathTree,
          view,
          parentPath,
          flexDirection,
          draggedFrame,
        )
        if (newIndex != null) {
          dragChanges.push(flexMoveChange(view, newIndex))
        }
      }
    } else {
      // TODO determine if node graph affects the drag

      const moveGuidelinesWithNoPoints: Array<GuidelineWithRelevantPoints> = moveGuidelines.map(
        (guideline) => ({ guideline, pointsOfRelevance: [] }),
      )

      const constrainedDragAxis: ConstrainedDragAxis | null =
        constrainDragAxis && furthestDragDelta != null
          ? determineConstrainedDragAxis(furthestDragDelta)
          : null
      const snapDelta = enableSnapping
        ? getSnapDelta(
            moveGuidelinesWithNoPoints,
            constrainedDragAxis,
            Utils.offsetRect(
              Utils.defaultIfNull(Utils.zeroRectangle as CanvasRectangle, dragSelectionBoundingBox),
              roundedDragDelta,
            ),
            scale,
          ).delta
        : (Utils.zeroPoint as CanvasPoint)

      const dragDeltaToApply = Guidelines.applyDirectionConstraint(
        constrainedDragAxis,
        Utils.offsetPoint(roundedDragDelta, snapDelta),
      )
      if (originalFrame.frame != null) {
        dragChanges.push(pinMoveChange(view, dragDeltaToApply))
      }
    }
  })
  return dragChanges
}

export function dragComponentForActions(
  componentsMetadata: ElementInstanceMetadataMap,
  elementPathTree: ElementPathTrees,
  selectedViews: Array<ElementPath>,
  originalFrames: Array<CanvasFrameAndTarget>,
  moveGuidelines: Array<Guideline>,
  dragSelectionBoundingBox: CanvasRectangle,
  furthestDragDelta: CanvasVector | null,
  dragDelta: CanvasVector,
  enableSnapping: boolean,
  constrainDragAxis: boolean,
  scale: number,
): Array<EditorAction> {
  const frameAndTargets = dragComponent(
    componentsMetadata,
    elementPathTree,
    selectedViews,
    originalFrames,
    moveGuidelines,
    dragSelectionBoundingBox,
    furthestDragDelta,
    dragDelta,
    enableSnapping,
    constrainDragAxis,
    scale,
  )
  return [setCanvasFrames(frameAndTargets, false)]
}

export function adjustAllSelectedFrames(
  editor: EditorState,
  dispatch: EditorDispatch,
  keepChildrenAtPlace: boolean,
  isResizing: boolean,
  directionModifier: -1 | 1,
  direction: 'vertical' | 'horizontal',
  adjustment: 1 | 10,
): Array<EditorAction> {
  if (
    editor.selectedViews.length === 0 ||
    (editor.selectedViews.some((view) => {
      return MetadataUtils.isParentYogaLayoutedContainerAndElementParticipatesInLayout(
        view,
        editor.jsxMetadata,
      )
    }) &&
      !isResizing)
  ) {
    // if any of the selected views have a Yoga parent, we bail out
    return []
  }
  const selectedFrames = mapDropNulls((view) => {
    const frame = MetadataUtils.getFrameInCanvasCoords(view, editor.jsxMetadata)
    return frame == null || isInfinityRectangle(frame) ? null : frame
  }, editor.selectedViews)

  const boundingBox = Utils.boundingRectangleArray(selectedFrames)

  if (boundingBox == null) {
    // none of the selected elements are layoutable, return early
    return []
  }

  let actions: Array<EditorAction> = []
  if (isResizing) {
    let roundedAdjustment: number = 0
    let newBoundingBox: CanvasRectangle
    if (direction === 'vertical') {
      roundedAdjustment = getRoundedAdjustment(
        boundingBox.y + boundingBox.height,
        adjustment,
        directionModifier,
      )
      newBoundingBox = Utils.combineRectangles(boundingBox, {
        x: 0,
        y: 0,
        width: 0,
        height: roundedAdjustment,
      } as CanvasRectangle)
    } else {
      roundedAdjustment = getRoundedAdjustment(
        boundingBox.x + boundingBox.width,
        adjustment,
        directionModifier,
      )
      newBoundingBox = Utils.combineRectangles(boundingBox, {
        x: 0,
        y: 0,
        width: roundedAdjustment,
        height: 0,
      } as CanvasRectangle)
    }
    const newFrameAndTargets = Utils.stripNulls(
      editor.selectedViews.map((view) => {
        const frame = MetadataUtils.getFrameInCanvasCoords(view, editor.jsxMetadata)
        if (frame == null || isInfinityRectangle(frame)) {
          return null
        } else {
          const newFrame = Utils.transformFrameUsingBoundingBox(newBoundingBox, boundingBox, frame)
          const hasFlexParent =
            MetadataUtils.isParentYogaLayoutedContainerAndElementParticipatesInLayout(
              view,
              editor.jsxMetadata,
            )
          if (hasFlexParent) {
            return flexResizeChange(view, 'flexBasis', adjustment * directionModifier)
          } else {
            return pinFrameChange(view, newFrame)
          }
        }
      }),
    )
    actions = [EditorActions.setCanvasFrames(newFrameAndTargets, keepChildrenAtPlace)]
  } else {
    const originalFrames: CanvasFrameAndTarget[] = Utils.stripNulls(
      editor.selectedViews.map((view) => {
        const frame = MetadataUtils.getFrameInCanvasCoords(view, editor.jsxMetadata)
        if (frame == null || isInfinityRectangle(frame)) {
          return null
        }
        return {
          target: view,
          frame: frame,
        }
      }),
    )

    const delta = getRoundedDeltaForKeyboardShortcut(
      boundingBox,
      direction,
      directionModifier,
      adjustment,
    )
    EditorActions.hideAndShowSelectionControls(dispatch)
    actions = dragComponentForActions(
      editor.jsxMetadata,
      editor.elementPathTree,
      editor.selectedViews,
      originalFrames,
      [],
      boundingBox,
      delta,
      delta,
      false,
      false,
      editor.canvas.scale,
    )
  }

  actions.push(EditorActions.startCheckpointTimer())
  return actions
}

function getRoundedDeltaForKeyboardShortcut(
  boundingBox: CanvasRectangle,
  side: 'vertical' | 'horizontal',
  directionModifier: -1 | 1,
  adjustment: 1 | 10,
): CanvasPoint {
  if (side === 'vertical') {
    return {
      x: 0,
      y: getRoundedAdjustment(boundingBox.y, adjustment, directionModifier),
    } as CanvasPoint
  } else {
    return {
      x: getRoundedAdjustment(boundingBox.x, adjustment, directionModifier),
      y: 0,
    } as CanvasPoint
  }
}

function getRoundedAdjustment(n: number, adjustment: 10 | 1, directionModifier: -1 | 1): number {
  let roundDelta = Utils.roundTo(n, 0) - n

  if (roundDelta === 0) {
    // default case, no need to round
    return adjustment * directionModifier
  } else if (roundDelta * directionModifier < 0) {
    // when the rounding and frameshift happens in opposite direction
    return (adjustment - Math.abs(roundDelta)) * directionModifier
  } else {
    return (adjustment + Math.abs(roundDelta)) * directionModifier
  }
}
