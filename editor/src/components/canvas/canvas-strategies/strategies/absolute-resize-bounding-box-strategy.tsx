import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import type { ElementInstanceMetadataMap } from '../../../../core/shared/element-template'
import type { CanvasRectangle, Size } from '../../../../core/shared/math-utils'
import {
  isFiniteRectangle,
  isInfinityRectangle,
  roundRectangleToNearestWhole,
  transformFrameUsingBoundingBox,
} from '../../../../core/shared/math-utils'
import type { ElementPath } from '../../../../core/shared/project-file-types'
import type { AllElementProps } from '../../../editor/store/editor-state'
import {
  getElementFromProjectContents,
  trueUpElementChanged,
} from '../../../editor/store/editor-state'
import type { EdgePosition } from '../../canvas-types'
import { EdgePositionTop, EdgePositionLeft, EdgePositionTopLeft } from '../../canvas-types'
import { pushIntendedBoundsAndUpdateGroups } from '../../commands/push-intended-bounds-and-update-groups-command'
import { setCursorCommand } from '../../commands/set-cursor-command'
import { setElementsToRerenderCommand } from '../../commands/set-elements-to-rerender-command'
import { setSnappingGuidelines } from '../../commands/set-snapping-guidelines-command'
import { updateHighlightedViews } from '../../commands/update-highlighted-views-command'
import { ImmediateParentBounds } from '../../controls/parent-bounds'
import { ImmediateParentOutlines } from '../../controls/parent-outlines'
import { AbsoluteResizeControl } from '../../controls/select-mode/absolute-resize-control'
import { ZeroSizeResizeControlWrapper } from '../../controls/zero-sized-element-controls'
import { onlyFitWhenDraggingThisControl } from '../canvas-strategies'
import type { CanvasStrategy, InteractionCanvasState } from '../canvas-strategy-types'
import {
  controlWithProps,
  emptyStrategyApplicationResult,
  getTargetPathsFromInteractionTarget,
  strategyApplicationResult,
} from '../canvas-strategy-types'
import type { InteractionSession } from '../interaction-state'
import {
  isAnySelectedElementAspectRatioLocked,
  getLockedAspectRatio,
  pickCursorFromEdgePosition,
  resizeBoundingBox,
  supportsAbsoluteResize,
} from './resize-helpers'
import { runLegacyAbsoluteResizeSnapping } from './shared-absolute-resize-strategy-helpers'
import { flattenSelection, getMultiselectBounds } from './shared-move-strategies-helpers'
import type { ElementPathTrees } from '../../../../core/shared/element-path-tree'
import { treatElementAsGroupLike } from './group-helpers'
import type { EnsureFramePointsExist } from './resize-strategy-helpers'
import { createResizeCommandsFromFrame } from './resize-strategy-helpers'
import { isEdgePositionEqualTo } from '../../canvas-utils'
import { queueGroupTrueUp } from '../../commands/queue-group-true-up-command'
import {
  getChildGroupsForNonGroupParents,
  retargetStrategyToChildrenOfFragmentLikeElements,
} from './fragment-like-helpers'
import { getSafeGroupChildConstraintsArray } from '../../../inspector/fill-hug-fixed-control'

export function absoluteResizeBoundingBoxStrategy(
  canvasState: InteractionCanvasState,
  interactionSession: InteractionSession | null,
): CanvasStrategy | null {
  const originalTargets = flattenSelection(
    getTargetPathsFromInteractionTarget(canvasState.interactionTarget),
  )
  const retargetedTargets = flattenSelection(
    retargetStrategyToChildrenOfFragmentLikeElements(canvasState),
  )
  if (
    retargetedTargets.length === 0 ||
    !retargetedTargets.every((element) => {
      return supportsAbsoluteResize(canvasState.startingMetadata, element, canvasState)
    })
  ) {
    return null
  }

  return {
    id: 'ABSOLUTE_RESIZE_BOUNDING_BOX',
    name: 'Resize',
    controlsToRender: [
      controlWithProps({
        control: AbsoluteResizeControl,
        props: { targets: originalTargets },
        key: 'absolute-resize-control',
        show: 'visible-except-when-other-strategy-is-active',
      }),
      controlWithProps({
        control: ZeroSizeResizeControlWrapper,
        props: { targets: originalTargets },
        key: 'zero-size-resize-control',
        show: 'visible-except-when-other-strategy-is-active',
      }),
      controlWithProps({
        control: ImmediateParentOutlines,
        props: { targets: originalTargets },
        key: 'parent-outlines-control',
        show: 'visible-only-while-active',
      }),
      controlWithProps({
        control: ImmediateParentBounds,
        props: { targets: originalTargets },
        key: 'parent-bounds-control',
        show: 'visible-only-while-active',
      }),
    ],
    fitness: onlyFitWhenDraggingThisControl(interactionSession, 'RESIZE_HANDLE', 1),
    apply: () => {
      if (
        interactionSession != null &&
        interactionSession.interactionData.type === 'DRAG' &&
        interactionSession.activeControl.type === 'RESIZE_HANDLE'
      ) {
        const childGroups = getChildGroupsForNonGroupParents(
          canvasState.startingMetadata,
          retargetedTargets,
        )

        const edgePosition = interactionSession.activeControl.edgePosition
        if (interactionSession.interactionData.drag != null) {
          const drag = interactionSession.interactionData.drag
          const originalBoundingBox = getMultiselectBounds(
            canvasState.startingMetadata,
            retargetedTargets,
          )
          const anySelectedElementAspectRatioLocked = isAnySelectedElementAspectRatioLocked(
            canvasState.startingMetadata,
            retargetedTargets,
          )
          if (originalBoundingBox != null) {
            const lockedAspectRatio = getLockedAspectRatio(
              interactionSession,
              interactionSession.interactionData.modifiers,
              originalBoundingBox,
              anySelectedElementAspectRatioLocked,
            )
            const centerBased = interactionSession.interactionData.modifiers.alt
              ? 'center-based'
              : 'non-center-based'
            const newBoundingBox = resizeBoundingBox(
              originalBoundingBox,
              drag,
              edgePosition,
              lockedAspectRatio,
              centerBased,
            )
            const { snappedBoundingBox, guidelinesWithSnappingVector } = snapBoundingBox(
              originalTargets,
              canvasState.startingMetadata,
              edgePosition,
              newBoundingBox,
              canvasState.scale,
              lockedAspectRatio,
              centerBased,
              canvasState.startingAllElementProps,
              canvasState.startingElementPathTree,
            )

            const commandsForSelectedElements = retargetedTargets.flatMap((selectedElement) => {
              const element = getElementFromProjectContents(
                selectedElement,
                canvasState.projectContents,
              )
              const originalFrame = MetadataUtils.getFrameInCanvasCoords(
                selectedElement,
                canvasState.startingMetadata,
              )

              if (element == null || originalFrame == null || isInfinityRectangle(originalFrame)) {
                return []
              }

              const elementIsGroup = treatElementAsGroupLike(
                canvasState.startingMetadata,
                selectedElement,
              )

              const newFrame = roundRectangleToNearestWhole(
                transformFrameUsingBoundingBox(
                  snappedBoundingBox,
                  originalBoundingBox,
                  originalFrame,
                ),
              )
              if (elementIsGroup) {
                const constrainedFrames = getConstrainedSizes(
                  canvasState.startingMetadata,
                  canvasState.startingAllElementProps,
                  selectedElement,
                  originalFrame,
                )
                if (constrainedFrames.length > 0) {
                  const horizontalAdjustments = getAdjustedOffsets(
                    constrainedFrames,
                    'width',
                    edgePosition,
                    originalFrame,
                    newFrame,
                  )
                  newFrame.x = horizontalAdjustments.offset
                  newFrame.width = horizontalAdjustments.size

                  const verticalAdjustments = getAdjustedOffsets(
                    constrainedFrames,
                    'height',
                    edgePosition,
                    originalFrame,
                    newFrame,
                  )
                  newFrame.y = verticalAdjustments.offset
                  newFrame.height = verticalAdjustments.size
                }
              }
              const metadata = MetadataUtils.findElementByElementPath(
                canvasState.startingMetadata,
                selectedElement,
              )
              const elementParentBounds =
                metadata?.specialSizeMeasurements.immediateParentBounds ?? null

              const elementParentFlexDirection =
                metadata?.specialSizeMeasurements.parentFlexDirection ?? null

              const ensureFramePointsExist: EnsureFramePointsExist =
                !elementIsGroup ||
                (isEdgePositionEqualTo(edgePosition, EdgePositionLeft) && originalFrame.x === 0) ||
                (isEdgePositionEqualTo(edgePosition, EdgePositionTop) && originalFrame.y === 0) ||
                (isEdgePositionEqualTo(edgePosition, EdgePositionTopLeft) &&
                  (originalFrame.x === 0 || originalFrame.y === 0))
                  ? 'ensure-two-frame-points-per-dimension-exists'
                  : 'only-offset-frame-points-are-needed'

              return [
                ...createResizeCommandsFromFrame(
                  element,
                  selectedElement,
                  newFrame,
                  originalFrame,
                  elementParentBounds,
                  elementParentFlexDirection,
                  edgePosition,
                  ensureFramePointsExist,
                ),
                pushIntendedBoundsAndUpdateGroups(
                  [{ target: selectedElement, frame: newFrame }],
                  'starting-metadata',
                ),
                queueGroupTrueUp(childGroups.map(trueUpElementChanged)),
              ]
            })

            return strategyApplicationResult([
              ...commandsForSelectedElements,
              setSnappingGuidelines('mid-interaction', guidelinesWithSnappingVector),
              updateHighlightedViews('mid-interaction', []),
              setCursorCommand(pickCursorFromEdgePosition(edgePosition)),
              setElementsToRerenderCommand(retargetedTargets),
            ])
          }
        } else {
          return strategyApplicationResult([
            setCursorCommand(pickCursorFromEdgePosition(edgePosition)),
            updateHighlightedViews('mid-interaction', []),
          ])
        }
      }
      // Fallback for when the checks above are not satisfied.
      return emptyStrategyApplicationResult
    },
  }
}

function getConstrainedSizes(
  jsxMetadata: ElementInstanceMetadataMap,
  allElementProps: AllElementProps,
  path: ElementPath,
  originalRect: CanvasRectangle,
): Array<Size> {
  let result: Array<Size> = []
  const children = MetadataUtils.getChildrenUnordered(jsxMetadata, path)
  for (const child of children) {
    const constraintsArray = getSafeGroupChildConstraintsArray(allElementProps, child.elementPath)
    const frame = child.localFrame
    const constraints = {
      width: constraintsArray.includes('width'),
      height: constraintsArray.includes('height'),
      top: constraintsArray.includes('top'),
      bottom: constraintsArray.includes('bottom'),
      left: constraintsArray.includes('left'),
      right: constraintsArray.includes('right'),
    }
    const constrained =
      constraints.top ||
      constraints.bottom ||
      constraints.height ||
      constraints.left ||
      constraints.right ||
      constraints.width
    if (frame != null && isFiniteRectangle(frame) && constrained) {
      let width = 0
      if (constraints.left && constraints.right) {
        width = originalRect.width
      } else if (constraints.left) {
        width = frame.x + frame.width
      } else if (constraints.right) {
        width = originalRect.width - frame.x
      } else if (constraints.width) {
        width = frame.width
      }

      let height = 0
      if (constraints.top && constraints.bottom) {
        height = originalRect.height
      } else if (constraints.top) {
        height = frame.y + frame.height
      } else if (constraints.bottom) {
        height = originalRect.height - frame.y
      } else if (constraints.height) {
        height = frame.height
      }

      result.push({
        width: width,
        height: height,
      })
    }
  }
  return result
}

function getMaxDimension(constrainedFrames: Size[], dimension: 'width' | 'height'): number {
  return constrainedFrames.reduce((max, frame) => {
    return frame[dimension] > max ? frame[dimension] : max
  }, -Infinity)
}

function getAdjustedOffsets(
  constrainedFrames: Size[],
  dimension: 'width' | 'height',
  edgePosition: EdgePosition,
  originalRect: CanvasRectangle,
  currentRect: CanvasRectangle,
): { offset: number; size: number } {
  const axis = dimension === 'width' ? 'x' : 'y'
  const max = getMaxDimension(constrainedFrames, dimension)
  let offset = currentRect[axis]
  if (currentRect[dimension] <= max) {
    if (edgePosition[axis] === 0) {
      offset = Math.max(originalRect[axis], originalRect[axis] + originalRect[dimension] - max)
    } else if (edgePosition[axis] === 1) {
      offset = originalRect[axis]
    }
  }
  return {
    offset: offset,
    size: Math.max(max, currentRect[dimension]),
  }
}

function snapBoundingBox(
  selectedElements: Array<ElementPath>,
  jsxMetadata: ElementInstanceMetadataMap,
  edgePosition: EdgePosition,
  resizedBounds: CanvasRectangle,
  canvasScale: number,
  lockedAspectRatio: number | null,
  centerBased: 'center-based' | 'non-center-based',
  allElementProps: AllElementProps,
  pathTrees: ElementPathTrees,
) {
  const { snappedBoundingBox, guidelinesWithSnappingVector } = runLegacyAbsoluteResizeSnapping(
    selectedElements,
    jsxMetadata,
    edgePosition,
    resizedBounds,
    canvasScale,
    lockedAspectRatio,
    centerBased,
    allElementProps,
    pathTrees,
  )

  return {
    snappedBoundingBox,
    guidelinesWithSnappingVector,
  }
}
