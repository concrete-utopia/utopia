import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import type { ElementInstanceMetadataMap } from '../../../../core/shared/element-template'
import type { CanvasRectangle } from '../../../../core/shared/math-utils'
import {
  isInfinityRectangle,
  roundRectangleToNearestWhole,
  transformFrameUsingBoundingBox,
} from '../../../../core/shared/math-utils'
import type { ElementPath } from '../../../../core/shared/project-file-types'
import type { AllElementProps } from '../../../editor/store/editor-state'
import { getElementFromProjectContents } from '../../../editor/store/editor-state'
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
  getRetargetedTargetsForResize,
} from './resize-helpers'
import { runLegacyAbsoluteResizeSnapping } from './shared-absolute-resize-strategy-helpers'
import { flattenSelection, getMultiselectBounds } from './shared-move-strategies-helpers'
import type { ElementPathTrees } from '../../../../core/shared/element-path-tree'
import { treatElementAsGroupLike } from './group-helpers'
import type { EnsureFramePointsExist } from './resize-strategy-helpers'
import { createResizeCommandsFromFrame } from './resize-strategy-helpers'
import { isEdgePositionEqualTo } from '../../canvas-utils'

export function absoluteResizeBoundingBoxStrategy(
  canvasState: InteractionCanvasState,
  interactionSession: InteractionSession | null,
): CanvasStrategy | null {
  const originalTargets = flattenSelection(
    getTargetPathsFromInteractionTarget(canvasState.interactionTarget),
  )
  const retargetedTargets = getRetargetedTargetsForResize(canvasState)

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
                canvasState.remixRoutingTable,
                canvasState.openFile,
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
