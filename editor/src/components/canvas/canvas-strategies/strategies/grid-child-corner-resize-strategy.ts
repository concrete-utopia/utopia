import { detectFillHugFixedState } from '../../../inspector/inspector-common'
import * as EP from '../../../../core/shared/element-path'
import { gridContainerIdentifier, gridItemIdentifier } from '../../../editor/store/editor-state'
import {
  EdgePositionBottomLeft,
  EdgePositionBottomRight,
  EdgePositionTopLeft,
  EdgePositionTopRight,
  type EdgePosition,
  type EdgePositionCorner,
} from '../../canvas-types'

import {
  controlsForGridPlaceholders,
  GridResizeControls,
} from '../../controls/grid-controls-for-strategies'
import {
  getDescriptiveStrategyLabelWithRetargetedPaths,
  onlyFitWhenDraggingThisControl,
} from '../canvas-strategies'
import type {
  CanvasStrategy,
  InteractionCanvasState,
  StrategyApplicationResult,
} from '../canvas-strategy-types'
import {
  emptyStrategyApplicationResult,
  getTargetPathsFromInteractionTarget,
} from '../canvas-strategy-types'
import type { StrategyApplicationStatus } from '../interaction-state'
import { GridResizeEdges, type GridResizeEdge, type InteractionSession } from '../interaction-state'
import { absoluteBoundingResize } from './absolute-resize-bounding-box-strategy'
import { getChildGroupsForNonGroupParents } from './fragment-like-helpers'
import type { ElementPath } from 'utopia-shared/src/types'
import { assertNever } from '../../../../core/shared/utils'
import { isEdgePositionEqualTo } from '../../canvas-utils'
import { gridResizeElement } from './grid-resize-element-strategy'
import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import type { ElementInstanceMetadata } from '../../../../core/shared/element-template'
import { isInfinityRectangle } from '../../../../core/shared/math-utils'
import type { CanvasCommand } from '../../commands/commands'
import { setCursorCommand } from '../../commands/set-cursor-command'
import { pickCursorFromEdgePosition } from './resize-helpers'

export function gridChildCornerResizeStrategy(
  canvasState: InteractionCanvasState,
  interactionSession: InteractionSession | null,
): CanvasStrategy | null {
  const selectedElements = getTargetPathsFromInteractionTarget(canvasState.interactionTarget)
  if (selectedElements.length !== 1) {
    return null
  }
  const selectedElement = selectedElements[0]

  const selectedElementMetadata = MetadataUtils.findElementByElementPath(
    canvasState.startingMetadata,
    selectedElement,
  )
  if (selectedElementMetadata == null) {
    return null
  }
  const nonNullSelectedElementMetadata: ElementInstanceMetadata = selectedElementMetadata

  const selectedElementBounds = MetadataUtils.getFrameInCanvasCoords(
    selectedElement,
    canvasState.startingMetadata,
  )
  if (selectedElementBounds == null || isInfinityRectangle(selectedElementBounds)) {
    return null
  }

  return {
    id: 'GRID_CHILD_CORNER_RESIZE',
    name: 'Grid Child Corner Resize',
    descriptiveLabel: getDescriptiveStrategyLabelWithRetargetedPaths('Resizing Elements', false),
    icon: {
      category: 'modalities',
      type: 'resize',
    },
    controlsToRender: [
      {
        control: GridResizeControls,
        props: { target: gridContainerIdentifier(selectedElement) },
        key: `grid-resize-controls-${EP.toString(selectedElement)}`,
        show: 'always-visible',
      },
      controlsForGridPlaceholders(gridItemIdentifier(selectedElement)),
    ],
    fitness: onlyFitWhenDraggingThisControl(interactionSession, 'GRID_CHILD_CORNER_HANDLE', 1),
    apply: (lifecycle) => {
      if (
        interactionSession != null &&
        interactionSession.interactionData.type === 'DRAG' &&
        interactionSession.activeControl.type === 'GRID_CHILD_CORNER_HANDLE'
      ) {
        const childGroups = getChildGroupsForNonGroupParents(
          canvasState.startingMetadata,
          selectedElements,
        )

        const { gridResizeEdges, edgePosition } = fromEdgePositionCornerToStrategyValues(
          canvasState,
          selectedElement,
          interactionSession.activeControl.corner,
        )

        let strategyApplicationResults: Array<StrategyApplicationResult> = []
        if (edgePosition != null) {
          strategyApplicationResults.push(
            absoluteBoundingResize(
              canvasState,
              interactionSession,
              selectedElements,
              selectedElements,
              childGroups,
              edgePosition,
            ),
          )
        }

        for (const gridResizeEdge of gridResizeEdges) {
          strategyApplicationResults.push(
            gridResizeElement(
              interactionSession,
              selectedElement,
              nonNullSelectedElementMetadata,
              selectedElementBounds,
              gridResizeEdge,
            ),
          )
        }

        if (strategyApplicationResults.length === 0) {
          return emptyStrategyApplicationResult
        }

        let commands: Array<CanvasCommand> = []
        let elementsToRerender: Array<ElementPath> = []
        let status: StrategyApplicationStatus = 'success'
        for (const strategyApplicationResult of strategyApplicationResults) {
          commands.push(...strategyApplicationResult.commands)
          elementsToRerender.push(...strategyApplicationResult.elementsToRerender)
          if (strategyApplicationResult.status === 'failure') {
            status = 'failure'
          }
        }

        commands.push(
          setCursorCommand(pickCursorFromEdgePosition(interactionSession.activeControl.corner)),
        )

        return {
          commands: commands,
          elementsToRerender: elementsToRerender,
          customStatePatch: {}, // Non-trivial to merge, but it appears not necessary for this.
          status: status,
        }
      }
      // Fallback for when the checks above are not satisfied.
      return emptyStrategyApplicationResult
    },
  }
}

export interface StrategyValuesFromCorner {
  gridResizeEdges: Array<GridResizeEdge>
  edgePosition: EdgePosition | null
}

export function strategyValuesFromCorner(
  gridResizeEdges: Array<GridResizeEdge>,
  edgePosition: EdgePosition | null,
): StrategyValuesFromCorner {
  return {
    gridResizeEdges: gridResizeEdges,
    edgePosition: edgePosition,
  }
}

export function fromEdgePositionCornerToStrategyValues(
  canvasState: InteractionCanvasState,
  element: ElementPath,
  corner: EdgePositionCorner,
): StrategyValuesFromCorner {
  // Check stretch values.
  const horizontalIsStretch =
    detectFillHugFixedState('horizontal', canvasState.startingMetadata, element).fixedHugFill
      ?.type === 'stretch'
  const verticalIsStretch =
    detectFillHugFixedState('vertical', canvasState.startingMetadata, element).fixedHugFill
      ?.type === 'stretch'

  const gridResizeEdges: Array<GridResizeEdge> = GridResizeEdges.filter((edge) => {
    switch (edge) {
      case 'column-start':
        return (
          horizontalIsStretch &&
          (isEdgePositionEqualTo(corner, EdgePositionTopLeft) ||
            isEdgePositionEqualTo(corner, EdgePositionBottomLeft))
        )
      case 'column-end':
        return (
          horizontalIsStretch &&
          (isEdgePositionEqualTo(corner, EdgePositionTopRight) ||
            isEdgePositionEqualTo(corner, EdgePositionBottomRight))
        )
      case 'row-start':
        return (
          verticalIsStretch &&
          (isEdgePositionEqualTo(corner, EdgePositionTopLeft) ||
            isEdgePositionEqualTo(corner, EdgePositionTopRight))
        )
      case 'row-end':
        return (
          verticalIsStretch &&
          (isEdgePositionEqualTo(corner, EdgePositionBottomLeft) ||
            isEdgePositionEqualTo(corner, EdgePositionBottomRight))
        )
      default:
        assertNever(edge)
    }
  })

  const edgePosition: EdgePosition = {
    x: horizontalIsStretch ? 0.5 : corner.x,
    y: verticalIsStretch ? 0.5 : corner.y,
  }

  return strategyValuesFromCorner(
    gridResizeEdges,
    isEdgePositionEqualTo(edgePosition, { x: 0.5, y: 0.5 }) ? null : edgePosition,
  )
}
