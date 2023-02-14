import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import {
  ElementInstanceMetadataMap,
  getJSXAttribute,
} from '../../../../core/shared/element-template'
import { ImmediateParentBounds } from '../../controls/parent-bounds'
import { ImmediateParentOutlines } from '../../controls/parent-outlines'
import { honoursPropsPosition } from './absolute-utils'
import {
  CanvasStrategy,
  controlWithProps,
  emptyStrategyApplicationResult,
  getTargetPathsFromInteractionTarget,
  InteractionCanvasState,
  MoveStrategy,
  targetPaths,
} from '../canvas-strategy-types'
import { InteractionSession } from '../interaction-state'
import {
  applyMoveCommon,
  getAdjustMoveCommands,
  getDragTargets,
} from './shared-move-strategies-helpers'
import { ZeroSizedElementControls } from '../../controls/zero-sized-element-controls'
import {
  DragOutlineControl,
  dragTargetsElementPaths,
  dragTargetsElementPathsLive,
} from '../../controls/select-mode/drag-outline-control'
import { toString } from '../../../../core/shared/element-path'
import { ElementPath } from '../../../../core/shared/project-file-types'

export function absoluteMoveStrategy(
  canvasState: InteractionCanvasState,
  interactionSession: InteractionSession | null,
): MoveStrategy | null {
  return (
    [groupLikeMoveStrategy, realAbsoluteMoveStrategy]
      .map((s) => s(canvasState, interactionSession))
      .find((s) => s != null) ?? null
  )
}

function groupLikeMoveStrategy(
  canvasState: InteractionCanvasState,
  interactionSession: InteractionSession | null,
): MoveStrategy | null {
  const selectedElements = getTargetPathsFromInteractionTarget(canvasState.interactionTarget)

  let actualTargetsIfApplicable: Array<ElementPath> = []

  const isApplicable =
    selectedElements.length > 0 &&
    getDragTargets(selectedElements).every((path) => {
      const elementMetadata = MetadataUtils.findElementByElementPath(
        canvasState.startingMetadata,
        path,
      )

      const elementProps = canvasState?.startingAllElementProps[toString(path)]

      const hasNoWidthAndHeightProps =
        elementProps?.['style']?.['width'] == null && elementProps?.['style']?.['height'] == null

      const parentIsNotFlex = elementMetadata?.specialSizeMeasurements.parentLayoutSystem !== 'flex'

      const isApplicableElement =
        elementMetadata?.specialSizeMeasurements.position === 'static' &&
        elementMetadata?.specialSizeMeasurements.layoutSystemForChildren === 'flow' &&
        hasNoWidthAndHeightProps &&
        parentIsNotFlex

      if (isApplicableElement) {
        const childPaths = MetadataUtils.getChildrenPaths(canvasState.startingMetadata, path)
        actualTargetsIfApplicable.push(...childPaths)
      }

      return isApplicableElement
    })

  if (!isApplicable) {
    return null
  }

  // behave like actualTargetsIfApplicable were the original multiselection, then delegate to realAbsoluteMoveStrategy
  const updatedCanvasState: InteractionCanvasState = {
    ...canvasState,
    interactionTarget: targetPaths(actualTargetsIfApplicable),
  }

  const realStrat = realAbsoluteMoveStrategy(updatedCanvasState, interactionSession)
  if (realStrat == null) {
    return null
  }

  return {
    ...realStrat,
    strategy: {
      ...realStrat.strategy,
      id: 'GROUP_LIKE_MOVE',
      name: 'Move (children)',
    },
  }
}

function realAbsoluteMoveStrategy(
  canvasState: InteractionCanvasState,
  interactionSession: InteractionSession | null,
): MoveStrategy | null {
  const selectedElements = getTargetPathsFromInteractionTarget(canvasState.interactionTarget)
  const isApplicable =
    selectedElements.length > 0 &&
    getDragTargets(selectedElements).every((element) => {
      const elementMetadata = MetadataUtils.findElementByElementPath(
        canvasState.startingMetadata,
        element,
      )
      return (
        elementMetadata?.specialSizeMeasurements.position === 'absolute' &&
        honoursPropsPosition(canvasState, element)
      )
    })

  if (!isApplicable) {
    return null
  }
  return {
    strategy: {
      id: 'ABSOLUTE_MOVE',
      name: 'Move',
      controlsToRender: [
        controlWithProps({
          control: ImmediateParentOutlines,
          props: { targets: selectedElements },
          key: 'parent-outlines-control',
          show: 'visible-only-while-active',
        }),
        controlWithProps({
          control: ImmediateParentBounds,
          props: { targets: selectedElements },
          key: 'parent-bounds-control',
          show: 'visible-only-while-active',
        }),
        controlWithProps({
          control: ZeroSizedElementControls,
          props: { showAllPossibleElements: true },
          key: 'zero-size-control',
          show: 'visible-only-while-active',
        }),
        {
          control: DragOutlineControl,
          props: dragTargetsElementPathsLive(selectedElements),
          key: 'ghost-outline-control',
          show: 'visible-only-while-active',
        },
      ], // Uses existing hooks in select-mode-hooks.tsx
      fitness:
        interactionSession?.interactionData.type === 'DRAG' &&
        interactionSession?.activeControl.type === 'BOUNDING_AREA'
          ? 1
          : 0,
      apply: () => {
        if (
          interactionSession?.interactionData.type === 'DRAG' &&
          interactionSession?.interactionData.drag != null
        ) {
          return applyMoveCommon(
            canvasState,
            interactionSession,
            getAdjustMoveCommands(canvasState, interactionSession),
          )
        }
        // Fallback for when the checks above are not satisfied.
        return emptyStrategyApplicationResult
      },
    },
    dragType: 'absolute',
  }
}
