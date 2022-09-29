import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import * as EP from '../../../core/shared/element-path'
import { offsetPoint } from '../../../core/shared/math-utils'
import { CSSCursor } from '../canvas-types'
import { reorderElement } from '../commands/reorder-element-command'
import { setCursorCommand } from '../commands/set-cursor-command'
import { setElementsToRerenderCommand } from '../commands/set-elements-to-rerender-command'
import { updateHighlightedViews } from '../commands/update-highlighted-views-command'
import { ParentBounds } from '../controls/parent-bounds'
import { ParentOutlines } from '../controls/parent-outlines'
import {
  CanvasStrategy,
  emptyStrategyApplicationResult,
  getTargetPathsFromInteractionTarget,
  InteractionCanvasState,
  strategyApplicationResult,
  StrategyApplicationResult,
} from './canvas-strategy-types'
import { absolute } from '../../../utils/utils'
import { InteractionSession, StrategyState, StrategyStateNew } from './interaction-state'
import { ElementInstanceMetadataMap } from '../../../core/shared/element-template'
import {
  getFlowReorderIndex,
  getNewDisplayTypeForIndex,
  getOptionalDisplayPropCommands,
  isValidFlowReorderTarget,
} from './flow-reorder-helpers'
import {
  FlowReorderAreaIndicator,
  FlowReorderDragOutline,
} from '../controls/flow-reorder-indicators'
import { AllElementProps } from '../../editor/store/editor-state'
import { isReorderAllowed } from './reorder-utils'

function isFlowReorderConversionApplicable(
  canvasState: InteractionCanvasState,
  interactionSession: InteractionSession | null,
  metadata: ElementInstanceMetadataMap,
  allElementProps: AllElementProps,
): boolean {
  const selectedElements = getTargetPathsFromInteractionTarget(canvasState.interactionTarget)
  if (selectedElements.length === 1) {
    const target = selectedElements[0]
    const elementMetadata = MetadataUtils.findElementByElementPath(metadata, target)
    const siblings = MetadataUtils.getSiblings(metadata, target)
    if (
      siblings.length > 1 &&
      MetadataUtils.isPositionedByFlow(elementMetadata) &&
      isValidFlowReorderTarget(elementMetadata)
    ) {
      return siblings.some((sibling) => MetadataUtils.isPositionedByFlow(sibling))
    } else {
      return false
    }
  } else {
    return false
  }
}

function flowReorderApplyCommon(
  canvasState: InteractionCanvasState,
  interactionState: InteractionSession,
  strategyState: StrategyStateNew,
): StrategyApplicationResult {
  if (interactionState.interactionData.type !== 'DRAG') {
    return emptyStrategyApplicationResult
  }

  if (interactionState.interactionData.drag != null) {
    const selectedElements = getTargetPathsFromInteractionTarget(canvasState.interactionTarget)
    const target = selectedElements[0] // TODO MULTISELECT??

    const siblingsOfTarget = MetadataUtils.getSiblingsProjectContentsOrdered(
      strategyState.startingMetadata,
      target,
    ).map((element) => element.elementPath)

    if (!isReorderAllowed(siblingsOfTarget)) {
      return strategyApplicationResult(
        [setCursorCommand('mid-interaction', CSSCursor.NotPermitted)],
        {},
        'failure',
      )
    }

    const rawPointOnCanvas = offsetPoint(
      interactionState.interactionData.dragStart,
      interactionState.interactionData.drag,
    )

    const unpatchedIndex = siblingsOfTarget.findIndex((sibling) => EP.pathsEqual(sibling, target))
    const lastReorderIdx = strategyState.customStrategyState.lastReorderIdx ?? unpatchedIndex

    const { newIndex, targetSiblingUnderMouse } = getFlowReorderIndex(
      interactionState.latestMetadata,
      strategyState.startingAllElementProps,
      rawPointOnCanvas,
      target,
    )

    const newIndexFound = newIndex > -1
    const mouseStillOverPreviousTargetSibling = EP.pathsEqual(
      targetSiblingUnderMouse,
      strategyState.customStrategyState.previousReorderTargetSiblingUnderMouse,
    )

    const newResultOrLastIndex =
      !mouseStillOverPreviousTargetSibling && newIndexFound ? newIndex : lastReorderIdx

    const newDisplayType = getNewDisplayTypeForIndex(
      strategyState.startingMetadata,
      target,
      siblingsOfTarget[newResultOrLastIndex],
    )

    return strategyApplicationResult(
      [
        reorderElement('always', target, absolute(newResultOrLastIndex)),
        setElementsToRerenderCommand(siblingsOfTarget),
        updateHighlightedViews('mid-interaction', []),
        setCursorCommand('mid-interaction', CSSCursor.Move),
        ...getOptionalDisplayPropCommands(target, newDisplayType),
      ],
      {
        lastReorderIdx: newResultOrLastIndex,
        previousReorderTargetSiblingUnderMouse: targetSiblingUnderMouse,
      },
    )
  } else {
    return strategyApplicationResult([setCursorCommand('mid-interaction', CSSCursor.Move)])
  }
}

export const flowReorderStrategy: CanvasStrategy = {
  id: 'FLOW_REORDER',
  name: () => 'Reorder (Flow)',
  isApplicable: isFlowReorderConversionApplicable,
  controlsToRender: [
    {
      control: ParentOutlines,
      key: 'parent-outlines-control',
      show: 'visible-only-while-active',
    },
    {
      control: ParentBounds,
      key: 'parent-bounds-control',
      show: 'visible-only-while-active',
    },
    {
      control: FlowReorderDragOutline,
      key: 'flow-reorder-drag-outline',
      show: 'visible-only-while-active',
    },
  ], // Uses existing hooks in select-mode-hooks.tsx
  fitness: (canvasState, interactionState, strategyState) => {
    return flowReorderStrategy.isApplicable(
      canvasState,
      interactionState,
      strategyState.startingMetadata,
      strategyState.startingAllElementProps,
    ) &&
      interactionState.interactionData.type === 'DRAG' &&
      interactionState.activeControl.type === 'BOUNDING_AREA'
      ? 3
      : 0
  },
  apply: flowReorderApplyCommon,
}
