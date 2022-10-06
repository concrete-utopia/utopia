import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import * as EP from '../../../core/shared/element-path'
import { absolute } from '../../../utils/utils'
import { CSSCursor } from '../canvas-types'
import { reorderElement } from '../commands/reorder-element-command'
import { setCursorCommand } from '../commands/set-cursor-command'
import { setElementsToRerenderCommand } from '../commands/set-elements-to-rerender-command'
import { updateHighlightedViews } from '../commands/update-highlighted-views-command'
import { FlowSliderControl } from '../controls/flow-slider-control'
import {
  CanvasStrategy,
  controlWithProps,
  emptyStrategyApplicationResult,
  getTargetPathsFromInteractionTarget,
  InteractionCanvasState,
  strategyApplicationResult,
} from './canvas-strategy-types'
import { findNewIndex, getOptionalDisplayPropCommands } from './flow-reorder-helpers'
import { InteractionSession } from './interaction-state'
import { isReorderAllowed } from './reorder-utils'

export function flowReorderSliderStategy(
  canvasState: InteractionCanvasState,
  interactionSession: InteractionSession | null,
): CanvasStrategy | null {
  const selectedElements = getTargetPathsFromInteractionTarget(canvasState.interactionTarget)
  if (selectedElements.length === 1) {
    const target = selectedElements[0]
    const elementMetadata = MetadataUtils.findElementByElementPath(
      canvasState.startingMetadata,
      target,
    )
    const siblings = MetadataUtils.getSiblings(canvasState.startingMetadata, target)
    if (siblings.length > 1 && MetadataUtils.isPositionedByFlow(elementMetadata)) {
      return {
        id: 'FLOW_REORDER_SLIDER',
        name: 'Reorder (Slider)',
        controlsToRender: [
          controlWithProps({
            control: FlowSliderControl,
            props: {},
            key: 'flow-slider-control',
            show: 'always-visible',
          }),
        ],
        fitness:
          interactionSession != null &&
          interactionSession.interactionData.type === 'DRAG' &&
          interactionSession.activeControl.type === 'FLOW_SLIDER'
            ? 100
            : 0,
        apply: () => {
          if (
            interactionSession != null &&
            interactionSession.interactionData.type === 'DRAG' &&
            interactionSession.activeControl.type === 'FLOW_SLIDER'
          ) {
            const siblingsOfTarget = siblings.map((element) => element.elementPath)

            if (!isReorderAllowed(siblingsOfTarget)) {
              return strategyApplicationResult(
                [setCursorCommand('mid-interaction', CSSCursor.NotPermitted)],
                {},
                'failure',
              )
            }

            if (interactionSession.interactionData.drag != null) {
              const unpatchedIndex = siblingsOfTarget.findIndex((sibling) =>
                EP.pathsEqual(sibling, target),
              )

              const newIndex = findNewIndex(
                unpatchedIndex,
                interactionSession.interactionData.drag,
                siblingsOfTarget,
                'rounded-value',
              )

              return strategyApplicationResult(
                [
                  reorderElement('always', target, absolute(newIndex)),
                  setElementsToRerenderCommand(siblingsOfTarget),
                  updateHighlightedViews('mid-interaction', []),
                  ...getOptionalDisplayPropCommands(
                    newIndex,
                    canvasState.interactionTarget,
                    canvasState.startingMetadata,
                  ),
                  setCursorCommand('mid-interaction', CSSCursor.ResizeEW),
                ],
                {
                  lastReorderIdx: newIndex,
                },
              )
            } else {
              // Fallback for when the checks above are not satisfied.
              return strategyApplicationResult([
                setCursorCommand('mid-interaction', CSSCursor.ResizeEW),
              ])
            }
          } else {
            return emptyStrategyApplicationResult
          }
        },
      }
    }
  }
  return null
}
