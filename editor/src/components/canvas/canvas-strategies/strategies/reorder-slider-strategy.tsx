import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import * as EP from '../../../../core/shared/element-path'
import { absolute } from '../../../../utils/utils'
import { CSSCursor } from '../../canvas-types'
import { reorderElement } from '../../commands/reorder-element-command'
import { setCursorCommand } from '../../commands/set-cursor-command'
import { setElementsToRerenderCommand } from '../../commands/set-elements-to-rerender-command'
import { updateHighlightedViews } from '../../commands/update-highlighted-views-command'
import { ReorderSliderControl } from '../../controls/reorder-slider-control'
import type { CanvasStrategy, InteractionCanvasState } from '../canvas-strategy-types'
import {
  controlWithProps,
  emptyStrategyApplicationResult,
  getTargetPathsFromInteractionTarget,
  strategyApplicationResult,
} from '../canvas-strategy-types'
import { areAllSiblingsInOneDimensionFlexOrFlow, findNewIndex } from './flow-reorder-helpers'
import type { InteractionSession } from '../interaction-state'
import { isReorderAllowed } from './reorder-utils'
import { onlyFitWhenDraggingThisControl } from '../canvas-strategies'

export function reorderSliderStategy(
  canvasState: InteractionCanvasState,
  interactionSession: InteractionSession | null,
): CanvasStrategy | null {
  const selectedElements = getTargetPathsFromInteractionTarget(canvasState.interactionTarget)
  if (selectedElements.length !== 1) {
    return null
  }
  const target = selectedElements[0]
  const elementMetadata = MetadataUtils.findElementByElementPath(
    canvasState.startingMetadata,
    target,
  )
  const siblings = MetadataUtils.getSiblingsOrdered(
    canvasState.startingMetadata,
    canvasState.startingElementPathTree,
    target,
  )
  const isAutoLayouted =
    MetadataUtils.isParentYogaLayoutedContainerAndElementParticipatesInLayout(
      target,
      canvasState.startingMetadata,
    ) || MetadataUtils.isPositionedByFlow(elementMetadata)
  const is1dLayout = areAllSiblingsInOneDimensionFlexOrFlow(
    target,
    canvasState.startingMetadata,
    canvasState.startingElementPathTree,
  )

  if (siblings.length <= 1 || !isAutoLayouted || is1dLayout) {
    return null
  }

  return {
    id: 'REORDER_SLIDER',
    name: 'Reorder (Slider)',
    descriptiveLabel: 'Reordering Elements',
    icon: {
      category: 'modalities',
      type: 'reorder-large',
    },
    controlsToRender: [
      controlWithProps({
        control: ReorderSliderControl,
        props: { target },
        key: 'reorder-slider-control',
        show: 'always-visible',
      }),
    ],
    fitness: onlyFitWhenDraggingThisControl(interactionSession, 'REORDER_SLIDER', 100),

    apply: () => {
      if (
        interactionSession != null &&
        interactionSession.interactionData.type === 'DRAG' &&
        interactionSession.activeControl.type === 'REORDER_SLIDER'
      ) {
        const siblingsOfTarget = siblings.map((element) => element.elementPath)

        const siblingsAndParent = siblingsOfTarget.concat(EP.parentPath(target))

        if (!isReorderAllowed(siblingsOfTarget)) {
          return strategyApplicationResult(
            [setCursorCommand(CSSCursor.NotPermitted)],
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
              setElementsToRerenderCommand(siblingsAndParent),
              updateHighlightedViews('mid-interaction', []),
              setCursorCommand(CSSCursor.ResizeEW),
            ],
            {
              lastReorderIdx: newIndex,
            },
          )
        } else {
          // Fallback for when the checks above are not satisfied.
          return strategyApplicationResult([setCursorCommand(CSSCursor.ResizeEW)])
        }
      } else {
        return emptyStrategyApplicationResult
      }
    },
  }
}
