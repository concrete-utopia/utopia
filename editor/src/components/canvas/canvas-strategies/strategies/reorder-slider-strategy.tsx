import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import * as EP from '../../../../core/shared/element-path'
import { absolute } from '../../../../utils/utils'
import { CSSCursor } from '../../canvas-types'
import { reorderElement } from '../../commands/reorder-element-command'
import { setCursorCommand } from '../../commands/set-cursor-command'
import { setElementsToRerenderCommand } from '../../commands/set-elements-to-rerender-command'
import { updateHighlightedViews } from '../../commands/update-highlighted-views-command'
import { ReorderSliderControl } from '../../controls/reorder-slider-control'
import {
  CanvasStrategy,
  controlWithProps,
  emptyStrategyApplicationResult,
  getTargetPathsFromInteractionTarget,
  InteractionCanvasState,
  strategyApplicationResult,
} from '../canvas-strategy-types'
import { areAllSiblingsInOneDimensionFlexOrFlow, findNewIndex } from './flow-reorder-helpers'
import { InteractionSession } from '../interaction-state'
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
  const siblings = MetadataUtils.getSiblingsUnordered(canvasState.startingMetadata, target)
  const isAutoLayouted =
    MetadataUtils.isParentYogaLayoutedContainerAndElementParticipatesInLayout(
      target,
      canvasState.startingMetadata,
    ) || MetadataUtils.isPositionedByFlow(elementMetadata)
  const is1dLayout = areAllSiblingsInOneDimensionFlexOrFlow(target, canvasState.startingMetadata)

  if (siblings.length <= 1 || !isAutoLayouted || is1dLayout) {
    return null
  }

  return {
    id: 'REORDER_SLIDER',
    name: 'Reorder (Slider)',
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
              reorderElement(
                'always',
                target,
                absolute(newIndex),
                'use-deprecated-insertJSXElementChild',
              ),
              setElementsToRerenderCommand(siblingsOfTarget),
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
