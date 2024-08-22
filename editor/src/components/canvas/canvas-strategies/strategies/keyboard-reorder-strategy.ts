import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import * as EP from '../../../../core/shared/element-path'
import type { ElementInstanceMetadata } from '../../../../core/shared/element-template'
import { shallowEqual } from '../../../../core/shared/equality-utils'
import { emptyModifiers, Modifier } from '../../../../utils/modifiers'
import { absolute } from '../../../../utils/utils'
import { reorderElement } from '../../commands/reorder-element-command'
import { setElementsToRerenderCommand } from '../../commands/set-elements-to-rerender-command'
import { updateHighlightedViews } from '../../commands/update-highlighted-views-command'
import type {
  CanvasStrategy,
  CustomStrategyState,
  InteractionCanvasState,
} from '../canvas-strategy-types'
import {
  emptyStrategyApplicationResult,
  getTargetPathsFromInteractionTarget,
  strategyApplicationResult,
} from '../canvas-strategy-types'
import type { InteractionSession } from '../interaction-state'
import { isReorderAllowed } from './reorder-utils'
import { accumulatePresses } from './shared-keyboard-strategy-helpers'

type ArrowKey = 'left' | 'right' | 'up' | 'down'

export function keyboardReorderStrategy(
  canvasState: InteractionCanvasState,
  interactionSession: InteractionSession | null,
  customStrategyState: CustomStrategyState,
): CanvasStrategy | null {
  if (interactionSession?.activeControl.type !== 'KEYBOARD_CATCHER_CONTROL') {
    return null
  }

  const selectedElements = getTargetPathsFromInteractionTarget(canvasState.interactionTarget)
  if (selectedElements.length !== 1) {
    return null
  }
  const target = selectedElements[0]
  const siblings = MetadataUtils.getSiblingsOrdered(
    canvasState.startingMetadata,
    canvasState.startingElementPathTree,
    target,
  ).map((element) => element.elementPath)
  const elementMetadata = MetadataUtils.findElementByElementPath(
    canvasState.startingMetadata,
    target,
  )
  const isFlexOrFlow =
    MetadataUtils.isParentYogaLayoutedContainerForElementAndElementParticipatesInLayout(
      elementMetadata,
    ) || MetadataUtils.isPositionedByFlow(elementMetadata)

  if (siblings.length <= 1 || !isReorderAllowed(siblings) || !isFlexOrFlow) {
    return null
  }

  return {
    id: 'KEYBOARD_REORDER',
    name: 'Reorder',
    descriptiveLabel: 'Reordering Elements',
    icon: {
      category: 'modalities',
      type: 'reorder-large',
    },
    controlsToRender: [],
    fitness: fitness(interactionSession),
    apply: () => {
      if (interactionSession != null && interactionSession.interactionData.type === 'KEYBOARD') {
        const accumulatedPresses = accumulatePresses(interactionSession.interactionData.keyStates)
        let keyboardResult: number = 0
        accumulatedPresses.forEach((accumulatedPress) => {
          accumulatedPress.keysPressed.forEach((key) => {
            if (key === 'left' || key === 'right' || key === 'up' || key === 'down') {
              const keyPressIndexChange = getIndexChangeDeltaFromKey(
                key,
                accumulatedPress.count,
                elementMetadata,
              )
              keyboardResult = keyboardResult + keyPressIndexChange
            }
          })
        })

        const unpatchedIndex = siblings.findIndex((sibling) => EP.pathsEqual(sibling, target))
        const result = unpatchedIndex + keyboardResult
        const newIndex = Math.min(Math.max(0, result), siblings.length - 1)

        const siblingsAndParent = siblings.concat(EP.parentPath(target))

        if (newIndex === unpatchedIndex) {
          return strategyApplicationResult(
            [
              updateHighlightedViews('mid-interaction', []),
              setElementsToRerenderCommand(siblingsAndParent),
            ],
            {
              lastReorderIdx: newIndex,
            },
          )
        } else {
          return strategyApplicationResult(
            [
              reorderElement('always', target, absolute(newIndex)),
              setElementsToRerenderCommand(siblingsAndParent),
              updateHighlightedViews('mid-interaction', []),
            ],
            {
              lastReorderIdx: newIndex,
            },
          )
        }
      } else {
        return emptyStrategyApplicationResult
      }
    },
  }
}

function fitness(interactionSession: InteractionSession | null): number {
  if (interactionSession == null || interactionSession.interactionData.type !== 'KEYBOARD') {
    return 0
  }

  const accumulatedPresses = accumulatePresses(interactionSession.interactionData.keyStates)
  const matches = accumulatedPresses.some(
    (accumulatedPress) =>
      Array.from(accumulatedPress.keysPressed).some(
        (key) => key === 'left' || key === 'right' || key === 'up' || key === 'down',
      ) && Modifier.equal(accumulatedPress.modifiers, emptyModifiers),
  )

  return matches ? 1 : 0
}

// This function creates the map which describes which keyboard cursor button should move the index which way.
// In standard layouts keyboard up and left moves backward and keyboard down and right moves forward.
// In flex reverse layouts this is fully reversed: keyboard up and left moves forward and keyboard down and right moves backward.
// If you have rtl text direction on top of any layouts, that should switch the effect of the left and right keys (but leave up and down as it is)
function getIndexChangesForArrowKeys(element: ElementInstanceMetadata | null): {
  left: number
  up: number
  right: number
  down: number
} {
  const textDirection = element?.specialSizeMeasurements.parentTextDirection ?? 'ltr'

  const deltasForKeypresses = (() => {
    if (
      MetadataUtils.isParentYogaLayoutedContainerForElementAndElementParticipatesInLayout(element)
    ) {
      const { forwardOrReverse } = MetadataUtils.flexDirectionToSimpleFlexDirection(
        element?.specialSizeMeasurements.parentFlexDirection ?? 'row',
      )

      // when flex is reversed we need to move the opposite way in the indexes as in the screen
      if (forwardOrReverse === 'reverse') {
        return {
          left: 1,
          up: 1,
          right: -1,
          down: -1,
        }
      }
    }
    return {
      left: -1,
      up: -1,
      right: 1,
      down: 1,
    }
  })()

  // when text direction is ltr, up and left keys should go backwards, down and right keys should go forward
  // when text direction is rtl, up and right keys should go backwards, down and left keys should go forward
  const deltasForKeypressesWithTextDirection =
    textDirection === 'ltr'
      ? deltasForKeypresses
      : {
          ...deltasForKeypresses,
          left: deltasForKeypresses['right'],
          right: deltasForKeypresses['left'],
        }

  return deltasForKeypressesWithTextDirection
}

function getIndexChangeDeltaFromKey(
  key: ArrowKey,
  delta: number,
  element: ElementInstanceMetadata | null,
): number {
  const indexChanges = getIndexChangesForArrowKeys(element)
  return delta * (indexChanges[key] ?? 0)
}
