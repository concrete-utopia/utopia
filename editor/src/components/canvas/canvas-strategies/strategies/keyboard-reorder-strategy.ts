import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import * as EP from '../../../../core/shared/element-path'
import { CanvasVector, offsetPoint, zeroCanvasPoint } from '../../../../core/shared/math-utils'
import { KeyCharacter } from '../../../../utils/keyboard'
import { absolute } from '../../../../utils/utils'
import { reorderElement } from '../../commands/reorder-element-command'
import { setElementsToRerenderCommand } from '../../commands/set-elements-to-rerender-command'
import { updateHighlightedViews } from '../../commands/update-highlighted-views-command'
import {
  CanvasStrategy,
  CustomStrategyState,
  emptyStrategyApplicationResult,
  getTargetPathsFromInteractionTarget,
  InteractionCanvasState,
  strategyApplicationResult,
} from '../canvas-strategy-types'
import { InteractionSession } from '../interaction-state'
import { getDirectionFlexOrFlow, isReorderAllowed } from './reorder-utils'
import { accumulatePresses } from './shared-keyboard-strategy-helpers'

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
  const siblings = MetadataUtils.getSiblings(canvasState.startingMetadata, target).map(
    (element) => element.elementPath,
  )
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

  const { direction, shouldReverse } = getDirectionFlexOrFlow(target, canvasState.startingMetadata)

  return {
    id: 'KEYBOARD_REORDER',
    name: 'Reorder',
    controlsToRender: [],
    fitness: 1,
    apply: () => {
      if (interactionSession != null && interactionSession.interactionData.type === 'KEYBOARD') {
        const accumulatedPresses = accumulatePresses(interactionSession.interactionData.keyStates)
        let keyboardResult: CanvasVector = zeroCanvasPoint
        accumulatedPresses.forEach((accumulatedPress) => {
          accumulatedPress.keysPressed.forEach((key) => {
            const keyPressIndexChange = getIndexChangeDeltaFromKey(key, accumulatedPress.count)
            keyboardResult = offsetPoint(keyboardResult, keyPressIndexChange)
          })
        })

        const unpatchedIndex = siblings.findIndex((sibling) => EP.pathsEqual(sibling, target))
        let newIndex = unpatchedIndex
        if (direction === 'horizontal') {
          // left and right arrow keys
          const result = unpatchedIndex + keyboardResult.x * (shouldReverse ? -1 : 1)
          newIndex = Math.min(Math.max(0, result), siblings.length - 1)
        } else {
          // up and down arrow keys
          const result = unpatchedIndex + keyboardResult.y * (shouldReverse ? -1 : 1)
          newIndex = Math.min(Math.max(0, result), siblings.length - 1)
        }

        if (newIndex === unpatchedIndex) {
          return strategyApplicationResult(
            [updateHighlightedViews('mid-interaction', []), setElementsToRerenderCommand(siblings)],
            {
              lastReorderIdx: newIndex,
            },
          )
        } else {
          return strategyApplicationResult(
            [
              reorderElement('always', target, absolute(newIndex)),
              setElementsToRerenderCommand(siblings),
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

function getIndexChangeDeltaFromKey(key: KeyCharacter, delta: number): CanvasVector {
  switch (key) {
    case 'left':
      return {
        x: -delta,
        y: 0,
      } as CanvasVector
    case 'right':
      return {
        x: delta,
        y: 0,
      } as CanvasVector
    case 'up':
      return {
        x: 0,
        y: -delta,
      } as CanvasVector
    case 'down':
      return {
        x: 0,
        y: delta,
      } as CanvasVector
    default:
      return {
        x: 0,
        y: 0,
      } as CanvasVector
  }
}
