import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import { Keyboard, KeyCharacter } from '../../../../utils/keyboard'
import {
  CanvasStrategy,
  emptyStrategyApplicationResult,
  getTargetPathsFromInteractionTarget,
  InteractionCanvasState,
  strategyApplicationResult,
} from '../canvas-strategy-types'
import {
  CanvasRectangle,
  canvasRectangle,
  CanvasVector,
  offsetPoint,
  offsetRect,
  scaleVector,
  zeroCanvasPoint,
  zeroRectangle,
} from '../../../../core/shared/math-utils'
import {
  getMoveCommandsForSelectedElement,
  getMultiselectBounds,
} from './shared-move-strategies-helpers'
import { setElementsToRerenderCommand } from '../../commands/set-elements-to-rerender-command'
import { setSnappingGuidelines } from '../../commands/set-snapping-guidelines-command'
import { updateHighlightedViews } from '../../commands/update-highlighted-views-command'
import { CanvasCommand } from '../../commands/commands'
import {
  accumulatePresses,
  getMovementDeltaFromKey,
  getKeyboardStrategyGuidelines,
  getLastKeyPressState,
} from './shared-keyboard-strategy-helpers'
import { defaultIfNull } from '../../../../core/shared/optional-utils'
import { pushIntendedBounds } from '../../commands/push-intended-bounds-command'
import { CanvasFrameAndTarget } from '../../canvas-types'
import { honoursPropsPosition } from './absolute-utils'
import { InteractionSession } from '../interaction-state'
import { ElementPath } from '../../../../core/shared/project-file-types'

export function keyboardAbsoluteMoveStrategy(
  canvasState: InteractionCanvasState,
  interactionSession: InteractionSession | null,
): CanvasStrategy | null {
  const selectedElements = getTargetPathsFromInteractionTarget(canvasState.interactionTarget)
  if (selectedElements.length === 0) {
    return null
  }
  if (!isApplicable(canvasState, selectedElements)) {
    return null
  }

  return {
    id: 'KEYBOARD_ABSOLUTE_MOVE',
    name: 'Move',
    controlsToRender: [], // Uses existing hooks in select-mode-hooks.tsx
    fitness: getFitness(interactionSession),
    apply: () => {
      if (interactionSession != null && interactionSession.interactionData.type === 'KEYBOARD') {
        const accumulatedPresses = accumulatePresses(interactionSession.interactionData.keyStates)
        let commands: Array<CanvasCommand> = []
        let intendedBounds: Array<CanvasFrameAndTarget> = []
        let keyboardMovement: CanvasVector = zeroCanvasPoint
        accumulatedPresses.forEach((accumulatedPress) => {
          accumulatedPress.keysPressed.forEach((key) => {
            const keyPressMovement = scaleVector(
              getMovementDeltaFromKey(key, accumulatedPress.modifiers),
              accumulatedPress.count,
            )
            keyboardMovement = offsetPoint(keyboardMovement, keyPressMovement)
          })
        })
        if (keyboardMovement.x !== 0 || keyboardMovement.y !== 0) {
          selectedElements.forEach((selectedElement) => {
            const elementResult = getMoveCommandsForSelectedElement(
              selectedElement,
              keyboardMovement,
              canvasState,
              interactionSession,
            )
            commands.push(...elementResult.commands)
            intendedBounds.push(...elementResult.intendedBounds)
          })
        }
        const multiselectBounds = getMultiselectBounds(
          canvasState.startingMetadata,
          selectedElements,
        )
        const newFrame = offsetRect(
          defaultIfNull(canvasRectangle(zeroRectangle), multiselectBounds),
          keyboardMovement,
        )

        const guidelines = getKeyboardStrategyGuidelines(canvasState, interactionSession, newFrame)

        commands.push(updateHighlightedViews('mid-interaction', []))
        commands.push(setSnappingGuidelines('mid-interaction', guidelines))
        commands.push(pushIntendedBounds(intendedBounds))
        commands.push(setElementsToRerenderCommand(selectedElements))
        return strategyApplicationResult(commands)
      } else {
        return emptyStrategyApplicationResult
      }
    },
  }
}

function isApplicable(canvasState: InteractionCanvasState, selectedElements: Array<ElementPath>) {
  return selectedElements.every((element) => {
    const elementMetadata = MetadataUtils.findElementByElementPath(
      canvasState.startingMetadata,
      element,
    )
    return (
      elementMetadata?.specialSizeMeasurements.position === 'absolute' &&
      honoursPropsPosition(canvasState, element)
    )
  })
}

function getFitness(interactionSession: InteractionSession | null): number {
  if (interactionSession != null && interactionSession.interactionData.type === 'KEYBOARD') {
    const lastKeyState = getLastKeyPressState(
      interactionSession.interactionData.keyStates,
      Keyboard.keyIsArrow,
    )
    if (lastKeyState != null) {
      // 'Alt' determines if the distance guidelines should be shown.
      const shiftOrNoModifier = !lastKeyState.modifiers.cmd && !lastKeyState.modifiers.ctrl

      if (shiftOrNoModifier) {
        return 1
      }
    }
  }

  return 0
}
