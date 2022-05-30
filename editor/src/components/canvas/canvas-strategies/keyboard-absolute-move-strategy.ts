import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { Keyboard, KeyCharacter } from '../../../utils/keyboard'
import { CanvasStrategy, emptyStrategyApplicationResult } from './canvas-strategy-types'
import { Modifiers } from '../../../utils/modifiers'
import { CanvasVector } from '../../../core/shared/math-utils'
import { getAbsoluteMoveCommandsForSelectedElement } from './shared-absolute-move-strategy-helpers'
import { AdjustCssLengthProperty } from '../commands/adjust-css-length-command'

export const keyboardAbsoluteMoveStrategy: CanvasStrategy = {
  id: 'KEYBOARD_ABSOLUTE_MOVE',
  name: 'Keyboard absolute Move',
  isApplicable: (canvasState, _interactionState, metadata) => {
    if (canvasState.selectedElements.length > 0) {
      return canvasState.selectedElements.every((element) => {
        const elementMetadata = MetadataUtils.findElementByElementPath(metadata, element)

        return elementMetadata?.specialSizeMeasurements.position === 'absolute'
      })
    } else {
      return false
    }
  },
  controlsToRender: [], // Uses existing hooks in select-mode-hooks.tsx
  fitness: (canvasState, interactionState, sessionState) => {
    if (
      keyboardAbsoluteMoveStrategy.isApplicable(
        canvasState,
        interactionState,
        sessionState.startingMetadata,
        sessionState.startingAllElementProps,
      ) &&
      interactionState.interactionData.type === 'KEYBOARD'
    ) {
      const { interactionData } = interactionState

      const arrowKeyPressed = interactionData.keysPressed.some(Keyboard.keyIsArrow)
      const shiftOrNoModifier =
        !interactionState.interactionData.modifiers.alt &&
        !interactionState.interactionData.modifiers.cmd &&
        !interactionState.interactionData.modifiers.ctrl

      if (arrowKeyPressed && shiftOrNoModifier) {
        return 1
      }
    }
    return 0
  },
  apply: (canvasState, interactionState, sessionState) => {
    if (interactionState.interactionData.type === 'KEYBOARD') {
      return {
        commands: interactionState.interactionData.keysPressed.flatMap<AdjustCssLengthProperty>(
          (key) => {
            if (key == null) {
              return []
            }
            const drag = getDragDeltaFromKey(key, interactionState.interactionData.modifiers)
            if (drag.x !== 0 || drag.y !== 0) {
              return canvasState.selectedElements.flatMap((selectedElement) =>
                getAbsoluteMoveCommandsForSelectedElement(
                  selectedElement,
                  drag,
                  canvasState,
                  sessionState,
                ),
              )
            } else {
              return []
            }
          },
        ),
        customState: null,
      }
    }
    return emptyStrategyApplicationResult
  },
}

function getDragDeltaFromKey(key: KeyCharacter, modifiers: Modifiers): CanvasVector {
  const step = modifiers.shift ? 10 : 1
  switch (key) {
    case 'left':
      return {
        x: -step,
        y: 0,
      } as CanvasVector
    case 'right':
      return {
        x: step,
        y: 0,
      } as CanvasVector
    case 'up':
      return {
        x: 0,
        y: -step,
      } as CanvasVector
    case 'down':
      return {
        x: 0,
        y: step,
      } as CanvasVector
    default:
      return {
        x: 0,
        y: 0,
      } as CanvasVector
  }
}
