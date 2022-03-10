import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { stylePropPathMappingFn } from '../../inspector/common/property-path-hooks'
import { adjustNumberProperty } from '../commands/adjust-number-command'
import { Keyboard } from '../../../utils/keyboard'
import { CanvasStrategy } from './canvas-strategy-types'
import { last } from '../../../core/shared/array-utils'

export const keyboardAbsoluteMoveStrategy: CanvasStrategy = {
  id: 'KEYBOARD_ABSOLUTE_MOVE',
  name: 'Keyboard absolute Move',
  isApplicable: (canvasState, _interactionState, metadata) => {
    if (canvasState.selectedElements.length === 1) {
      const elementMetadata = MetadataUtils.findElementByElementPath(
        metadata,
        canvasState.selectedElements[0],
      )

      return elementMetadata?.specialSizeMeasurements.position === 'absolute'
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
      ) &&
      interactionState.interactionData.type === 'KEYBOARD_ARROW'
    ) {
      const { interactionData } = interactionState

      const validKeyPress = interactionData.keyPresses.some((keyPress) => {
        const singleKeyPressed = keyPress.keysPressed.length === 1
        const arrowKeyPressed = Keyboard.keyIsArrow(keyPress.keysPressed[0])
        const shiftOrNoModifier =
          !keyPress.modifiers.alt && !keyPress.modifiers.cmd && !keyPress.modifiers.ctrl

        return singleKeyPressed && arrowKeyPressed && shiftOrNoModifier
      })

      if (validKeyPress) {
        return 1
      }
    }
    return 0
  },
  apply: (canvasState, interactionState, sessionState) => {
    // TODO: absolutely minimal implementation
    if (interactionState.interactionData.type === 'KEYBOARD_ARROW') {
      const commands = interactionState.interactionData.keyPresses.flatMap((keyPress) => {
        const key = keyPress.keysPressed[0]
        if (key == null) {
          return []
        }
        switch (key) {
          case 'left':
            return canvasState.selectedElements.flatMap((selectedElement) => [
              adjustNumberProperty(
                'permanent',
                selectedElement,
                stylePropPathMappingFn('left', ['style']),
                -10,
                true,
              ),
            ])
          case 'right':
            return canvasState.selectedElements.flatMap((selectedElement) => [
              adjustNumberProperty(
                'permanent',
                selectedElement,
                stylePropPathMappingFn('left', ['style']),
                10,
                true,
              ),
            ])
          case 'up':
            return canvasState.selectedElements.flatMap((selectedElement) => [
              adjustNumberProperty(
                'permanent',
                selectedElement,
                stylePropPathMappingFn('top', ['style']),
                -10,
                true,
              ),
            ])
          case 'down':
            return canvasState.selectedElements.flatMap((selectedElement) => [
              adjustNumberProperty(
                'permanent',
                selectedElement,
                stylePropPathMappingFn('top', ['style']),
                10,
                true,
              ),
            ])
        }
        return []
      })
      return commands
    }
    return []
  },
}
