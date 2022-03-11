import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { stylePropPathMappingFn } from '../../inspector/common/property-path-hooks'
import { adjustNumberProperty } from '../commands/adjust-number-command'
import { wildcardPatch } from '../commands/wildcard-patch-command'
import { Keyboard } from '../../../utils/keyboard'
import { CanvasStrategy } from './canvas-strategy-types'

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
      interactionState.interactionData.type === 'KEYBOARD'
    ) {
      const { interactionData } = interactionState

      const singleKeyPressed = interactionData.keysPressed.length === 1
      const arrowKeyPressed = Keyboard.keyIsArrow(interactionData.keysPressed[0])
      const shiftOrNoModifier =
        !interactionState.interactionData.modifiers.alt &&
        !interactionState.interactionData.modifiers.cmd &&
        !interactionState.interactionData.modifiers.ctrl

      if (singleKeyPressed && arrowKeyPressed && shiftOrNoModifier) {
        return 1
      }
    }
    return 0
  },
  apply: (canvasState, interactionState, sessionState) => {
    // TODO: absolutely minimal implementation
    if (interactionState.interactionData.type === 'KEYBOARD') {
      const key = interactionState.interactionData.keysPressed[0]
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
    }
    // Fallback for when the checks above are not satisfied.
    return []
  },
}
