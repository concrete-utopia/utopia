import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { Keyboard, KeyCharacter } from '../../../utils/keyboard'
import {
  CanvasStrategy,
  emptyStrategyApplicationResult,
  InteractionCanvasState,
} from './canvas-strategy-types'
import {
  CanvasRectangle,
  canvasRectangle,
  CanvasVector,
  offsetPoint,
  offsetRect,
  scaleVector,
  zeroCanvasPoint,
  zeroRectangle,
} from '../../../core/shared/math-utils'
import {
  getAbsoluteMoveCommandsForSelectedElement,
  getMultiselectBounds,
  snapDrag,
} from './shared-absolute-move-strategy-helpers'
import { AdjustCssLengthProperty } from '../commands/adjust-css-length-command'
import { setElementsToRerenderCommand } from '../commands/set-elements-to-rerender-command'
import { setSnappingGuidelines } from '../commands/set-snapping-guidelines-command'
import { updateHighlightedViews } from '../commands/update-highlighted-views-command'
import { CanvasCommand } from '../commands/commands'
import {
  accumulatePresses,
  getDragDeltaFromKey,
  getKeyboardStrategyGuidelines,
  getLastKeyPressState,
} from './shared-keyboard-strategy-helpers'
import { mapDropNulls } from '../../../core/shared/array-utils'
import { defaultIfNull } from '../../../core/shared/optional-utils'
import {
  collectParentAndSiblingGuidelines,
  oneGuidelinePerDimension,
} from '../controls/guideline-helpers'
import { GuidelineWithSnappingVector, Guidelines } from '../guideline'
import Utils from '../../../utils/utils'
import { StrategyState, InteractionSession } from './interaction-state'

export const keyboardAbsoluteMoveStrategy: CanvasStrategy = {
  id: 'KEYBOARD_ABSOLUTE_MOVE',
  name: 'Keyboard Absolute Move',
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

      const lastKeyState = getLastKeyPressState(interactionData.keyStates)
      if (lastKeyState != null) {
        // 'Alt' determines if the distance guidelines should be shown.
        const shiftOrNoModifier = !lastKeyState.modifiers.cmd && !lastKeyState.modifiers.ctrl

        if (shiftOrNoModifier) {
          return 1
        }
      }
    }
    return 0
  },
  apply: (canvasState, interactionState, sessionState) => {
    if (interactionState.interactionData.type === 'KEYBOARD') {
      const accumulatedPresses = accumulatePresses(interactionState.interactionData.keyStates)
      let commands: Array<CanvasCommand> = []
      let drag: CanvasVector = zeroCanvasPoint
      accumulatedPresses.forEach((accumulatedPress) => {
        accumulatedPress.keysPressed.forEach((key) => {
          const keyPressDrag = scaleVector(
            getDragDeltaFromKey(key, accumulatedPress.modifiers),
            accumulatedPress.count,
          )
          drag = offsetPoint(drag, keyPressDrag)
        })
      })
      if (drag.x !== 0 || drag.y !== 0) {
        const moveCommands = canvasState.selectedElements.flatMap((selectedElement) =>
          getAbsoluteMoveCommandsForSelectedElement(
            selectedElement,
            drag,
            canvasState,
            sessionState,
          ),
        )
        const multiselectBounds = getMultiselectBounds(
          sessionState.startingMetadata,
          canvasState.selectedElements,
        )
        const draggedFrame = offsetRect(
          defaultIfNull(canvasRectangle(zeroRectangle), multiselectBounds),
          drag,
        )

        const guidelines = getKeyboardStrategyGuidelines(
          sessionState,
          canvasState,
          interactionState,
          draggedFrame,
        )

        commands.push(...moveCommands)
        commands.push(updateHighlightedViews('transient', []))
        commands.push(setSnappingGuidelines('transient', guidelines))
        commands.push(setElementsToRerenderCommand(canvasState.selectedElements))
      }
      return {
        commands: commands,
        customState: null,
      }
    } else {
      return emptyStrategyApplicationResult
    }
  },
}
