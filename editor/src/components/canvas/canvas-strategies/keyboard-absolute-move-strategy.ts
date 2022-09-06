import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { Keyboard, KeyCharacter } from '../../../utils/keyboard'
import {
  CanvasStrategy,
  emptyStrategyApplicationResult,
  getTargetPathsFromInteractionTarget,
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
} from './shared-absolute-move-strategy-helpers'
import { AdjustCssLengthProperty } from '../commands/adjust-css-length-command'
import { setElementsToRerenderCommand } from '../commands/set-elements-to-rerender-command'
import { setSnappingGuidelines } from '../commands/set-snapping-guidelines-command'
import { updateHighlightedViews } from '../commands/update-highlighted-views-command'
import { CanvasCommand } from '../commands/commands'
import {
  accumulatePresses,
  getMovementDeltaFromKey,
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
import { pushIntendedBounds } from '../commands/push-intended-bounds-command'
import { CanvasFrameAndTarget } from '../canvas-types'
import { honoursPropsPosition } from './absolute-utils'

export const keyboardAbsoluteMoveStrategy: CanvasStrategy = {
  id: 'KEYBOARD_ABSOLUTE_MOVE',
  name: 'Keyboard Absolute Move',
  isApplicable: (canvasState, _interactionState, metadata) => {
    const selectedElements = getTargetPathsFromInteractionTarget(canvasState.interactionTarget)
    if (selectedElements.length > 0) {
      return selectedElements.every((element) => {
        const elementMetadata = MetadataUtils.findElementByElementPath(metadata, element)
        return (
          elementMetadata?.specialSizeMeasurements.position === 'absolute' &&
          honoursPropsPosition(canvasState, element)
        )
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
    const selectedElements = getTargetPathsFromInteractionTarget(canvasState.interactionTarget)
    if (interactionState.interactionData.type === 'KEYBOARD') {
      const accumulatedPresses = accumulatePresses(interactionState.interactionData.keyStates)
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
          const elementResult = getAbsoluteMoveCommandsForSelectedElement(
            selectedElement,
            keyboardMovement,
            canvasState,
            interactionState,
            sessionState,
          )
          commands.push(...elementResult.commands)
          intendedBounds.push(...elementResult.intendedBounds)
        })
      }
      const multiselectBounds = getMultiselectBounds(
        sessionState.startingMetadata,
        selectedElements,
      )
      const newFrame = offsetRect(
        defaultIfNull(canvasRectangle(zeroRectangle), multiselectBounds),
        keyboardMovement,
      )

      const guidelines = getKeyboardStrategyGuidelines(
        sessionState,
        canvasState,
        interactionState,
        newFrame,
      )

      commands.push(updateHighlightedViews('mid-interaction', []))
      commands.push(setSnappingGuidelines('mid-interaction', guidelines))
      commands.push(pushIntendedBounds(intendedBounds))
      commands.push(setElementsToRerenderCommand(selectedElements))
      return {
        commands: commands,
        customState: null,
      }
    } else {
      return emptyStrategyApplicationResult
    }
  },
}
