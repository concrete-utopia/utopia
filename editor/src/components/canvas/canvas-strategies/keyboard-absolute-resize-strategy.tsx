import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { Keyboard, KeyCharacter } from '../../../utils/keyboard'
import { CanvasStrategy, emptyStrategyApplicationResult } from './canvas-strategy-types'
import { Modifiers } from '../../../utils/modifiers'
import {
  canvasRectangle,
  CanvasVector,
  offsetPoint,
  scaleVector,
  zeroRectangle,
} from '../../../core/shared/math-utils'
import { AdjustCssLengthProperty } from '../commands/adjust-css-length-command'
import { createResizeCommands, resizeBoundingBox } from './shared-absolute-resize-strategy-helpers'
import { withUnderlyingTarget } from '../../editor/store/editor-state'
import { CanvasFrameAndTarget, EdgePosition } from '../canvas-types'
import { AbsoluteResizeControl } from '../controls/select-mode/absolute-resize-control'
import {
  SetElementsToRerenderCommand,
  setElementsToRerenderCommand,
} from '../commands/set-elements-to-rerender-command'
import { CanvasCommand } from '../commands/commands'
import { last } from '../../../core/shared/array-utils'
import {
  AccumulatedPresses,
  accumulatePresses,
  getMovementDeltaFromKey,
  getKeyboardStrategyGuidelines,
  getLastKeyPressState,
} from './shared-keyboard-strategy-helpers'
import { getMultiselectBounds } from './shared-absolute-move-strategy-helpers'
import { setSnappingGuidelines } from '../commands/set-snapping-guidelines-command'
import { pushIntendedBounds } from '../commands/push-intended-bounds-command'
import { supportsStyle } from './absolute-utils'

interface VectorAndEdge {
  movement: CanvasVector
  edge: EdgePosition
}

function pressesToVectorAndEdges(
  accumulatedPresses: Array<AccumulatedPresses>,
): Array<VectorAndEdge> {
  let result: Array<VectorAndEdge> = []

  accumulatedPresses.forEach((accumulatedPress) => {
    accumulatedPress.keysPressed.forEach((key) => {
      const keyPressMovement = scaleVector(
        getMovementDeltaFromKey(key, accumulatedPress.modifiers),
        accumulatedPress.count,
      )
      const edgePosition = getEdgePositionFromKey(key)
      if (edgePosition != null) {
        let foundVectorAndEdge: VectorAndEdge | null = null
        for (let vectorAndEdge of result) {
          if (vectorAndEdge.edge.x === edgePosition.x && vectorAndEdge.edge.y === edgePosition.y) {
            foundVectorAndEdge = vectorAndEdge
          }
        }
        if (foundVectorAndEdge == null) {
          result.push({
            movement: keyPressMovement,
            edge: edgePosition,
          })
        } else {
          foundVectorAndEdge.movement = offsetPoint(foundVectorAndEdge.movement, keyPressMovement)
        }
      }
    })
  })

  return result
}

export const keyboardAbsoluteResizeStrategy: CanvasStrategy = {
  id: 'KEYBOARD_ABSOLUTE_RESIZE',
  name: 'Keyboard absolute resize',
  isApplicable: (canvasState, interactionState, metadata) => {
    if (canvasState.selectedElements.length > 0) {
      return canvasState.selectedElements.every((element) => {
        const elementMetadata = MetadataUtils.findElementByElementPath(metadata, element)
        return (
          elementMetadata?.specialSizeMeasurements.position === 'absolute' &&
          supportsStyle(canvasState, element)
        )
      })
    } else {
      return false
    }
  },
  controlsToRender: [
    { control: AbsoluteResizeControl, key: 'absolute-resize-control', show: 'always-visible' },
  ],
  fitness: (canvasState, interactionState, sessionState) => {
    if (
      keyboardAbsoluteResizeStrategy.isApplicable(
        canvasState,
        interactionState,
        sessionState.startingMetadata,
        sessionState.startingAllElementProps,
      ) &&
      interactionState.interactionData.type === 'KEYBOARD'
    ) {
      const { interactionData } = interactionState
      const lastKeyPress = getLastKeyPressState(interactionData.keyStates)
      if (lastKeyPress != null) {
        const cmdAndOptionallyShiftModifier =
          lastKeyPress.modifiers.cmd && !lastKeyPress.modifiers.alt && !lastKeyPress.modifiers.ctrl

        if (cmdAndOptionallyShiftModifier) {
          return 1
        }
      }
    }
    return 0
  },
  apply: (canvasState, interactionState, sessionState) => {
    if (interactionState.interactionData.type === 'KEYBOARD') {
      const accumulatedPresses = accumulatePresses(interactionState.interactionData.keyStates)
      const movementsWithEdges = pressesToVectorAndEdges(accumulatedPresses)

      // Start with the frame as it is at the start of the interaction.
      let newFrame =
        getMultiselectBounds(sessionState.startingMetadata, canvasState.selectedElements) ??
        canvasRectangle(zeroRectangle)

      let commands: Array<CanvasCommand> = []
      let intendedBounds: Array<CanvasFrameAndTarget> = []

      movementsWithEdges.forEach((movementWithEdge) => {
        if (movementWithEdge.movement.x !== 0 || movementWithEdge.movement.y !== 0) {
          newFrame = resizeBoundingBox(
            newFrame,
            movementWithEdge.movement,
            movementWithEdge.edge,
            null,
            'non-center-based',
          )
          canvasState.selectedElements.forEach((selectedElement) => {
            const element = withUnderlyingTarget(
              selectedElement,
              canvasState.projectContents,
              {},
              canvasState.openFile,
              null,
              (_, e) => e,
            )
            const elementParentBounds =
              MetadataUtils.findElementByElementPath(sessionState.startingMetadata, selectedElement)
                ?.specialSizeMeasurements.immediateParentBounds ?? null

            const elementGlobalFrame = MetadataUtils.getFrameInCanvasCoords(
              selectedElement,
              sessionState.startingMetadata,
            )

            if (element != null) {
              const elementResult = createResizeCommands(
                element,
                selectedElement,
                movementWithEdge.edge,
                movementWithEdge.movement,
                elementGlobalFrame,
                elementParentBounds,
              )
              commands.push(...elementResult.commands)
              if (elementResult.intendedBounds != null) {
                intendedBounds.push(elementResult.intendedBounds)
              }
            }
          })
        }
      })
      const guidelines = getKeyboardStrategyGuidelines(
        sessionState,
        canvasState,
        interactionState,
        newFrame,
      )
      commands.push(setSnappingGuidelines('mid-interaction', guidelines))
      commands.push(pushIntendedBounds(intendedBounds))
      commands.push(setElementsToRerenderCommand(canvasState.selectedElements))
      return {
        commands: commands,
        customState: null,
      }
    } else {
      return emptyStrategyApplicationResult
    }
  },
}

function getEdgePositionFromKey(key: KeyCharacter): EdgePosition | null {
  switch (key) {
    case 'left':
    case 'right':
      return {
        x: 1,
        y: 0.5,
      } as EdgePosition
    case 'up':
    case 'down':
      return {
        x: 0.5,
        y: 1,
      } as EdgePosition
    default:
      return null
  }
}
