import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { Keyboard, KeyCharacter } from '../../../utils/keyboard'
import { CanvasStrategy, emptyStrategyApplicationResult } from './canvas-strategy-types'
import { Modifiers } from '../../../utils/modifiers'
import { CanvasVector, offsetPoint, scaleVector } from '../../../core/shared/math-utils'
import { AdjustCssLengthProperty } from '../commands/adjust-css-length-command'
import { createResizeCommands } from './shared-absolute-resize-strategy-helpers'
import { withUnderlyingTarget } from '../../editor/store/editor-state'
import { EdgePosition } from '../canvas-types'
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
  getDragDeltaFromKey,
  getLastKeyPressState,
} from './shared-keyboard-strategy-helpers'

interface VectorAndEdge {
  drag: CanvasVector
  edge: EdgePosition
}

function pressesToVectorAndEdges(
  accumulatedPresses: Array<AccumulatedPresses>,
): Array<VectorAndEdge> {
  let result: Array<VectorAndEdge> = []

  accumulatedPresses.forEach((accumulatedPress) => {
    accumulatedPress.keysPressed.forEach((key) => {
      const keyPressDrag = scaleVector(
        getDragDeltaFromKey(key, accumulatedPress.modifiers),
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
            drag: keyPressDrag,
            edge: edgePosition,
          })
        } else {
          foundVectorAndEdge.drag = offsetPoint(foundVectorAndEdge.drag, keyPressDrag)
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

        return elementMetadata?.specialSizeMeasurements.position === 'absolute'
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
      const dragsWithEdges = pressesToVectorAndEdges(accumulatedPresses)
      let commands: Array<CanvasCommand> = []
      dragsWithEdges.forEach((dragWithEdge) => {
        if (dragWithEdge.drag.x !== 0 || dragWithEdge.drag.y !== 0) {
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

            if (element != null) {
              commands.push(
                ...createResizeCommands(
                  element,
                  selectedElement,
                  dragWithEdge.edge,
                  dragWithEdge.drag,
                  elementParentBounds,
                ),
              )
            }
          })
        }
      })
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
