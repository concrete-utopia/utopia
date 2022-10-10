import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import {
  canvasRectangle,
  CanvasVector,
  offsetPoint,
  scaleVector,
  zeroRectangle,
} from '../../../../core/shared/math-utils'
import { KeyCharacter } from '../../../../utils/keyboard'
import { withUnderlyingTarget } from '../../../editor/store/editor-state'
import { CanvasFrameAndTarget, EdgePosition } from '../../canvas-types'
import { CanvasCommand } from '../../commands/commands'
import { pushIntendedBounds } from '../../commands/push-intended-bounds-command'
import { setElementsToRerenderCommand } from '../../commands/set-elements-to-rerender-command'
import { setSnappingGuidelines } from '../../commands/set-snapping-guidelines-command'
import { AbsoluteResizeControl } from '../../controls/select-mode/absolute-resize-control'
import { supportsAbsoluteResize } from './absolute-resize-helpers'
import {
  CanvasStrategy,
  controlWithProps,
  emptyStrategyApplicationResult,
  getTargetPathsFromInteractionTarget,
  InteractionCanvasState,
  strategyApplicationResult,
} from '../canvas-strategy-types'
import { InteractionSession } from '../interaction-state'
import { createResizeCommands, resizeBoundingBox } from './shared-absolute-resize-strategy-helpers'
import {
  AccumulatedPresses,
  accumulatePresses,
  getKeyboardStrategyGuidelines,
  getLastKeyPressState,
  getMovementDeltaFromKey,
} from './shared-keyboard-strategy-helpers'
import { getMultiselectBounds } from './shared-move-strategies-helpers'

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

function getFitness(interactionSession: InteractionSession | null): number {
  if (interactionSession != null && interactionSession.interactionData.type === 'KEYBOARD') {
    const lastKeyState = getLastKeyPressState(interactionSession.interactionData.keyStates)
    if (lastKeyState != null) {
      const cmdAndOptionallyShiftModifier =
        lastKeyState.modifiers.cmd && !lastKeyState.modifiers.alt && !lastKeyState.modifiers.ctrl

      if (cmdAndOptionallyShiftModifier) {
        return 1
      }
    }
  }

  return 0
}

export function keyboardAbsoluteResizeStrategy(
  canvasState: InteractionCanvasState,
  interactionSession: InteractionSession | null,
): CanvasStrategy | null {
  const selectedElements = getTargetPathsFromInteractionTarget(canvasState.interactionTarget)
  if (
    selectedElements.length === 0 ||
    !selectedElements.every((element) => {
      return supportsAbsoluteResize(canvasState.startingMetadata, element, canvasState)
    })
  ) {
    return null
  }

  return {
    id: 'KEYBOARD_ABSOLUTE_RESIZE',
    name: 'Resize',
    controlsToRender: [
      controlWithProps({
        control: AbsoluteResizeControl,
        props: {},
        key: 'absolute-resize-control',
        show: 'visible-except-when-other-strategy-is-active',
      }),
    ],
    fitness: getFitness(interactionSession),
    apply: () => {
      if (interactionSession != null && interactionSession.interactionData.type === 'KEYBOARD') {
        const accumulatedPresses = accumulatePresses(interactionSession.interactionData.keyStates)
        const movementsWithEdges = pressesToVectorAndEdges(accumulatedPresses)

        // Start with the frame as it is at the start of the interaction.
        let newFrame =
          getMultiselectBounds(canvasState.startingMetadata, selectedElements) ??
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
            selectedElements.forEach((selectedElement) => {
              const element = withUnderlyingTarget(
                selectedElement,
                canvasState.projectContents,
                {},
                canvasState.openFile,
                null,
                (_, e) => e,
              )
              const elementParentBounds =
                MetadataUtils.findElementByElementPath(
                  canvasState.startingMetadata,
                  selectedElement,
                )?.specialSizeMeasurements.immediateParentBounds ?? null

              const elementGlobalFrame = MetadataUtils.getFrameInCanvasCoords(
                selectedElement,
                canvasState.startingMetadata,
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
        const guidelines = getKeyboardStrategyGuidelines(canvasState, interactionSession, newFrame)
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
