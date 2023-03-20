import { isJSXElement } from '../../../../core/shared/element-template'
import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import {
  canvasRectangle,
  canvasVector,
  CanvasVector,
  nullIfInfinity,
  offsetPoint,
  scaleVector,
  zeroRectangle,
} from '../../../../core/shared/math-utils'
import Keyboard, { KeyCharacter } from '../../../../utils/keyboard'
import { withUnderlyingTarget } from '../../../editor/store/editor-state'
import {
  CanvasFrameAndTarget,
  EdgePosition,
  EdgePositionBottom,
  EdgePositionRight,
} from '../../canvas-types'
import { CanvasCommand } from '../../commands/commands'
import { pushIntendedBounds } from '../../commands/push-intended-bounds-command'
import { setElementsToRerenderCommand } from '../../commands/set-elements-to-rerender-command'
import { setSnappingGuidelines } from '../../commands/set-snapping-guidelines-command'
import { AbsoluteResizeControl } from '../../controls/select-mode/absolute-resize-control'
import {
  CanvasStrategy,
  controlWithProps,
  emptyStrategyApplicationResult,
  InteractionCanvasState,
  strategyApplicationResult,
} from '../canvas-strategy-types'
import { InteractionSession } from '../interaction-state'
import { resizeBoundingBox, supportsAbsoluteResize } from './resize-helpers'
import { createResizeCommands } from './shared-absolute-resize-strategy-helpers'
import {
  AccumulatedPresses,
  accumulatePresses,
  getKeyboardStrategyGuidelines,
  getLastKeyPressState,
  getMovementDeltaFromKey,
} from './shared-keyboard-strategy-helpers'
import { getMultiselectBounds } from './shared-move-strategies-helpers'
import { retargetStrategyToChildrenOfContentAffectingElements } from './group-like-helpers'

interface VectorAndEdge {
  movement: CanvasVector
  edge: EdgePosition
}

function pressesToVectorAndEdges(
  accumulatedPresses: Array<AccumulatedPresses>,
  maxNegativeMovement: CanvasVector,
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
            movement: getLimitedMovement(keyPressMovement, maxNegativeMovement),
            edge: edgePosition,
          })
        } else {
          const accumulatedMovement = offsetPoint(foundVectorAndEdge.movement, keyPressMovement)
          foundVectorAndEdge.movement = getLimitedMovement(accumulatedMovement, maxNegativeMovement)
        }
      }
    })
  })

  return result
}

function getLimitedMovement(
  movement: CanvasVector,
  maxNegativeMovement: CanvasVector,
): CanvasVector {
  return {
    x: Math.max(movement.x, maxNegativeMovement.x),
    y: Math.max(movement.y, maxNegativeMovement.y),
  } as CanvasVector
}

function getFitness(interactionSession: InteractionSession | null): number {
  if (interactionSession != null && interactionSession.interactionData.type === 'KEYBOARD') {
    const lastKeyState = getLastKeyPressState(
      interactionSession.interactionData.keyStates,
      Keyboard.keyIsArrow,
    )
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

export const ResizeMinimumValue = 1

export function keyboardAbsoluteResizeStrategy(
  canvasState: InteractionCanvasState,
  interactionSession: InteractionSession | null,
): CanvasStrategy | null {
  const selectedElements = retargetStrategyToChildrenOfContentAffectingElements(canvasState)
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
        props: { targets: selectedElements },
        key: 'absolute-resize-control',
        show: 'visible-except-when-other-strategy-is-active',
      }),
    ],
    fitness: getFitness(interactionSession),
    apply: () => {
      if (interactionSession != null && interactionSession.interactionData.type === 'KEYBOARD') {
        const originalFrame =
          getMultiselectBounds(canvasState.startingMetadata, selectedElements) ??
          canvasRectangle(zeroRectangle)
        const accumulatedPresses = accumulatePresses(interactionSession.interactionData.keyStates)
        const maxNegativeMovement = canvasVector({
          x: -originalFrame.width + ResizeMinimumValue,
          y: -originalFrame.height + ResizeMinimumValue,
        })
        const movementsWithEdges = pressesToVectorAndEdges(accumulatedPresses, maxNegativeMovement)

        // Start with the frame as it is at the start of the interaction.
        let newFrame = originalFrame

        let commands: Array<CanvasCommand> = []
        let intendedBounds: Array<CanvasFrameAndTarget> = []

        movementsWithEdges.forEach((movementWithEdge) => {
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

            const elementMetadata = MetadataUtils.findElementByElementPath(
              canvasState.startingMetadata,
              selectedElement,
            )
            const elementParentBounds =
              elementMetadata?.specialSizeMeasurements.immediateParentBounds ?? null
            const elementParentFlexDirection =
              elementMetadata?.specialSizeMeasurements.parentFlexDirection ?? null
            const elementGlobalFrame = nullIfInfinity(elementMetadata?.globalFrame ?? null)

            if (element != null && isJSXElement(element)) {
              const elementResult = createResizeCommands(
                element,
                selectedElement,
                movementWithEdge.edge,
                movementWithEdge.movement,
                elementGlobalFrame,
                elementParentBounds,
                elementParentFlexDirection,
              )
              commands.push(...elementResult.commands)
              if (elementResult.intendedBounds != null) {
                intendedBounds.push(elementResult.intendedBounds)
              }
            }
          })
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
      return EdgePositionRight
    case 'up':
    case 'down':
      return EdgePositionBottom
    default:
      return null
  }
}
