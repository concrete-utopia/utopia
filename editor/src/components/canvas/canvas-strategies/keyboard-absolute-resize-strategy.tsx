import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { Keyboard, KeyCharacter } from '../../../utils/keyboard'
import { CanvasStrategy } from './canvas-strategy-types'
import { Modifiers } from '../../../utils/modifiers'
import { CanvasVector } from '../../../core/shared/math-utils'
import { AdjustCssLengthProperty } from '../commands/adjust-css-length-command'
import { createResizeCommands } from './shared-absolute-resize-strategy-helpers'
import { withUnderlyingTarget } from '../../editor/store/editor-state'
import { EdgePosition } from '../canvas-types'
import { AbsoluteResizeControl } from '../controls/select-mode/absolute-resize-control'

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
      ) &&
      interactionState.interactionData.type === 'KEYBOARD'
    ) {
      const { interactionData } = interactionState

      const arrowKeyPressed = interactionData.keysPressed.some(Keyboard.keyIsArrow)
      const cmdAndOptionallyShiftModifier =
        interactionState.interactionData.modifiers.cmd &&
        !interactionState.interactionData.modifiers.alt &&
        !interactionState.interactionData.modifiers.ctrl

      if (arrowKeyPressed && cmdAndOptionallyShiftModifier) {
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
            const edgePosition = getEdgePositionFromKey(key)
            if (drag.x !== 0 || drag.y !== 0) {
              return canvasState.selectedElements.flatMap((selectedElement) => {
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
                    sessionState.startingMetadata,
                    selectedElement,
                  )?.specialSizeMeasurements.immediateParentBounds ?? null

                if (element == null || edgePosition == null) {
                  return []
                }
                return createResizeCommands(
                  element,
                  selectedElement,
                  edgePosition,
                  drag,
                  elementParentBounds,
                )
              })
            } else {
              return []
            }
          },
        ),
      }
    }
    return {
      commands: [],
    }
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
