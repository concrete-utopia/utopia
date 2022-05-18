import { addAllUniquely } from '../../../core/shared/array-utils'
import { ElementInstanceMetadataMap } from '../../../core/shared/element-template'
import {
  CanvasPoint,
  CanvasVector,
  offsetPoint,
  pointDifference,
  zeroCanvasPoint,
} from '../../../core/shared/math-utils'
import { ElementPath } from '../../../core/shared/project-file-types'
import { KeyCharacter } from '../../../utils/keyboard'
import { Modifiers } from '../../../utils/modifiers'
import { EditorStatePatch } from '../../editor/store/editor-state'
import { EdgePosition } from '../canvas-types'
import { MoveIntoDragThreshold } from '../canvas-utils'
import { CanvasCommand } from '../commands/commands'
import {
  CanvasStrategy,
  CanvasStrategyId,
  CustomStrategyState,
  defaultCustomStrategyState,
} from './canvas-strategy-types'

export interface DragInteractionData {
  type: 'DRAG'
  dragStart: CanvasPoint
  drag: CanvasVector | null
  prevDrag: CanvasVector | null
  dragThresholdPassed: boolean
  originalDragStart: CanvasPoint
  modifiers: Modifiers
  globalTime: number
}

interface KeyboardInteractionData {
  type: 'KEYBOARD'
  keysPressed: Array<KeyCharacter>
  // keysPressed also includes modifiers, but we want the separate modifiers array since they are captured and mapped to a specific
  // set via modifiersForEvent in keyboard.ts
  modifiers: Modifiers
}

export type InputData = KeyboardInteractionData | DragInteractionData

export function isDragInteractionData(inputData: InputData): inputData is DragInteractionData {
  return inputData.type === 'DRAG'
}

export function isKeyboardInteractionData(
  inputData: InputData,
): inputData is KeyboardInteractionData {
  return inputData.type === 'KEYBOARD'
}

export interface InteractionSession {
  // This represents an actual interaction that has started as the result of a key press or a drag
  interactionData: InputData
  activeControl: CanvasControlType
  sourceOfUpdate: CanvasControlType
  lastInteractionTime: number
  metadata: ElementInstanceMetadataMap

  // To track if the user selected a strategy
  userPreferredStrategy: CanvasStrategyId | null

  startedAt: number
}

export type InteractionSessionWithoutMetadata = Omit<InteractionSession, 'metadata'>

export interface CommandDescription {
  description: string
  transient: boolean
}

export interface StrategyState {
  // Need to track here which strategy is being applied.
  currentStrategy: CanvasStrategyId | null
  currentStrategyFitness: number
  currentStrategyCommands: Array<CanvasCommand>
  accumulatedPatches: Array<EditorStatePatch>
  commandDescriptions: Array<CommandDescription>
  sortedApplicableStrategies: Array<CanvasStrategy>

  // Checkpointed metadata at the point at which a strategy change has occurred.
  startingMetadata: ElementInstanceMetadataMap
  customStrategyState: CustomStrategyState
}

export function createEmptyStrategyState(metadata?: ElementInstanceMetadataMap): StrategyState {
  return {
    currentStrategy: null,
    currentStrategyFitness: 0,
    currentStrategyCommands: [],
    accumulatedPatches: [],
    commandDescriptions: [],
    sortedApplicableStrategies: [],
    startingMetadata: metadata ?? {},
    customStrategyState: defaultCustomStrategyState(),
  }
}

export function createInteractionViaMouse(
  mouseDownPoint: CanvasPoint,
  modifiers: Modifiers,
  activeControl: CanvasControlType,
): InteractionSessionWithoutMetadata {
  return {
    interactionData: {
      type: 'DRAG',
      dragStart: mouseDownPoint,
      drag: null,
      prevDrag: null,
      dragThresholdPassed: false,
      originalDragStart: mouseDownPoint,
      modifiers: modifiers,
      globalTime: Date.now(),
    },
    activeControl: activeControl,
    sourceOfUpdate: activeControl,
    lastInteractionTime: Date.now(),
    userPreferredStrategy: null,
    startedAt: Date.now(),
  }
}

function dragExceededThreshold(drag: CanvasVector): boolean {
  const xDiff = Math.abs(drag.x)
  const yDiff = Math.abs(drag.y)
  return xDiff > MoveIntoDragThreshold || yDiff > MoveIntoDragThreshold
}

export function updateInteractionViaMouse(
  currentState: InteractionSessionWithoutMetadata,
  drag: CanvasVector,
  modifiers: Modifiers,
  sourceOfUpdate: CanvasControlType | null, // If null it means the active control is the source
): InteractionSessionWithoutMetadata {
  if (currentState.interactionData.type === 'DRAG') {
    const dragThresholdPassed =
      currentState.interactionData.dragThresholdPassed || dragExceededThreshold(drag)
    return {
      interactionData: {
        type: 'DRAG',
        dragStart: currentState.interactionData.dragStart,
        drag: dragThresholdPassed ? drag : null,
        prevDrag: currentState.interactionData.drag,
        dragThresholdPassed: dragThresholdPassed,
        originalDragStart: currentState.interactionData.originalDragStart,
        modifiers: modifiers,
        globalTime: Date.now(),
      },
      activeControl: currentState.activeControl,
      sourceOfUpdate: sourceOfUpdate ?? currentState.activeControl,
      lastInteractionTime: Date.now(),
      userPreferredStrategy: currentState.userPreferredStrategy,
      startedAt: currentState.startedAt,
    }
  } else {
    return currentState
  }
}

export function createInteractionViaKeyboard(
  keysPressed: Array<KeyCharacter>,
  modifiers: Modifiers,
  activeControl: CanvasControlType,
): InteractionSessionWithoutMetadata {
  return {
    interactionData: {
      type: 'KEYBOARD',
      keysPressed: keysPressed,
      modifiers: modifiers,
    },
    activeControl: activeControl,
    sourceOfUpdate: activeControl,
    lastInteractionTime: Date.now(),
    userPreferredStrategy: null,
    startedAt: Date.now(),
  }
}

export function updateInteractionViaKeyboard(
  currentState: InteractionSession,
  addedKeysPressed: Array<KeyCharacter>,
  keysReleased: Array<KeyCharacter>,
  modifiers: Modifiers,
  sourceOfUpdate: CanvasControlType,
): InteractionSessionWithoutMetadata {
  switch (currentState.interactionData.type) {
    case 'KEYBOARD': {
      const withRemovedKeys = currentState.interactionData.keysPressed.filter(
        (k) => !keysReleased.includes(k),
      )
      const newKeysPressed = addAllUniquely(withRemovedKeys, addedKeysPressed)

      return {
        interactionData: {
          type: 'KEYBOARD',
          keysPressed: newKeysPressed,
          modifiers: modifiers,
        },
        activeControl: currentState.activeControl,
        sourceOfUpdate: sourceOfUpdate,
        lastInteractionTime: Date.now(),
        userPreferredStrategy: currentState.userPreferredStrategy,
        startedAt: currentState.startedAt,
      }
    }
    case 'DRAG': {
      return {
        interactionData: {
          type: 'DRAG',
          dragStart: currentState.interactionData.dragStart,
          drag: currentState.interactionData.drag,
          prevDrag: currentState.interactionData.prevDrag,
          dragThresholdPassed: currentState.interactionData.dragThresholdPassed,
          originalDragStart: currentState.interactionData.originalDragStart,
          modifiers: modifiers,
          globalTime: Date.now(),
        },
        activeControl: currentState.activeControl,
        sourceOfUpdate: currentState.activeControl,
        lastInteractionTime: Date.now(),
        userPreferredStrategy: currentState.userPreferredStrategy,
        startedAt: currentState.startedAt,
      }
    }
    default:
      const _exhaustiveCheck: never = currentState.interactionData
      throw new Error(`Unhandled interaction type ${JSON.stringify(currentState.interactionData)}`)
  }
}

export function strategySwitchInteractionDataReset(interactionData: InputData): InputData {
  switch (interactionData.type) {
    case 'DRAG':
      if (interactionData.drag == null || interactionData.prevDrag == null) {
        return interactionData
      } else {
        return {
          ...interactionData,
          dragStart: offsetPoint(interactionData.dragStart, interactionData.prevDrag),
          drag: pointDifference(interactionData.prevDrag, interactionData.drag),
          prevDrag: null,
        }
      }
    case 'KEYBOARD':
      return interactionData
    default:
      const _exhaustiveCheck: never = interactionData
      throw new Error(`Unhandled interaction type ${JSON.stringify(interactionData)}`)
  }
}

// Hard reset means we need to ignore everything happening in the interaction until now, and replay all the dragging
export function interactionDataHardReset(interactionData: InputData): InputData {
  switch (interactionData.type) {
    case 'DRAG':
      if (interactionData.drag == null) {
        return interactionData
      } else {
        const currentDrag = interactionData.drag ?? zeroCanvasPoint
        return {
          ...interactionData,
          dragStart: interactionData.originalDragStart,
          drag: pointDifference(
            interactionData.originalDragStart,
            offsetPoint(interactionData.dragStart, currentDrag),
          ),
        }
      }
    case 'KEYBOARD':
      return interactionData
    default:
      const _exhaustiveCheck: never = interactionData
      throw new Error(`Unhandled interaction type ${JSON.stringify(interactionData)}`)
  }
}

export function strategySwitchInteractionSessionReset(
  interactionSession: InteractionSession,
): InteractionSession {
  return {
    ...interactionSession,
    interactionData: strategySwitchInteractionDataReset(interactionSession.interactionData),
  }
}

// Hard reset means we need to ignore everything happening in the interaction until now, and replay all the dragging
export function interactionSessionHardReset(
  interactionSession: InteractionSession,
): InteractionSession {
  return {
    ...interactionSession,
    interactionData: interactionDataHardReset(interactionSession.interactionData),
  }
}

export const KeyboardInteractionTimeout = 3000

export function hasDragModifiersChanged(
  prevInteractionData: InputData | null,
  interactionData: InputData | null,
): boolean {
  return (
    interactionData?.type === 'DRAG' &&
    prevInteractionData?.type === 'DRAG' &&
    (interactionData.modifiers.alt !== prevInteractionData.modifiers.alt ||
      interactionData.modifiers.cmd !== prevInteractionData.modifiers.cmd ||
      interactionData.modifiers.ctrl !== prevInteractionData.modifiers.ctrl ||
      interactionData.modifiers.shift !== prevInteractionData.modifiers.shift)
  )
}

interface BoundingArea {
  type: 'BOUNDING_AREA'
  target: ElementPath
}

interface ResizeHandle {
  type: 'RESIZE_HANDLE'
  edgePosition: EdgePosition
}

interface FlexGapHandle {
  type: 'FLEX_GAP_HANDLE'
}

interface KeyboardCatcherControl {
  type: 'KEYBOARD_CATCHER_CONTROL'
}

export type CanvasControlType = BoundingArea | ResizeHandle | FlexGapHandle | KeyboardCatcherControl
