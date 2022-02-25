import { addAllUniquely } from '../../../core/shared/array-utils'
import { ElementInstanceMetadataMap } from '../../../core/shared/element-template'
import { CanvasPoint, CanvasVector } from '../../../core/shared/math-utils'
import { ElementPath } from '../../../core/shared/project-file-types'
import { KeyCharacter } from '../../../utils/keyboard'
import { Modifiers } from '../../../utils/modifiers'
import { ProjectContentTreeRoot } from '../../assets'
import { CanvasControlType } from './canvas-strategy-types'
import { MoveIntoDragThreshold } from '../canvas-utils'
import { CanvasCommand } from '../commands/commands'

export interface InteractionCanvasState {
  // The idea here being that we should be restricting the model we're supplying to the interactions system,
  // but that's not a requirement of this proposal
  selectedElements: Array<ElementPath>
  // metadata: ElementInstanceMetadataMap // We can add metadata back if live metadata is necessary
  projectContents: ProjectContentTreeRoot
  openFile: string | null | undefined
  scale: number
  canvasOffset: CanvasVector
}

export interface DragInteractionData {
  type: 'DRAG'
  dragStart: CanvasPoint
  drag: CanvasVector | null
  prevDrag: CanvasVector | null
  dragThresholdPassed: boolean
  originalDragStart: CanvasPoint
  modifiers: Modifiers
}

interface KeyboardInteractionData {
  type: 'KEYBOARD'
  keysPressed: Array<KeyCharacter>
  // keysPressed also includes modifiers, but we want the separate modifiers array since they are captured and mapped to a specific
  // set via modifiersForEvent in keyboard.ts
  modifiers: Modifiers
}

export type InteractionData = KeyboardInteractionData | DragInteractionData

export interface InteractionState {
  // This represents an actual interaction that has started as the result of a key press or a drag
  interactionData: InteractionData
  activeControl: CanvasControlType
  sourceOfUpdate: CanvasControlType
  lastInteractionTime: number
  metadata: ElementInstanceMetadataMap

  // To track if the user selected a strategy
  userPreferredStrategy: string | null

  startedAt: number
  globalTime: number
}

export type InteractionStateWithoutMetadata = Omit<InteractionState, 'metadata'>

export interface StrategyState {
  type: 'STRATEGY_STATE'
}

export function createEmptyStrategyState(): StrategyState {
  return {
    type: 'STRATEGY_STATE',
  }
}

export interface CommandDescription {
  description: string
  transient: boolean
}

export interface StrategyAndAccumulatedCommands {
  strategy: string | null
  commands: Array<CanvasCommand>
}

export interface SessionStateState {
  // PLEASE RENAME ME
  // Need to track here which strategy is being applied.
  currentStrategy: string | null
  currentStrategyFitness: number
  currentStrategyCommands: Array<CanvasCommand>
  accumulatedCommands: Array<StrategyAndAccumulatedCommands>
  commandDescriptions: Array<CommandDescription>

  // this is the inner state of the Strategies, can be changed via commands
  strategyState: StrategyState

  // Checkpointed metadata at the point at which a strategy change has occurred.
  startingMetadata: ElementInstanceMetadataMap
}

export function createEmptySessionStateState(
  metadata?: ElementInstanceMetadataMap,
): SessionStateState {
  return {
    currentStrategy: null,
    currentStrategyFitness: 0,
    currentStrategyCommands: [],
    accumulatedCommands: [],
    commandDescriptions: [],
    strategyState: createEmptyStrategyState(),
    startingMetadata: metadata ?? {},
  }
}

export function createInteractionViaMouse(
  mouseDownPoint: CanvasPoint,
  modifiers: Modifiers,
  activeControl: CanvasControlType,
): InteractionStateWithoutMetadata {
  return {
    interactionData: {
      type: 'DRAG',
      dragStart: mouseDownPoint,
      drag: null,
      prevDrag: null,
      dragThresholdPassed: false,
      originalDragStart: mouseDownPoint,
      modifiers: modifiers,
    },
    activeControl: activeControl,
    sourceOfUpdate: activeControl,
    lastInteractionTime: Date.now(),
    userPreferredStrategy: null,
    startedAt: Date.now(),
    globalTime: Date.now(),
  }
}

function dragExceededThreshold(drag: CanvasVector): boolean {
  const xDiff = Math.abs(drag.x)
  const yDiff = Math.abs(drag.y)
  return xDiff > MoveIntoDragThreshold || yDiff > MoveIntoDragThreshold
}

export function updateInteractionViaMouse(
  currentState: InteractionState,
  drag: CanvasVector,
  modifiers: Modifiers,
  sourceOfUpdate: CanvasControlType | null, // If null it means the active control is the source
): InteractionStateWithoutMetadata {
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
      },
      activeControl: currentState.activeControl,
      sourceOfUpdate: sourceOfUpdate ?? currentState.activeControl,
      lastInteractionTime: Date.now(),
      userPreferredStrategy: currentState.userPreferredStrategy,
      startedAt: currentState.startedAt,
      globalTime: Date.now(),
    }
  } else {
    return {
      ...currentState,
      globalTime: Date.now(),
    }
  }
}

export function createInteractionViaKeyboard(
  keysPressed: Array<KeyCharacter>,
  modifiers: Modifiers,
  activeControl: CanvasControlType,
): InteractionStateWithoutMetadata {
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
    globalTime: Date.now(),
  }
}

export function updateInteractionViaKeyboard(
  currentState: InteractionState,
  addedKeysPressed: Array<KeyCharacter>,
  keysReleased: Array<KeyCharacter>,
  modifiers: Modifiers,
  sourceOfUpdate: CanvasControlType,
): InteractionStateWithoutMetadata {
  if (currentState.interactionData.type === 'KEYBOARD') {
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
      globalTime: Date.now(),
    }
  } else if (currentState.interactionData.type === 'DRAG') {
    return {
      interactionData: {
        type: 'DRAG',
        dragStart: currentState.interactionData.dragStart,
        drag: currentState.interactionData.drag,
        prevDrag: currentState.interactionData.prevDrag,
        dragThresholdPassed: currentState.interactionData.dragThresholdPassed,
        originalDragStart: currentState.interactionData.originalDragStart,
        modifiers: modifiers,
      },
      activeControl: currentState.activeControl,
      sourceOfUpdate: currentState.activeControl,
      lastInteractionTime: Date.now(),
      userPreferredStrategy: currentState.userPreferredStrategy,
      startedAt: currentState.startedAt,
      globalTime: Date.now(),
    }
  } else {
    return {
      ...currentState,
      globalTime: Date.now(),
    }
  }
}

export function updateInteractionViaTimeStep(currentState: InteractionState): InteractionState {
  return {
    ...currentState,
    globalTime: Date.now(),
  }
}
