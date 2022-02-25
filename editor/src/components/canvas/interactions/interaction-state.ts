import { addAllUniquely } from '../../../core/shared/array-utils'
import { ElementInstanceMetadataMap } from '../../../core/shared/element-template'
import { CanvasPoint, CanvasVector } from '../../../core/shared/math-utils'
import { ElementPath } from '../../../core/shared/project-file-types'
import { KeyCharacter } from '../../../utils/keyboard'
import { Modifiers } from '../../../utils/modifiers'
import { ProjectContentTreeRoot } from '../../assets'
import { CanvasControlType } from '../canvas-strategies/canvas-strategy-types'
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

// Should we be limiting the scope here to only interactions that can update the project itself?
// If yes, what are the implications of the model? Do we need to maintain InputState?
// - Yes

// Does something have to have a lifecycle for it to be considered an interaction
// - no consensus here, but it doesn't block us

// Should we be separating keyboard and mouse interactions?
// What would separting them mean?
// What would be the benefit of this vs 1 combined interaction state?
// 1. An interaction can be started be either a key press or a mouse down
//   - keyboard interactions don't use the mouse in any way
//   - mouse interactions don't use the keyboard in any way other than modifier keys present on mouse events

// Do we desire interactions that are tweaked by a keyboard modifier that isn't cmd, shift, alt, ctrl?
// - likely not inside strategies (we could want them for making changes to the editor "mode", or chosen strategy)

// TODO
// - [X] Update accumulatedCommands
// - [X] Check fitness functions and apply chosen strategy in the dispatch function
//       removing the editorStatePatch from the TransientCanvasState
// - [X] Apply the strategies to the patched editor to get the new patch
// - [X] Implement shouldKeepCommands
// - [ ] Support natural handovers when a specific strategy was chosen (e.g. move then reparent)

// - [X] Check available strategies on each render(?) and render their controls in the canvas controls layer
// - [X] Make sure it actually works once applyCanvasStrategy is fixed

// - [X] Use patched editor for rendering the canvas, rather than the transient canvas state
//       Transient state being null results in some optimisations that we need to check for here (or possibly the drag state)

// - [X] Track the strategy being applied
// - [ ] Need to store some state to bridge across changes in a strategy - e.g. individual segments in a drag (which prop you are changing)
//       We already have a solution for this in SelectModeCanvasSessionState

// - [X] Need to actually end the sessions (we create and update the sessions but don't ever close them out)
// - [X] When closing the session we either apply the non-transient updates only, OR cancel the session which bins the commands

// - [ ] Insertion lives in the drag state

export interface InteractionState {
  // This represents an actual interaction that has started as the result of a key press or a drag
  interactionData: InteractionData
  activeControl: CanvasControlType // Do we need to guard against multiple controls trying to trigger or update an interaction session?
  sourceOfUpdate: CanvasControlType
  lastInteractionTime: number
  metadata: ElementInstanceMetadataMap

  // To track if the user selected a strategy
  userPreferredStrategy: string | null

  startedAt: number
  globalTime: number

  // Need to store some state to bridge across changes in a strategy - e.g. individual segments in a drag (which prop you are changing)

  // The latest strategy might want to replace the last commands based on the reason
  // e.g. a continuous drag would be continuously updating the commands that it is generating

  // Alternatively, the InteractionSession could include the currently active strategy, which the apply function could check
  // _or_ the current active strategy is used to determine if the strategy has changed - the strategy switch itself then can determine
  // if it wants to maintain what was being changed previously or do something else

  // Perhaps a better alternative here is to have the session maintain the modelStatePatch, i.e. the patch that applies to the project
  // model, and is therefore non-transient

  // Another alternative - strategy pickers live outside of this (but as part of the interaction / strategy process), and that is the
  // part that determines if the next strategy should replace or append commands

  // Major benefit of maintaining either the accumulated commands or model patch is that we can present the final result to the user
  // and give them the option to cancel some parts of that without undoing the entire interaction
  // Sean:
  // The above is predicated on the commands setting discrete values and/or not changing something on another element at the same time.
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

// Does this need to be split into a default mouse interaction state and a separate drag interaction state?
// Thinking here in terms of highlight and selection
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
