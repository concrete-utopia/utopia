import { EdgePosition } from './components/canvas/canvas-types'
import { MoveIntoDragThreshold } from './components/canvas/canvas-utils'
import { ElementInstanceMetadata } from './core/shared/element-template'
import { CanvasPoint, CanvasVector } from './core/shared/math-utils'
import { ElementPath } from './core/shared/project-file-types'
import { KeyCharacter, Modifier } from './utils/keyboard'
import { CanvasControlType } from './components/canvas/canvas-strategies/canvas-strategy-types'
import { addAllUniquely } from './core/shared/array-utils'
import { Modifiers } from './utils/modifiers'

interface CanvasState {
  // The idea here being that we should be restricting the model we're supplying to the interactions system,
  // but that's not a requirement of this proposal
  selectedElements: Array<ElementInstanceMetadata>
}

interface MoveElement {
  type: 'MOVE_ELEMENT'
  target: ElementPath
  x: number
  y: number
}

function moveElement(target: ElementPath, x: number, y: number): MoveElement {
  return {
    type: 'MOVE_ELEMENT',
    target: target,
    x: x,
    y: y,
  }
}

// Commands that update the project model itself, and will be applied at the end of the interaction
type ModelUpdateCommand = MoveElement

interface ModelUpdateCommandsWithReason {
  commands: Array<ModelUpdateCommand>
  reason: string // The name of the strategy that created these commands? This exists so that a strategy can update its own commands
}

interface SetDragMinimumExceededCommand {
  type: 'SET_DRAG_MININUM_EXCEEDED'
}

// Commands that update other parts of the editor state, and will be discarded at the end of the interaction
type EditorUpdateCommand = SetDragMinimumExceededCommand

interface MouseInteraction {
  mousePosition: CanvasPoint
  dragStart: CanvasPoint | null
  drag: CanvasVector | null
  dragThresholdPassed: boolean
  // Should dragging be moved more into the strategy somehow?
}

// Are interaction sessions created from a mouse move? This is required if we want to pull highlighting and selection into this
// If the answer is yes, how does a dragging session start? Do we then require strategies to end the current session and create
// a new session?

// Separate InteractionSession into "state of the universe" (which includes current keys pressed, mouse position, which mouse
// buttons are pressed), and "session specific data" (which includes dragging, active control etc.)

interface MouseState {
  mousePosition: CanvasPoint
  primaryButtonDown: boolean
}

interface DragInteractionData {
  type: 'DRAG'
  dragStart: CanvasPoint
  drag: CanvasVector | null
  dragThresholdPassed: boolean
  modifiers: Modifiers
}

interface KeyboardInteractionData {
  type: 'KEYBOARD'
  keysPressed: Array<KeyCharacter>
  // keysPressed also includes modifiers, but we want the separate modifiers array since they are captured and mapped to a specific
  // set via modifiersForEvent in keyboard.ts
  modifiers: Modifiers
}

type InteractionData = KeyboardInteractionData | DragInteractionData

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

export interface InteractionState {
  // This represents an actual interaction that has started as the result of a key press or a drag
  interactionData: InteractionData
  activeControl: CanvasControlType // Do we need to guard against multiple controls trying to trigger or update an interaction session?
  sourceOfUpdate: CanvasControlType
  lastInteractionTime: number
  accumulatedCommands: Array<ModelUpdateCommandsWithReason>

  // Need to track here which strategy is being applied
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

// Does this need to be split into a default mouse interaction state and a separate drag interaction state?
// Thinking here in terms of highlight and selection
export function createInteractionViaMouse(
  mouseDownPoint: CanvasPoint,
  modifiers: Modifiers,
  activeControl: CanvasControlType,
): InteractionState {
  return {
    interactionData: {
      type: 'DRAG',
      dragStart: mouseDownPoint,
      drag: null,
      dragThresholdPassed: false,
      modifiers: modifiers,
    },
    activeControl: activeControl,
    sourceOfUpdate: activeControl,
    lastInteractionTime: Date.now(),
    accumulatedCommands: [],
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
): InteractionState {
  if (currentState.interactionData.type === 'DRAG') {
    const dragThresholdPassed =
      currentState.interactionData.dragThresholdPassed || dragExceededThreshold(drag)
    return {
      interactionData: {
        type: 'DRAG',
        dragStart: currentState.interactionData.dragStart,
        drag: dragThresholdPassed ? drag : null,
        dragThresholdPassed: dragThresholdPassed,
        modifiers: modifiers,
      },
      activeControl: currentState.activeControl,
      sourceOfUpdate: sourceOfUpdate ?? currentState.activeControl,
      lastInteractionTime: Date.now(),
      accumulatedCommands: currentState.accumulatedCommands,
    }
  } else {
    return currentState
  }
}

export function createInteractionViaKeyboard(
  keysPressed: Array<KeyCharacter>,
  modifiers: Modifiers,
  activeControl: CanvasControlType,
): InteractionState {
  return {
    interactionData: {
      type: 'KEYBOARD',
      keysPressed: keysPressed,
      modifiers: modifiers,
    },
    activeControl: activeControl,
    sourceOfUpdate: activeControl,
    lastInteractionTime: Date.now(),
    accumulatedCommands: [],
  }
}

export function updateInteractionViaKeyboard(
  currentState: InteractionState,
  addedKeysPressed: Array<KeyCharacter>,
  keysReleased: Array<KeyCharacter>,
  modifiers: Modifiers,
  sourceOfUpdate: CanvasControlType,
): InteractionState {
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
      accumulatedCommands: currentState.accumulatedCommands,
    }
  } else if (currentState.interactionData.type === 'DRAG') {
    return {
      interactionData: {
        type: 'DRAG',
        dragStart: currentState.interactionData.dragStart,
        drag: currentState.interactionData.drag,
        dragThresholdPassed: currentState.interactionData.dragThresholdPassed,
        modifiers: modifiers,
      },
      activeControl: currentState.activeControl,
      sourceOfUpdate: currentState.activeControl,
      lastInteractionTime: Date.now(),
      accumulatedCommands: currentState.accumulatedCommands,
    }
  } else {
    return currentState
  }
}

interface StrategyApplicationResult {
  modelUpdates: Array<ModelUpdateCommandsWithReason>
  transientUpdates: Array<EditorUpdateCommand>
  // Sean:
  // We currently have duplication change the selected view to the newly created elements,
  // does this mean that we can't do that or do we need a way of changing that for both types of command?
}

export interface CanvasStrategyMeta {
  shouldKeepCommands: (
    previousStrategy: string,
    nextStrategy: string | null,
    interactionState: InteractionState,
  ) => boolean
  // Sean:
  // Returns a boolean indicating if the latest/current collection of commands should be
  // added on to `accumulatedCommands` when switching the strategy or "completing" the interaction.
}

export interface CanvasStrategy {
  name: string // We'd need to do something to guarantee uniqueness here if using this for the commands' reason

  isApplicable: (canvasState: CanvasState, interactionState: InteractionState | null) => boolean
  // Determines if we should show the controls that this strategy renders
  // Maybe this can just be rolled into controlsToRender?

  controlsToRender: (
    canvasState: CanvasState,
    interactionState: InteractionState | null,
  ) => Array<CanvasControlType>
  // The controls to render when this strategy is applicable, regardless of if it is currently active
  // Other options:
  //  1. the controls are an array of React Components, and we let the render functions of those determine
  //     whether or not to render them
  //  2. the controlsToRender is just a static array, meaning the CanvasControlType becomes more specific
  //     e.g. BoundingBoxForSelectedElementControl
  // Also, should this return a Set rather than an Array? We probably need to reconcile these before rendering as strategies
  // will share controls (but will multiple strategies with the same controls for the same elements be applicable
  // at the same time?
  // Sean:
  // - Can't use a Set for something like a JavaScript object as it doesn't do any deep introspection.
  // - Returning the objects like the ones we have in this example seems like a better option, if only for testing purposes.
  // - If we're eliminating most strategies by using `isApplicable`, we'd only be running a small subset of the strategies.

  fitness: (canvasState: CanvasState, interactionState: InteractionState) => number
  // As before, for determining the relative ordering of applicable strategies during an interaction, and therefore which one to apply

  apply: (canvasState: CanvasState, interactionState: InteractionState) => StrategyApplicationResult
  // Returns the commands that inform how the model and the editor should be updated
}

function movementForKeys(keysPressed: Array<KeyCharacter>): CanvasVector | null {
  if (keysPressed.includes('left')) {
    return { x: -5, y: 0 } as CanvasVector
  } else {
    return null
  }
}

export const SomeCanvasStrategyMeta: CanvasStrategyMeta = {
  shouldKeepCommands: (
    previousStrategy: string,
    nextStrategy: string | null,
    interactionState: InteractionState,
  ) => {
    return nextStrategy !== AbsoluteMoveStrategy.name
  },
}

export const AbsoluteMoveStrategy: CanvasStrategy = {
  name: 'AbsoluteMoveStrategy',
  isApplicable: (canvasState: CanvasState, interactionState: InteractionState | null): boolean => {
    return canvasState.selectedElements.some(
      (e) => e.specialSizeMeasurements.position === 'absolute',
    )
  },
  controlsToRender: (
    canvasState: CanvasState,
    interactionState: InteractionState | null,
  ): Array<CanvasControlType> => {
    const boundingBoxes: Array<CanvasControlType> = canvasState.selectedElements.map((e) => ({
      type: 'BOUNDING_AREA',
      target: e.elementPath,
    }))
    return boundingBoxes.concat({ type: 'KEYBOARD_CATCHER_CONTROL' })
  },
  fitness: (canvasState: CanvasState, interactionState: InteractionState): number => {
    if (AbsoluteMoveStrategy.isApplicable(canvasState, interactionState)) {
      if (
        interactionState.activeControl.type === 'BOUNDING_AREA' &&
        interactionState.interactionData.type === 'DRAG'
      ) {
        return interactionState.interactionData.dragThresholdPassed ? 1 : 0
      } else if (
        interactionState.activeControl.type === 'KEYBOARD_CATCHER_CONTROL' &&
        interactionState.interactionData.type === 'KEYBOARD'
      ) {
        return movementForKeys(interactionState.interactionData.keysPressed) == null ? 0 : 1
      }
    }
    return 0
  },
  apply: (
    canvasState: CanvasState,
    interactionState: InteractionState,
  ): StrategyApplicationResult => {
    const movement =
      interactionState.interactionData.type === 'DRAG'
        ? interactionState.interactionData.drag
        : interactionState.interactionData.type === 'KEYBOARD'
        ? movementForKeys(interactionState.interactionData.keysPressed)
        : null

    if (movement == null) {
      // TODO Handle key up using a timeout to apply the final result
      return {
        modelUpdates: interactionState.accumulatedCommands,
        transientUpdates: [], // Should this return the previous transient updates? I'm not sure
      }
    } else {
      const newModelCommands: ModelUpdateCommandsWithReason = {
        reason: AbsoluteMoveStrategy.name,
        commands: canvasState.selectedElements.map((e) =>
          moveElement(e.elementPath, movement.x, movement.y),
        ),
      }

      const lastAppliedCommands =
        interactionState.accumulatedCommands[interactionState.accumulatedCommands.length - 1]
      const replaceLastAppliedCommands = lastAppliedCommands?.reason === AbsoluteMoveStrategy.name
      // I wonder if instead of providing a reason, and then trying to match on that, a better option would be to use some
      // sort of "marker" command to mark the point at which the strategy changed. The motivation for the reason
      // here, however, also includes e.g. replacing a previous command for updating width with one for updating minWidth
      // when the user explicitly changes the chosen strategy via the picker menu mid-interaction
      const previousCommands = replaceLastAppliedCommands
        ? interactionState.accumulatedCommands.slice(0, -1)
        : interactionState.accumulatedCommands

      return {
        modelUpdates: previousCommands.concat(newModelCommands),
        transientUpdates: [],
      }
    }
  },
}
