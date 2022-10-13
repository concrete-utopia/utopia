import { last } from '../../../core/shared/array-utils'
import { ElementInstanceMetadataMap } from '../../../core/shared/element-template'
import {
  CanvasPoint,
  CanvasVector,
  offsetPoint,
  pointDifference,
  roundPointTo,
  zeroCanvasPoint,
} from '../../../core/shared/math-utils'
import { ElementPath } from '../../../core/shared/project-file-types'
import { KeyCharacter } from '../../../utils/keyboard'
import { Modifiers } from '../../../utils/modifiers'
import { AllElementProps, EditorStatePatch } from '../../editor/store/editor-state'
import { EdgePosition } from '../canvas-types'
import { MoveIntoDragThreshold } from '../canvas-utils'
import { CanvasCommand } from '../commands/commands'
import { ApplicableStrategy } from './canvas-strategies'
import {
  CanvasStrategyId,
  CustomStrategyState,
  defaultCustomStrategyState,
} from './canvas-strategy-types'
import type { ReparentTarget } from './strategies/reparent-strategy-helpers'

export interface DragInteractionData {
  type: 'DRAG'
  dragStart: CanvasPoint
  drag: CanvasVector | null
  prevDrag: CanvasVector | null
  originalDragStart: CanvasPoint
  modifiers: Modifiers
  globalTime: number
  hasMouseMoved: boolean
  _accumulatedMovement: CanvasVector
}

export interface HoverInteractionData {
  type: 'HOVER'
  point: CanvasPoint
  modifiers: Modifiers
}

export interface KeyState {
  keysPressed: Set<KeyCharacter>
  modifiers: Modifiers
}

export interface KeyboardInteractionData {
  type: 'KEYBOARD'
  keyStates: Array<KeyState>
}

export type InputData = KeyboardInteractionData | DragInteractionData | HoverInteractionData

export function isDragInteractionData(inputData: InputData): inputData is DragInteractionData {
  return inputData.type === 'DRAG'
}

export function isKeyboardInteractionData(
  inputData: InputData,
): inputData is KeyboardInteractionData {
  return inputData.type === 'KEYBOARD'
}

export function isHoverInteractionData(inputData: InputData): inputData is HoverInteractionData {
  return inputData.type === 'HOVER'
}

export type UpdatedPathMap = { [oldPathString: string]: ElementPath }

export interface ReparentTargetsToFilter {
  'use-strict-bounds': ReparentTarget | null
  'allow-missing-bounds': ReparentTarget | null
}

export type MissingBoundsHandling = 'use-strict-bounds' | 'allow-missing-bounds'

export function reparentTargetsToFilter(
  strictBoundsTarget: ReparentTarget | null,
  missingBoundsTarget: ReparentTarget | null,
): ReparentTargetsToFilter {
  return {
    'use-strict-bounds': strictBoundsTarget,
    'allow-missing-bounds': missingBoundsTarget,
  }
}

export interface InteractionSession {
  // This represents an actual interaction that has started as the result of a key press or a drag
  interactionData: InputData
  activeControl: CanvasControlType
  sourceOfUpdate: CanvasControlType
  lastInteractionTime: number
  latestMetadata: ElementInstanceMetadataMap
  latestAllElementProps: AllElementProps

  // To track if the user selected a strategy
  userPreferredStrategy: CanvasStrategyId | null

  startedAt: number

  startingTargetParentsToFilterOut: ReparentTargetsToFilter | null
  updatedTargetPaths: UpdatedPathMap
  aspectRatioLock: number | null
}

export function interactionSession(
  interactionData: InputData,
  activeControl: CanvasControlType,
  sourceOfUpdate: CanvasControlType,
  lastInteractionTime: number,
  metadata: ElementInstanceMetadataMap,
  userPreferredStrategy: CanvasStrategyId | null,
  startedAt: number,
  allElementProps: AllElementProps,
  startingTargetParentsToFilterOut: ReparentTargetsToFilter | null,
  updatedTargetPaths: UpdatedPathMap,
  aspectRatioLock: number | null,
): InteractionSession {
  return {
    interactionData: interactionData,
    activeControl: activeControl,
    sourceOfUpdate: sourceOfUpdate,
    lastInteractionTime: lastInteractionTime,
    latestMetadata: metadata,
    userPreferredStrategy: userPreferredStrategy,
    startedAt: startedAt,
    latestAllElementProps: allElementProps,
    startingTargetParentsToFilterOut: startingTargetParentsToFilterOut,
    updatedTargetPaths: updatedTargetPaths,
    aspectRatioLock: aspectRatioLock,
  }
}

export type InteractionSessionWithoutMetadata = Omit<
  InteractionSession,
  'latestMetadata' | 'latestAllElementProps' | 'startingTargetParentsToFilterOut'
>

export interface CommandDescription {
  description: string
  transient: boolean
}

export type StrategyApplicationStatus = 'success' | 'failure'

export interface StrategyState {
  // Need to track here which strategy is being applied.
  currentStrategy: CanvasStrategyId | null
  currentStrategyFitness: number
  currentStrategyCommands: Array<CanvasCommand>
  commandDescriptions: Array<CommandDescription>
  sortedApplicableStrategies: Array<ApplicableStrategy> | null
  status: StrategyApplicationStatus

  startingMetadata: ElementInstanceMetadataMap // TODO delete me!
  startingAllElementProps: AllElementProps // TODO delete me!!!!

  customStrategyState: CustomStrategyState
}

export function createEmptyStrategyState(
  metadata: ElementInstanceMetadataMap,
  allElementProps: AllElementProps,
): StrategyState {
  return {
    currentStrategy: null,
    currentStrategyFitness: 0,
    currentStrategyCommands: [],
    commandDescriptions: [],
    sortedApplicableStrategies: null,
    status: 'success',
    startingMetadata: metadata,
    customStrategyState: defaultCustomStrategyState(),
    startingAllElementProps: allElementProps,
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
      originalDragStart: mouseDownPoint,
      modifiers: modifiers,
      globalTime: Date.now(),
      hasMouseMoved: false,
      _accumulatedMovement: zeroCanvasPoint,
    },
    activeControl: activeControl,
    sourceOfUpdate: activeControl,
    lastInteractionTime: Date.now(),
    userPreferredStrategy: null,
    startedAt: Date.now(),
    updatedTargetPaths: {},
    aspectRatioLock: null,
  }
}

export function createHoverInteractionViaMouse(
  mousePoint: CanvasPoint,
  modifiers: Modifiers,
  activeControl: CanvasControlType,
): InteractionSessionWithoutMetadata {
  return {
    interactionData: {
      type: 'HOVER',
      point: mousePoint,
      modifiers: modifiers,
    },
    activeControl: activeControl,
    sourceOfUpdate: activeControl,
    lastInteractionTime: Date.now(),
    userPreferredStrategy: null,
    startedAt: Date.now(),
    updatedTargetPaths: {},
    aspectRatioLock: null,
  }
}

function dragExceededThreshold(drag: CanvasVector): boolean {
  const xDiff = Math.abs(drag.x)
  const yDiff = Math.abs(drag.y)
  return xDiff > MoveIntoDragThreshold || yDiff > MoveIntoDragThreshold
}

export function updateInteractionViaDragDelta(
  currentState: InteractionSessionWithoutMetadata,
  modifiers: Modifiers,
  sourceOfUpdate: CanvasControlType | null, // If null it means the active control is the source
  movement: CanvasVector,
): InteractionSessionWithoutMetadata {
  if (currentState.interactionData.type === 'DRAG') {
    const accumulatedMovement = roundPointTo(
      offsetPoint(currentState.interactionData._accumulatedMovement, movement),
      0,
    )
    const dragThresholdPassed = dragExceededThreshold(accumulatedMovement)
    return {
      interactionData: {
        type: 'DRAG',
        dragStart: currentState.interactionData.dragStart,
        drag: dragThresholdPassed ? accumulatedMovement : null,
        prevDrag: currentState.interactionData.drag,
        originalDragStart: currentState.interactionData.originalDragStart,
        modifiers: modifiers,
        globalTime: Date.now(),
        hasMouseMoved: true,
        _accumulatedMovement: accumulatedMovement,
      },
      activeControl: currentState.activeControl,
      sourceOfUpdate: sourceOfUpdate ?? currentState.activeControl,
      lastInteractionTime: Date.now(),
      userPreferredStrategy: currentState.userPreferredStrategy,
      startedAt: currentState.startedAt,
      updatedTargetPaths: currentState.updatedTargetPaths,
      aspectRatioLock: currentState.aspectRatioLock,
    }
  } else {
    return currentState
  }
}

export function updateInteractionViaMouse(
  currentState: InteractionSessionWithoutMetadata,
  drag: CanvasVector,
  modifiers: Modifiers,
  sourceOfUpdate: CanvasControlType | null, // If null it means the active control is the source
): InteractionSessionWithoutMetadata {
  if (currentState.interactionData.type === 'DRAG') {
    const dragThresholdPassed =
      currentState.interactionData.drag != null || dragExceededThreshold(drag)
    return {
      interactionData: {
        type: 'DRAG',
        dragStart: currentState.interactionData.dragStart,
        drag: dragThresholdPassed ? drag : null,
        prevDrag: currentState.interactionData.drag,
        originalDragStart: currentState.interactionData.originalDragStart,
        modifiers: modifiers,
        globalTime: Date.now(),
        hasMouseMoved: true,
        _accumulatedMovement: currentState.interactionData._accumulatedMovement,
      },
      activeControl: currentState.activeControl,
      sourceOfUpdate: sourceOfUpdate ?? currentState.activeControl,
      lastInteractionTime: Date.now(),
      userPreferredStrategy: currentState.userPreferredStrategy,
      startedAt: currentState.startedAt,
      updatedTargetPaths: currentState.updatedTargetPaths,
      aspectRatioLock: currentState.aspectRatioLock,
    }
  } else {
    return currentState
  }
}

export function updateHoverInteractionViaMouse(
  currentState: InteractionSessionWithoutMetadata,
  mousePoint: CanvasVector,
  modifiers: Modifiers,
  sourceOfUpdate: CanvasControlType | null, // If null it means the active control is the source
): InteractionSessionWithoutMetadata {
  if (currentState.interactionData.type === 'HOVER') {
    return {
      interactionData: {
        type: 'HOVER',
        point: mousePoint,
        modifiers: modifiers,
      },
      activeControl: currentState.activeControl,
      sourceOfUpdate: sourceOfUpdate ?? currentState.activeControl,
      lastInteractionTime: Date.now(),
      userPreferredStrategy: currentState.userPreferredStrategy,
      startedAt: currentState.startedAt,
      updatedTargetPaths: currentState.updatedTargetPaths,
      aspectRatioLock: currentState.aspectRatioLock,
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
      keyStates: [
        {
          keysPressed: new Set(keysPressed),
          modifiers: modifiers,
        },
      ],
    },
    activeControl: activeControl,
    sourceOfUpdate: activeControl,
    lastInteractionTime: Date.now(),
    userPreferredStrategy: null,
    startedAt: Date.now(),
    updatedTargetPaths: {},
    aspectRatioLock: null,
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
      const lastKeyState = last(currentState.interactionData.keyStates)
      let newKeyState: KeyState
      if (lastKeyState == null || modifiers.cmd) {
        // This is needed only for macbooks, when cmd is down, other keys don't trigger keyup events.
        newKeyState = {
          keysPressed: new Set(addedKeysPressed),
          modifiers: modifiers,
        }
      } else {
        let newKeysPressed = new Set(lastKeyState.keysPressed)
        addedKeysPressed.forEach((key) => newKeysPressed.add(key))
        keysReleased.forEach((key) => newKeysPressed.delete(key))
        newKeyState = {
          keysPressed: newKeysPressed,
          modifiers: modifiers,
        }
      }
      return {
        interactionData: {
          type: 'KEYBOARD',
          keyStates: [...currentState.interactionData.keyStates, newKeyState],
        },
        activeControl: currentState.activeControl,
        sourceOfUpdate: sourceOfUpdate,
        lastInteractionTime: Date.now(),
        userPreferredStrategy: currentState.userPreferredStrategy,
        startedAt: currentState.startedAt,
        updatedTargetPaths: currentState.updatedTargetPaths,
        aspectRatioLock: currentState.aspectRatioLock,
      }
    }
    case 'DRAG': {
      return {
        interactionData: {
          type: 'DRAG',
          dragStart: currentState.interactionData.dragStart,
          drag: currentState.interactionData.drag,
          prevDrag: currentState.interactionData.prevDrag,
          originalDragStart: currentState.interactionData.originalDragStart,
          modifiers: modifiers,
          globalTime: Date.now(),
          hasMouseMoved: currentState.interactionData.hasMouseMoved,
          _accumulatedMovement: currentState.interactionData._accumulatedMovement,
        },
        activeControl: currentState.activeControl,
        sourceOfUpdate: currentState.activeControl,
        lastInteractionTime: Date.now(),
        userPreferredStrategy: currentState.userPreferredStrategy,
        startedAt: currentState.startedAt,
        updatedTargetPaths: currentState.updatedTargetPaths,
        aspectRatioLock: currentState.aspectRatioLock,
      }
    }
    case 'HOVER': {
      return {
        interactionData: {
          type: 'HOVER',
          point: currentState.interactionData.point,
          modifiers: modifiers,
        },
        activeControl: currentState.activeControl,
        sourceOfUpdate: currentState.activeControl,
        lastInteractionTime: Date.now(),
        userPreferredStrategy: currentState.userPreferredStrategy,
        startedAt: currentState.startedAt,
        updatedTargetPaths: currentState.updatedTargetPaths,
        aspectRatioLock: currentState.aspectRatioLock,
      }
    }
    default:
      const _exhaustiveCheck: never = currentState.interactionData
      throw new Error(`Unhandled interaction type ${JSON.stringify(currentState.interactionData)}`)
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
    case 'HOVER':
      return interactionData
    case 'KEYBOARD':
      const lastKeyState = last(interactionData.keyStates)
      return {
        ...interactionData,
        keyStates: lastKeyState == null ? [] : [lastKeyState],
      }
    default:
      const _exhaustiveCheck: never = interactionData
      throw new Error(`Unhandled interaction type ${JSON.stringify(interactionData)}`)
  }
}

// Hard reset means we need to ignore everything happening in the interaction until now, and replay all the dragging
export function interactionSessionHardReset(
  interactionSessionToReset: InteractionSession,
): InteractionSession {
  return {
    ...interactionSessionToReset,
    interactionData: interactionDataHardReset(interactionSessionToReset.interactionData),
  }
}

export const KeyboardInteractionTimeout = 600

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

export interface BoundingArea {
  type: 'BOUNDING_AREA'
}

export function boundingArea(): BoundingArea {
  return {
    type: 'BOUNDING_AREA',
  }
}

export interface ResizeHandle {
  type: 'RESIZE_HANDLE'
  edgePosition: EdgePosition
}

export function resizeHandle(edgePosition: EdgePosition): ResizeHandle {
  return {
    type: 'RESIZE_HANDLE',
    edgePosition: edgePosition,
  }
}

export interface FlexGapHandle {
  type: 'FLEX_GAP_HANDLE'
}

export function flexGapHandle(): FlexGapHandle {
  return {
    type: 'FLEX_GAP_HANDLE',
  }
}

export interface KeyboardCatcherControl {
  type: 'KEYBOARD_CATCHER_CONTROL'
}

export function keyboardCatcherControl(): KeyboardCatcherControl {
  return {
    type: 'KEYBOARD_CATCHER_CONTROL',
  }
}
export interface FlowSlider {
  type: 'FLOW_SLIDER'
}

export function flowSlider(): FlowSlider {
  return {
    type: 'FLOW_SLIDER',
  }
}

export type CanvasControlType =
  | BoundingArea
  | ResizeHandle
  | FlexGapHandle
  | KeyboardCatcherControl
  | FlowSlider
