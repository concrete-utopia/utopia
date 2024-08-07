import type { ElementPathTrees } from '../../../core/shared/element-path-tree'
import { last } from '../../../core/shared/array-utils'
import type { ElementInstanceMetadataMap } from '../../../core/shared/element-template'
import type { CanvasPoint, CanvasVector } from '../../../core/shared/math-utils'
import {
  magnitude,
  offsetPoint,
  pointDifference,
  roundPointToNearestWhole,
  zeroCanvasPoint,
} from '../../../core/shared/math-utils'
import type { ElementPath } from '../../../core/shared/project-file-types'
import { assertNever } from '../../../core/shared/utils'
import type { KeyCharacter } from '../../../utils/keyboard'
import type { Modifiers } from '../../../utils/modifiers'
import type { AllElementProps } from '../../editor/store/editor-state'
import type { BorderRadiusCorner } from '../border-radius-control-utils'
import type { EdgePiece, EdgePosition } from '../canvas-types'
import { MoveIntoDragThreshold } from '../canvas-utils'
import type { CanvasCommand } from '../commands/commands'
import type { ApplicableStrategy } from './canvas-strategies'
import type {
  CanvasStrategyIcon,
  CanvasStrategyId,
  CustomStrategyState,
} from './canvas-strategy-types'
import { defaultCustomStrategyState } from './canvas-strategy-types'
import type { VariablesInScope } from '../ui-jsx-canvas'

export type ZeroDragPermitted = 'zero-drag-permitted' | 'zero-drag-not-permitted'

export interface DragInteractionData {
  type: 'DRAG'
  dragStart: CanvasPoint
  drag: CanvasVector | null
  prevDrag: CanvasVector | null
  originalDragStart: CanvasPoint
  modifiers: Modifiers
  hasMouseMoved: boolean
  _accumulatedMovement: CanvasVector
  spacePressed: boolean
  zeroDragPermitted: ZeroDragPermitted // Will still complete the interaction with no drag distance applied.
}

export interface HoverInteractionData {
  type: 'HOVER'
  point: CanvasPoint
  modifiers: Modifiers
  zeroDragPermitted: ZeroDragPermitted // Will still complete the interaction with no drag distance applied.
}

export interface KeyState {
  keysPressed: Set<KeyCharacter>
  modifiers: Modifiers
}

export interface KeyboardInteractionData {
  type: 'KEYBOARD'
  keyStates: Array<KeyState>
}

export type InputData = KeyboardInteractionData | MouseInteractionData

export type MouseInteractionData = DragInteractionData | HoverInteractionData

export function isDragInteractionData(inputData: InputData): inputData is DragInteractionData {
  return inputData.type === 'DRAG'
}

export function isNotYetStartedDragInteraction(
  inputData: InputData,
): inputData is DragInteractionData {
  return isDragInteractionData(inputData) && inputData.drag == null
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

export type AllowSmallerParent = 'allow-smaller-parent' | 'disallow-smaller-parent'

export interface InteractionSession {
  // This represents an actual interaction that has started as the result of a key press or a drag
  interactionData: InputData
  activeControl: CanvasControlType
  lastInteractionTime: number
  latestMetadata: ElementInstanceMetadataMap
  latestAllElementProps: AllElementProps
  latestVariablesInScope: VariablesInScope
  latestElementPathTree: ElementPathTrees

  // To track if the user selected a strategy
  userPreferredStrategy: CanvasStrategyId | null

  startedAt: number

  updatedTargetPaths: UpdatedPathMap
  aspectRatioLock: number | null
}

export function interactionSession(
  interactionData: InputData,
  activeControl: CanvasControlType,
  lastInteractionTime: number,
  metadata: ElementInstanceMetadataMap,
  userPreferredStrategy: CanvasStrategyId | null,
  startedAt: number,
  allElementProps: AllElementProps,
  latestVariablesInScope: VariablesInScope,
  updatedTargetPaths: UpdatedPathMap,
  aspectRatioLock: number | null,
  elementPathTree: ElementPathTrees,
): InteractionSession {
  return {
    interactionData: interactionData,
    activeControl: activeControl,
    lastInteractionTime: lastInteractionTime,
    latestMetadata: metadata,
    userPreferredStrategy: userPreferredStrategy,
    startedAt: startedAt,
    latestAllElementProps: allElementProps,
    latestVariablesInScope: latestVariablesInScope,
    updatedTargetPaths: updatedTargetPaths,
    aspectRatioLock: aspectRatioLock,
    latestElementPathTree: elementPathTree,
  }
}

export function interactionSessionIsActive(session: InteractionSession | null): boolean {
  if (session == null) {
    return false
  } else {
    switch (session.interactionData.type) {
      case 'DRAG':
        return session.interactionData.drag != null
      case 'HOVER':
        return true
      case 'KEYBOARD':
        return true
    }
  }
}

export type InteractionSessionWithoutMetadata = Omit<
  InteractionSession,
  'latestMetadata' | 'latestAllElementProps' | 'latestElementPathTree' | 'latestVariablesInScope'
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
  currentStrategyDescriptiveLabel: string | null
  currentStrategyIcon: CanvasStrategyIcon | null
  currentStrategyCommands: Array<CanvasCommand>
  commandDescriptions: Array<CommandDescription>
  sortedApplicableStrategies: Array<ApplicableStrategy> | null
  status: StrategyApplicationStatus

  startingMetadata: ElementInstanceMetadataMap // TODO delete me!
  startingAllElementProps: AllElementProps // TODO delete me!!!!
  startingElementPathTree: ElementPathTrees // TODO delete me!!!!

  customStrategyState: CustomStrategyState
}

export function createEmptyStrategyState(
  metadata: ElementInstanceMetadataMap,
  allElementProps: AllElementProps,
  elementPathTree: ElementPathTrees,
): StrategyState {
  return {
    currentStrategy: null,
    currentStrategyFitness: 0,
    currentStrategyDescriptiveLabel: null,
    currentStrategyIcon: null,
    currentStrategyCommands: [],
    commandDescriptions: [],
    sortedApplicableStrategies: null,
    status: 'success',
    startingMetadata: metadata,
    customStrategyState: defaultCustomStrategyState(),
    startingAllElementProps: allElementProps,
    startingElementPathTree: elementPathTree,
  }
}

export function createInteractionViaMouse(
  mouseDownPoint: CanvasPoint,
  modifiers: Modifiers,
  activeControl: CanvasControlType,
  zeroDragPermitted: ZeroDragPermitted,
): InteractionSessionWithoutMetadata {
  return {
    interactionData: {
      type: 'DRAG',
      dragStart: mouseDownPoint,
      drag: null,
      prevDrag: null,
      originalDragStart: mouseDownPoint,
      modifiers: modifiers,
      hasMouseMoved: false,
      _accumulatedMovement: zeroCanvasPoint,
      spacePressed: false,
      zeroDragPermitted: zeroDragPermitted,
    },
    activeControl: activeControl,
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
  zeroDragPermitted: ZeroDragPermitted,
): InteractionSessionWithoutMetadata {
  return {
    interactionData: {
      type: 'HOVER',
      point: mousePoint,
      modifiers: modifiers,
      zeroDragPermitted: zeroDragPermitted,
    },
    activeControl: activeControl,
    lastInteractionTime: Date.now(),
    userPreferredStrategy: null,
    startedAt: Date.now(),
    updatedTargetPaths: {},
    aspectRatioLock: null,
  }
}

function dragExceededThreshold(drag: CanvasVector): boolean {
  return magnitude(drag) > MoveIntoDragThreshold
}

export function updateInteractionViaDragDelta(
  currentState: InteractionSessionWithoutMetadata,
  modifiers: Modifiers,
  sourceOfUpdate: CanvasControlType | null, // If null it means the active control is the source
  movement: CanvasVector,
): InteractionSessionWithoutMetadata {
  if (currentState.interactionData.type === 'DRAG') {
    const accumulatedMovement = roundPointToNearestWhole(
      offsetPoint(currentState.interactionData._accumulatedMovement, movement),
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
        hasMouseMoved: true,
        _accumulatedMovement: accumulatedMovement,
        spacePressed: currentState.interactionData.spacePressed,
        zeroDragPermitted: currentState.interactionData.zeroDragPermitted,
      },
      activeControl: sourceOfUpdate ?? currentState.activeControl,
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
  newInteractionType: 'HOVER' | 'DRAG',
  drag: CanvasVector,
  modifiers: Modifiers,
  sourceOfUpdate: CanvasControlType | null, // If null it means the active control is the source
): InteractionSessionWithoutMetadata {
  if (
    currentState.interactionData.type === 'DRAG' ||
    currentState.interactionData.type === 'HOVER'
  ) {
    return {
      interactionData: updateInteractionDataViaMouse(
        currentState.interactionData,
        newInteractionType,
        drag,
        modifiers,
      ),
      activeControl: sourceOfUpdate ?? currentState.activeControl,
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

function updateInteractionDataViaMouse(
  currentData: MouseInteractionData,
  newInteractionType: 'HOVER' | 'DRAG',
  mousePoint: CanvasVector,
  modifiers: Modifiers,
): MouseInteractionData {
  switch (newInteractionType) {
    case 'HOVER':
      return {
        type: 'HOVER',
        point: mousePoint,
        modifiers: modifiers,
        zeroDragPermitted: currentData.zeroDragPermitted,
      }

    case 'DRAG':
      switch (currentData.type) {
        case 'DRAG':
          const dragThresholdPassed = currentData.drag != null || dragExceededThreshold(mousePoint)
          return {
            type: 'DRAG',
            dragStart: currentData.dragStart,
            drag: dragThresholdPassed ? mousePoint : null,
            prevDrag: currentData.drag,
            originalDragStart: currentData.originalDragStart,
            modifiers: modifiers,
            hasMouseMoved: true,
            _accumulatedMovement: currentData._accumulatedMovement,
            spacePressed: currentData.spacePressed,
            zeroDragPermitted: currentData.zeroDragPermitted,
          }
        case 'HOVER':
          return {
            type: 'DRAG',
            dragStart: mousePoint,
            drag: null,
            prevDrag: null,
            originalDragStart: mousePoint,
            modifiers: modifiers,
            hasMouseMoved: false,
            _accumulatedMovement: zeroCanvasPoint,
            spacePressed: false,
            zeroDragPermitted: currentData.zeroDragPermitted,
          }
        default:
          assertNever(currentData)
      }
      break
    default:
      assertNever(newInteractionType)
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
  activeControl: CanvasControlType | null,
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
        activeControl: activeControl ?? currentState.activeControl,
        lastInteractionTime: Date.now(),
        userPreferredStrategy: currentState.userPreferredStrategy,
        startedAt: currentState.startedAt,
        updatedTargetPaths: currentState.updatedTargetPaths,
        aspectRatioLock: currentState.aspectRatioLock,
      }
    }
    case 'DRAG': {
      const isSpacePressed =
        (currentState.interactionData.spacePressed ||
          addedKeysPressed.some((key) => key === 'space')) &&
        !keysReleased.some((key) => key === 'space')

      return {
        interactionData: {
          type: 'DRAG',
          dragStart: currentState.interactionData.dragStart,
          drag: currentState.interactionData.drag,
          prevDrag: currentState.interactionData.prevDrag,
          originalDragStart: currentState.interactionData.originalDragStart,
          modifiers: modifiers,
          hasMouseMoved: currentState.interactionData.hasMouseMoved,
          _accumulatedMovement: currentState.interactionData._accumulatedMovement,
          spacePressed: isSpacePressed,
          zeroDragPermitted: currentState.interactionData.zeroDragPermitted,
        },
        activeControl: currentState.activeControl,
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
          zeroDragPermitted: currentState.interactionData.zeroDragPermitted,
        },
        activeControl: currentState.activeControl,
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

export interface GridAxisHandle {
  type: 'GRID_AXIS_HANDLE'
  axis: 'column' | 'row'
  columnOrRow: number
}

export function gridAxisHandle(axis: 'column' | 'row', columnOrRow: number): GridAxisHandle {
  return {
    type: 'GRID_AXIS_HANDLE',
    axis: axis,
    columnOrRow: columnOrRow,
  }
}

export interface PaddingResizeHandle {
  type: 'PADDING_RESIZE_HANDLE'
  edgePiece: EdgePiece
}

export function paddingResizeHandle(edgePosition: EdgePiece): PaddingResizeHandle {
  return {
    type: 'PADDING_RESIZE_HANDLE',
    edgePiece: edgePosition,
  }
}

export interface BorderRadiusResizeHandle {
  type: 'BORDER_RADIUS_RESIZE_HANDLE'
  corner: BorderRadiusCorner
}

export function borderRadiusResizeHandle(corner: BorderRadiusCorner): BorderRadiusResizeHandle {
  return {
    type: 'BORDER_RADIUS_RESIZE_HANDLE',
    corner: corner,
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
export interface ReorderSlider {
  type: 'REORDER_SLIDER'
}

export function reorderSlider(): ReorderSlider {
  return {
    type: 'REORDER_SLIDER',
  }
}

export interface GridCellHandle {
  type: 'GRID_CELL_HANDLE'
  id: string
}

export function gridCellHandle(params: { id: string }): GridCellHandle {
  return {
    type: 'GRID_CELL_HANDLE',
    id: params.id,
  }
}

export const GridResizeEdges = ['row-start', 'row-end', 'column-start', 'column-end'] as const
export type GridResizeEdge = (typeof GridResizeEdges)[number]

export type GridResizeEdgeProperties = {
  isRow: boolean
  isColumn: boolean
  isStart: boolean
  isEnd: boolean
}

export function gridResizeEdgeProperties(edge: GridResizeEdge): GridResizeEdgeProperties {
  return {
    isRow: edge === 'row-start' || edge === 'row-end',
    isColumn: edge === 'column-start' || edge === 'column-end',
    isStart: edge === 'row-start' || edge === 'column-start',
    isEnd: edge === 'row-end' || edge === 'column-end',
  }
}

export interface GridResizeHandle {
  type: 'GRID_RESIZE_HANDLE'
  id: string
  edge: GridResizeEdge
}

export function gridResizeHandle(id: string, edge: GridResizeEdge): GridResizeHandle {
  return {
    type: 'GRID_RESIZE_HANDLE',
    id: id,
    edge: edge,
  }
}

export type CanvasControlType =
  | BoundingArea
  | ResizeHandle
  | FlexGapHandle
  | PaddingResizeHandle
  | KeyboardCatcherControl
  | ReorderSlider
  | BorderRadiusResizeHandle
  | GridCellHandle
  | GridAxisHandle
  | GridResizeHandle

export function isDragToPan(
  interaction: InteractionSession | null,
  spacePressed: boolean | undefined,
): boolean {
  return spacePressed === true && interaction == null
}
