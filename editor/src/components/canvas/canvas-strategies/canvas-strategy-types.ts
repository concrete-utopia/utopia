import type { Spec } from 'immutability-helper'
import type { CanvasPoint, CanvasVector } from '../../../core/shared/math-utils'
import type {
  EditorState,
  EditorStatePatch,
  TransientCanvasState,
  TransientFilesState,
} from '../../editor/store/editor-state'
import type { EdgePosition } from '../canvas-types'

interface BoundingArea {
  type: 'BOUNDING_AREA'
}

interface ResizeHandle {
  type: 'RESIZE_HANDLE'
  edgePosition: EdgePosition
}

interface FlexGapHandle {
  type: 'FLEX_GAP_HANDLE'
}

export interface FlexAlignControlRectProps {
  x: number
  y: number
  width: number
  height: number
  highlighted: boolean
  associatedFlexProp: any // TODO
}

export interface FlexGapControlRectProps {
  x: number
  y: number
  width: number
  height: number
}

type CanvasControlType = BoundingArea | ResizeHandle | FlexGapHandle

export interface CanvasStrategy {
  name: string
  updateFn: CanvasStrategyUpdateFn
  fitnessFn: CanvasStrategyFitnessFn
}

export type CanvasSessionPatch = Spec<SelectModeCanvasSession>

export type CanvasStrategyUpdateFnResult = {
  newSessionState: SelectModeCanvasSessionState
  transientFilesState: TransientFilesState
  editorStatePatch: EditorStatePatch
}

export type CanvasStrategyUpdateFn = (
  lifecycle: 'transient' | 'final',
  editorState: EditorState,
  sessionProps: SelectModeCanvasSessionProps,
  sessionState: SelectModeCanvasSessionState,
) => CanvasStrategyUpdateFnResult

export type CanvasStrategyFitnessFn = (
  editorState: EditorState,
  sessionProps: SelectModeCanvasSessionProps,
  sessionState: SelectModeCanvasSessionState,
) => number

export type CanvasInteractionSession = SelectModeCanvasSession

export interface SelectModeCanvasSession {
  type: 'SELECT_MODE_CANVAS_SESSION'
  sessionProps: SelectModeCanvasSessionProps
}

export interface SelectModeCanvasSessionProps {
  start: CanvasPoint
  mousePosition: CanvasPoint
  drag: CanvasVector | null
  activeControl: CanvasControlType
  globalTime: number
  lastTimeMouseMoved: number
}

export interface SelectModeCanvasSessionState {
  activeStrategy: CanvasStrategy | null
  dragDeltaMinimumPassed: boolean
}

export const emptySelectModeCanvasSessionState: SelectModeCanvasSessionState = {
  activeStrategy: null,
  dragDeltaMinimumPassed: false,
}

export function startNewSelectModeCanvasSession(
  start: CanvasPoint,
  activeControl: CanvasControlType,
): SelectModeCanvasSession {
  return {
    type: 'SELECT_MODE_CANVAS_SESSION',
    sessionProps: {
      start: start,
      mousePosition: start,
      activeControl: activeControl,
      drag: null,
      globalTime: Date.now(),
      lastTimeMouseMoved: Date.now(),
    },
  }
}

export function updateSelectModeCanvasSessionDragVector(
  current: SelectModeCanvasSession,
  mousePosition: CanvasPoint,
  drag: CanvasVector | null,
): SelectModeCanvasSession {
  return {
    ...current,
    sessionProps: {
      ...current.sessionProps,
      mousePosition: mousePosition,
      drag: drag,
      lastTimeMouseMoved: Date.now(),
    },
  }
}
