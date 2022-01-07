import type { Spec } from 'immutability-helper'
import type { CanvasPoint, CanvasVector } from '../../../core/shared/math-utils'
import type {
  EditorState,
  EditorStatePatch,
  TransientCanvasState,
  TransientFilesState,
} from '../../editor/store/editor-state'
import type { EdgePosition } from '../canvas-types'

type BoundingArea = {
  type: 'BOUNDING_AREA'
}

type ResizeHandle = {
  type: 'RESIZE_HANDLE'
  edgePosition: EdgePosition
}

export interface FlexAlignControlRectProps {
  x: number
  y: number
  width: number
  height: number
  highlighted: boolean
  associatedFlexProp: any // TODO
}

type CanvasControlType = BoundingArea | ResizeHandle

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
  activeStrategy: CanvasStrategyUpdateFn | null // should this live here, or inside sessionProps? or inside SelectModeCanvasSessionState? should it exist at all?
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
  dragDeltaMinimumPassed: boolean
}

export const emptySelectModeCanvasSessionState: SelectModeCanvasSessionState = {
  dragDeltaMinimumPassed: false,
}

export function startNewSelectModeCanvasSession(
  start: CanvasPoint,
  activeControl: CanvasControlType,
): SelectModeCanvasSession {
  return {
    type: 'SELECT_MODE_CANVAS_SESSION',
    activeStrategy: null,
    sessionProps: {
      start: start,
      mousePosition: start, // TODO maybe this should be independent of start?
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
