import type { Spec } from 'immutability-helper'
import type { CanvasPoint, CanvasVector } from '../../../core/shared/math-utils'
import { ElementPath } from '../../../core/shared/project-file-types'
import { CanvasStrategy } from '../../../interactions_proposal'
import type {
  EditorState,
  EditorStatePatch,
  TransientCanvasState,
  TransientFilesState,
} from '../../editor/store/editor-state'
import type { EdgePosition } from '../canvas-types'
import { CanvasCommand } from '../commands/commands'

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

export type CanvasControlType = BoundingArea | ResizeHandle | FlexGapHandle | KeyboardCatcherControl

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
