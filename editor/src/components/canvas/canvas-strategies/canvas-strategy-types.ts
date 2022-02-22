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

type CanvasControlType = BoundingArea | ResizeHandle | FlexGapHandle

export interface CanvasStrategy {
  name: string
  updateFn: CanvasStrategyUpdateFn
  fitnessFn: CanvasStrategyFitnessFn
}

export type CanvasStrategyUpdateFn = (
  lifecycle: 'transient' | 'final',
  editorState: EditorState,
  sessionProps: SelectModeCanvasSessionProps,
  sessionState: SelectModeCanvasSessionState,
) => any

export type CanvasStrategyFitnessFn = (
  editorState: EditorState,
  sessionProps: SelectModeCanvasSessionProps,
  sessionState: SelectModeCanvasSessionState,
) => number

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
