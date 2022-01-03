import { sortBy } from '../../../core/shared/array-utils'
import {
  EditorState,
  transientCanvasState,
  TransientCanvasState,
} from '../../editor/store/editor-state'
import {
  CanvasStrategy,
  CanvasStrategyUpdateFn,
  SelectModeCanvasSession,
} from './canvas-strategy-types'
import { flexAlignParentStrategy } from './flex-align-parent-strategy'

const RegisteredCanvasStrategies: Array<CanvasStrategy> = [flexAlignParentStrategy]

export function pickDefaultCanvasStrategy(
  editorState: EditorState,
  currentSession: SelectModeCanvasSession,
): CanvasStrategyUpdateFn | null {
  sortBy(RegisteredCanvasStrategies, (l, r) => {
    // sort by fitness, descending
    return r.fitnessFn(editorState, currentSession) - l.fitnessFn(editorState, currentSession)
  })
  return RegisteredCanvasStrategies[0]?.updateFn ?? null
}

export function applyCanvasStrategy(
  lifecycle: 'transient' | 'final',
  editorState: EditorState,
  canvasSession: SelectModeCanvasSession,
): TransientCanvasState | null {
  const strategy = pickDefaultCanvasStrategy(editorState, canvasSession)
  const result = strategy?.(lifecycle, editorState, canvasSession) ?? null
  // TODO BEFORE MERGE APPLY result?.newSessionState !!!!
  return transientCanvasState(
    null,
    null,
    result?.transientFilesState ?? null,
    [],
    result?.editorStatePatch ?? null,
  )
}
