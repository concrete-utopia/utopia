import { sortBy } from '../../../core/shared/array-utils'
import { EditorState, TransientCanvasState } from '../../editor/store/editor-state'
import { CanvasStrategy, CanvasStrategyUpdateFn, SelectModeCanvasSession } from '../canvas-types'
import { flexAlignParentStrategy } from './flex-align-parent-strategy'

const RegisteredCanvasStrategies: Array<CanvasStrategy> = [flexAlignParentStrategy]

export function pickDefaultCanvasStrategy(
  editorState: EditorState,
  currentSession: SelectModeCanvasSession,
  previousTransientState: TransientCanvasState | null,
): CanvasStrategyUpdateFn | null {
  sortBy(RegisteredCanvasStrategies, (l, r) => {
    // sort by fitness, descending
    return (
      r.fitnessFn(editorState, currentSession, previousTransientState) -
      l.fitnessFn(editorState, currentSession, previousTransientState)
    )
  })
  return RegisteredCanvasStrategies[0]?.updateFn ?? null
}

export function applyCanvasStrategy(
  editorState: EditorState,
  canvasSession: SelectModeCanvasSession,
  previousTransientState: TransientCanvasState | null,
): TransientCanvasState | null {
  const strategy = pickDefaultCanvasStrategy(editorState, canvasSession, previousTransientState)
  return strategy?.(editorState, canvasSession, previousTransientState) ?? null
}
