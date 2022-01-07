import { sortBy } from '../../../core/shared/array-utils'
import {
  EditorState,
  transientCanvasState,
  TransientCanvasState,
  transientCanvasStateForSession,
} from '../../editor/store/editor-state'
import {
  CanvasStrategy,
  CanvasStrategyUpdateFn,
  emptySelectModeCanvasSessionState,
  SelectModeCanvasSession,
  SelectModeCanvasSessionState,
} from './canvas-strategy-types'
import { flexAlignParentStrategy } from './flex-align-parent-strategy'

const RegisteredCanvasStrategies: Array<CanvasStrategy> = [flexAlignParentStrategy]

export function pickDefaultCanvasStrategy(
  editorState: EditorState,
  sessionProps: SelectModeCanvasSession,
  sessionState: SelectModeCanvasSessionState,
): CanvasStrategyUpdateFn | null {
  sortBy(RegisteredCanvasStrategies, (l, r) => {
    // sort by fitness, descending
    return (
      r.fitnessFn(editorState, sessionProps, sessionState) -
      l.fitnessFn(editorState, sessionProps, sessionState)
    )
  })
  return RegisteredCanvasStrategies[0]?.updateFn ?? null
}

export function applyCanvasStrategy(
  lifecycle: 'transient' | 'final',
  editorState: EditorState,
  canvasSessionProps: SelectModeCanvasSession,
  canvasSessionState: SelectModeCanvasSessionState | null,
): TransientCanvasState | null {
  const sessionStateToUse = canvasSessionState ?? emptySelectModeCanvasSessionState

  const strategy = pickDefaultCanvasStrategy(editorState, canvasSessionProps, sessionStateToUse)

  const result = strategy?.(lifecycle, editorState, canvasSessionProps, sessionStateToUse) ?? null

  if (result != null) {
    // TODO BEFORE MERGE APPLY result?.newSessionState !!!!
    return transientCanvasStateForSession(
      result.newSessionState,
      result.transientFilesState,
      result.editorStatePatch,
    )
  } else {
    // no strategy was active, return empty result
    return transientCanvasStateForSession(canvasSessionState, null, null)
  }
}
