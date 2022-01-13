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
  SelectModeCanvasSessionProps,
  SelectModeCanvasSessionState,
} from './canvas-strategy-types'
import { flexAlignParentStrategy } from './flex-align-parent-strategy'
import { flexGapStrategy } from './flex-gap-strategy'

const RegisteredCanvasStrategies: Array<CanvasStrategy> = [flexGapStrategy, flexAlignParentStrategy]

export function pickDefaultCanvasStrategy(
  editorState: EditorState,
  sessionProps: SelectModeCanvasSessionProps,
  sessionState: SelectModeCanvasSessionState,
): CanvasStrategy | null {
  sortBy(RegisteredCanvasStrategies, (l, r) => {
    // sort by fitness, descending
    return (
      r.fitnessFn(editorState, sessionProps, sessionState) -
      l.fitnessFn(editorState, sessionProps, sessionState)
    )
  })
  return RegisteredCanvasStrategies[0] ?? null
}

export function applyCanvasStrategy(
  lifecycle: 'transient' | 'final',
  editorState: EditorState,
  canvasSession: SelectModeCanvasSession,
  canvasSessionState: SelectModeCanvasSessionState | null,
): TransientCanvasState | null {
  const sessionStateToUse = canvasSessionState ?? emptySelectModeCanvasSessionState

  const strategy = pickDefaultCanvasStrategy(
    editorState,
    canvasSession.sessionProps,
    sessionStateToUse,
  )
  const sessionStateWithStrategy: SelectModeCanvasSessionState = {
    ...sessionStateToUse,
    activeStrategy: strategy,
  }

  const result =
    strategy?.updateFn?.(
      lifecycle,
      editorState,
      canvasSession.sessionProps,
      sessionStateWithStrategy,
    ) ?? null

  if (result == null) {
    // no strategy was active, return empty result
    return transientCanvasStateForSession(canvasSessionState, null, null)
  } else {
    return transientCanvasStateForSession(
      result.newSessionState,
      result.transientFilesState,
      result.editorStatePatch,
    )
  }
}
