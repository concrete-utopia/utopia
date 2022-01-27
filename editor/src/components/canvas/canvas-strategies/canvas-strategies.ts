import { sortBy } from '../../../core/shared/array-utils'
import {
  EditorState,
  transientCanvasState,
  TransientCanvasState,
  transientCanvasStateForSession,
} from '../../editor/store/editor-state'
import { foldCommands } from '../commands/commands'
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
  const applicableStrategies = RegisteredCanvasStrategies.map((s) => ({
    fitness: s.fitnessFn(editorState, sessionProps, sessionState),
    strategy: s,
  })).filter((s) => {
    // discard strategies with 0 fitness
    return s.fitness > 0
  })

  return (
    sortBy(applicableStrategies, (l, r) => {
      // sort by fitness, descending
      return r.fitness - l.fitness
    })[0]?.strategy ?? null
  )
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
    return transientCanvasStateForSession(canvasSessionState, [])
  } else {
    const commandArray = Array.isArray(result) ? result : [result]
    const commandsResults = foldCommands(editorState, sessionStateWithStrategy, commandArray)
    return transientCanvasStateForSession(sessionStateWithStrategy, commandsResults.statePatches)
  }
}
