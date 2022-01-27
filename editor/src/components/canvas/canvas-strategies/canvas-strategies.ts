import { sortBy } from '../../../core/shared/array-utils'
import {
  EditorState,
  transientCanvasState,
  TransientCanvasState,
  transientCanvasStateForSession,
} from '../../editor/store/editor-state'
import { foldCommands, TransientOrNot } from '../commands/commands'
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
  // Compute the fitness results upfront.
  const strategiesWithFitness = RegisteredCanvasStrategies.map((strategy) => {
    return {
      fitness: strategy.fitnessFn(editorState, sessionProps, sessionState),
      strategy: strategy,
    }
  })

  // Filter out those which have declared (by virtue of a null result) that they
  // cannot be candidates.
  let filteredStrategies: Array<{ fitness: number; strategy: CanvasStrategy }> = []
  for (const strategyWithFitness of strategiesWithFitness) {
    if (strategyWithFitness.fitness != null) {
      filteredStrategies.push({
        fitness: strategyWithFitness.fitness,
        strategy: strategyWithFitness.strategy,
      })
    }
  }

  sortBy(filteredStrategies, (l, r) => {
    // Sort by fitness, descending.
    return r.fitness - l.fitness
  })
  return filteredStrategies[0]?.strategy ?? null
}

export function applyCanvasStrategy(
  lifecycle: TransientOrNot,
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
    strategy?.updateFn?.(editorState, canvasSession.sessionProps, sessionStateWithStrategy) ?? null

  if (result == null) {
    // no strategy was active, return empty result
    return transientCanvasStateForSession(canvasSessionState, [])
  } else {
    const commandArray = Array.isArray(result) ? result : [result]
    const commandsResults = foldCommands(
      editorState,
      sessionStateWithStrategy,
      commandArray,
      lifecycle,
    )
    return transientCanvasStateForSession(sessionStateWithStrategy, commandsResults.statePatches)
  }
}
