import { sortBy } from '../../../core/shared/array-utils'
import { CanvasStrategy, CanvasState, InteractionState } from '../../../interactions_proposal'
import {
  EditorState,
  transientCanvasState,
  TransientCanvasState,
  transientCanvasStateForSession,
} from '../../editor/store/editor-state'
import { foldCommands, TransientOrNot } from '../commands/commands'
import {
  emptySelectModeCanvasSessionState,
  SelectModeCanvasSession,
  SelectModeCanvasSessionProps,
  SelectModeCanvasSessionState,
} from './canvas-strategy-types'
import { flexGapStrategy } from './flex-gap-strategy'

const RegisteredCanvasStrategies: Array<CanvasStrategy> = [flexGapStrategy] //, flexAlignParentStrategy]

export function pickDefaultCanvasStrategy(
  canvasState: CanvasState,
  interactionState: InteractionState,
): CanvasStrategy | null {
  const applicableStrategies = RegisteredCanvasStrategies.filter((strategy) => {
    return strategy.isApplicable(canvasState, interactionState)
  })

  // Compute the fitness results upfront.
  const strategiesWithFitness = applicableStrategies.map((strategy) => {
    return {
      fitness: strategy.fitness(canvasState, interactionState),
      strategy: strategy,
    }
  })

  sortBy(strategiesWithFitness, (l, r) => {
    // Sort by fitness, descending.
    return r.fitness - l.fitness
  })
  return strategiesWithFitness[0]?.strategy ?? null
}

export function applyCanvasStrategy(
  lifecycle: TransientOrNot,
  editorState: EditorState,
  canvasState: CanvasState,
  interactionState: InteractionState,
): TransientCanvasState | null {
  const strategy = pickDefaultCanvasStrategy(canvasState, interactionState)

  const result = strategy?.apply?.(canvasState, interactionState) ?? null

  return null

  // FIXME: Some variant of this will need to be implemented.
  /*
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
  */
}
