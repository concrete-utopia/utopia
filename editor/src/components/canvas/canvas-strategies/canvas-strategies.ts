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

interface StrategiesWithFitness {
  fitness: number
  strategy: CanvasStrategy
}

function getApplicableStrategies(
  canvasState: CanvasState,
  interactionState: InteractionState,
): Array<StrategiesWithFitness> {
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

  return sortBy(strategiesWithFitness, (l, r) => {
    // sort by fitness, descending
    return r.fitness - l.fitness
  })
}

export function pickDefaultCanvasStrategy(
  applicableStrategies: Array<StrategiesWithFitness>,
): CanvasStrategy | null {
  return applicableStrategies[0]?.strategy ?? null
}

function pickStrategy(
  applicableStrategies: Array<StrategiesWithFitness>,
  interactionState: InteractionState,
): CanvasStrategy | null {
  if (interactionState.userPreferredStrategy != null) {
    const foundStrategyByName = applicableStrategies.find(
      (s) => s.strategy.name === interactionState.userPreferredStrategy,
    )
    if (foundStrategyByName != null) {
      return foundStrategyByName.strategy
    }
  }
  // fall back to default strategy
  return pickDefaultCanvasStrategy(applicableStrategies)
}

export function applyCanvasStrategy(
  lifecycle: TransientOrNot,
  editorState: EditorState,
  canvasState: CanvasState,
  interactionState: InteractionState,
): TransientCanvasState | null {
  const applicableStrategies = getApplicableStrategies(canvasState, interactionState)
  const strategy = pickStrategy(applicableStrategies, interactionState)

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
