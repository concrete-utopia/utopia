import { sortBy } from '../../../core/shared/array-utils'
import { CanvasState, CanvasStrategy, InteractionState } from '../../../interactions_proposal'
import { CanvasCommand } from '../commands/commands'
import { flexAlignParentStrategy } from './flex-align-parent-strategy'
import { flexGapStrategy } from './flex-gap-strategy'

const RegisteredCanvasStrategies: Array<CanvasStrategy> = [flexGapStrategy, flexAlignParentStrategy]

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
  canvasState: CanvasState,
  interactionState: InteractionState,
): { commands: Array<CanvasCommand>; strategy: string | null } {
  const applicableStrategies = getApplicableStrategies(canvasState, interactionState)
  const strategy = pickStrategy(applicableStrategies, interactionState)
  if (strategy == null) {
    return {
      commands: [],
      strategy: null,
    }
  } else {
    const commands = strategy.apply(canvasState, interactionState)
    return {
      commands: commands,
      strategy: strategy.name,
    }
  }
}
