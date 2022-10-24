import { CanvasStrategyFactory } from '../canvas-strategies'
import { ReparentTarget } from './reparent-strategy-helpers'
import { baseReparentToFlexStrategy } from './reparent-to-flex-strategy'

export function reparentToFlowPlaceholderStrategy(
  reparentTarget: ReparentTarget,
  fitness: number,
): CanvasStrategyFactory {
  return baseReparentToFlexStrategy(reparentTarget, fitness)
}
