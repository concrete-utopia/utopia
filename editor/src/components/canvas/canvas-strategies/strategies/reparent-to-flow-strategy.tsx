import { CanvasStrategyFactory } from '../canvas-strategies'
import { baseFlexReparentToFlexStrategy } from './flex-reparent-to-flex-strategy'
import { ReparentTarget } from './reparent-strategy-helpers'

export function reparentToFlowPlaceholderStrategy(
  reparentTarget: ReparentTarget,
  fitness: number,
): CanvasStrategyFactory {
  return baseFlexReparentToFlexStrategy(reparentTarget, fitness)
}
