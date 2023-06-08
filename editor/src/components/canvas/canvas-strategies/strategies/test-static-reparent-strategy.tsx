import { CanvasStrategy, emptyStrategyApplicationResult } from '../canvas-strategy-types'
import { InteractionSession } from '../interaction-state'

export const TestStaticReparentStrategy = (
  interactionSession: InteractionSession,
): CanvasStrategy | null => {
  if (interactionSession.interactionData.type !== 'STATIC_REPARENT') {
    return null
  }

  return {
    id: 'test-static-reparent-strategy',
    name: 'Test Static Reparent Strategy',
    fitness: 1,
    controlsToRender: [],
    apply: () => emptyStrategyApplicationResult,
  }
}
