import { stripNulls } from '../../../core/shared/array-utils'
import { offsetPoint } from '../../../core/shared/math-utils'
import { assertNever } from '../../../core/shared/utils'
import {
  forcedAbsoluteReparentStrategy,
  absoluteReparentStrategy,
} from './absolute-reparent-strategy'
import { absoluteReparentToFlexStrategy } from './absolute-reparent-to-flex-strategy'
import { MetaCanvasStrategy } from './canvas-strategies'
import {
  InteractionCanvasState,
  CustomStrategyState,
  getTargetPathsFromInteractionTarget,
  CanvasStrategy,
} from './canvas-strategy-types'
import {
  forcedFlexReparentToAbsoluteStrategy,
  flexReparentToAbsoluteStrategy,
} from './flex-reparent-to-absolute-strategy'
import { flexReparentToFlexStrategy } from './flex-reparent-to-flex-strategy'
import { InteractionSession } from './interaction-state'
import { existingReparentSubjects, findReparentStrategies } from './reparent-strategy-helpers'
import { getDragTargets } from './shared-move-strategies-helpers'

export const reparentMetaStrategy: MetaCanvasStrategy = (
  canvasState: InteractionCanvasState,
  interactionSession: InteractionSession | null,
  customStrategyState: CustomStrategyState,
) => {
  if (
    interactionSession == null ||
    interactionSession.interactionData.type !== 'DRAG' ||
    interactionSession.interactionData.drag == null
  ) {
    return []
  }
  const pointOnCanvas = offsetPoint(
    interactionSession.interactionData.originalDragStart,
    interactionSession.interactionData.drag,
  )
  return stripNulls(
    findReparentStrategies(canvasState, pointOnCanvas).map((result): CanvasStrategy | null => {
      switch (result.strategy) {
        case 'do-not-reparent':
          return null
        case 'ABSOLUTE_REPARENT_TO_ABSOLUTE':
          return result.forcingRequired
            ? forcedAbsoluteReparentStrategy(canvasState, interactionSession)
            : absoluteReparentStrategy(canvasState, interactionSession)
        case 'FLEX_REPARENT_TO_ABSOLUTE':
          return result.forcingRequired
            ? forcedFlexReparentToAbsoluteStrategy(
                canvasState,
                interactionSession,
                customStrategyState,
              )
            : flexReparentToAbsoluteStrategy(canvasState, interactionSession, customStrategyState)
        case 'ABSOLUTE_REPARENT_TO_FLEX':
          return absoluteReparentToFlexStrategy(canvasState, interactionSession)
        case 'FLEX_REPARENT_TO_FLEX':
          return flexReparentToFlexStrategy(canvasState, interactionSession)
        default:
          assertNever(result)
      }
    }),
  )
}
