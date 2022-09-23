import type { MetaCanvasStrategy } from './canvas-strategies'
import {
  absoluteReparentStrategy,
  forcedAbsoluteReparentStrategy,
} from './absolute-reparent-strategy'
import { absoluteReparentToFlexStrategy } from './absolute-reparent-to-flex-strategy'
import {
  flexReparentToAbsoluteStrategy,
  forcedFlexReparentToAbsoluteStrategy,
} from './flex-reparent-to-absolute-strategy'
import { flexReparentToFlexStrategy } from './flex-reparent-to-flex-strategy'
import { CanvasStrategy } from './canvas-strategy-types'
import { findReparentStrategy } from './reparent-strategy-helpers'

export const reparentMetaStrategy: MetaCanvasStrategy = (
  canvasState,
  interactionSession,
  startingMetadata,
  startingAllElementProps,
) => {
  if (interactionSession == null) {
    return []
  }
  const strictReparentStrategy: Array<CanvasStrategy> = (() => {
    const findStrategyResult = findReparentStrategy(
      canvasState,
      interactionSession,
      startingMetadata,
      startingAllElementProps,
      'use-strict-bounds', // TODO cater for missing bounds case
    )
    if (findStrategyResult.strategy === 'ABSOLUTE_REPARENT_TO_ABSOLUTE') {
      return [absoluteReparentStrategy]
    } else if (findStrategyResult.strategy === 'FLEX_REPARENT_TO_ABSOLUTE') {
      return [flexReparentToAbsoluteStrategy]
    } else if (findStrategyResult.strategy === 'ABSOLUTE_REPARENT_TO_FLEX') {
      return [absoluteReparentToFlexStrategy]
    } else if (findStrategyResult.strategy === 'FLEX_REPARENT_TO_FLEX') {
      return [flexReparentToFlexStrategy]
    } else if (findStrategyResult.strategy === 'do-not-reparent') {
      return []
    } else {
      throw new Error(`Unhandled strategy ${findStrategyResult.strategy}`)
    }
  })()

  const forcedReparentStrategy: Array<CanvasStrategy> = (() => {
    const findStrategyResult = findReparentStrategy(
      canvasState,
      interactionSession,
      startingMetadata,
      startingAllElementProps,
      'allow-missing-bounds',
    )
    if (findStrategyResult.strategy === 'ABSOLUTE_REPARENT_TO_ABSOLUTE') {
      return [forcedAbsoluteReparentStrategy]
    } else if (findStrategyResult.strategy === 'FLEX_REPARENT_TO_ABSOLUTE') {
      return [forcedFlexReparentToAbsoluteStrategy]
    } else {
      // TODO cater for flex here?
      return []
    }
  })()
  return [...strictReparentStrategy, ...forcedReparentStrategy]
}
