import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import { mapDropNulls } from '../../../../core/shared/array-utils'
import { offsetPoint } from '../../../../core/shared/math-utils'
import { assertNever } from '../../../../core/shared/utils'
import { MetaCanvasStrategy } from '../canvas-strategies'
import {
  CustomStrategyState,
  getTargetPathsFromInteractionTarget,
  InteractionCanvasState,
} from '../canvas-strategy-types'
import { InteractionSession, MissingBoundsHandling } from '../interaction-state'
import { baseAbsoluteReparentStrategy } from './absolute-reparent-strategy'
import { absoluteReparentToFlexStrategy } from './absolute-reparent-to-flex-strategy'
import { baseFlexReparentToAbsoluteStrategy } from './flex-reparent-to-absolute-strategy'
import { flexReparentToFlexStrategy } from './flex-reparent-to-flex-strategy'
import { findPartialReparentStrategies } from './reparent-strategy-helpers'
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

  const reparentSubjects = getDragTargets(
    getTargetPathsFromInteractionTarget(canvasState.interactionTarget),
  )

  const allDraggedElementsAbsolute = reparentSubjects.every((element) =>
    MetadataUtils.isPositionAbsolute(
      MetadataUtils.findElementByElementPath(canvasState.startingMetadata, element),
    ),
  )

  const allDraggedElementsFlex = reparentSubjects.every((element) =>
    MetadataUtils.isParentYogaLayoutedContainerAndElementParticipatesInLayout(
      element,
      canvasState.startingMetadata,
    ),
  )

  if (!allDraggedElementsAbsolute && !allDraggedElementsFlex) {
    return []
  }

  const pointOnCanvas = offsetPoint(
    interactionSession.interactionData.originalDragStart,
    interactionSession.interactionData.drag,
  )

  const cmdPressed = interactionSession.interactionData.modifiers.cmd
  const factories = findPartialReparentStrategies(canvasState, cmdPressed, pointOnCanvas).map(
    (result) => {
      const missingBoundsHandling: MissingBoundsHandling = result.forcingRequired
        ? 'allow-missing-bounds'
        : 'use-strict-bounds'
      switch (result.strategy) {
        case 'REPARENT_TO_ABSOLUTE':
          if (allDraggedElementsAbsolute) {
            return baseAbsoluteReparentStrategy(missingBoundsHandling)
          } else {
            return baseFlexReparentToAbsoluteStrategy(missingBoundsHandling)
          }
        case 'REPARENT_TO_FLEX':
          if (allDraggedElementsAbsolute) {
            return absoluteReparentToFlexStrategy
          } else {
            return flexReparentToFlexStrategy
          }
        default:
          assertNever(result.strategy)
      }
    },
  )

  return mapDropNulls(
    (factory) => factory(canvasState, interactionSession, customStrategyState),
    factories,
  )
}
