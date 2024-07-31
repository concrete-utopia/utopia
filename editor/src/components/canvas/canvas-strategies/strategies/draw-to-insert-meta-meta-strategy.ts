import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import { stripNulls } from '../../../../core/shared/array-utils'
import { findElementPathUnderInteractionPoint, type MetaCanvasStrategy } from '../canvas-strategies'
import type { CustomStrategyState, InteractionCanvasState } from '../canvas-strategy-types'
import type { InteractionSession } from '../interaction-state'
import { drawToInsertMetaStrategy } from './draw-to-insert-metastrategy'
import { gridDrawToInsertStrategy } from './grid-draw-to-insert-strategy'

export const drawToInsertMetaMetaStrategy: MetaCanvasStrategy = (
  canvasState: InteractionCanvasState,
  interactionSession: InteractionSession | null,
  customStrategyState: CustomStrategyState,
) => {
  const targetParent = findElementPathUnderInteractionPoint(canvasState, interactionSession)

  if (
    MetadataUtils.isGridLayoutedContainer(
      MetadataUtils.findElementByElementPath(canvasState.startingMetadata, targetParent),
    )
  ) {
    return stripNulls([
      gridDrawToInsertStrategy(canvasState, interactionSession, customStrategyState),
    ])
  }

  return drawToInsertMetaStrategy(canvasState, interactionSession, customStrategyState)
}
