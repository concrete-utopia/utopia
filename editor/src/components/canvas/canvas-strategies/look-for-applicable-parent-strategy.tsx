import { ElementPath } from '../../../core/shared/project-file-types'
import { CSSCursor } from '../canvas-types'
import { highlightElementsCommand } from '../commands/highlight-element-command'
import { setCursorCommand } from '../commands/set-cursor-command'
import { MetaCanvasStrategy } from './canvas-strategies'
import { CanvasStrategy, CanvasStrategyId } from './canvas-strategy-types'
import {
  lookForParentApplicableStrategy,
  patchCanvasStateInteractionTargetPath,
} from './look-for-applicable-parent-strategy-helpers'

export const lookForApplicableParentStrategy: MetaCanvasStrategy = (
  canvasSate,
  interactionSession,
  metadata,
  allElementProps,
) => {
  if (interactionSession == null) {
    return []
  }

  const result = lookForParentApplicableStrategy(
    canvasSate,
    interactionSession,
    metadata,
    allElementProps,
  )

  if (result == null || result.strategies.length < 1) {
    return []
  }

  return [tweakStrategy(result.strategies[0], result.effectiveTarget, result.componentsInSubtree)]
}

function tweakStrategy(
  strategy: CanvasStrategy,
  effectiveTarget: Array<ElementPath>,
  componentsInSubtree: Array<ElementPath>,
): CanvasStrategy {
  const { controlsToRender } = strategy

  const isApplicable: CanvasStrategy['isApplicable'] = () => true

  const fitness: CanvasStrategy['fitness'] = () => 1

  const apply: CanvasStrategy['apply'] = (c, i, s, l) => {
    const patchedCanvasState = patchCanvasStateInteractionTargetPath(c, effectiveTarget)
    const result = strategy.apply(patchedCanvasState, i, s, l)
    return {
      ...result,
      commands: [
        ...result.commands,
        highlightElementsCommand(componentsInSubtree),
        setCursorCommand('mid-interaction', CSSCursor.MovingMagic),
      ],
    }
  }

  const name: CanvasStrategy['name'] = (c, i, s) => strategy.name(c, i, s) + ' *'

  const id: CanvasStrategyId = 'LOOK_FOR_APPLICABLE_PARENT_ID'

  return { apply, name, fitness, id, isApplicable, controlsToRender }
}
