import { ElementPath } from '../../../core/shared/project-file-types'
import { CSSCursor } from '../canvas-types'
import { highlightElementsCommand } from '../commands/highlight-element-command'
import { setCursorCommand } from '../commands/set-cursor-command'
import { MetaCanvasStrategy } from './canvas-strategies'
import { CanvasStrategy } from './canvas-strategy-types'
import {
  lookForParentApplicableStrategy,
  patchCanvasStateInteractionTargetPath,
} from './look-for-applicable-parent'

export const lookForApplicableParentMeta: MetaCanvasStrategy = (
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

  if (result == null) {
    return []
  }

  return result.strategies.map((s) =>
    tweakStrategy(s, result.effectiveTarget, result.componentsInSubtree),
  )
}

function tweakStrategy(
  strategy: CanvasStrategy,
  effectiveTarget: Array<ElementPath>,
  componentsInSubtree: Array<ElementPath>,
): CanvasStrategy {
  const { controlsToRender } = strategy

  const isApplicable: CanvasStrategy['isApplicable'] = (c, i, m, a) => {
    const patchedCanvasState = patchCanvasStateInteractionTargetPath(c, effectiveTarget)
    return strategy.isApplicable(patchedCanvasState, i, m, a)
  }

  const fitness: CanvasStrategy['fitness'] = (c, i, s) => {
    const patchedCanvasState = patchCanvasStateInteractionTargetPath(c, effectiveTarget)
    return strategy.fitness(patchedCanvasState, i, s)
  }

  const apply: CanvasStrategy['apply'] = (c, i, s) => {
    const patchedCanvasState = patchCanvasStateInteractionTargetPath(c, effectiveTarget)
    const result = strategy.apply(patchedCanvasState, i, s)
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

  const id: string = 'LOOK_FOR_APPLICABLE_PARENT_ID' + '_' + strategy.id

  return { apply, name, fitness, id, isApplicable, controlsToRender }
}
