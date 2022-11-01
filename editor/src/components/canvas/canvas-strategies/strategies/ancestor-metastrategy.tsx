import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import * as EP from '../../../../core/shared/element-path'
import { MetaCanvasStrategy } from '../canvas-strategies'
import {
  getTargetPathsFromInteractionTarget,
  InteractionCanvasState,
  targetPaths,
} from '../canvas-strategy-types'

export function ancestorMetaStrategy(
  allOtherStrategies: Array<MetaCanvasStrategy>,
  level: number,
): MetaCanvasStrategy {
  return (canvasState, interactionSession, customStrategyState) => {
    // Don't apply during insertion
    if (canvasState.interactionTarget.type === 'INSERTION_SUBJECTS') {
      return []
    }

    const targets = getTargetPathsFromInteractionTarget(canvasState.interactionTarget)

    if (targets.length !== 1) {
      return []
    }

    const target = targets[0]

    // Avoid children of the storyboard
    if (
      EP.isEmptyPath(targets[0]) ||
      EP.isStoryboardPath(targets[0]) ||
      EP.isStoryboardChild(targets[0])
    ) {
      // TODO Maybe avoid root elements?
      return []
    }

    // Is the selected element an only child?
    const siblings = MetadataUtils.getSiblings(canvasState.startingMetadata, target)
    if (siblings.length > 1) {
      return []
    }

    // Is the selected element a flow layout element?
    const targetMetadata = MetadataUtils.findElementByElementPath(
      canvasState.startingMetadata,
      target,
    )
    const isStaticLayout = !(
      MetadataUtils.isPositionAbsolute(targetMetadata) ||
      MetadataUtils.isPositionRelative(targetMetadata) ||
      MetadataUtils.isParentYogaLayoutedContainerForElementAndElementParticipatesInLayout(
        targetMetadata,
      )
    )

    if (!isStaticLayout) {
      return []
    }

    // Time to offer up available strategies for the parent
    const parentPath = EP.parentPath(target)
    const adjustedCanvasState: InteractionCanvasState = {
      ...canvasState,
      interactionTarget: targetPaths([parentPath]),
    }

    // Avoid a cyclic dependency by explicitly passing the other metastrategies when creating the next layer's meta strategy
    const nextAncestorResult = ancestorMetaStrategy(allOtherStrategies, level + 1)(
      adjustedCanvasState,
      interactionSession,
      customStrategyState,
    )

    if (nextAncestorResult.length > 0) {
      return nextAncestorResult
    } else {
      return allOtherStrategies.flatMap((metaStrategy) =>
        metaStrategy(adjustedCanvasState, interactionSession, customStrategyState).map((s) => ({
          ...s,
          id: `${s.id}_${level}`,
          name: applyLevelSuffix(s.name, level),
          fitness: s.fitness > 0 ? s.fitness + 10 : s.fitness, // Ancestor strategies should always take priority
        })),
      )
    }
  }
}

function applyLevelSuffix(name: string, level: number): string {
  // FIXME What should we use for the label here?
  const newSuffix = `(Up ${level})`
  const oldSuffixIndex = name.indexOf(' (Up')
  const withoutOldSuffix = oldSuffixIndex > 0 ? name.slice(0, oldSuffixIndex) : name
  return `${withoutOldSuffix} ${newSuffix}`
}
