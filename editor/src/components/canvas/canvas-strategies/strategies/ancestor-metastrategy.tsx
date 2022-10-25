import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import { MetaCanvasStrategy } from '../canvas-strategies'
import {
  getTargetPathsFromInteractionTarget,
  InteractionCanvasState,
  targetPaths,
} from '../canvas-strategy-types'
import * as EP from '../../../../core/shared/element-path'
import { ElementInstanceMetadataMap } from '../../../../core/shared/element-template'
import { ElementPath } from '../../../../core/shared/project-file-types'
import {
  canvasRectangle,
  CanvasRectangle,
  rectanglesEqual,
} from '../../../../core/shared/math-utils'

function getPaddingAdjustedFrame(
  metadata: ElementInstanceMetadataMap,
  targetPath: ElementPath,
): CanvasRectangle | null {
  const targetMetadata = MetadataUtils.findElementByElementPath(metadata, targetPath)
  const targetBoundingFrame = targetMetadata?.globalFrame ?? null
  const targetPadding = targetMetadata?.specialSizeMeasurements.padding

  if (targetBoundingFrame == null || targetPadding == null) {
    return targetBoundingFrame
  } else {
    const { left, right, top, bottom } = targetPadding
    const l = left ?? 0
    const r = right ?? 0
    const t = top ?? 0
    const b = bottom ?? 0
    return canvasRectangle({
      x: targetBoundingFrame.x + l,
      y: targetBoundingFrame.y + t,
      width: targetBoundingFrame.width - (l + r),
      height: targetBoundingFrame.height - (t + b),
    })
  }
}

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

    // Avoid children of the storyboard or root elements
    // TODO Maybe don't avoid root elements??
    if (EP.isStoryboardChild(targets[0])) {
      //} || EP.isRootElementOfInstance(targets[0])) {
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

    // Does the bounding frame match the parent's (offsetting based on the padding)
    // FIXME This doesn't work, as flow elements with no styling will fill either the width or height
    // const boundingFrame = targetMetadata?.globalFrame
    const parentPath = EP.parentPath(target)
    // const paddingAdjustedParentFrame = getPaddingAdjustedFrame(
    //   canvasState.startingMetadata,
    //   parentPath,
    // )
    // if (
    //   boundingFrame == null ||
    //   paddingAdjustedParentFrame == null ||
    //   !rectanglesEqual(boundingFrame, paddingAdjustedParentFrame)
    // ) {
    //   return []
    // }

    // Time to offer up available strategies for the parent
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
          fitness: s.fitness > 0 ? s.fitness + 1 : s.fitness,
        })),
      )
    }
  }
}

function applyLevelSuffix(name: string, level: number): string {
  const newSuffix = `(Up ${level})`
  const oldSuffixIndex = name.indexOf(' (Up')
  const withoutOldSuffix = oldSuffixIndex > 0 ? name.slice(0, oldSuffixIndex) : name
  return `${withoutOldSuffix} ${newSuffix}`
}
