import { getLayoutProperty } from '../../../../core/layout/getLayoutProperty'
import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import { foldEither, isLeft, right } from '../../../../core/shared/either'
import * as EP from '../../../../core/shared/element-path'
import {
  ElementInstanceMetadataMap,
  isJSXElement,
  JSXElement,
} from '../../../../core/shared/element-template'
import { rectanglesEqual } from '../../../../core/shared/math-utils'
import { memoize } from '../../../../core/shared/memoize'
import { ElementPath } from '../../../../core/shared/project-file-types'
import { assertNever } from '../../../../core/shared/utils'
import { isFeatureEnabled } from '../../../../utils/feature-switches'
import { CSSCursor } from '../../canvas-types'
import { highlightElementsCommand } from '../../commands/highlight-element-command'
import { setCursorCommand } from '../../commands/set-cursor-command'
import { isZeroSizedElement } from '../../controls/outline-utils'
import {
  existingStrategies,
  getApplicableStrategiesOrderedByFitness,
  MetaCanvasStrategy,
} from '../canvas-strategies'
import {
  CanvasStrategy,
  CanvasStrategyId,
  CustomStrategyState,
  defaultCustomStrategyState,
  InteractionCanvasState,
  InteractionTarget,
} from '../canvas-strategy-types'
import { InteractionSession } from '../interaction-state'

export const lookForApplicableParentStrategy: MetaCanvasStrategy = (
  canvasSate,
  interactionSession,
  customStrategyState,
) => {
  if (interactionSession == null) {
    return []
  }

  const result = lookForApplicableParentStrategyInner(
    canvasSate,
    interactionSession,
    customStrategyState,
  )

  if (result == null || result.strategies.length < 1) {
    return []
  }

  return [tweakStrategy(result.strategies[0], result.componentsInSubtree)]
}

function tweakStrategy(
  strategy: CanvasStrategy,
  componentsInSubtree: Array<ElementPath>,
): CanvasStrategy {
  const { controlsToRender } = strategy

  const fitness: CanvasStrategy['fitness'] = 1

  const apply: CanvasStrategy['apply'] = (strategyLifecycle) => {
    const result = strategy.apply(strategyLifecycle)
    return {
      ...result,
      commands: [
        ...result.commands,
        highlightElementsCommand(componentsInSubtree),
        setCursorCommand('mid-interaction', CSSCursor.MovingMagic),
      ],
    }
  }

  const name: CanvasStrategy['name'] = strategy.name + ' *'

  const id: CanvasStrategyId = 'LOOK_FOR_APPLICABLE_PARENT_ID'

  return { apply, name, fitness, id, controlsToRender }
}

function* elementAncestry(path: ElementPath) {
  let currentParentPath = EP.parentPath(path)
  const componentsInSubtree: Array<ElementPath> = [path, currentParentPath]
  while (!EP.isStoryboardChild(currentParentPath)) {
    yield {
      root: currentParentPath,
      componentsInSubtree,
    }
    const parent = EP.parentPath(currentParentPath)
    componentsInSubtree.push(parent)
    currentParentPath = parent
  }
}

function isSingletonAbsoluteMove(strategies: Array<CanvasStrategy>): boolean {
  return strategies.length === 1 && strategies[0].id === 'CONVERT_TO_ABSOLUTE_AND_MOVE_STRATEGY'
}

function patchCanvasStateInteractionTargetPath(
  canvasState: InteractionCanvasState,
  path: ElementPath[],
): InteractionCanvasState {
  return {
    ...canvasState,
    interactionTarget: {
      type: 'TARGET_PATHS',
      elements: path,
    },
  }
}

function lookForApplicableParentStrategyInner(
  canvasState: InteractionCanvasState,
  interactionSession: InteractionSession,
  customStrategyState: CustomStrategyState,
): ParentApplicableStrategyResult | null {
  if (interactionSession.interactionData.type !== 'DRAG') {
    return null
  }

  const sortedStrategies = getApplicableStrategiesOrderedByFitness(
    [existingStrategies],
    canvasState,
    interactionSession,
    defaultCustomStrategyState(), // TODO possibly replace with real strategy state?
  ).map((s) => s.strategy)

  if (
    !isParentFindingStrategyApplicable(
      sortedStrategies,
      pathsFromInteractionTarget(canvasState.interactionTarget),
      canvasState.startingMetadata,
    )
  ) {
    return null
  }

  const result = isApplicableTraverseMemo(canvasState, interactionSession)

  if (result == null || result.strategies.length < 1) {
    return null
  }

  return result
}

function isParentFindingStrategyApplicable(
  applicableStrategies: Array<CanvasStrategy>,
  interactionTarget: Array<ElementPath>,
  metadata: ElementInstanceMetadataMap,
): boolean {
  if (interactionTarget.length !== 1) {
    return false
  }

  const singletonAbsoluteMove = isSingletonAbsoluteMove(applicableStrategies)
  const parentContiguousFlexParent =
    isFeatureEnabled('Single child, contiguous parent: move parent') &&
    isParentContiguous(interactionTarget[0], metadata)

  const parentZeroSized =
    isFeatureEnabled('Single child, zero sized parent: move parent') &&
    isParentZeroSized(interactionTarget[0], metadata)

  return singletonAbsoluteMove || parentContiguousFlexParent || parentZeroSized
}

interface ParentApplicableStrategyResult {
  strategies: Array<CanvasStrategy>
  effectiveTarget: Array<ElementPath>
  componentsInSubtree: Array<ElementPath>
}

function isApplicableTraverse(
  canvasState: InteractionCanvasState,
  interactionSession: InteractionSession,
): ParentApplicableStrategyResult | null {
  const customStrategyState = defaultCustomStrategyState() // TODO I'm using the default state here to not change behavior, but I _think_ this should use the real customStrategyState instead

  if (
    canvasState.interactionTarget.type !== 'TARGET_PATHS' ||
    canvasState.interactionTarget.elements.length !== 1
  ) {
    const sortedStrategies = getApplicableStrategiesOrderedByFitness(
      [existingStrategies],
      canvasState,
      interactionSession,
      customStrategyState,
    ).map((s) => s.strategy)
    return {
      strategies: sortedStrategies,
      effectiveTarget: pathsFromInteractionTarget(canvasState.interactionTarget),
      componentsInSubtree: [],
    }
  }

  for (const { root, componentsInSubtree } of elementAncestry(
    canvasState.interactionTarget.elements[0],
  )) {
    const patchedCanvasState = patchCanvasStateInteractionTargetPath(canvasState, [root])

    const sortedStrategies = getApplicableStrategiesOrderedByFitness(
      [existingStrategies],
      patchedCanvasState,
      interactionSession,
      customStrategyState,
    ).map((s) => s.strategy)

    if (!isSingletonAbsoluteMove(sortedStrategies)) {
      return {
        strategies: sortedStrategies,
        effectiveTarget: [root],
        componentsInSubtree,
      }
    }
  }

  return null
}

function pathsFromInteractionTarget(interactionTarget: InteractionTarget): Array<ElementPath> {
  switch (interactionTarget.type) {
    case 'TARGET_PATHS':
      return interactionTarget.elements
    case 'INSERTION_SUBJECTS':
      return []
    default:
      assertNever(interactionTarget)
  }
}

const isApplicableTraverseMemo = memoize(isApplicableTraverse)

function isParentContiguous(
  elementPath: ElementPath,
  metadata: ElementInstanceMetadataMap,
): boolean {
  const childElement = MetadataUtils.findElementByElementPath(metadata, elementPath)
  if (childElement == null) {
    return false
  }

  const parentPath = EP.parentPath(elementPath)
  const parent = MetadataUtils.findElementByElementPath(metadata, parentPath)

  if (parent == null || EP.isStoryboardChild(parentPath)) {
    return false
  }

  if (parent.globalFrame == null || childElement.globalFrame == null) {
    return true
  }

  return rectanglesEqual(parent.globalFrame, childElement.globalFrame)
}

function isParentZeroSized(
  elementPath: ElementPath,
  metadata: ElementInstanceMetadataMap,
): boolean {
  const parentPath = EP.parentPath(elementPath)
  const parent = MetadataUtils.findElementByElementPath(metadata, parentPath)
  if (parent == null) {
    return false
  }

  const parentExplicitlySized = foldEither(
    (_) => false,
    (e) => isJSXElement(e) && isElementExplicitlySized(e),
    parent.element,
  )
  if (!parentExplicitlySized) {
    return true
  }

  return parent.globalFrame != null && isZeroSizedElement(parent.globalFrame)
}

function isElementExplicitlySized(element: JSXElement): boolean {
  const width = getLayoutProperty('width', right(element.props), ['style'])
  const height = getLayoutProperty('height', right(element.props), ['style'])
  if (isLeft(width) || isLeft(height)) {
    return false
  }

  if (width.value == null || height.value == null) {
    return false
  }

  return true
}
