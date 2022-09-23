import { isJSXElement, JSXElement } from '../../../core/shared/element-template'
import { getLayoutProperty } from '../../../core/layout/getLayoutProperty'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { foldEither, isLeft, right } from '../../../core/shared/either'
import * as EP from '../../../core/shared/element-path'
import { ElementInstanceMetadataMap } from '../../../core/shared/element-template'
import { memoize } from '../../../core/shared/memoize'
import { ElementPath } from '../../../core/shared/project-file-types'
import { assertNever } from '../../../core/shared/utils'
import { AllElementProps } from '../../editor/store/editor-state'
import { CSSCursor } from '../canvas-types'
import { highlightElementsCommand } from '../commands/highlight-element-command'
import { setCursorCommand } from '../commands/set-cursor-command'
import { ParentBounds } from '../controls/parent-bounds'
import { ParentOutlines } from '../controls/parent-outlines'
import { DragOutlineControl } from '../controls/select-mode/drag-outline-control'
import {
  getApplicableStrategiesOrderedByFitness,
  RegisteredCanvasStrategies,
} from './canvas-strategies'
import {
  CanvasStrategy,
  emptyStrategyApplicationResult,
  InteractionCanvasState,
  InteractionTarget,
  strategyApplicationResult,
} from './canvas-strategy-types'
import { createEmptyStrategyState, InteractionSession, StrategyState } from './interaction-state'
import { isFeatureEnabled } from '../../../utils/feature-switches'
import { isZeroSizedElement } from '../controls/outline-utils'
import { rectanglesEqual } from '../../../core/shared/math-utils'

export const lookForApplicableParentStrategy: CanvasStrategy = {
  id: 'LOOK_FOR_APPLICABLE_PARENT_ID',
  name: (canvasState, interactionSession, strategyState) => {
    const defaultName = 'Applicable parent'
    if (interactionSession == null) {
      return defaultName
    }

    const result = lookForParentApplicableStrategy(
      canvasState,
      interactionSession,
      strategyState.startingMetadata,
      strategyState.startingAllElementProps,
    )

    if (result == null || result.strategies.length === 0) {
      return defaultName
    }

    const patchedCanvasState = patchCanvasStateInteractionTargetPath(
      canvasState,
      result.effectiveTarget,
    )

    const fittestStrategy = calcFittestStrategy(
      result.strategies,
      patchedCanvasState,
      interactionSession,
      strategyState,
    )

    return fittestStrategy.name(canvasState, interactionSession, strategyState) + '*'
  },
  controlsToRender: [
    {
      control: DragOutlineControl,
      key: 'ghost-outline-control',
      show: 'visible-only-while-active',
    },
    {
      control: ParentOutlines,
      key: 'parent-outlines-control',
      show: 'visible-only-while-active',
    },
    {
      control: ParentBounds,
      key: 'parent-bounds-control',
      show: 'visible-only-while-active',
    },
  ],
  isApplicable: (canvasState, interactionSession, metadata, allElementProps) => {
    if (interactionSession == null || interactionSession.activeControl.type !== 'BOUNDING_AREA') {
      return false
    }

    const strategies = lookForParentApplicableStrategy(
      canvasState,
      interactionSession,
      metadata,
      allElementProps,
    )
    return strategies != null
  },

  fitness: (canvasState, interactionSession, strategyState) => {
    return lookForApplicableParentStrategy.isApplicable(
      canvasState,
      interactionSession,
      strategyState.startingMetadata,
      strategyState.startingAllElementProps,
    )
      ? 1
      : 0
  },

  apply: (canvasState, interactionSession, strategyState) => {
    const result = lookForParentApplicableStrategy(
      canvasState,
      interactionSession,
      strategyState.startingMetadata,
      strategyState.startingAllElementProps,
    )

    if (result == null) {
      return emptyStrategyApplicationResult
    }

    const { strategies, effectiveTarget, componentsInSubtree } = result
    if (strategies.length < 1) {
      return emptyStrategyApplicationResult
    }

    const patchedCanvasState = patchCanvasStateInteractionTargetPath(canvasState, effectiveTarget)

    const chosenStrategy = calcFittestStrategy(
      result.strategies,
      patchedCanvasState,
      interactionSession,
      strategyState,
    )

    const chosenStrategyApplicationResult = chosenStrategy.apply(
      patchedCanvasState,
      interactionSession,
      strategyState,
    )

    return strategyApplicationResult(
      [
        ...chosenStrategyApplicationResult.commands,
        highlightElementsCommand(componentsInSubtree),
        setCursorCommand('mid-interaction', CSSCursor.MovingMagic),
      ],
      chosenStrategyApplicationResult.customStatePatch,
      chosenStrategyApplicationResult.status,
    )
  },
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

function lookForParentApplicableStrategy(
  canvasState: InteractionCanvasState,
  interactionSession: InteractionSession,
  metadata: ElementInstanceMetadataMap,
  allElementProps: AllElementProps,
): ParentApplicableStrategyResult | null {
  if (interactionSession.interactionData.type !== 'DRAG') {
    return null
  }

  const strategiesMinusTraverse = RegisteredCanvasStrategies.filter(
    ({ id }) => id !== 'LOOK_FOR_APPLICABLE_PARENT_ID',
  )

  const applicableStrategies = getApplicableStrategiesOrderedByFitness(
    strategiesMinusTraverse,
    canvasState,
    interactionSession,
    createEmptyStrategyState(metadata, allElementProps),
  ).map((s) => s.strategy)

  if (
    !isParentFindingStrategyApplicable(
      applicableStrategies,
      pathsFromInteractionTarget(canvasState.interactionTarget),
      metadata,
      allElementProps,
    )
  ) {
    return null
  }

  const result = isApplicableTraverseMemo(
    strategiesMinusTraverse,
    canvasState,
    interactionSession,
    metadata,
    allElementProps,
  )

  if (result == null || result.strategies.length < 1) {
    return null
  }

  return result
}

function isParentFindingStrategyApplicable(
  applicableStrategies: Array<CanvasStrategy>,
  interactionTarget: Array<ElementPath>,
  metadata: ElementInstanceMetadataMap,
  allElementProps: AllElementProps,
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
  strategies: Array<CanvasStrategy>,
  canvasState: InteractionCanvasState,
  interactionSession: InteractionSession,
  metadata: ElementInstanceMetadataMap,
  allElementProps: AllElementProps,
): ParentApplicableStrategyResult | null {
  const strategyState = createEmptyStrategyState(metadata, allElementProps)
  if (
    canvasState.interactionTarget.type !== 'TARGET_PATHS' ||
    canvasState.interactionTarget.elements.length !== 1
  ) {
    const applicableStrategies = getApplicableStrategiesOrderedByFitness(
      strategies,
      canvasState,
      interactionSession,
      strategyState,
    ).map((s) => s.strategy)
    return {
      strategies: applicableStrategies,
      effectiveTarget: pathsFromInteractionTarget(canvasState.interactionTarget),
      componentsInSubtree: [],
    }
  }

  for (const { root: path, componentsInSubtree } of elementAncestry(
    canvasState.interactionTarget.elements[0],
  )) {
    const patchedCanvasState = patchCanvasStateInteractionTargetPath(canvasState, [path])
    const applicableStrategies = getApplicableStrategiesOrderedByFitness(
      strategies,
      patchedCanvasState,
      interactionSession,
      strategyState,
    ).map((s) => s.strategy)

    if (!isSingletonAbsoluteMove(applicableStrategies)) {
      return {
        strategies: applicableStrategies,
        effectiveTarget: [path],
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

function calcFittestStrategy(
  strategies: Array<CanvasStrategy>,
  canvasState: InteractionCanvasState,
  interactionSession: InteractionSession,
  strategyState: StrategyState,
): CanvasStrategy {
  return getApplicableStrategiesOrderedByFitness(
    strategies,
    canvasState,
    interactionSession,
    strategyState,
  )[0].strategy
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
