import React from 'react'
import { mapDropNulls, pushUniquelyBy, sortBy } from '../../../core/shared/array-utils'
import type { ElementInstanceMetadataMap } from '../../../core/shared/element-template'
import { arrayEqualsByReference, assertNever } from '../../../core/shared/utils'
import type {
  AllElementProps,
  EditorState,
  EditorStatePatch,
  EditorStorePatched,
  GridIdentifier,
} from '../../editor/store/editor-state'
import { Substores, useEditorState, useSelectorWithCallback } from '../../editor/store/store-hook'
import type {
  CanvasStrategy,
  CanvasStrategyId,
  ControlWithProps,
  InteractionCanvasState,
  InteractionTarget,
  StrategyApplicationResult,
  InteractionLifecycle,
  CustomStrategyState,
  WhenToShowControl,
} from './canvas-strategy-types'
import {
  ControlDelay,
  insertionSubjects,
  targetPaths,
  controlWithProps,
  getTargetPathsFromInteractionTarget,
} from './canvas-strategy-types'
import type { CanvasControlType, InteractionSession, StrategyState } from './interaction-state'
import { isNotYetStartedDragInteraction } from './interaction-state'
import { keyboardAbsoluteMoveStrategy } from './strategies/keyboard-absolute-move-strategy'
import { absoluteResizeBoundingBoxStrategy } from './strategies/absolute-resize-bounding-box-strategy'
import { keyboardAbsoluteResizeStrategy } from './strategies/keyboard-absolute-resize-strategy'
import {
  convertToAbsoluteAndMoveAndSetParentFixedStrategy,
  convertToAbsoluteAndMoveStrategy,
} from './strategies/convert-to-absolute-and-move-strategy'
import { absoluteDuplicateStrategy } from './strategies/absolute-duplicate-strategy'
import type { BuiltInDependencies } from '../../../core/es-modules/package-manager/built-in-dependencies-list'
import type { StateSelector } from 'zustand'
import { reorderSliderStategy } from './strategies/reorder-slider-strategy'
import { NonResizableControl } from '../controls/select-mode/non-resizable-control'
import { flexResizeBasicStrategy } from './strategies/flex-resize-basic-strategy'
import { optionalMap } from '../../../core/shared/optional-utils'
import { setPaddingStrategy } from './strategies/set-padding-strategy'
import { drawToInsertMetaStrategy } from './strategies/draw-to-insert-metastrategy'
import { dragToInsertMetaStrategy } from './strategies/drag-to-insert-metastrategy'
import {
  DoNothingStrategyID,
  doNothingStrategy,
  dragToMoveMetaStrategy,
} from './strategies/drag-to-move-metastrategy'
import { ancestorMetaStrategy } from './strategies/ancestor-metastrategy'
import { keyboardReorderStrategy } from './strategies/keyboard-reorder-strategy'
import { setFlexGapStrategy } from './strategies/set-flex-gap-strategy'
import { setBorderRadiusStrategy } from './strategies/set-border-radius-strategy'
import { flattenSelection } from './strategies/shared-move-strategies-helpers'
import * as EP from '../../../core/shared/element-path'
import { keyboardSetFontSizeStrategy } from './strategies/keyboard-set-font-size-strategy'
import { keyboardSetFontWeightStrategy } from './strategies/keyboard-set-font-weight-strategy'
import { keyboardSetOpacityStrategy } from './strategies/keyboard-set-opacity-strategy'
import { drawToInsertTextMetaStrategy } from './strategies/draw-to-insert-text-strategy'
import { flexResizeStrategy } from './strategies/flex-resize-strategy'
import { basicResizeStrategy } from './strategies/basic-resize-strategy'
import type { InsertionSubject, InsertionSubjectWrapper } from '../../editor/editor-modes'
import { generateUidWithExistingComponents } from '../../../core/model/element-template-utils'
import { retargetStrategyToChildrenOfFragmentLikeElements } from './strategies/fragment-like-helpers'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { gridChangeElementLocationStrategy } from './strategies/grid-change-element-location-strategy'
import { resizeGridStrategy } from './strategies/resize-grid-strategy'
import { gridResizeElementStrategy } from './strategies/grid-resize-element-strategy'
import { gridResizeElementRulerStrategy } from './strategies/grid-resize-element-ruler-strategy'
import { gridChangeElementLocationDuplicateStrategy } from './strategies/grid-change-element-location-duplicate-strategy'
import { setGridGapStrategy } from './strategies/set-grid-gap-strategy'
import type { CanvasCommand } from '../commands/commands'
import { foldAndApplyCommandsInner } from '../commands/commands'
import { updateFunctionCommand } from '../commands/update-function-command'
import { wrapInContainerCommand } from '../commands/wrap-in-container-command'
import type { ElementPath } from 'utopia-shared/src/types'
import { reparentSubjectsForInteractionTarget } from './strategies/reparent-helpers/reparent-strategy-helpers'
import { getReparentTargetUnified } from './strategies/reparent-helpers/reparent-strategy-parent-lookup'
import { gridChangeElementLocationResizeKeyboardStrategy } from './strategies/grid-change-element-location-keyboard-strategy'
import createCachedSelector from 're-reselect'
import { getActivePlugin, patchRemovedProperties } from '../plugins/style-plugins'
import {
  controlsForGridPlaceholders,
  GridControls,
  isGridControlsProps,
} from '../controls/grid-controls-for-strategies'
import { gridReorderStrategy } from './strategies/grid-reorder-strategy'
import { gridMoveAbsoluteStrategy } from './strategies/grid-move-absolute'

export type CanvasStrategyFactory = (
  canvasState: InteractionCanvasState,
  interactionSession: InteractionSession | null,
  customStrategyState: CustomStrategyState,
) => CanvasStrategy | null

export type MetaCanvasStrategy = (
  canvasState: InteractionCanvasState,
  interactionSession: InteractionSession | null,
  customStrategyState: CustomStrategyState,
) => Array<CanvasStrategy>

const moveOrReorderStrategies: MetaCanvasStrategy = (
  canvasState: InteractionCanvasState,
  interactionSession: InteractionSession | null,
  customStrategyState: CustomStrategyState,
): Array<CanvasStrategy> => {
  return mapDropNulls(
    (factory) => factory(canvasState, interactionSession, customStrategyState),
    [
      absoluteDuplicateStrategy,
      keyboardAbsoluteMoveStrategy,
      keyboardReorderStrategy,
      convertToAbsoluteAndMoveStrategy,
      convertToAbsoluteAndMoveAndSetParentFixedStrategy,
      reorderSliderStategy,
      gridChangeElementLocationStrategy,
      gridChangeElementLocationDuplicateStrategy,
      gridReorderStrategy,
      gridChangeElementLocationResizeKeyboardStrategy,
      gridMoveAbsoluteStrategy,
    ],
  )
}

const resizeStrategies: MetaCanvasStrategy = (
  canvasState: InteractionCanvasState,
  interactionSession: InteractionSession | null,
  customStrategyState: CustomStrategyState,
): Array<CanvasStrategy> => {
  return mapDropNulls(
    (factory) => factory(canvasState, interactionSession, customStrategyState),
    [
      keyboardAbsoluteResizeStrategy,
      absoluteResizeBoundingBoxStrategy,
      flexResizeBasicStrategy,
      flexResizeStrategy,
      basicResizeStrategy,
      resizeGridStrategy,
      gridResizeElementStrategy,
      gridResizeElementRulerStrategy,
    ],
  )
}

const propertyControlStrategies: MetaCanvasStrategy = (
  canvasState: InteractionCanvasState,
  interactionSession: InteractionSession | null,
  customStrategyState: CustomStrategyState,
): Array<CanvasStrategy> => {
  return mapDropNulls(
    (factory) => factory(canvasState, interactionSession, customStrategyState),
    [setPaddingStrategy, setFlexGapStrategy, setGridGapStrategy, setBorderRadiusStrategy],
  )
}

const preventOnRootElements: (metaStrategy: MetaCanvasStrategy) => MetaCanvasStrategy = (
  metaStrategy: MetaCanvasStrategy,
) => {
  return (
    canvasState: InteractionCanvasState,
    interactionSession: InteractionSession | null,
    customStrategyState: CustomStrategyState,
  ): Array<CanvasStrategy> => {
    const selectedElements = flattenSelection(
      getTargetPathsFromInteractionTarget(canvasState.interactionTarget),
    )

    if (selectedElements.length === 0 || selectedElements.some(EP.isRootElementOfInstance)) {
      return []
    }

    return metaStrategy(canvasState, interactionSession, customStrategyState)
  }
}

const preventAllOnRootElements = (metaStrategies: Array<MetaCanvasStrategy>) =>
  metaStrategies.map(preventOnRootElements)

const AncestorCompatibleStrategies: Array<MetaCanvasStrategy> = preventAllOnRootElements([
  moveOrReorderStrategies,
  dragToMoveMetaStrategy,
])

const keyboardShortcutStrategies: MetaCanvasStrategy = (
  canvasState: InteractionCanvasState,
  interactionSession: InteractionSession | null,
  customStrategyState: CustomStrategyState,
): Array<CanvasStrategy> => {
  return mapDropNulls(
    (factory) => factory(canvasState, interactionSession),
    [keyboardSetFontSizeStrategy, keyboardSetFontWeightStrategy, keyboardSetOpacityStrategy],
  )
}

export const RegisteredCanvasStrategies: Array<MetaCanvasStrategy> = [
  ...AncestorCompatibleStrategies,
  preventOnRootElements(resizeStrategies),
  propertyControlStrategies,
  drawToInsertMetaStrategy,
  dragToInsertMetaStrategy,
  ancestorMetaStrategy(AncestorCompatibleStrategies, 1),
  keyboardShortcutStrategies,
  drawToInsertTextMetaStrategy,
]

export function pickCanvasStateFromEditorState(
  localSelectedViews: Array<ElementPath>,
  editorState: EditorState,
  builtInDependencies: BuiltInDependencies,
): InteractionCanvasState {
  const activePlugin = getActivePlugin(editorState)
  return {
    builtInDependencies: builtInDependencies,
    interactionTarget: getInteractionTargetFromEditorState(editorState, localSelectedViews),
    projectContents: editorState.projectContents,
    nodeModules: editorState.nodeModules.files,
    openFile: editorState.canvas.openFile?.filename,
    scale: editorState.canvas.scale,
    canvasOffset: editorState.canvas.roundedCanvasOffset,
    startingMetadata: editorState.jsxMetadata,
    startingElementPathTree: editorState.elementPathTree,
    startingAllElementProps: editorState.allElementProps,
    propertyControlsInfo: editorState.propertyControlsInfo,
    styleInfoReader: activePlugin.styleInfoFactory({
      projectContents: editorState.projectContents,
    }),
  }
}

export function pickCanvasStateFromEditorStateWithMetadata(
  localSelectedViews: Array<ElementPath>,
  editorState: EditorState,
  builtInDependencies: BuiltInDependencies,
  metadata: ElementInstanceMetadataMap,
  allElementProps?: AllElementProps,
): InteractionCanvasState {
  const activePlugin = getActivePlugin(editorState)

  return {
    builtInDependencies: builtInDependencies,
    interactionTarget: getInteractionTargetFromEditorState(editorState, localSelectedViews),
    projectContents: editorState.projectContents,
    nodeModules: editorState.nodeModules.files,
    openFile: editorState.canvas.openFile?.filename,
    scale: editorState.canvas.scale,
    canvasOffset: editorState.canvas.roundedCanvasOffset,
    startingMetadata: metadata,
    startingElementPathTree: editorState.elementPathTree, // IMPORTANT! This isn't based on the passed in metadata
    startingAllElementProps: allElementProps ?? editorState.allElementProps,
    propertyControlsInfo: editorState.propertyControlsInfo,
    styleInfoReader: activePlugin.styleInfoFactory({
      projectContents: editorState.projectContents,
    }),
  }
}

function getInteractionTargetFromEditorState(
  editor: EditorState,
  localSelectedViews: Array<ElementPath>,
): InteractionTarget {
  switch (editor.mode.type) {
    case 'insert':
      return insertionSubjects(editor.mode.subjects)
    case 'live':
    case 'select':
    case 'textEdit':
    case 'comment':
    case 'follow':
      return targetPaths(localSelectedViews)
    default:
      assertNever(editor.mode)
  }
}

export interface ApplicableStrategy {
  strategy: CanvasStrategy
  name: string
}

export function applicableStrategy(strategy: CanvasStrategy, name: string): ApplicableStrategy {
  return {
    strategy: strategy,
    name: name,
  }
}

function codeElementsTargeted(canvasState: InteractionCanvasState): boolean {
  const originalTargets = flattenSelection(
    getTargetPathsFromInteractionTarget(canvasState.interactionTarget),
  )
  const retargetedTargets = retargetStrategyToChildrenOfFragmentLikeElements(canvasState).paths
  return [...originalTargets, ...retargetedTargets].some(
    (target) =>
      MetadataUtils.isExpressionOtherJavascript(target, canvasState.startingMetadata) ||
      MetadataUtils.isJSXMapExpression(target, canvasState.startingMetadata),
  )
}

export function getApplicableStrategies(
  strategies: Array<MetaCanvasStrategy>,
  canvasState: InteractionCanvasState,
  interactionSession: InteractionSession | null,
  customStrategyState: CustomStrategyState,
): Array<CanvasStrategy> {
  if (codeElementsTargeted(canvasState)) {
    return []
  }

  return strategies.flatMap((s) => s(canvasState, interactionSession, customStrategyState))
}

const getApplicableStrategiesSelector = createCachedSelector(
  (store: EditorStorePatched, _) =>
    optionalMap(
      (sas) => sas.map((s) => s.strategy),
      store.strategyState.sortedApplicableStrategies,
    ),
  (store: EditorStorePatched, localSelectedViews: Array<ElementPath>): InteractionCanvasState => {
    return pickCanvasStateFromEditorState(
      localSelectedViews,
      store.editor,
      store.builtInDependencies,
    )
  },
  (store: EditorStorePatched, _) => store.editor.canvas.interactionSession,
  (store: EditorStorePatched, _) => store.strategyState.customStrategyState,
  (
    applicableStrategiesFromStrategyState: Array<CanvasStrategy> | null,
    canvasState: InteractionCanvasState,
    interactionSession: InteractionSession | null,
    customStrategyState: CustomStrategyState,
  ): Array<CanvasStrategy> => {
    if (applicableStrategiesFromStrategyState != null) {
      return applicableStrategiesFromStrategyState
    } else {
      return getApplicableStrategies(
        RegisteredCanvasStrategies,
        canvasState,
        interactionSession,
        customStrategyState,
      )
    }
  },
)((_, localSelectedViews: Array<ElementPath>) => localSelectedViews.map(EP.toString).join(','))

function useGetApplicableStrategies(localSelectedViews: Array<ElementPath>): Array<CanvasStrategy> {
  return useEditorState(
    Substores.fullStore,
    (store) => getApplicableStrategiesSelector(store, localSelectedViews),
    'useGetApplicableStrategies',
    arrayEqualsByReference,
  )
}

export interface StrategyWithFitness {
  fitness: number
  strategy: CanvasStrategy
}

export function getApplicableStrategiesOrderedByFitness(
  strategies: Array<MetaCanvasStrategy>,
  canvasState: InteractionCanvasState,
  interactionSession: InteractionSession,
  customStrategyState: CustomStrategyState,
): Array<StrategyWithFitness> {
  const applicableStrategies = getApplicableStrategies(
    strategies,
    canvasState,
    interactionSession,
    customStrategyState,
  )

  const strategiesWithFitness = mapDropNulls((strategy) => {
    const fitness = strategy.fitness
    if (fitness <= 0) {
      return null
    } else {
      return {
        fitness: fitness,
        strategy: strategy,
      }
    }
  }, applicableStrategies)

  const sortedStrategies = sortBy(strategiesWithFitness, (l, r) => {
    // sort by fitness, descending
    return r.fitness - l.fitness
  })

  return fixDoNothingStrategies(sortedStrategies, canvasState)
}

// Special cases for the DO NOTHING strategy - it should never be a fallback strategy,
// and when there is no other applicable strategy then do nothing should always appear as a single one
function fixDoNothingStrategies(
  sortedStrategies: Array<StrategyWithFitness>,
  canvasState: InteractionCanvasState,
): Array<StrategyWithFitness> {
  const positiveFitnessStrategyExists = sortedStrategies.find(({ fitness }) => fitness > 0) != null

  const doNothing = doNothingStrategy(canvasState)

  if (!positiveFitnessStrategyExists) {
    return [
      {
        strategy: doNothing,
        fitness: doNothing.fitness,
      },
      ...sortedStrategies,
    ]
  }

  return sortedStrategies.filter(
    ({ strategy }, index) => index === 0 || strategy.id !== DoNothingStrategyID,
  )
}

function pickDefaultCanvasStrategy(
  sortedApplicableStrategies: Array<StrategyWithFitness>,
  previousStrategyId: string | null,
): { strategy: StrategyWithFitness | null; previousStrategy: StrategyWithFitness | null } {
  const currentBestStrategy = sortedApplicableStrategies[0] ?? null
  const previousStrategy =
    sortedApplicableStrategies.find((s) => s.strategy.id === previousStrategyId) ?? null
  if (previousStrategy != null && previousStrategy.fitness === currentBestStrategy.fitness) {
    return { strategy: previousStrategy, previousStrategy: previousStrategy }
  } else {
    return { strategy: currentBestStrategy, previousStrategy: previousStrategy }
  }
}

function pickStrategy(
  sortedApplicableStrategies: Array<StrategyWithFitness>,
  interactionSession: InteractionSession,
  previousStrategyId: CanvasStrategyId | null,
): { strategy: StrategyWithFitness | null; previousStrategy: StrategyWithFitness | null } {
  // FIXME Explicitly picking a strategy will prevent natural handovers that otherwise should occur

  if (interactionSession.userPreferredStrategy != null) {
    const foundStrategyByName = sortedApplicableStrategies.find(
      (s) => s.strategy.id === interactionSession.userPreferredStrategy,
    )
    const foundPreviousStrategy =
      sortedApplicableStrategies.find((s) => s.strategy.id === previousStrategyId) ?? null

    if (foundStrategyByName != null) {
      return { strategy: foundStrategyByName, previousStrategy: foundPreviousStrategy }
    }
  }
  // fall back to default strategy
  return pickDefaultCanvasStrategy(sortedApplicableStrategies, previousStrategyId)
}

export interface FindCanvasStrategyResult {
  strategy: StrategyWithFitness | null
  previousStrategy: StrategyWithFitness | null
  sortedApplicableStrategies: Array<ApplicableStrategy>
}

export function findCanvasStrategy(
  strategies: Array<MetaCanvasStrategy>,
  canvasState: InteractionCanvasState,
  interactionSession: InteractionSession,
  customStrategyState: CustomStrategyState,
  previousStrategyId: CanvasStrategyId | null,
): FindCanvasStrategyResult {
  const sortedApplicableStrategies = getApplicableStrategiesOrderedByFitness(
    strategies,
    canvasState,
    interactionSession,
    customStrategyState,
  )

  return {
    ...pickStrategy(sortedApplicableStrategies, interactionSession, previousStrategyId),
    sortedApplicableStrategies: sortedApplicableStrategies.map((s) => ({
      strategy: s.strategy,
      name: s.strategy.name,
    })),
  }
}

export function applyCanvasStrategy(
  strategy: CanvasStrategy,
  canvasState: InteractionCanvasState,
  interactionSession: InteractionSession,
  customStrategyState: CustomStrategyState,
  strategyLifecycle: InteractionLifecycle,
): StrategyApplicationResult {
  return strategy.apply(strategyLifecycle)
}

export function applyElementsToRerenderFromStrategyResultAndPatchRemovedProps(
  editorState: EditorState,
  strategyResult: StrategyApplicationResult,
): EditorState {
  return applyElementsToRerenderFromStrategyResult(
    patchRemovedProperties(editorState),
    strategyResult,
  )
}

export function applyElementsToRerenderFromStrategyResult(
  editorState: EditorState,
  strategyResult: StrategyApplicationResult,
): EditorState {
  return {
    ...editorState,
    canvas: {
      ...editorState.canvas,
      elementsToRerender: strategyResult.elementsToRerender,
    },
  }
}

export function useDelayedEditorState<T>(
  selector: StateSelector<EditorStorePatched, T | null>,
  selectorName: string,
): T | null {
  /**
   * onMouseDown selection shows canvas controls that are active when a strategy runs with a delay (double click selection in hierarchy)
   * but when a drag threshold passes before the timer ends it shows up without delay
   */

  const actualValue = React.useRef<T | null>(null)
  const [delayedValue, setDelayedValue] = React.useState<T | null>(null)
  const [timer, setTimer] = React.useState<number | null>(null)

  const setDelayedValueToActualValue = React.useCallback(() => {
    setDelayedValue(actualValue.current)
    if (timer != null) {
      window.clearTimeout(timer)
      setTimer(null)
    }
  }, [timer, setTimer, setDelayedValue])

  const callback = React.useCallback(
    ({ value: currentValue, immediate }: { value: T | null; immediate: boolean }) => {
      actualValue.current = currentValue

      const shouldDelay = !immediate && currentValue != null && delayedValue == null
      if (shouldDelay) {
        if (timer == null) {
          setTimer(
            window.setTimeout(() => {
              setDelayedValueToActualValue()
              setTimer(null)
            }, ControlDelay),
          )
        }
      } else {
        setDelayedValueToActualValue()
      }
    },
    [setDelayedValueToActualValue, delayedValue, timer, setTimer],
  )

  useSelectorWithCallback(
    Substores.fullStore,
    (store) => {
      const immediate =
        store.editor.canvas.interactionSession?.interactionData.type === 'DRAG' &&
        store.editor.canvas.interactionSession?.interactionData.hasMouseMoved

      return { value: selector(store), immediate: immediate }
    },
    callback,
    selectorName,
  )

  return delayedValue
}

export const useDelayedCurrentStrategy = () => {
  const selector = (store: EditorStorePatched) => store.strategyState.currentStrategy
  return useDelayedEditorState<CanvasStrategyId | null>(selector, 'useDelayedCurrentStrategy')
}

const notResizableControls = controlWithProps({
  control: NonResizableControl,
  props: {},
  key: 'not-resizable-control',
  show: 'visible-except-when-other-strategy-is-active',
})

export function getApplicableControls(
  currentStrategy: CanvasStrategyId | null,
  strategy: CanvasStrategy,
): Array<ControlWithProps<unknown>> {
  return strategy.controlsToRender.filter((control) => {
    return (
      control.show === 'always-visible' ||
      (control.show === 'visible-only-while-active' && strategy.id === currentStrategy) ||
      (control.show === 'visible-except-when-other-strategy-is-active' &&
        (currentStrategy == null || strategy.id === currentStrategy))
    )
  })
}

export function isResizableStrategy(canvasStrategy: CanvasStrategy): boolean {
  switch (canvasStrategy.id) {
    case 'ABSOLUTE_RESIZE_BOUNDING_BOX':
    case 'KEYBOARD_ABSOLUTE_RESIZE':
    case 'FLEX_RESIZE_BASIC':
    case 'FLEX_RESIZE':
    case 'BASIC_RESIZE':
    case 'GRID-CELL-RESIZE-STRATEGY':
      return true
    default:
      return false
  }
}

export function isKeyboardAbsoluteStrategy(currentStrategy: string | null): boolean {
  if (currentStrategy == null) {
    return false
  }
  switch (currentStrategy) {
    case 'KEYBOARD_ABSOLUTE_RESIZE':
    case 'KEYBOARD_ABSOLUTE_MOVE':
      return true
    default:
      return false
  }
}

export function isKeyboardReorderStrategy(currentStrategy: string | null): boolean {
  return currentStrategy === 'KEYBOARD_REORDER'
}

export function interactionInProgress(interactionSession: InteractionSession | null): boolean {
  if (interactionSession == null) {
    return false
  } else {
    switch (interactionSession.interactionData.type) {
      case 'DRAG':
        return (
          !isNotYetStartedDragInteraction(interactionSession.interactionData) ||
          interactionSession.interactionData.zeroDragPermitted === 'zero-drag-permitted'
        )
      case 'KEYBOARD':
      case 'HOVER':
        return true
      default:
        const _exhaustiveCheck: never = interactionSession.interactionData
        throw new Error(`Unhandled interaction data type: ${interactionSession.interactionData}`)
    }
  }
}

function controlPriorityToNumber(prio: ControlWithProps<any>['priority']): number {
  switch (prio) {
    case 'bottom':
      return 0
    case undefined:
      return 1
    case 'top':
      return 2
  }
}

export function combineApplicableControls(
  strategyControls: Array<ControlWithProps<unknown>>,
): Array<ControlWithProps<unknown>> {
  // Separate out the instances of `GridControls`.
  let result: Array<ControlWithProps<unknown>> = []
  let gridControlsInstances: Array<ControlWithProps<unknown>> = []
  for (const control of strategyControls) {
    if (control.control === GridControls) {
      gridControlsInstances.push(control)
    } else {
      result.push(control)
    }
  }

  // Sift the instances of `GridControls`, storing their targets by when they should be shown.
  let gridControlsTargets: Map<WhenToShowControl, Array<GridIdentifier>> = new Map()
  for (const control of gridControlsInstances) {
    if (isGridControlsProps(control.props)) {
      let possibleTargets = gridControlsTargets.get(control.show)
      if (possibleTargets == null) {
        gridControlsTargets.set(control.show, control.props.targets)
      } else {
        possibleTargets.push(...control.props.targets)
      }
    }
  }

  // Create new instances of `GridControls` with the combined targets.
  for (const [show, targets] of gridControlsTargets) {
    result.push(controlsForGridPlaceholders(targets, show, `-${show}`))
  }

  // Return the newly created controls with the combined entries.
  return result
}

const controlEquals = (l: ControlWithProps<any>, r: ControlWithProps<any>) => {
  return l.control === r.control && l.key === r.key
}

export function useGetApplicableStrategyControls(localSelectedViews: Array<ElementPath>): {
  bottomStrategyControls: Array<ControlWithProps<unknown>>
  middleStrategyControls: Array<ControlWithProps<unknown>>
  topStrategyControls: Array<ControlWithProps<unknown>>
} {
  const applicableStrategies = useGetApplicableStrategies(localSelectedViews)
  const currentStrategy = useDelayedCurrentStrategy()
  const currentlyInProgress = useEditorState(
    Substores.canvas,
    (store) => {
      return interactionInProgress(store.editor.canvas.interactionSession)
    },
    'useGetApplicableStrategyControls currentlyInProgress',
  )
  return React.useMemo(() => {
    let strategyControls: Array<ControlWithProps<unknown>> = []
    let isResizable: boolean = false
    // Add the controls for currently applicable strategies.
    for (const strategy of applicableStrategies) {
      if (isResizableStrategy(strategy)) {
        isResizable = true
      }
      strategyControls.push(...getApplicableControls(currentStrategy, strategy))
    }
    const combinedControls = combineApplicableControls(strategyControls)
    const bottomStrategyControls: Array<ControlWithProps<unknown>> = []
    const middleStrategyControls: Array<ControlWithProps<unknown>> = []
    const topStrategyControls: Array<ControlWithProps<unknown>> = []

    // uniquely add the strategyControls to the bottom, middle, and top arrays
    for (const control of combinedControls) {
      switch (control.priority) {
        case 'bottom':
          pushUniquelyBy(bottomStrategyControls, control, controlEquals)
          break
        case undefined:
          pushUniquelyBy(middleStrategyControls, control, controlEquals)
          break
        case 'top':
          pushUniquelyBy(topStrategyControls, control, controlEquals)
          break
        default:
          assertNever(control.priority)
      }
    }

    // Special case controls.
    if (!isResizable && !currentlyInProgress) {
      middleStrategyControls.push(notResizableControls)
    }

    return {
      bottomStrategyControls: bottomStrategyControls,
      middleStrategyControls: middleStrategyControls,
      topStrategyControls: topStrategyControls,
    }
  }, [applicableStrategies, currentStrategy, currentlyInProgress])
}

export function isStrategyActive(strategyState: StrategyState): boolean {
  return strategyState.currentStrategyCommands.length > 0
}

export function onlyFitWhenThisControlIsActive(
  interactionSession: InteractionSession | null,
  controlType: CanvasControlType['type'],
  fitnessWhenFit: number,
): number {
  if (interactionSession != null && interactionSession.activeControl.type === controlType) {
    return fitnessWhenFit
  } else {
    return 0
  }
}

export function onlyFitWhenDraggingThisControl(
  interactionSession: InteractionSession | null,
  controlType: CanvasControlType['type'],
  fitnessWhenFit: number,
): number {
  if (
    interactionSession != null &&
    interactionSession.interactionData.type === 'DRAG' &&
    interactionSession.activeControl.type === controlType
  ) {
    return fitnessWhenFit
  } else {
    return 0
  }
}

export interface WrapperWithUid {
  wrapper: InsertionSubjectWrapper
  uid: string
}

export function getWrapperWithGeneratedUid(
  customStrategyState: CustomStrategyState,
  canvasState: InteractionCanvasState,
  subjects: Array<InsertionSubject>,
): WrapperWithUid | null {
  const insertionSubjectWrapper = subjects.at(0)?.insertionSubjectWrapper ?? null
  if (insertionSubjectWrapper == null) {
    return null
  }

  const uid =
    customStrategyState.strategyGeneratedUidsCache[subjects[0].uid] ??
    generateUidWithExistingComponents(canvasState.projectContents)

  return { wrapper: insertionSubjectWrapper, uid: uid }
}

export function getWrappingCommands(
  wrappedElementPath: ElementPath,
  wrapperWithUid: WrapperWithUid,
): CanvasCommand[] {
  return [
    updateFunctionCommand(
      'always',
      (editorState, lifecycle): Array<EditorStatePatch> =>
        foldAndApplyCommandsInner(
          editorState,
          [],
          [
            wrapInContainerCommand(
              'always',
              wrappedElementPath,
              wrapperWithUid.uid,
              wrapperWithUid.wrapper,
            ),
          ],
          lifecycle,
        ).statePatches,
    ),
  ]
}

export function findElementPathUnderInteractionPoint(
  canvasState: InteractionCanvasState,
  interactionSession: InteractionSession | null,
): ElementPath | null {
  const reparentSubjects = reparentSubjectsForInteractionTarget(canvasState.interactionTarget)

  if (interactionSession == null || interactionSession.interactionData.type === 'KEYBOARD') {
    return null
  }

  const { interactionData } = interactionSession

  const pointOnCanvas =
    interactionData.type === 'DRAG' ? interactionData.originalDragStart : interactionData.point

  const targetParent = getReparentTargetUnified(
    reparentSubjects,
    pointOnCanvas,
    true, // cmd is necessary to allow reparenting,
    canvasState,
    canvasState.startingMetadata,
    canvasState.startingElementPathTree,
    canvasState.startingAllElementProps,
    'allow-smaller-parent',
    ['supportsChildren'],
    canvasState.propertyControlsInfo,
  )?.newParent.intendedParentPath

  return targetParent ?? null
}

export function getDescriptiveStrategyLabelWithRetargetedPaths(
  originalLabel: string,
  pathsWereReplaced: boolean,
): string {
  if (pathsWereReplaced) {
    return `${originalLabel} (Children)`
  }
  return originalLabel
}

function isOnlyDoNothingStrategy(strategies: Array<ApplicableStrategy>): boolean {
  // This is an optimization, we should check all strategies, but we know we can not have do_nothing strategy in non-zero position
  if (strategies.length > 1) {
    return false
  }
  if (strategies.length === 0) {
    return true
  }
  return strategies[0].strategy.id === 'DO_NOTHING'
}

export function useIsOnlyDoNothingStrategy(): boolean {
  return useEditorState(
    Substores.restOfStore,
    (store) => isOnlyDoNothingStrategy(store.strategyState.sortedApplicableStrategies ?? []),
    'useIsOnlyDoNothingStrategy',
  )
}
