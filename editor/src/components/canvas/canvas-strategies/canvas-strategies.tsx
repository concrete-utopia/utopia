import React from 'react'
import { createSelector } from 'reselect'
import { addAllUniquelyBy, mapDropNulls, sortBy } from '../../../core/shared/array-utils'
import type { ElementInstanceMetadataMap } from '../../../core/shared/element-template'
import { arrayEqualsByReference, assertNever } from '../../../core/shared/utils'
import {
  getJSXElementFromProjectContents,
  type AllElementProps,
  type EditorState,
  type EditorStatePatch,
  type EditorStorePatched,
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
  UIFrameworkPlugin,
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
import { gridRearrangeMoveStrategy } from './strategies/grid-rearrange-move-strategy'
import { resizeGridStrategy } from './strategies/resize-grid-strategy'
import { rearrangeGridSwapStrategy } from './strategies/rearrange-grid-swap-strategy'
import { gridResizeElementStrategy } from './strategies/grid-resize-element-strategy'
import { gridRearrangeMoveDuplicateStrategy } from './strategies/grid-rearrange-move-duplicate-strategy'
import { setGridGapStrategy } from './strategies/set-grid-gap-strategy'
import type { CanvasCommand } from '../commands/commands'
import {
  foldAndApplyCommandsInner,
  foldAndApplyCommandsSimple,
  runCanvasCommand,
} from '../commands/commands'
import { updateFunctionCommand } from '../commands/update-function-command'
import { wrapInContainerCommand } from '../commands/wrap-in-container-command'
import type {
  ElementPath,
  JSXAttributesEntry,
  ProjectContentsTree,
  PropertyPath,
} from 'utopia-shared/src/types'
import { reparentSubjectsForInteractionTarget } from './strategies/reparent-helpers/reparent-strategy-helpers'
import { getReparentTargetUnified } from './strategies/reparent-helpers/reparent-strategy-parent-lookup'
import { gridRearrangeResizeKeyboardStrategy } from './strategies/grid-rearrange-keyboard-strategy'
import { deleteProperties } from '../commands/delete-properties-command'
import * as PP from '../../../core/shared/property-path'
import { propertyToSet, setProperty, updateBulkProperties } from '../commands/set-property-command'

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
      gridRearrangeMoveStrategy,
      rearrangeGridSwapStrategy,
      gridRearrangeMoveDuplicateStrategy,
      gridRearrangeResizeKeyboardStrategy,
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
  editorState: EditorState,
  builtInDependencies: BuiltInDependencies,
): InteractionCanvasState {
  return {
    builtInDependencies: builtInDependencies,
    interactionTarget: getInteractionTargetFromEditorState(editorState),
    projectContents: editorState.projectContents,
    nodeModules: editorState.nodeModules.files,
    openFile: editorState.canvas.openFile?.filename,
    scale: editorState.canvas.scale,
    canvasOffset: editorState.canvas.roundedCanvasOffset,
    startingMetadata: editorState.jsxMetadata,
    startingElementPathTree: editorState.elementPathTree,
    startingAllElementProps: editorState.allElementProps,
    propertyControlsInfo: editorState.propertyControlsInfo,
  }
}

export function pickCanvasStateFromEditorStateWithMetadata(
  editorState: EditorState,
  builtInDependencies: BuiltInDependencies,
  metadata: ElementInstanceMetadataMap,
  allElementProps?: AllElementProps,
): InteractionCanvasState {
  return {
    builtInDependencies: builtInDependencies,
    interactionTarget: getInteractionTargetFromEditorState(editorState),
    projectContents: editorState.projectContents,
    nodeModules: editorState.nodeModules.files,
    openFile: editorState.canvas.openFile?.filename,
    scale: editorState.canvas.scale,
    canvasOffset: editorState.canvas.roundedCanvasOffset,
    startingMetadata: metadata,
    startingElementPathTree: editorState.elementPathTree, // IMPORTANT! This isn't based on the passed in metadata
    startingAllElementProps: allElementProps ?? editorState.allElementProps,
    propertyControlsInfo: editorState.propertyControlsInfo,
  }
}

function getInteractionTargetFromEditorState(editor: EditorState): InteractionTarget {
  switch (editor.mode.type) {
    case 'insert':
      return insertionSubjects(editor.mode.subjects)
    case 'live':
    case 'select':
    case 'textEdit':
    case 'comment':
    case 'follow':
      return targetPaths(editor.selectedViews)
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

const getApplicableStrategiesSelector = createSelector(
  (store: EditorStorePatched) =>
    optionalMap(
      (sas) => sas.map((s) => s.strategy),
      store.strategyState.sortedApplicableStrategies,
    ),
  (store: EditorStorePatched): InteractionCanvasState => {
    return pickCanvasStateFromEditorState(store.editor, store.builtInDependencies)
  },
  (store: EditorStorePatched) => store.editor.canvas.interactionSession,
  (store: EditorStorePatched) => store.strategyState.customStrategyState,
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
)

function useGetApplicableStrategies(): Array<CanvasStrategy> {
  return useEditorState(
    Substores.fullStore,
    getApplicableStrategiesSelector,
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

export function useGetApplicableStrategyControls(): Array<ControlWithProps<unknown>> {
  const applicableStrategies = useGetApplicableStrategies()
  const currentStrategy = useDelayedCurrentStrategy()
  const currentlyInProgress = useEditorState(
    Substores.canvas,
    (store) => {
      return interactionInProgress(store.editor.canvas.interactionSession)
    },
    'useGetApplicableStrategyControls currentlyInProgress',
  )
  return React.useMemo(() => {
    let applicableControls: Array<ControlWithProps<unknown>> = []
    let isResizable: boolean = false
    // Add the controls for currently applicable strategies.
    for (const strategy of applicableStrategies) {
      if (isResizableStrategy(strategy)) {
        isResizable = true
      }
      const strategyControls = getApplicableControls(currentStrategy, strategy)
      applicableControls = addAllUniquelyBy(
        applicableControls,
        strategyControls,
        (l, r) => l.control === r.control && l.key === r.key,
      )
    }
    // Special case controls.
    if (!isResizable && !currentlyInProgress) {
      applicableControls.push(notResizableControls)
    }

    applicableControls = applicableControls.sort(
      (a, b) => controlPriorityToNumber(a.priority) - controlPriorityToNumber(b.priority),
    )

    return applicableControls
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

const InlineStyleNormalizationPlugin: UIFrameworkPlugin = {
  id: 'INLINE_STYLE_NORMALIZATION',
  normalizeFromInlineStyles: (editorState) => {
    return editorState // nothing needs to be changed
  },
}

function getInlineStylePropsFromElement(
  projectContents: EditorState['projectContents'],
  view: ElementPath,
) {
  const element = getJSXElementFromProjectContents(view, projectContents)
  if (element == null) {
    return []
  }

  const styleAttribute = element.props.find(
    (prop) => prop.type === 'JSX_ATTRIBUTES_ENTRY' && prop.key === 'style',
  ) as JSXAttributesEntry | undefined
  if (styleAttribute == null) {
    return []
  }
  const styleValue = styleAttribute.value
  if (styleValue.type !== 'ATTRIBUTE_NESTED_OBJECT') {
    return []
  }
  const styleProps = mapDropNulls(
    (c) =>
      c.type === 'PROPERTY_ASSIGNMENT' && c.value.type === 'ATTRIBUTE_VALUE'
        ? { key: c.key, value: c.value.value }
        : null,
    styleValue.content,
  )

  return styleProps
}

const EmotionNormalizationPlugin: UIFrameworkPlugin = {
  id: 'EMOTION_NORMALIZATION',
  normalizeFromInlineStyles: (editorState: EditorState, elementsToTarget: Array<ElementPath>) => {
    const commands = elementsToTarget.flatMap((elementPath) => {
      const inlineStyleProps = getInlineStylePropsFromElement(
        editorState.projectContents,
        elementPath,
      )

      return [
        updateBulkProperties(
          'always',
          elementPath,
          inlineStyleProps.map(({ key, value }) =>
            propertyToSet(PP.create('css', '&:hover', key.toString()), value as string | number),
          ),
        ),
        deleteProperties('always', elementPath, [
          ...inlineStyleProps.map(({ key }) => PP.create('style', key)),
          PP.create('style'),
        ]),
      ]
    })

    if (commands.length === 0) {
      return editorState
    }

    return foldAndApplyCommandsSimple(editorState, commands)
  },
}

export function runNormalizationPlugins(
  editorState: EditorState,
  elementsToNormalize: Array<ElementPath>,
): EditorState {
  const plugins: Array<UIFrameworkPlugin> = [
    EmotionNormalizationPlugin,
    InlineStyleNormalizationPlugin,
  ]
  const chosenPlugin = plugins[0]
  return chosenPlugin.normalizeFromInlineStyles(editorState, elementsToNormalize)
}
