import {
  ApplicableStrategy,
  applyCanvasStrategy,
  findCanvasStrategy,
  interactionInProgress,
  MetaCanvasStrategy,
  pickCanvasStateFromEditorState,
  StrategyWithFitness,
} from '../../canvas/canvas-strategies/canvas-strategies'
import {
  createEmptyStrategyState,
  hasDragModifiersChanged,
  InteractionSession,
  interactionSessionHardReset,
  isKeyboardInteractionData,
  isNotYetStartedDragInteraction,
  KeyboardInteractionData,
  StrategyState,
} from '../../canvas/canvas-strategies/interaction-state'
import { foldAndApplyCommands } from '../../canvas/commands/commands'
import { strategySwitched } from '../../canvas/commands/strategy-switched-command'
import {
  EditorAction,
  ExecutePostActionMenuChoice as ExecutePostActionMenuChoice,
  StartPostActionSession,
} from '../action-types'
import {
  isClearInteractionSession,
  isCreateOrUpdateInteractionSession,
  isTransientAction,
  shouldApplyClearInteractionSessionResult,
} from '../actions/action-utils'
import {
  DerivedState,
  deriveState,
  EditorState,
  EditorStoreFull,
  EditorStoreUnpatched,
} from './editor-state'
import {
  CustomStrategyState,
  CustomStrategyStatePatch,
  InteractionCanvasState,
  strategyApplicationResult,
} from '../../canvas/canvas-strategies/canvas-strategy-types'
import { isFeatureEnabled } from '../../../utils/feature-switches'
import { PERFORMANCE_MARKS_ALLOWED } from '../../../common/env-vars'
import { last } from '../../../core/shared/array-utils'
import { BuiltInDependencies } from '../../../core/es-modules/package-manager/built-in-dependencies-list'
import { isInsertMode } from '../editor-modes'

interface HandleStrategiesResult {
  unpatchedEditorState: EditorState
  patchedEditorState: EditorState
  newStrategyState: StrategyState
}

export function interactionFinished(
  strategies: Array<MetaCanvasStrategy>,
  storedState: EditorStoreFull,
  result: EditorStoreUnpatched,
): HandleStrategiesResult {
  let newEditorState = result.unpatchedEditor
  const withClearedSession = createEmptyStrategyState(
    newEditorState.canvas.interactionSession?.latestMetadata ?? newEditorState.jsxMetadata,
    newEditorState.canvas.interactionSession?.latestAllElementProps ??
      newEditorState.allElementProps,
    newEditorState.canvas.interactionSession?.latestElementPathTree ??
      newEditorState.elementPathTree,
  )
  const canvasState: InteractionCanvasState = pickCanvasStateFromEditorState(
    newEditorState,
    result.builtInDependencies,
  )
  const interactionSession = storedState.unpatchedEditor.canvas.interactionSession
  if (interactionSession != null && interactionInProgress(interactionSession)) {
    // Determine the new canvas strategy to run this time around.
    const { strategy } = findCanvasStrategy(
      strategies,
      canvasState,
      interactionSession,
      result.strategyState.customStrategyState,
      result.strategyState.currentStrategy,
    )

    const strategyResult =
      strategy != null
        ? applyCanvasStrategy(
            strategy.strategy,
            canvasState,
            interactionSession,
            result.strategyState.customStrategyState,
            'end-interaction',
          )
        : {
            commands: [],
          }
    const commandResult = foldAndApplyCommands(
      newEditorState,
      storedState.patchedEditor,
      [],
      strategyResult.commands,
      'end-interaction',
    )

    const finalEditor: EditorState = {
      ...commandResult.editorState,
      // TODO instead of clearing the metadata, we should save the latest valid metadata here to save a dom-walker run
      jsxMetadata: {},
      domMetadata: {},
      spyMetadata: {},
    }

    return {
      unpatchedEditorState: finalEditor,
      patchedEditorState: finalEditor,
      newStrategyState: withClearedSession,
    }
  } else {
    // Try to keep any updated metadata that may have been populated into here
    // in the meantime.
    newEditorState = {
      ...newEditorState,
      domMetadata: storedState.patchedEditor.domMetadata,
      spyMetadata: storedState.patchedEditor.spyMetadata,
      jsxMetadata: storedState.patchedEditor.jsxMetadata,
    }
    return {
      unpatchedEditorState: newEditorState,
      patchedEditorState: newEditorState,
      newStrategyState: withClearedSession,
    }
  }
}

export function interactionHardReset(
  strategies: Array<MetaCanvasStrategy>,
  storedState: EditorStoreFull,
  result: EditorStoreUnpatched,
): HandleStrategiesResult {
  const newEditorState = result.unpatchedEditor
  const withClearedSession = {
    ...storedState.strategyState,
    startingMetadata: storedState.unpatchedEditor.jsxMetadata,
  }
  const canvasState: InteractionCanvasState = pickCanvasStateFromEditorState(
    newEditorState,
    result.builtInDependencies,
  )
  const interactionSession = newEditorState.canvas.interactionSession
  if (
    interactionSession == null ||
    isNotYetStartedDragInteraction(interactionSession.interactionData)
  ) {
    return {
      unpatchedEditorState: newEditorState,
      patchedEditorState: newEditorState,
      newStrategyState: withClearedSession,
    }
  } else {
    const resetInteractionSession = interactionSessionHardReset(interactionSession)
    const resetStrategyState = {
      ...result.strategyState,
      startingMetadata: storedState.unpatchedEditor.jsxMetadata,
    }
    // Determine the new canvas strategy to run this time around.
    const { strategy, sortedApplicableStrategies } = findCanvasStrategy(
      strategies,
      canvasState,
      resetInteractionSession,
      resetStrategyState.customStrategyState,
      resetStrategyState.currentStrategy,
    )

    // If there is a current strategy, produce the commands from it.
    if (strategy != null && newEditorState.canvas.interactionSession != null) {
      const strategyResult = applyCanvasStrategy(
        strategy.strategy,
        canvasState,
        newEditorState.canvas.interactionSession,
        resetStrategyState.customStrategyState,
        'mid-interaction',
      )
      const commandResult = foldAndApplyCommands(
        newEditorState,
        storedState.patchedEditor,
        [],
        strategyResult.commands,
        'mid-interaction',
      )
      const newStrategyState: StrategyState = {
        currentStrategy: strategy.strategy.id,
        currentStrategyFitness: strategy.fitness,
        currentStrategyCommands: strategyResult.commands,
        commandDescriptions: commandResult.commandDescriptions,
        sortedApplicableStrategies: sortedApplicableStrategies,
        status: strategyResult.status,
        startingMetadata: resetStrategyState.startingMetadata,
        customStrategyState: patchCustomStrategyState(
          result.strategyState.customStrategyState,
          strategyResult.customStatePatch,
        ),
        startingAllElementProps: resetStrategyState.startingAllElementProps,
        startingElementPathTree: newEditorState.canvas.interactionSession.latestElementPathTree,
      }

      return {
        unpatchedEditorState: newEditorState,
        patchedEditorState: commandResult.editorState,
        newStrategyState: newStrategyState,
      }
    } else {
      return {
        unpatchedEditorState: newEditorState,
        patchedEditorState: newEditorState,
        newStrategyState: withClearedSession,
      }
    }
  }
}

export function interactionUpdate(
  strategies: Array<MetaCanvasStrategy>,
  storedState: EditorStoreFull,
  result: EditorStoreUnpatched,
  actionType: 'interaction-create-or-update' | 'non-interaction',
): HandleStrategiesResult {
  const newEditorState = result.unpatchedEditor
  const canvasState: InteractionCanvasState = pickCanvasStateFromEditorState(
    newEditorState,
    result.builtInDependencies,
  )
  const interactionSession = newEditorState.canvas.interactionSession
  if (
    interactionSession == null ||
    (isNotYetStartedDragInteraction(interactionSession.interactionData) &&
      !isInsertMode(storedState.unpatchedEditor.mode))
  ) {
    return {
      unpatchedEditorState: newEditorState,
      patchedEditorState: newEditorState,
      newStrategyState: result.strategyState,
    }
  } else {
    // Determine the new canvas strategy to run this time around.
    const { strategy, previousStrategy, sortedApplicableStrategies } = findCanvasStrategy(
      strategies,
      canvasState,
      interactionSession,
      result.strategyState.customStrategyState,
      result.strategyState.currentStrategy,
    )

    if (interactionSession.userPreferredStrategy != null) {
      const userChangedStrategy =
        interactionSession.userPreferredStrategy !=
        storedState.unpatchedEditor.canvas.interactionSession?.userPreferredStrategy
      if (userChangedStrategy) {
        return handleUserChangedStrategy(
          result.builtInDependencies,
          newEditorState,
          storedState.patchedEditor,
          result.strategyState,
          strategy,
          previousStrategy,
          sortedApplicableStrategies,
        )
      }
    }

    if (
      result.unpatchedEditor.canvas.interactionSession?.interactionData.type === 'KEYBOARD' &&
      actionType === 'interaction-create-or-update' &&
      strategy?.strategy !== previousStrategy?.strategy
    ) {
      return handleAccumulatingKeypresses(
        result.builtInDependencies,
        newEditorState,
        storedState.patchedEditor,
        result.strategyState,
        strategy,
        previousStrategy,
        sortedApplicableStrategies,
      )
    }
    return handleUpdate(
      result.builtInDependencies,
      newEditorState,
      storedState.patchedEditor,
      result.strategyState,
      strategy,
      previousStrategy,
      sortedApplicableStrategies,
    )
  }
}

export function interactionStart(
  strategies: Array<MetaCanvasStrategy>,
  storedState: EditorStoreFull,
  result: EditorStoreUnpatched,
): HandleStrategiesResult {
  const newEditorState = result.unpatchedEditor
  const withClearedSession = createEmptyStrategyState(
    newEditorState.canvas.interactionSession?.latestMetadata ?? newEditorState.jsxMetadata,
    newEditorState.canvas.interactionSession?.latestAllElementProps ??
      newEditorState.allElementProps,
    newEditorState.canvas.interactionSession?.latestElementPathTree ??
      newEditorState.elementPathTree,
  )
  const canvasState: InteractionCanvasState = pickCanvasStateFromEditorState(
    newEditorState,
    result.builtInDependencies,
  )
  const interactionSession = newEditorState.canvas.interactionSession
  if (
    interactionSession == null ||
    isNotYetStartedDragInteraction(interactionSession.interactionData)
  ) {
    return {
      unpatchedEditorState: newEditorState,
      patchedEditorState: newEditorState,
      newStrategyState: withClearedSession,
    }
  } else {
    // Determine the new canvas strategy to run this time around.
    const { strategy, sortedApplicableStrategies } = findCanvasStrategy(
      strategies,
      canvasState,
      interactionSession,
      withClearedSession.customStrategyState,
      result.strategyState.currentStrategy,
    )

    // If there is a current strategy, produce the commands from it.
    if (strategy != null && newEditorState.canvas.interactionSession != null) {
      const strategyResult = applyCanvasStrategy(
        strategy.strategy,
        canvasState,
        newEditorState.canvas.interactionSession,
        withClearedSession.customStrategyState,
        'mid-interaction',
      )
      const commandResult = foldAndApplyCommands(
        newEditorState,
        storedState.patchedEditor,
        [],
        strategyResult.commands,
        'mid-interaction',
      )

      const newStrategyState: StrategyState = {
        currentStrategy: strategy.strategy.id,
        currentStrategyFitness: strategy.fitness,
        currentStrategyCommands: strategyResult.commands,
        commandDescriptions: commandResult.commandDescriptions,
        sortedApplicableStrategies: sortedApplicableStrategies,
        status: strategyResult.status,
        startingMetadata: newEditorState.canvas.interactionSession.latestMetadata,
        customStrategyState: patchCustomStrategyState(
          result.strategyState.customStrategyState,
          strategyResult.customStatePatch,
        ),
        startingAllElementProps: newEditorState.canvas.interactionSession.latestAllElementProps,
        startingElementPathTree: newEditorState.canvas.interactionSession.latestElementPathTree,
      }

      return {
        unpatchedEditorState: newEditorState,
        patchedEditorState: commandResult.editorState,
        newStrategyState: newStrategyState,
      }
    } else {
      return {
        unpatchedEditorState: newEditorState,
        patchedEditorState: newEditorState,
        newStrategyState: withClearedSession,
      }
    }
  }
}

export function interactionCancel(
  storedState: EditorStoreFull,
  result: EditorStoreUnpatched,
): HandleStrategiesResult {
  const interactionWasInProgress = interactionInProgress(
    storedState.unpatchedEditor.canvas.interactionSession,
  )
  const updatedEditorState: EditorState = {
    ...result.unpatchedEditor,
    canvas: {
      ...result.unpatchedEditor.canvas,
      interactionSession: null,
    },
    jsxMetadata: interactionWasInProgress ? {} : result.unpatchedEditor.jsxMetadata,
    domMetadata: interactionWasInProgress ? {} : result.unpatchedEditor.domMetadata,
    spyMetadata: interactionWasInProgress ? {} : result.unpatchedEditor.spyMetadata,
  }

  return {
    unpatchedEditorState: updatedEditorState,
    patchedEditorState: updatedEditorState,
    newStrategyState: createEmptyStrategyState({}, {}, {}),
  }
}

function handleUserChangedStrategy(
  builtInDependencies: BuiltInDependencies,
  newEditorState: EditorState,
  storedEditorState: EditorState,
  strategyState: StrategyState,
  strategy: StrategyWithFitness | null,
  previousStrategy: StrategyWithFitness | null,
  sortedApplicableStrategies: Array<ApplicableStrategy>,
): HandleStrategiesResult {
  const canvasState: InteractionCanvasState = pickCanvasStateFromEditorState(
    newEditorState,
    builtInDependencies,
  )

  // If there is a current strategy, produce the commands from it.
  if (strategy != null && newEditorState.canvas.interactionSession != null) {
    const strategyChangedLogCommands = [
      {
        strategy: null,
        commands: [
          strategySwitched(
            'user-input',
            strategy.strategy.name,
            true,
            previousStrategy?.fitness ?? NaN,
            strategy.fitness,
          ),
        ],
      },
    ]

    const strategyResult = applyCanvasStrategy(
      strategy.strategy,
      canvasState,
      newEditorState.canvas.interactionSession,
      strategyState.customStrategyState,
      'mid-interaction',
    )
    const commandResult = foldAndApplyCommands(
      newEditorState,
      storedEditorState,
      strategyChangedLogCommands.flatMap((c) => c.commands),
      strategyResult.commands,
      'mid-interaction',
    )
    const newStrategyState: StrategyState = {
      currentStrategy: strategy.strategy.id,
      currentStrategyFitness: strategy.fitness,
      currentStrategyCommands: strategyResult.commands,
      commandDescriptions: commandResult.commandDescriptions,
      sortedApplicableStrategies: sortedApplicableStrategies,
      status: strategyResult.status,
      startingMetadata: strategyState.startingMetadata,
      customStrategyState: patchCustomStrategyState(
        strategyState.customStrategyState,
        strategyResult.customStatePatch,
      ),
      startingAllElementProps: strategyState.startingAllElementProps,
      startingElementPathTree: newEditorState.canvas.interactionSession.latestElementPathTree,
    }

    return {
      unpatchedEditorState: newEditorState,
      patchedEditorState: commandResult.editorState,
      newStrategyState: newStrategyState,
    }
  } else {
    return {
      unpatchedEditorState: newEditorState,
      patchedEditorState: newEditorState,
      newStrategyState: strategyState,
    }
  }
}

function handleAccumulatingKeypresses(
  builtInDependencies: BuiltInDependencies,
  newEditorState: EditorState,
  storedEditorState: EditorState,
  strategyState: StrategyState,
  strategy: StrategyWithFitness | null,
  previousStrategy: StrategyWithFitness | null,
  sortedApplicableStrategies: Array<ApplicableStrategy>,
): HandleStrategiesResult {
  const canvasState: InteractionCanvasState = pickCanvasStateFromEditorState(
    newEditorState,
    builtInDependencies,
  )
  // If there is a current strategy, produce the commands from it.
  if (newEditorState.canvas.interactionSession != null) {
    const interactionData = newEditorState.canvas.interactionSession.interactionData
    if (isKeyboardInteractionData(interactionData)) {
      const lastKeyState = last(interactionData.keyStates)
      const updatedInteractionData: KeyboardInteractionData = {
        ...interactionData,
        keyStates: lastKeyState == null ? [] : [lastKeyState],
      }
      const updatedInteractionSession: InteractionSession = {
        ...newEditorState.canvas.interactionSession,
        interactionData: updatedInteractionData,
      }
      const updatedEditorState = {
        ...newEditorState,
        canvas: {
          ...newEditorState.canvas,
          interactionSession: updatedInteractionSession,
        },
      }
      const strategyResult =
        strategy != null
          ? applyCanvasStrategy(
              strategy.strategy,
              canvasState,
              updatedInteractionSession,
              strategyState.customStrategyState,
              'mid-interaction',
            )
          : strategyApplicationResult([])
      const commandResult = foldAndApplyCommands(
        updatedEditorState,
        storedEditorState,
        strategyState.currentStrategyCommands,
        strategyResult.commands,
        'mid-interaction',
      )
      const newStrategyState: StrategyState = {
        currentStrategy: strategy?.strategy.id ?? null,
        currentStrategyFitness: strategy?.fitness ?? 0,
        currentStrategyCommands: strategyResult.commands,
        commandDescriptions: commandResult.commandDescriptions,
        sortedApplicableStrategies: sortedApplicableStrategies,
        status: strategyResult.status,
        startingMetadata: strategyState.startingMetadata,
        customStrategyState: patchCustomStrategyState(
          strategyState.customStrategyState,
          strategyResult.customStatePatch,
        ),
        startingAllElementProps: strategyState.startingAllElementProps,
        startingElementPathTree: newEditorState.canvas.interactionSession.latestElementPathTree,
      }

      return {
        unpatchedEditorState: updatedEditorState,
        patchedEditorState: commandResult.editorState,
        newStrategyState: newStrategyState,
      }
    }
  }
  return {
    unpatchedEditorState: newEditorState,
    patchedEditorState: newEditorState,
    newStrategyState: strategyState,
  }
}

function handleUpdate(
  builtInDependencies: BuiltInDependencies,
  newEditorState: EditorState,
  storedEditorState: EditorState,
  strategyState: StrategyState,
  strategy: StrategyWithFitness | null,
  previousStrategy: StrategyWithFitness | null,
  sortedApplicableStrategies: Array<ApplicableStrategy>,
): HandleStrategiesResult {
  const canvasState: InteractionCanvasState = pickCanvasStateFromEditorState(
    newEditorState,
    builtInDependencies,
  )
  // If there is a current strategy, produce the commands from it.
  if (newEditorState.canvas.interactionSession != null) {
    const strategyResult =
      strategy != null
        ? applyCanvasStrategy(
            strategy.strategy,
            canvasState,
            newEditorState.canvas.interactionSession,
            strategyState.customStrategyState,
            'mid-interaction',
          )
        : strategyApplicationResult([])
    const commandResult = foldAndApplyCommands(
      newEditorState,
      storedEditorState,
      [],
      strategyResult.commands,
      'mid-interaction',
    )
    const newStrategyState: StrategyState = {
      currentStrategy: strategy?.strategy.id ?? null,
      currentStrategyFitness: strategy?.fitness ?? 0,
      currentStrategyCommands: strategyResult.commands,
      commandDescriptions: commandResult.commandDescriptions,
      sortedApplicableStrategies: sortedApplicableStrategies,
      status: strategyResult.status,
      startingMetadata: strategyState.startingMetadata,
      customStrategyState: patchCustomStrategyState(
        strategyState.customStrategyState,
        strategyResult.customStatePatch,
      ),
      startingAllElementProps: strategyState.startingAllElementProps,
      startingElementPathTree: newEditorState.canvas.interactionSession.latestElementPathTree,
    }
    return {
      unpatchedEditorState: newEditorState,
      patchedEditorState: commandResult.editorState,
      newStrategyState: newStrategyState,
    }
  } else {
    return {
      unpatchedEditorState: newEditorState,
      patchedEditorState: newEditorState,
      newStrategyState: strategyState,
    }
  }
}

export function handleStrategies(
  strategies: Array<MetaCanvasStrategy>,
  dispatchedActions: readonly EditorAction[],
  storedState: EditorStoreFull,
  result: EditorStoreUnpatched,
  oldDerivedState: DerivedState,
): HandleStrategiesResult & { patchedDerivedState: DerivedState } {
  const MeasureDispatchTime =
    (isFeatureEnabled('Debug – Performance Marks (Fast)') ||
      isFeatureEnabled('Debug – Performance Marks (Slow)')) &&
    PERFORMANCE_MARKS_ALLOWED

  if (MeasureDispatchTime) {
    window.performance.mark('strategies_begin')
  }
  const { unpatchedEditorState, patchedEditorState, newStrategyState } = handleStrategiesInner(
    strategies,
    dispatchedActions,
    storedState,
    result,
  )

  const patchedEditorWithMetadata: EditorState = {
    ...patchedEditorState,
    jsxMetadata:
      patchedEditorState.canvas.interactionSession?.latestMetadata ??
      patchedEditorState.jsxMetadata,
    elementPathTree:
      patchedEditorState.canvas.interactionSession?.latestElementPathTree ??
      patchedEditorState.elementPathTree,
  }

  if (MeasureDispatchTime) {
    window.performance.mark('strategies_derive_state')
  }

  const patchedDerivedState = deriveState(patchedEditorWithMetadata, oldDerivedState, 'patched')

  if (MeasureDispatchTime) {
    window.performance.mark('strategies_end')
    window.performance.measure(`Handle Strategies`, 'strategies_begin', 'strategies_end')
    window.performance.measure(
      'Strategies DeriveState',
      'strategies_derive_state',
      'strategies_end',
    )
  }

  return {
    unpatchedEditorState: updatePostActionState(unpatchedEditorState, dispatchedActions),
    patchedEditorState: patchedEditorWithMetadata,
    patchedDerivedState,
    newStrategyState: newStrategyState,
  }
}

function injectNewMetadataToOldEditorState(
  oldEditorState: EditorState,
  newEditorState: EditorState,
): EditorState {
  if (oldEditorState.canvas.interactionSession != null) {
    // we expect metadata to live in EditorState.canvas.interactionSession.metadata
    if (newEditorState.canvas.interactionSession == null) {
      throw new Error(
        'Dispatch error: SAVE_DOM_REPORT changed canvas.interactionSession in an illegal way',
      )
    } else {
      return {
        ...oldEditorState,
        jsxMetadata: newEditorState.jsxMetadata,
        domMetadata: newEditorState.domMetadata,
        spyMetadata: newEditorState.spyMetadata,
        canvas: {
          ...oldEditorState.canvas,
          interactionSession: {
            ...oldEditorState.canvas.interactionSession,
            latestMetadata: newEditorState.canvas.interactionSession.latestMetadata, // the fresh metadata from SAVE_DOM_REPORT
            latestElementPathTree: newEditorState.canvas.interactionSession.latestElementPathTree,
          },
        },
      }
    }
  } else if (oldEditorState.canvas.dragState != null) {
    // we expect metadata to live in EditorState.canvas.dragState.metadata
    if (newEditorState.canvas.dragState == null) {
      throw new Error('Dispatch error: SAVE_DOM_REPORT changed canvas.dragState in an illegal way')
    } else {
      return {
        ...oldEditorState,
        jsxMetadata: newEditorState.jsxMetadata,
        domMetadata: newEditorState.domMetadata,
        spyMetadata: newEditorState.spyMetadata,
        canvas: {
          ...oldEditorState.canvas,
          dragState: {
            ...oldEditorState.canvas.dragState,
            metadata: newEditorState.canvas.dragState.metadata, // the fresh metadata from SAVE_DOM_REPORT
          },
        },
      }
    }
  } else {
    return {
      ...oldEditorState, // the "old" patched editor from the action dispatch that triggered SAVE_DOM_WALKER
      jsxMetadata: newEditorState.jsxMetadata, // the fresh metadata from SAVE_DOM_REPORT
      domMetadata: newEditorState.domMetadata,
      spyMetadata: newEditorState.spyMetadata,
      elementPathTree: newEditorState.elementPathTree,
    }
  }
}

function handleStrategiesInner(
  strategies: Array<MetaCanvasStrategy>,
  dispatchedActions: readonly EditorAction[],
  storedState: EditorStoreFull,
  result: EditorStoreUnpatched,
): HandleStrategiesResult {
  const isSaveDomReport = dispatchedActions.some((a) => a.action === 'SAVE_DOM_REPORT')

  const makeChangesPermanent = dispatchedActions.some(shouldApplyClearInteractionSessionResult)
  const cancelInteraction =
    dispatchedActions.some(isClearInteractionSession) && !makeChangesPermanent
  const isInteractionAction = dispatchedActions.some(isCreateOrUpdateInteractionSession)
    ? 'interaction-create-or-update'
    : 'non-interaction'

  if (isSaveDomReport) {
    // SAVE_DOM_REPORT is a special action that is part of the dispatch flow.
    // here we do not want to re-run strategies at all, just update the jsxMetadata in the patched EditorState

    const oldPatchedEditorWithNewMetadata: EditorState = injectNewMetadataToOldEditorState(
      storedState.patchedEditor,
      result.unpatchedEditor,
    )
    return {
      unpatchedEditorState: result.unpatchedEditor, // we return the fresh unpatchedEditor, containing the up-to-date domMetadata and spyMetadata
      patchedEditorState: oldPatchedEditorWithNewMetadata, // the previous patched editor with updated metadata
      newStrategyState: storedState.strategyState,
    }
  } else if (storedState.unpatchedEditor.canvas.interactionSession == null) {
    if (result.unpatchedEditor.canvas.interactionSession == null) {
      return {
        unpatchedEditorState: result.unpatchedEditor,
        patchedEditorState: result.unpatchedEditor,
        newStrategyState: result.strategyState,
      }
    } else {
      return interactionStart(strategies, storedState, result)
    }
  } else {
    if (cancelInteraction) {
      return interactionCancel(storedState, result)
    } else if (makeChangesPermanent) {
      return interactionFinished(strategies, storedState, result)
    } else {
      const interactionHardResetNeeded = hasDragModifiersChanged(
        storedState.unpatchedEditor.canvas.interactionSession?.interactionData ?? null,
        result.unpatchedEditor.canvas.interactionSession?.interactionData ?? null,
      )
      if (interactionHardResetNeeded) {
        return interactionHardReset(strategies, storedState, result)
      } else {
        return interactionUpdate(strategies, storedState, result, isInteractionAction)
      }
    }
  }
}

function patchCustomStrategyState(
  existingState: CustomStrategyState,
  patch: CustomStrategyStatePatch,
): CustomStrategyState {
  return {
    ...existingState,
    ...patch,
  }
}

export function updatePostActionState(
  editorState: EditorState,
  actions: readonly EditorAction[],
): EditorState {
  const nonTransientActions = actions.filter((a) => !isTransientAction(a))
  const setSelectedViewsActions = actions.filter((a) => a.action === 'SELECT_COMPONENTS')
  const executePostActionMenuChoiceAction = actions.find(
    (a) => a.action === 'EXECUTE_POST_ACTION_MENU_CHOICE',
  ) as ExecutePostActionMenuChoice | null

  const startPostActionSessionAction = actions.find(
    (a) => a.action === 'START_POST_ACTION_SESSION',
  ) as StartPostActionSession | null

  if (startPostActionSessionAction != null || executePostActionMenuChoiceAction != null) {
    // do nothing, `postActionInteractionData` was already set in the respective meta actions
    return editorState
  }

  if (nonTransientActions.length > 0 || setSelectedViewsActions.length > 0) {
    // reset `postActionInteractionData`
    return {
      ...editorState,
      postActionInteractionSession: null,
    }
  }

  // do nothing
  return editorState
}
