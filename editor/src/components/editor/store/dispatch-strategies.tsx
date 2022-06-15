import {
  applyCanvasStrategy,
  findCanvasStrategy,
  pickCanvasStateFromEditorState,
  StrategyWithFitness,
} from '../../canvas/canvas-strategies/canvas-strategies'
import {
  createEmptyStrategyState,
  hasDragModifiersChanged,
  interactionSessionHardReset,
  StrategyState,
  strategySwitchInteractionSessionReset,
} from '../../canvas/canvas-strategies/interaction-state'
import { foldAndApplyCommands } from '../../canvas/commands/commands'
import { strategySwitched } from '../../canvas/commands/strategy-switched-command'
import { EditorAction } from '../action-types'
import {
  isClearInteractionSession,
  isCreateOrUpdateInteractionSession,
  shouldApplyClearInteractionSessionResult,
} from '../actions/action-utils'
import { InnerDispatchResult } from './dispatch'
import { DerivedState, deriveState, EditorState, EditorStoreFull } from './editor-state'
import {
  CanvasStrategy,
  InteractionCanvasState,
} from '../../canvas/canvas-strategies/canvas-strategy-types'
import { isFeatureEnabled } from '../../../utils/feature-switches'
import { PERFORMANCE_MARKS_ALLOWED } from '../../../common/env-vars'
import { saveDOMReport } from '../actions/action-creators'

interface HandleStrategiesResult {
  unpatchedEditorState: EditorState
  patchedEditorState: EditorState
  newStrategyState: StrategyState
}

export function interactionFinished(
  strategies: Array<CanvasStrategy>,
  storedState: EditorStoreFull,
  result: InnerDispatchResult,
): HandleStrategiesResult {
  const newEditorState = result.unpatchedEditor
  const withClearedSession = createEmptyStrategyState(
    newEditorState.canvas.interactionSession?.metadata ?? newEditorState.jsxMetadata,
    newEditorState.canvas.interactionSession?.allElementProps ?? newEditorState.allElementProps,
  )
  const canvasState: InteractionCanvasState = pickCanvasStateFromEditorState(newEditorState)
  const interactionSession = storedState.unpatchedEditor.canvas.interactionSession
  if (interactionSession == null) {
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
      result.strategyState,
      result.strategyState.currentStrategy,
    )

    const newStrategyState = {
      ...withClearedSession,
      sortedApplicableStrategies: sortedApplicableStrategies,
    }

    const strategyResult =
      strategy != null
        ? applyCanvasStrategy(
            strategy.strategy,
            canvasState,
            interactionSession,
            result.strategyState,
          )
        : {
            commands: [],
          }
    const commandResult = foldAndApplyCommands(
      newEditorState,
      storedState.patchedEditor,
      result.strategyState.accumulatedPatches,
      [],
      strategyResult.commands,
      'permanent',
    )

    return {
      unpatchedEditorState: commandResult.editorState,
      patchedEditorState: commandResult.editorState,
      newStrategyState: newStrategyState,
    }
  }
}

export function interactionHardReset(
  strategies: Array<CanvasStrategy>,
  storedState: EditorStoreFull,
  result: InnerDispatchResult,
): HandleStrategiesResult {
  const newEditorState = result.unpatchedEditor
  const withClearedSession = {
    ...storedState.strategyState,
    startingMetadata: storedState.unpatchedEditor.jsxMetadata,
  }
  const canvasState: InteractionCanvasState = pickCanvasStateFromEditorState(newEditorState)
  const interactionSession = newEditorState.canvas.interactionSession
  if (interactionSession == null) {
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
      resetStrategyState,
      resetStrategyState.currentStrategy,
    )

    // If there is a current strategy, produce the commands from it.
    if (strategy != null && newEditorState.canvas.interactionSession != null) {
      const strategyResult = applyCanvasStrategy(
        strategy.strategy,
        canvasState,
        newEditorState.canvas.interactionSession,
        resetStrategyState,
      )
      const commandResult = foldAndApplyCommands(
        newEditorState,
        storedState.patchedEditor,
        [],
        [],
        strategyResult.commands,
        'transient',
      )
      const newStrategyState: StrategyState = {
        currentStrategy: strategy.strategy.id,
        currentStrategyFitness: strategy.fitness,
        currentStrategyCommands: strategyResult.commands,
        accumulatedPatches: [],
        commandDescriptions: commandResult.commandDescriptions,
        sortedApplicableStrategies: sortedApplicableStrategies,
        startingMetadata: resetStrategyState.startingMetadata,
        customStrategyState: strategyResult.customState ?? result.strategyState.customStrategyState,
        startingAllElementProps: resetStrategyState.startingAllElementProps,
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
  strategies: Array<CanvasStrategy>,
  storedState: EditorStoreFull,
  result: InnerDispatchResult,
  actionType: 'interaction-create-or-update' | 'non-interaction',
): HandleStrategiesResult {
  const newEditorState = result.unpatchedEditor
  const canvasState: InteractionCanvasState = pickCanvasStateFromEditorState(newEditorState)
  const interactionSession = newEditorState.canvas.interactionSession
  if (interactionSession == null) {
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
      result.strategyState,
      result.strategyState.currentStrategy,
    )

    if (interactionSession.userPreferredStrategy != null) {
      const userChangedStrategy =
        interactionSession.userPreferredStrategy !=
        storedState.unpatchedEditor.canvas.interactionSession?.userPreferredStrategy
      if (userChangedStrategy) {
        return handleUserChangedStrategy(
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
      actionType === 'interaction-create-or-update'
    ) {
      return handleAccumulatingKeypresses(
        newEditorState,
        storedState.patchedEditor,
        result.strategyState,
        strategy,
        previousStrategy,
        sortedApplicableStrategies,
      )
    }
    return handleUpdate(
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
  strategies: Array<CanvasStrategy>,
  storedState: EditorStoreFull,
  result: InnerDispatchResult,
): HandleStrategiesResult {
  const newEditorState = result.unpatchedEditor
  const withClearedSession = createEmptyStrategyState(
    newEditorState.canvas.interactionSession?.metadata ?? newEditorState.jsxMetadata,
    newEditorState.canvas.interactionSession?.allElementProps ?? newEditorState.allElementProps,
  )
  const canvasState: InteractionCanvasState = pickCanvasStateFromEditorState(newEditorState)
  const interactionSession = newEditorState.canvas.interactionSession
  if (interactionSession == null) {
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
      withClearedSession,
      result.strategyState.currentStrategy,
    )

    // If there is a current strategy, produce the commands from it.
    if (strategy != null && newEditorState.canvas.interactionSession != null) {
      const strategyResult = applyCanvasStrategy(
        strategy.strategy,
        canvasState,
        newEditorState.canvas.interactionSession,
        result.strategyState,
      )
      const commandResult = foldAndApplyCommands(
        newEditorState,
        storedState.patchedEditor,
        [],
        [],
        strategyResult.commands,
        'transient',
      )

      const newStrategyState: StrategyState = {
        currentStrategy: strategy.strategy.id,
        currentStrategyFitness: strategy.fitness,
        currentStrategyCommands: strategyResult.commands,
        accumulatedPatches: [],
        commandDescriptions: commandResult.commandDescriptions,
        sortedApplicableStrategies: sortedApplicableStrategies,
        startingMetadata: newEditorState.canvas.interactionSession.metadata,
        customStrategyState: strategyResult.customState ?? result.strategyState.customStrategyState,
        startingAllElementProps: newEditorState.canvas.interactionSession.allElementProps,
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
  result: InnerDispatchResult,
): HandleStrategiesResult {
  const updatedEditorState: EditorState = {
    ...result.unpatchedEditor,
    canvas: {
      ...result.unpatchedEditor.canvas,
      interactionSession: null,
    },
  }

  return {
    unpatchedEditorState: updatedEditorState,
    patchedEditorState: updatedEditorState,
    newStrategyState: createEmptyStrategyState({}, {}),
  }
}

function handleUserChangedStrategy(
  newEditorState: EditorState,
  storedEditorState: EditorState,
  strategyState: StrategyState,
  strategy: StrategyWithFitness | null,
  previousStrategy: StrategyWithFitness | null,
  sortedApplicableStrategies: Array<CanvasStrategy>,
): HandleStrategiesResult {
  const canvasState: InteractionCanvasState = pickCanvasStateFromEditorState(newEditorState)

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
      strategyState,
    )
    const commandResult = foldAndApplyCommands(
      newEditorState,
      storedEditorState,
      strategyState.accumulatedPatches,
      strategyChangedLogCommands.flatMap((c) => c.commands),
      strategyResult.commands,
      'transient',
    )
    const newStrategyState: StrategyState = {
      currentStrategy: strategy.strategy.id,
      currentStrategyFitness: strategy.fitness,
      currentStrategyCommands: strategyResult.commands,
      accumulatedPatches: commandResult.accumulatedPatches,
      commandDescriptions: commandResult.commandDescriptions,
      sortedApplicableStrategies: sortedApplicableStrategies,
      startingMetadata: strategyState.startingMetadata,
      customStrategyState: strategyResult.customState ?? strategyState.customStrategyState,
      startingAllElementProps: strategyState.startingAllElementProps,
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
  newEditorState: EditorState,
  storedEditorState: EditorState,
  strategyState: StrategyState,
  strategy: StrategyWithFitness | null,
  previousStrategy: StrategyWithFitness | null,
  sortedApplicableStrategies: Array<CanvasStrategy>,
): HandleStrategiesResult {
  const canvasState: InteractionCanvasState = pickCanvasStateFromEditorState(newEditorState)
  // If there is a current strategy, produce the commands from it.
  if (newEditorState.canvas.interactionSession != null) {
    const strategyResult =
      strategy != null
        ? applyCanvasStrategy(
            strategy.strategy,
            canvasState,
            newEditorState.canvas.interactionSession,
            strategyState,
          )
        : {
            commands: [],
            customState: strategyState.customStrategyState,
          }
    const commandResult = foldAndApplyCommands(
      newEditorState,
      storedEditorState,
      strategyState.accumulatedPatches,
      strategyState.currentStrategyCommands,
      strategyResult.commands,
      'transient',
    )
    const newStrategyState: StrategyState = {
      currentStrategy: strategy?.strategy.id ?? null,
      currentStrategyFitness: strategy?.fitness ?? 0,
      currentStrategyCommands: strategyResult.commands,
      accumulatedPatches: commandResult.accumulatedPatches,
      commandDescriptions: commandResult.commandDescriptions,
      sortedApplicableStrategies: sortedApplicableStrategies,
      startingMetadata: strategyState.startingMetadata,
      customStrategyState: strategyResult.customState ?? strategyState.customStrategyState,
      startingAllElementProps: strategyState.startingAllElementProps,
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

function handleUpdate(
  newEditorState: EditorState,
  storedEditorState: EditorState,
  strategyState: StrategyState,
  strategy: StrategyWithFitness | null,
  previousStrategy: StrategyWithFitness | null,
  sortedApplicableStrategies: Array<CanvasStrategy>,
): HandleStrategiesResult {
  const canvasState: InteractionCanvasState = pickCanvasStateFromEditorState(newEditorState)
  // If there is a current strategy, produce the commands from it.
  if (newEditorState.canvas.interactionSession != null) {
    const strategyResult =
      strategy != null
        ? applyCanvasStrategy(
            strategy.strategy,
            canvasState,
            newEditorState.canvas.interactionSession,
            strategyState,
          )
        : {
            commands: [],
            customState: strategyState.customStrategyState,
          }
    const commandResult = foldAndApplyCommands(
      newEditorState,
      storedEditorState,
      strategyState.accumulatedPatches,
      [],
      strategyResult.commands,
      'transient',
    )
    const newStrategyState: StrategyState = {
      currentStrategy: strategy?.strategy.id ?? null,
      currentStrategyFitness: strategy?.fitness ?? 0,
      currentStrategyCommands: strategyResult.commands,
      accumulatedPatches: strategyState.accumulatedPatches,
      commandDescriptions: commandResult.commandDescriptions,
      sortedApplicableStrategies: sortedApplicableStrategies,
      startingMetadata: strategyState.startingMetadata,
      customStrategyState: strategyResult.customState ?? strategyState.customStrategyState,
      startingAllElementProps: strategyState.startingAllElementProps,
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
  strategies: Array<CanvasStrategy>,
  dispatchedActions: readonly EditorAction[],
  storedState: EditorStoreFull,
  result: InnerDispatchResult,
  oldDerivedState: DerivedState,
): HandleStrategiesResult & { patchedDerivedState: DerivedState } {
  const MeasureDispatchTime =
    isFeatureEnabled('Debug mode – Performance Marks') && PERFORMANCE_MARKS_ALLOWED

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
      patchedEditorState.canvas.interactionSession?.metadata ?? patchedEditorState.jsxMetadata,
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
    unpatchedEditorState,
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
            metadata: newEditorState.canvas.interactionSession.metadata, // the fresh metadata from SAVE_DOM_REPORT
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
    }
  }
}

function handleStrategiesInner(
  strategies: Array<CanvasStrategy>,
  dispatchedActions: readonly EditorAction[],
  storedState: EditorStoreFull,
  result: InnerDispatchResult,
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
