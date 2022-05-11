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

    const commands =
      strategy != null
        ? applyCanvasStrategy(
            strategy.strategy,
            canvasState,
            interactionSession,
            result.strategyState,
          )
        : []
    const commandResult = foldAndApplyCommands(
      newEditorState,
      storedState.patchedEditor,
      result.strategyState.accumulatedPatches,
      [],
      commands,
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
      const commands = applyCanvasStrategy(
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
        commands,
        'transient',
      )
      const newStrategyState: StrategyState = {
        currentStrategy: strategy.strategy.id,
        currentStrategyFitness: strategy.fitness,
        currentStrategyCommands: commands,
        accumulatedPatches: [],
        commandDescriptions: commandResult.commandDescriptions,
        sortedApplicableStrategies: sortedApplicableStrategies,
        startingMetadata: resetStrategyState.startingMetadata,
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
      const commands = applyCanvasStrategy(
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
        commands,
        'transient',
      )

      const newStrategyState: StrategyState = {
        currentStrategy: strategy.strategy.id,
        currentStrategyFitness: strategy.fitness,
        currentStrategyCommands: commands,
        accumulatedPatches: [],
        commandDescriptions: commandResult.commandDescriptions,
        sortedApplicableStrategies: sortedApplicableStrategies,
        startingMetadata: newEditorState.canvas.interactionSession.metadata,
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
    newStrategyState: createEmptyStrategyState(),
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

    const commands = applyCanvasStrategy(
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
      commands,
      'transient',
    )
    const newStrategyState: StrategyState = {
      currentStrategy: strategy.strategy.id,
      currentStrategyFitness: strategy.fitness,
      currentStrategyCommands: commands,
      accumulatedPatches: commandResult.accumulatedPatches,
      commandDescriptions: commandResult.commandDescriptions,
      sortedApplicableStrategies: sortedApplicableStrategies,
      startingMetadata: strategyState.startingMetadata,
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
    const commands =
      strategy != null
        ? applyCanvasStrategy(
            strategy.strategy,
            canvasState,
            newEditorState.canvas.interactionSession,
            strategyState,
          )
        : []
    const commandResult = foldAndApplyCommands(
      newEditorState,
      storedEditorState,
      strategyState.accumulatedPatches,
      strategyState.currentStrategyCommands,
      commands,
      'transient',
    )
    const newStrategyState: StrategyState = {
      currentStrategy: strategy?.strategy.id ?? null,
      currentStrategyFitness: strategy?.fitness ?? 0,
      currentStrategyCommands: commands,
      accumulatedPatches: commandResult.accumulatedPatches,
      commandDescriptions: commandResult.commandDescriptions,
      sortedApplicableStrategies: sortedApplicableStrategies,
      startingMetadata: strategyState.startingMetadata,
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
    const commands =
      strategy != null
        ? applyCanvasStrategy(
            strategy.strategy,
            canvasState,
            newEditorState.canvas.interactionSession,
            strategyState,
          )
        : []
    const commandResult = foldAndApplyCommands(
      newEditorState,
      storedEditorState,
      strategyState.accumulatedPatches,
      [],
      commands,
      'transient',
    )
    const newStrategyState: StrategyState = {
      currentStrategy: strategy?.strategy.id ?? null,
      currentStrategyFitness: strategy?.fitness ?? 0,
      currentStrategyCommands: commands,
      accumulatedPatches: strategyState.accumulatedPatches,
      commandDescriptions: commandResult.commandDescriptions,
      sortedApplicableStrategies: sortedApplicableStrategies,
      startingMetadata: strategyState.startingMetadata,
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

function handleStrategiesInner(
  strategies: Array<CanvasStrategy>,
  dispatchedActions: readonly EditorAction[],
  storedState: EditorStoreFull,
  result: InnerDispatchResult,
): HandleStrategiesResult {
  const makeChangesPermanent = dispatchedActions.some(shouldApplyClearInteractionSessionResult)
  const cancelInteraction =
    dispatchedActions.some(isClearInteractionSession) && !makeChangesPermanent
  const isInteractionAction = dispatchedActions.some(isCreateOrUpdateInteractionSession)
    ? 'interaction-create-or-update'
    : 'non-interaction'
  if (storedState.unpatchedEditor.canvas.interactionSession == null) {
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
