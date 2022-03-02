import {
  applyCanvasStrategy,
  findCanvasStrategy,
  findCanvasStrategyFromDispatchResult,
  pickCanvasStateFromEditorState,
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
  shouldApplyClearInteractionSessionResult,
} from '../actions/action-utils'
import { InnerDispatchResult } from './dispatch'
import { DerivedState, deriveState, EditorState, EditorStoreFull } from './editor-state'
import {
  CanvasStrategy,
  InteractionCanvasState,
} from '../../canvas/canvas-strategies/canvas-strategy-types'

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
    const { strategy } = findCanvasStrategy(
      strategies,
      canvasState,
      interactionSession,
      result.strategyState,
      result.strategyState.currentStrategy,
    )

    // If there is a current strategy, produce the commands from it.
    if (strategy == null) {
      return {
        unpatchedEditorState: newEditorState,
        patchedEditorState: newEditorState,
        newStrategyState: withClearedSession,
      }
    } else {
      const commands = applyCanvasStrategy(
        strategy.strategy,
        canvasState,
        interactionSession,
        result.strategyState,
      )
      const commandResult = foldAndApplyCommands(
        newEditorState,
        storedState.patchedEditor,
        [...result.strategyState.accumulatedCommands.flatMap((c) => c.commands), ...commands],
        'permanent',
      )

      return {
        unpatchedEditorState: commandResult.editorState,
        patchedEditorState: commandResult.editorState,
        newStrategyState: withClearedSession,
      }
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
    const { strategy, previousStrategy } = findCanvasStrategy(
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
        commands,
        'transient',
      )
      const newStrategyState: StrategyState = {
        currentStrategy: strategy.strategy.id,
        currentStrategyFitness: strategy.fitness,
        currentStrategyCommands: commands,
        accumulatedCommands: [],
        commandDescriptions: commandResult.commandDescriptions,
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
    const { strategy } = findCanvasStrategy(
      strategies,
      canvasState,
      interactionSession,
      result.strategyState,
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
        [...result.strategyState.accumulatedCommands.flatMap((c) => c.commands), ...commands],
        'transient',
      )
      const newStrategyState: StrategyState = {
        currentStrategy: strategy.strategy.id,
        currentStrategyFitness: strategy.fitness,
        currentStrategyCommands: commands,
        accumulatedCommands: result.strategyState.accumulatedCommands,
        commandDescriptions: commandResult.commandDescriptions,
        startingMetadata: result.strategyState.startingMetadata,
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
        newStrategyState: result.strategyState,
      }
    }
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
    const { strategy, previousStrategy } = findCanvasStrategy(
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
        commands,
        'transient',
      )

      const newStrategyState: StrategyState = {
        currentStrategy: strategy.strategy.id,
        currentStrategyFitness: strategy.fitness,
        currentStrategyCommands: commands,
        accumulatedCommands: [],
        commandDescriptions: commandResult.commandDescriptions,
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

export function interactionUserChangedStrategy(
  strategies: Array<CanvasStrategy>,
  storedState: EditorStoreFull,
  result: InnerDispatchResult,
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
    const { strategy, previousStrategy } = findCanvasStrategy(
      strategies,
      canvasState,
      interactionSession,
      result.strategyState,
      result.strategyState.currentStrategy,
    )
    const strategyId = strategy?.strategy.id
    if (strategyId != result.unpatchedEditor.canvas.interactionSession?.userPreferredStrategy) {
      console.warn(
        'Entered interactionUserChangedStrategy but the user preferred strategy is not applied',
      )
    }

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
        result.strategyState,
      )
      const newAccumulatedCommands = [
        ...result.strategyState.accumulatedCommands,
        ...strategyChangedLogCommands,
      ]
      const commandResult = foldAndApplyCommands(
        newEditorState,
        storedState.patchedEditor,
        [...newAccumulatedCommands.flatMap((c) => c.commands), ...commands],
        'transient',
      )
      const newStrategyState: StrategyState = {
        currentStrategy: strategy.strategy.id,
        currentStrategyFitness: strategy.fitness,
        currentStrategyCommands: commands,
        accumulatedCommands: newAccumulatedCommands,
        commandDescriptions: commandResult.commandDescriptions,
        startingMetadata: result.strategyState.startingMetadata,
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
        newStrategyState: result.strategyState,
      }
    }
  }
}

export function interactionStrategyChangeStacked(
  strategies: Array<CanvasStrategy>,
  storedState: EditorStoreFull,
  result: InnerDispatchResult,
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
    const { strategy, previousStrategy } = findCanvasStrategy(
      strategies,
      canvasState,
      interactionSession,
      result.strategyState,
      result.strategyState.currentStrategy,
    )
    const strategyId = strategy?.strategy.id
    if (strategyId === result.strategyState.currentStrategy) {
      console.warn("Entered interactionStrategyChangeStacked but the strategy haven't changed")
    }

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
        result.strategyState,
      )
      const newAccumulatedCommands = [
        ...result.strategyState.accumulatedCommands,
        {
          strategy: result.strategyState.currentStrategy,
          commands: result.strategyState.currentStrategyCommands,
        },
        ...strategyChangedLogCommands,
      ]
      const commandResult = foldAndApplyCommands(
        newEditorState,
        storedState.patchedEditor,
        [...newAccumulatedCommands.flatMap((c) => c.commands), ...commands],
        'transient',
      )
      const newStrategyState: StrategyState = {
        currentStrategy: strategy.strategy.id,
        currentStrategyFitness: strategy.fitness,
        currentStrategyCommands: commands,
        accumulatedCommands: newAccumulatedCommands,
        commandDescriptions: commandResult.commandDescriptions,
        startingMetadata: result.strategyState.startingMetadata,
      }

      const patchedEditorState = {
        ...newEditorState,
        canvas: {
          ...newEditorState.canvas,
          interactionSession: strategySwitchInteractionSessionReset(
            newEditorState.canvas.interactionSession,
          ),
        },
      }

      return {
        unpatchedEditorState: newEditorState,
        patchedEditorState: patchedEditorState,
        newStrategyState: newStrategyState,
      }
    } else {
      return {
        unpatchedEditorState: newEditorState,
        patchedEditorState: newEditorState,
        newStrategyState: result.strategyState,
      }
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

  const patchedDerivedState = deriveState(patchedEditorWithMetadata, oldDerivedState, 'patched')

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
      const interactionHardResetNeeded =
        hasDragModifiersChanged(
          storedState.unpatchedEditor.canvas.interactionSession?.interactionData ?? null,
          result.unpatchedEditor.canvas.interactionSession?.interactionData ?? null,
        ) || result.strategyState.currentStrategy == null // TODO: do we really need the currentStrategy == null part?
      if (interactionHardResetNeeded) {
        return interactionHardReset(strategies, storedState, result)
      } else {
        if (result.unpatchedEditor.canvas.interactionSession?.userPreferredStrategy != null) {
          const userChangedStrategy =
            result.unpatchedEditor.canvas.interactionSession?.userPreferredStrategy !=
            storedState.unpatchedEditor.canvas.interactionSession?.userPreferredStrategy
          if (userChangedStrategy) {
            return interactionUserChangedStrategy(strategies, storedState, result)
          }
        }

        const strategy = findCanvasStrategyFromDispatchResult(strategies, result)
        if (strategy?.strategy.id !== result.strategyState.currentStrategy) {
          return interactionStrategyChangeStacked(strategies, storedState, result)
        }

        return interactionUpdate(strategies, storedState, result)
      }
    }
  }
}
