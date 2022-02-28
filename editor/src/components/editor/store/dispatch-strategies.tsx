import {
  StrategyState,
  createEmptyStrategyState,
  strategySwitchInteractionSessionReset,
  hasModifiersChanged,
  interactionSessionHardReset,
} from '../../canvas/canvas-strategies/interaction-state'
import {
  findCanvasStrategy,
  applyCanvasStrategy,
  findCanvasStrategyFromDispatchResult,
} from '../../canvas/canvas-strategies/canvas-strategies'
import { foldAndApplyCommands, strategySwitched } from '../../canvas/commands/commands'
import { EditorAction } from '../action-types'
import {
  shouldApplyClearInteractionSessionResult,
  isClearInteractionSession,
} from '../actions/action-utils'
import { InnerDispatchResult } from './dispatch'
import { DerivedState, deriveState, EditorState, EditorStoreFull } from './editor-state'
import { InteractionCanvasState } from '../../canvas/canvas-strategies/canvas-strategy-types'

interface HandleStrategiesResult {
  unpatchedEditorState: EditorState
  patchedEditorState: EditorState
  newStrategyState: StrategyState
}

export function interactionFinished(
  storedState: EditorStoreFull,
  result: InnerDispatchResult,
): HandleStrategiesResult {
  const newEditorState = result.unpatchedEditor
  const withClearedSession = createEmptyStrategyState(
    newEditorState.canvas.interactionSession?.metadata ?? newEditorState.jsxMetadata,
  )
  const canvasState: InteractionCanvasState = {
    selectedElements: newEditorState.selectedViews,
    // metadata: store.editor.jsxMetadata, // We can add metadata back if live metadata is necessary
    projectContents: newEditorState.projectContents,
    openFile: newEditorState.canvas.openFile?.filename,
    scale: newEditorState.canvas.scale,
    canvasOffset: newEditorState.canvas.roundedCanvasOffset,
  }
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
  storedState: EditorStoreFull,
  result: InnerDispatchResult,
): HandleStrategiesResult {
  const newEditorState = result.unpatchedEditor
  const withClearedSession = {
    ...storedState.strategyState,
    startingMetadata: storedState.unpatchedEditor.jsxMetadata,
  }
  const canvasState: InteractionCanvasState = {
    selectedElements: newEditorState.selectedViews,
    // metadata: store.editor.jsxMetadata, // We can add metadata back if live metadata is necessary
    projectContents: newEditorState.projectContents,
    openFile: newEditorState.canvas.openFile?.filename,
    scale: newEditorState.canvas.scale,
    canvasOffset: newEditorState.canvas.roundedCanvasOffset,
  }
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
        currentStrategy: strategy.strategy.name,
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
  storedState: EditorStoreFull,
  result: InnerDispatchResult,
): HandleStrategiesResult {
  const newEditorState = result.unpatchedEditor
  const canvasState: InteractionCanvasState = {
    selectedElements: newEditorState.selectedViews,
    // metadata: store.editor.jsxMetadata, // We can add metadata back if live metadata is necessary
    projectContents: newEditorState.projectContents,
    openFile: newEditorState.canvas.openFile?.filename,
    scale: newEditorState.canvas.scale,
    canvasOffset: newEditorState.canvas.roundedCanvasOffset,
  }
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
        currentStrategy: strategy.strategy.name,
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
  storedState: EditorStoreFull,
  result: InnerDispatchResult,
): HandleStrategiesResult {
  const newEditorState = result.unpatchedEditor
  const withClearedSession = createEmptyStrategyState(
    newEditorState.canvas.interactionSession?.metadata ?? newEditorState.jsxMetadata,
  )
  const canvasState: InteractionCanvasState = {
    selectedElements: newEditorState.selectedViews,
    // metadata: store.editor.jsxMetadata, // We can add metadata back if live metadata is necessary
    projectContents: newEditorState.projectContents,
    openFile: newEditorState.canvas.openFile?.filename,
    scale: newEditorState.canvas.scale,
    canvasOffset: newEditorState.canvas.roundedCanvasOffset,
  }
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
        currentStrategy: strategy.strategy.name,
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
  storedState: EditorStoreFull,
  result: InnerDispatchResult,
): HandleStrategiesResult {
  const newEditorState = result.unpatchedEditor
  const canvasState: InteractionCanvasState = {
    selectedElements: newEditorState.selectedViews,
    // metadata: store.editor.jsxMetadata, // We can add metadata back if live metadata is necessary
    projectContents: newEditorState.projectContents,
    openFile: newEditorState.canvas.openFile?.filename,
    scale: newEditorState.canvas.scale,
    canvasOffset: newEditorState.canvas.roundedCanvasOffset,
  }
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
      canvasState,
      interactionSession,
      result.strategyState,
      result.strategyState.currentStrategy,
    )
    const strategyName = strategy?.strategy.name
    if (strategyName != result.unpatchedEditor.canvas.interactionSession?.userPreferredStrategy) {
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
              strategyName!,
              false,
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
        currentStrategy: strategy.strategy.name,
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

function interactionStrategyChangeStacked(
  storedState: EditorStoreFull,
  result: InnerDispatchResult,
): HandleStrategiesResult {
  const newEditorState = result.unpatchedEditor
  const canvasState: InteractionCanvasState = {
    selectedElements: newEditorState.selectedViews,
    // metadata: store.editor.jsxMetadata, // We can add metadata back if live metadata is necessary
    projectContents: newEditorState.projectContents,
    openFile: newEditorState.canvas.openFile?.filename,
    scale: newEditorState.canvas.scale,
    canvasOffset: newEditorState.canvas.roundedCanvasOffset,
  }
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
      canvasState,
      interactionSession,
      result.strategyState,
      result.strategyState.currentStrategy,
    )
    const strategyName = strategy?.strategy.name
    if (strategyName === result.strategyState.currentStrategy) {
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
              strategyName!,
              false,
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
        currentStrategy: strategy.strategy.name,
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
  dispatchedActions: readonly EditorAction[],
  storedState: EditorStoreFull,
  result: InnerDispatchResult,
  oldDerivedState: DerivedState,
): HandleStrategiesResult & { patchedDerivedState: DerivedState } {
  const { unpatchedEditorState, patchedEditorState, newStrategyState } = handleStrategiesInner(
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
      return interactionStart(storedState, result)
    }
  } else {
    if (cancelInteraction) {
      return interactionCancel(storedState, result)
    } else if (makeChangesPermanent) {
      return interactionFinished(storedState, result)
    } else {
      const interactionHardResetNeeded =
        hasModifiersChanged(
          storedState.unpatchedEditor.canvas.interactionSession?.interactionData ?? null,
          result.unpatchedEditor.canvas.interactionSession?.interactionData ?? null,
        ) || result.strategyState.currentStrategy == null // TODO: do we really need the currentStrategy == null part?
      if (interactionHardResetNeeded) {
        return interactionHardReset(storedState, result)
      } else {
        if (result.unpatchedEditor.canvas.interactionSession?.userPreferredStrategy != null) {
          const userChangedStrategy =
            result.unpatchedEditor.canvas.interactionSession?.userPreferredStrategy !=
            storedState.unpatchedEditor.canvas.interactionSession?.userPreferredStrategy
          if (userChangedStrategy) {
            return interactionUserChangedStrategy(storedState, result)
          }
        }

        const strategy = findCanvasStrategyFromDispatchResult(result)
        if (strategy?.strategy.name !== result.strategyState.currentStrategy) {
          return interactionStrategyChangeStacked(storedState, result)
        }

        return interactionUpdate(storedState, result)
      }
    }
  }
}
