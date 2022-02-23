import {
  SessionStateState,
  createEmptySessionStateState,
  createEmptyStrategyState,
  InteractionCanvasState,
} from '../../../interactions_proposal'
import {
  findCanvasStrategy,
  applyCanvasStrategy,
  interactionStateHardReset,
  strategySwitchInteractionStateReset,
  hasModifiersChanged,
  findCanvasStrategyFromDispatchResult,
} from '../../canvas/canvas-strategies/canvas-strategies'
import { foldAndApplyCommands, strategySwitched } from '../../canvas/commands/commands'
import { EditorAction } from '../action-types'
import {
  shouldApplyClearInteractionStateResult,
  isClearInteractionState,
} from '../actions/action-utils'
import { DispatchResult, InnerDispatchResult } from './dispatch'
import { DerivedState, deriveState, EditorState, EditorStoreFull } from './editor-state'

interface HandleStrategiesResult {
  unpatchedEditorState: EditorState
  patchedEditorState: EditorState
  newSessionStateState: SessionStateState
}

export function interactionFinished(
  storedState: EditorStoreFull,
  result: InnerDispatchResult,
): HandleStrategiesResult {
  const newEditorState = result.unpatchedEditor
  const withClearedSession = createEmptySessionStateState(newEditorState.jsxMetadata)
  const canvasState: InteractionCanvasState = {
    selectedElements: newEditorState.selectedViews,
    // metadata: store.editor.jsxMetadata, // We can add metadata back if live metadata is necessary
    projectContents: newEditorState.projectContents,
    openFile: newEditorState.canvas.openFile?.filename,
    scale: newEditorState.canvas.scale,
    canvasOffset: newEditorState.canvas.roundedCanvasOffset,
  }
  const interactionState = storedState.unpatchedEditor.canvas.interactionState
  if (interactionState == null) {
    return {
      unpatchedEditorState: newEditorState,
      patchedEditorState: newEditorState,
      newSessionStateState: withClearedSession,
    }
  } else {
    // Determine the new canvas strategy to run this time around.
    const { strategy } = findCanvasStrategy(
      canvasState,
      interactionState,
      result.sessionStateState,
      result.sessionStateState.currentStrategy,
    )

    // If there is a current strategy, produce the commands from it.
    if (strategy == null) {
      return {
        unpatchedEditorState: newEditorState,
        patchedEditorState: newEditorState,
        newSessionStateState: withClearedSession,
      }
    } else {
      const commands = applyCanvasStrategy(
        strategy.strategy,
        canvasState,
        interactionState,
        result.sessionStateState,
      )
      const commandResult = foldAndApplyCommands(
        newEditorState,
        storedState.patchedEditor,
        storedState.sessionStateState.strategyState,
        [...result.sessionStateState.accumulatedCommands.flatMap((c) => c.commands), ...commands],
        'permanent',
      )

      return {
        unpatchedEditorState: commandResult.editorState,
        patchedEditorState: commandResult.editorState,
        newSessionStateState: withClearedSession,
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
    ...storedState.sessionStateState,
    startingMetadata: storedState.sessionStateState.originalMetadata,
  }
  const canvasState: InteractionCanvasState = {
    selectedElements: newEditorState.selectedViews,
    // metadata: store.editor.jsxMetadata, // We can add metadata back if live metadata is necessary
    projectContents: newEditorState.projectContents,
    openFile: newEditorState.canvas.openFile?.filename,
    scale: newEditorState.canvas.scale,
    canvasOffset: newEditorState.canvas.roundedCanvasOffset,
  }
  const interactionState = newEditorState.canvas.interactionState
  if (interactionState == null) {
    return {
      unpatchedEditorState: newEditorState,
      patchedEditorState: newEditorState,
      newSessionStateState: withClearedSession,
    }
  } else {
    const resetInteractionState = interactionStateHardReset(interactionState)
    const resetSessionState = {
      ...result.sessionStateState,
      startingMetadata: result.sessionStateState.originalMetadata,
    }
    // Determine the new canvas strategy to run this time around.
    const { strategy, previousStrategy } = findCanvasStrategy(
      canvasState,
      resetInteractionState,
      resetSessionState,
      resetSessionState.currentStrategy,
    )

    // If there is a current strategy, produce the commands from it.
    if (strategy != null && newEditorState.canvas.interactionState != null) {
      const commands = applyCanvasStrategy(
        strategy.strategy,
        canvasState,
        newEditorState.canvas.interactionState,
        resetSessionState,
      )
      const commandResult = foldAndApplyCommands(
        newEditorState,
        storedState.patchedEditor,
        storedState.sessionStateState.strategyState,
        commands,
        'transient',
      )
      const newSessionStateState: SessionStateState = {
        currentStrategy: strategy.strategy.name,
        currentStrategyFitness: strategy.fitness,
        currentStrategyCommands: commands,
        accumulatedCommands: [],
        commandDescriptions: commandResult.commandDescriptions,
        strategyState: createEmptyStrategyState(),
        startingMetadata: resetSessionState.startingMetadata,
        originalMetadata: resetSessionState.originalMetadata,
      }

      return {
        unpatchedEditorState: newEditorState,
        patchedEditorState: commandResult.editorState,
        newSessionStateState: newSessionStateState,
      }
    } else {
      return {
        unpatchedEditorState: newEditorState,
        patchedEditorState: newEditorState,
        newSessionStateState: withClearedSession,
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
  const interactionState = newEditorState.canvas.interactionState
  if (interactionState == null) {
    return {
      unpatchedEditorState: newEditorState,
      patchedEditorState: newEditorState,
      newSessionStateState: result.sessionStateState,
    }
  } else {
    // Determine the new canvas strategy to run this time around.
    const { strategy, previousStrategy } = findCanvasStrategy(
      canvasState,
      interactionState,
      result.sessionStateState,
      result.sessionStateState.currentStrategy,
    )

    // If there is a current strategy, produce the commands from it.
    if (strategy != null && newEditorState.canvas.interactionState != null) {
      const commands = applyCanvasStrategy(
        strategy.strategy,
        canvasState,
        newEditorState.canvas.interactionState,
        result.sessionStateState,
      )
      const commandResult = foldAndApplyCommands(
        newEditorState,
        storedState.patchedEditor,
        storedState.sessionStateState.strategyState,
        [...result.sessionStateState.accumulatedCommands.flatMap((c) => c.commands), ...commands],
        'transient',
      )
      const newSessionStateState: SessionStateState = {
        currentStrategy: strategy.strategy.name,
        currentStrategyFitness: strategy.fitness,
        currentStrategyCommands: commands,
        accumulatedCommands: result.sessionStateState.accumulatedCommands,
        commandDescriptions: commandResult.commandDescriptions,
        strategyState: createEmptyStrategyState(),
        startingMetadata: result.sessionStateState.startingMetadata,
        originalMetadata: result.sessionStateState.originalMetadata,
      }

      return {
        unpatchedEditorState: newEditorState,
        patchedEditorState: commandResult.editorState,
        newSessionStateState: newSessionStateState,
      }
    } else {
      return {
        unpatchedEditorState: newEditorState,
        patchedEditorState: newEditorState,
        newSessionStateState: result.sessionStateState,
      }
    }
  }
}

export function interactionStart(
  storedState: EditorStoreFull,
  result: InnerDispatchResult,
): HandleStrategiesResult {
  const newEditorState = result.unpatchedEditor
  const withClearedSession = createEmptySessionStateState(newEditorState.jsxMetadata)
  const canvasState: InteractionCanvasState = {
    selectedElements: newEditorState.selectedViews,
    // metadata: store.editor.jsxMetadata, // We can add metadata back if live metadata is necessary
    projectContents: newEditorState.projectContents,
    openFile: newEditorState.canvas.openFile?.filename,
    scale: newEditorState.canvas.scale,
    canvasOffset: newEditorState.canvas.roundedCanvasOffset,
  }
  const interactionState = newEditorState.canvas.interactionState
  if (interactionState == null) {
    return {
      unpatchedEditorState: newEditorState,
      patchedEditorState: newEditorState,
      newSessionStateState: withClearedSession,
    }
  } else {
    // Determine the new canvas strategy to run this time around.
    const { strategy, previousStrategy } = findCanvasStrategy(
      canvasState,
      interactionState,
      withClearedSession,
      result.sessionStateState.currentStrategy,
    )

    // If there is a current strategy, produce the commands from it.
    if (strategy != null && newEditorState.canvas.interactionState != null) {
      const commands = applyCanvasStrategy(
        strategy.strategy,
        canvasState,
        newEditorState.canvas.interactionState,
        result.sessionStateState,
      )
      const commandResult = foldAndApplyCommands(
        newEditorState,
        storedState.patchedEditor,
        storedState.sessionStateState.strategyState,
        commands,
        'transient',
      )
      const newSessionStateState: SessionStateState = {
        currentStrategy: strategy.strategy.name,
        currentStrategyFitness: strategy.fitness,
        currentStrategyCommands: commands,
        accumulatedCommands: [],
        commandDescriptions: commandResult.commandDescriptions,
        strategyState: createEmptyStrategyState(),
        startingMetadata: newEditorState.jsxMetadata,
        originalMetadata: newEditorState.jsxMetadata,
      }

      return {
        unpatchedEditorState: newEditorState,
        patchedEditorState: commandResult.editorState,
        newSessionStateState: newSessionStateState,
      }
    } else {
      return {
        unpatchedEditorState: newEditorState,
        patchedEditorState: newEditorState,
        newSessionStateState: withClearedSession,
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
      interactionState: null,
    },
  }

  return {
    unpatchedEditorState: updatedEditorState,
    patchedEditorState: updatedEditorState,
    newSessionStateState: createEmptySessionStateState(updatedEditorState.jsxMetadata),
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
  const interactionState = newEditorState.canvas.interactionState
  if (interactionState == null) {
    return {
      unpatchedEditorState: newEditorState,
      patchedEditorState: newEditorState,
      newSessionStateState: result.sessionStateState,
    }
  } else {
    // Determine the new canvas strategy to run this time around.
    const { strategy, previousStrategy } = findCanvasStrategy(
      canvasState,
      interactionState,
      result.sessionStateState,
      result.sessionStateState.currentStrategy,
    )
    const strategyName = strategy?.strategy.name
    if (strategyName != result.unpatchedEditor.canvas.interactionState?.userPreferredStrategy) {
      console.warn(
        'Entered interactionUserChangedStrategy but the user preferred strategy is not applied',
      )
    }

    // If there is a current strategy, produce the commands from it.
    if (strategy != null && newEditorState.canvas.interactionState != null) {
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
        newEditorState.canvas.interactionState,
        result.sessionStateState,
      )
      const newAccumulatedCommands = [
        ...result.sessionStateState.accumulatedCommands,
        ...strategyChangedLogCommands,
      ]
      const commandResult = foldAndApplyCommands(
        newEditorState,
        storedState.patchedEditor,
        storedState.sessionStateState.strategyState,
        [...newAccumulatedCommands.flatMap((c) => c.commands), ...commands],
        'transient',
      )
      const newSessionStateState: SessionStateState = {
        currentStrategy: strategy.strategy.name,
        currentStrategyFitness: strategy.fitness,
        currentStrategyCommands: commands,
        accumulatedCommands: newAccumulatedCommands,
        commandDescriptions: commandResult.commandDescriptions,
        strategyState: createEmptyStrategyState(),
        startingMetadata: result.sessionStateState.startingMetadata,
        originalMetadata: result.sessionStateState.originalMetadata,
      }

      return {
        unpatchedEditorState: newEditorState,
        patchedEditorState: commandResult.editorState,
        newSessionStateState: newSessionStateState,
      }
    } else {
      return {
        unpatchedEditorState: newEditorState,
        patchedEditorState: newEditorState,
        newSessionStateState: result.sessionStateState,
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
  const interactionState = newEditorState.canvas.interactionState
  if (interactionState == null) {
    return {
      unpatchedEditorState: newEditorState,
      patchedEditorState: newEditorState,
      newSessionStateState: result.sessionStateState,
    }
  } else {
    // Determine the new canvas strategy to run this time around.
    const { strategy, previousStrategy } = findCanvasStrategy(
      canvasState,
      interactionState,
      result.sessionStateState,
      result.sessionStateState.currentStrategy,
    )
    const strategyName = strategy?.strategy.name
    if (strategyName === result.sessionStateState.currentStrategy) {
      console.warn("Entered interactionStrategyChangeStacked but the strategy haven't changed")
    }

    // If there is a current strategy, produce the commands from it.
    if (strategy != null && newEditorState.canvas.interactionState != null) {
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
        newEditorState.canvas.interactionState,
        result.sessionStateState,
      )
      const newAccumulatedCommands = [
        ...result.sessionStateState.accumulatedCommands,
        {
          strategy: result.sessionStateState.currentStrategy,
          commands: result.sessionStateState.currentStrategyCommands,
        },
        ...strategyChangedLogCommands,
      ]
      const commandResult = foldAndApplyCommands(
        newEditorState,
        storedState.patchedEditor,
        storedState.sessionStateState.strategyState,
        [...newAccumulatedCommands.flatMap((c) => c.commands), ...commands],
        'transient',
      )
      const newSessionStateState: SessionStateState = {
        currentStrategy: strategy.strategy.name,
        currentStrategyFitness: strategy.fitness,
        currentStrategyCommands: commands,
        accumulatedCommands: newAccumulatedCommands,
        commandDescriptions: commandResult.commandDescriptions,
        strategyState: createEmptyStrategyState(),
        startingMetadata: result.sessionStateState.startingMetadata,
        originalMetadata: result.sessionStateState.originalMetadata,
      }

      const patchedEditorState = {
        ...newEditorState,
        canvas: {
          ...newEditorState.canvas,
          interactionState: strategySwitchInteractionStateReset(
            newEditorState.canvas.interactionState,
          ),
        },
      }

      return {
        unpatchedEditorState: newEditorState,
        patchedEditorState: patchedEditorState,
        newSessionStateState: newSessionStateState,
      }
    } else {
      return {
        unpatchedEditorState: newEditorState,
        patchedEditorState: newEditorState,
        newSessionStateState: result.sessionStateState,
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
  const { unpatchedEditorState, patchedEditorState, newSessionStateState } = handleStrategiesInner(
    dispatchedActions,
    storedState,
    result,
  )

  const patchedDerivedState = deriveState(patchedEditorState, oldDerivedState)

  return {
    unpatchedEditorState,
    patchedEditorState,
    patchedDerivedState,
    newSessionStateState,
  }
}

function handleStrategiesInner(
  dispatchedActions: readonly EditorAction[],
  storedState: EditorStoreFull,
  result: InnerDispatchResult,
): HandleStrategiesResult {
  const makeChangesPermanent = dispatchedActions.some(shouldApplyClearInteractionStateResult)
  const cancelInteraction = dispatchedActions.some(isClearInteractionState) && !makeChangesPermanent
  if (storedState.unpatchedEditor.canvas.interactionState == null) {
    if (result.unpatchedEditor.canvas.interactionState == null) {
      return {
        unpatchedEditorState: result.unpatchedEditor,
        patchedEditorState: result.unpatchedEditor,
        newSessionStateState: result.sessionStateState,
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
          storedState.unpatchedEditor.canvas.interactionState?.interactionData ?? null,
          result.unpatchedEditor.canvas.interactionState?.interactionData ?? null,
        ) || result.sessionStateState.currentStrategy == null // TODO: do we really need the currentStrategy == null part?
      if (interactionHardResetNeeded) {
        return interactionHardReset(storedState, result)
      } else {
        if (result.unpatchedEditor.canvas.interactionState?.userPreferredStrategy != null) {
          const userChangedStrategy =
            result.unpatchedEditor.canvas.interactionState?.userPreferredStrategy !=
            storedState.unpatchedEditor.canvas.interactionState?.userPreferredStrategy
          if (userChangedStrategy) {
            return interactionUserChangedStrategy(storedState, result)
          }
        }

        const strategy = findCanvasStrategyFromDispatchResult(result)
        if (strategy?.strategy.name !== result.sessionStateState.currentStrategy) {
          return interactionStrategyChangeStacked(storedState, result)
        }

        return interactionUpdate(storedState, result)
      }
    }
  }
}
