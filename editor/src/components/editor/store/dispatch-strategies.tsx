import {
  SessionStateState,
  createEmptySessionStateState,
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
  newSessionStateState: SessionStateState
}

export function interactionFinished(
  storedState: EditorStoreFull,
  result: InnerDispatchResult,
): HandleStrategiesResult {
  const newEditorState = result.unpatchedEditor
  const withClearedSession = createEmptySessionStateState(
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
      newSessionStateState: withClearedSession,
    }
  } else {
    // Determine the new canvas strategy to run this time around.
    const { strategy } = findCanvasStrategy(
      canvasState,
      interactionSession,
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
        interactionSession,
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
      newSessionStateState: withClearedSession,
    }
  } else {
    const resetInteractionSession = interactionSessionHardReset(interactionSession)
    const resetSessionState = {
      ...result.sessionStateState,
      startingMetadata: storedState.unpatchedEditor.jsxMetadata,
    }
    // Determine the new canvas strategy to run this time around.
    const { strategy, previousStrategy } = findCanvasStrategy(
      canvasState,
      resetInteractionSession,
      resetSessionState,
      resetSessionState.currentStrategy,
    )

    // If there is a current strategy, produce the commands from it.
    if (strategy != null && newEditorState.canvas.interactionSession != null) {
      const commands = applyCanvasStrategy(
        strategy.strategy,
        canvasState,
        newEditorState.canvas.interactionSession,
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
  const interactionSession = newEditorState.canvas.interactionSession
  if (interactionSession == null) {
    return {
      unpatchedEditorState: newEditorState,
      patchedEditorState: newEditorState,
      newSessionStateState: result.sessionStateState,
    }
  } else {
    // Determine the new canvas strategy to run this time around.
    const { strategy } = findCanvasStrategy(
      canvasState,
      interactionSession,
      result.sessionStateState,
      result.sessionStateState.currentStrategy,
    )

    // If there is a current strategy, produce the commands from it.
    if (strategy != null && newEditorState.canvas.interactionSession != null) {
      const commands = applyCanvasStrategy(
        strategy.strategy,
        canvasState,
        newEditorState.canvas.interactionSession,
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
  const withClearedSession = createEmptySessionStateState(
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
      newSessionStateState: withClearedSession,
    }
  } else {
    // Determine the new canvas strategy to run this time around.
    const { strategy, previousStrategy } = findCanvasStrategy(
      canvasState,
      interactionSession,
      withClearedSession,
      result.sessionStateState.currentStrategy,
    )

    // If there is a current strategy, produce the commands from it.
    if (strategy != null && newEditorState.canvas.interactionSession != null) {
      const commands = applyCanvasStrategy(
        strategy.strategy,
        canvasState,
        newEditorState.canvas.interactionSession,
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
        startingMetadata: newEditorState.canvas.interactionSession.metadata,
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
      interactionSession: null,
    },
  }

  return {
    unpatchedEditorState: updatedEditorState,
    patchedEditorState: updatedEditorState,
    newSessionStateState: createEmptySessionStateState(),
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
      newSessionStateState: result.sessionStateState,
    }
  } else {
    // Determine the new canvas strategy to run this time around.
    const { strategy, previousStrategy } = findCanvasStrategy(
      canvasState,
      interactionSession,
      result.sessionStateState,
      result.sessionStateState.currentStrategy,
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
  const interactionSession = newEditorState.canvas.interactionSession
  if (interactionSession == null) {
    return {
      unpatchedEditorState: newEditorState,
      patchedEditorState: newEditorState,
      newSessionStateState: result.sessionStateState,
    }
  } else {
    // Determine the new canvas strategy to run this time around.
    const { strategy, previousStrategy } = findCanvasStrategy(
      canvasState,
      interactionSession,
      result.sessionStateState,
      result.sessionStateState.currentStrategy,
    )
    const strategyName = strategy?.strategy.name
    if (strategyName === result.sessionStateState.currentStrategy) {
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

  const patchedEditorWithMetadata: EditorState = {
    ...patchedEditorState,
    jsxMetadata:
      patchedEditorState.canvas.interactionSession?.metadata ?? patchedEditorState.jsxMetadata,
  }

  const patchedDerivedState = deriveState(patchedEditorWithMetadata, oldDerivedState)

  return {
    unpatchedEditorState,
    patchedEditorState: patchedEditorWithMetadata,
    patchedDerivedState,
    newSessionStateState,
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
          storedState.unpatchedEditor.canvas.interactionSession?.interactionData ?? null,
          result.unpatchedEditor.canvas.interactionSession?.interactionData ?? null,
        ) || result.sessionStateState.currentStrategy == null // TODO: do we really need the currentStrategy == null part?
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
        if (strategy?.strategy.name !== result.sessionStateState.currentStrategy) {
          return interactionStrategyChangeStacked(storedState, result)
        }

        return interactionUpdate(storedState, result)
      }
    }
  }
}
