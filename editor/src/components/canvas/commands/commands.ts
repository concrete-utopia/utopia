import update from 'immutability-helper'
import { ElementPath } from '../../../core/shared/project-file-types'
import { keepDeepReferenceEqualityIfPossible } from '../../../utils/react-performance'
import { EditorState, EditorStatePatch } from '../../editor/store/editor-state'
import { CommandDescription } from '../canvas-strategies/interaction-state'
import { AdjustNumberProperty, runAdjustNumberProperty } from './adjust-number-command'
import { ReparentElement, runReparentElement } from './reparent-element-command'
import { runStrategySwitchedCommand, StrategySwitched } from './strategy-switched-command'
import { runUpdateSelectedViews, UpdateSelectedViews } from './update-selected-views-command'
import { runWildcardPatch, WildcardPatch } from './wildcard-patch-command'

export interface CommandFunctionResult {
  editorStatePatch: EditorStatePatch
  commandDescription: string
}

export type CommandFunction<T> = (editorState: EditorState, command: T) => CommandFunctionResult

export type TransientOrNot = 'transient' | 'permanent'

export interface BaseCommand {
  transient: TransientOrNot
}

export type CanvasCommand =
  | WildcardPatch
  | StrategySwitched
  | AdjustNumberProperty
  | ReparentElement
  | UpdateSelectedViews

export const runCanvasCommand: CommandFunction<CanvasCommand> = (
  editorState: EditorState,
  command: CanvasCommand,
) => {
  switch (command.type) {
    case 'WILDCARD_PATCH':
      return runWildcardPatch(editorState, command)
    case 'STRATEGY_SWITCHED':
      return runStrategySwitchedCommand(command)
    case 'ADJUST_NUMBER_PROPERTY':
      return runAdjustNumberProperty(editorState, command)
    case 'REPARENT_ELEMENT':
      return runReparentElement(editorState, command)
    case 'UPDATE_SELECTED_VIEWS':
      return runUpdateSelectedViews(editorState, command)
    default:
      const _exhaustiveCheck: never = command
      throw new Error(`Unhandled canvas command ${JSON.stringify(command)}`)
  }
}

export function foldAndApplyCommands(
  editorState: EditorState,
  priorPatchedState: EditorState,
  commands: Array<CanvasCommand>,
  transient: TransientOrNot,
): {
  editorState: EditorState
  editorStatePatches: Array<EditorStatePatch>
  commandDescriptions: Array<CommandDescription>
} {
  const commandResult = foldCommands(editorState, commands, transient)
  const updatedEditorState = applyStatePatches(
    editorState,
    priorPatchedState,
    commandResult.editorStatePatches,
  )
  return {
    editorState: updatedEditorState,
    editorStatePatches: commandResult.editorStatePatches,
    commandDescriptions: commandResult.commandDescriptions,
  }
}

function foldCommands(
  editorState: EditorState,
  commands: Array<CanvasCommand>,
  transient: TransientOrNot,
): {
  editorStatePatches: Array<EditorStatePatch>
  commandDescriptions: Array<CommandDescription>
} {
  let statePatches: Array<EditorStatePatch> = []
  let workingEditorState: EditorState = editorState
  let workingCommandDescriptions: Array<CommandDescription> = []
  for (const command of commands) {
    // Allow every command if this is a transient fold, otherwise only allow commands that are not transient.
    if (transient === 'transient' || command.transient === 'permanent') {
      // Run the command with our current states.
      const commandResult = runCanvasCommand(workingEditorState, command)
      // Capture values from the result.
      const statePatch = commandResult.editorStatePatch
      // Apply the update to the editor state.
      workingEditorState = update(workingEditorState, statePatch)
      // Collate the patches.
      statePatches.push(statePatch)
      workingCommandDescriptions.push({
        description: commandResult.commandDescription,
        transient: command.transient === 'transient',
      })
    }
  }

  return {
    editorStatePatches: statePatches,
    commandDescriptions: workingCommandDescriptions,
  }
}

export function applyStatePatches(
  editorState: EditorState,
  priorPatchedState: EditorState,
  patches: Array<EditorStatePatch>,
): EditorState {
  if (patches.length === 0) {
    return editorState
  } else {
    return keepDeepReferenceEqualityIfPossible(
      priorPatchedState,
      patches.reduce((workingState, patch) => {
        return update(workingState, patch)
      }, editorState),
    )
  }
}
