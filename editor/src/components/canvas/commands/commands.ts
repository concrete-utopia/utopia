import update from 'immutability-helper'
import { ElementPath } from '../../../core/shared/project-file-types'
import { keepDeepReferenceEqualityIfPossible } from '../../../utils/react-performance'
import { Canvas } from '../../editor/action-types'
import { EditorState, EditorStatePatch } from '../../editor/store/editor-state'
import { CommandDescription } from '../canvas-strategies/interaction-state'
import { AdjustCssLengthProperty, runAdjustCssLengthProperty } from './adjust-css-length-command'
import { AdjustNumberProperty, runAdjustNumberProperty } from './adjust-number-command'
import { mergePatches } from './merge-patches'
import { ReparentElement, runReparentElement } from './reparent-element-command'
import { runSetSnappingGuidelines, SetSnappingGuidelines } from './set-snapping-guidelines-command'
import { runStrategySwitchedCommand, StrategySwitched } from './strategy-switched-command'
import {
  runUpdateHighlightedViews,
  UpdateHighlightedViews,
} from './update-highlighted-views-command'
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
  | AdjustCssLengthProperty
  | ReparentElement
  | UpdateSelectedViews
  | UpdateHighlightedViews
  | SetSnappingGuidelines

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
    case 'ADJUST_CSS_LENGTH_PROPERTY':
      return runAdjustCssLengthProperty(editorState, command)
    case 'REPARENT_ELEMENT':
      return runReparentElement(editorState, command)
    case 'UPDATE_SELECTED_VIEWS':
      return runUpdateSelectedViews(editorState, command)
    case 'UPDATE_HIGHLIGHTED_VIEWS':
      return runUpdateHighlightedViews(editorState, command)
    case 'SET_SNAPPING_GUIDELINES':
      return runSetSnappingGuidelines(editorState, command)
    default:
      const _exhaustiveCheck: never = command
      throw new Error(`Unhandled canvas command ${JSON.stringify(command)}`)
  }
}

export function foldAndApplyCommands(
  editorState: EditorState,
  priorPatchedState: EditorState,
  patches: Array<EditorStatePatch>,
  commandsToAccumulate: Array<CanvasCommand>,
  commands: Array<CanvasCommand>,
  transient: TransientOrNot,
): {
  editorState: EditorState
  accumulatedPatches: Array<EditorStatePatch>
  commandDescriptions: Array<CommandDescription>
} {
  const commandResult = foldCommands(
    editorState,
    patches,
    commandsToAccumulate,
    commands,
    transient,
  )
  const updatedEditorState = applyStatePatches(
    editorState,
    priorPatchedState,
    commandResult.editorStatePatches,
  )
  return {
    editorState: updatedEditorState,
    accumulatedPatches: commandResult.accumulatedPatches,
    commandDescriptions: commandResult.commandDescriptions,
  }
}

function foldCommands(
  editorState: EditorState,
  patches: Array<EditorStatePatch>,
  commandsToAccumulate: Array<CanvasCommand>,
  commands: Array<CanvasCommand>,
  transient: TransientOrNot,
): {
  editorStatePatches: Array<EditorStatePatch>
  accumulatedPatches: Array<EditorStatePatch>
  commandDescriptions: Array<CommandDescription>
} {
  let statePatches: Array<EditorStatePatch> = [...patches]
  let accumulatedPatches: Array<EditorStatePatch> = [...patches]
  let workingEditorState: EditorState = patches.reduce((workingState, patch) => {
    return update(workingState, patch)
  }, editorState)
  let workingCommandDescriptions: Array<CommandDescription> = []

  const runCommand = (command: CanvasCommand, shouldAccumulatePatches: boolean) => {
    if (transient === 'transient' || command.transient === 'permanent') {
      // Run the command with our current states.
      const commandResult = runCanvasCommand(workingEditorState, command)
      // Capture values from the result.
      const statePatch = commandResult.editorStatePatch
      // Apply the update to the editor state.
      workingEditorState = update(workingEditorState, statePatch)
      // Collate the patches.
      statePatches.push(statePatch)
      if (shouldAccumulatePatches) {
        accumulatedPatches.push(statePatch)
      }
      workingCommandDescriptions.push({
        description: commandResult.commandDescription,
        transient: command.transient === 'transient',
      })
    }
  }

  commandsToAccumulate.forEach((command) => runCommand(command, true))
  commands.forEach((command) => runCommand(command, false))

  return {
    editorStatePatches: mergePatches(statePatches),
    accumulatedPatches: mergePatches(accumulatedPatches),
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
