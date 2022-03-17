import * as Diff from 'diff'
import { produceWithPatches, enablePatches, Patch, applyPatches } from 'immer'
import update from 'immutability-helper'
import { ElementPath, TextFile } from '../../../core/shared/project-file-types'
import { keepDeepReferenceEqualityIfPossible } from '../../../utils/react-performance'
import { ProjectContentDirectory, ProjectContentFile } from '../../assets'
import { EditorState, EditorStatePatch } from '../../editor/store/editor-state'
import { CommandDescription } from '../canvas-strategies/interaction-state'
import { AdjustCssLengthProperty, runAdjustCssLengthProperty } from './adjust-css-length-command'
import { AdjustNumberProperty, runAdjustNumberProperty } from './adjust-number-command'
import { ReparentElement, runReparentElement } from './reparent-element-command'
import { runSetSnappingGuidelines, SetSnappingGuidelines } from './set-snapping-guidelines-command'
import { runStrategySwitchedCommand, StrategySwitched } from './strategy-switched-command'
import {
  runUpdateHighlightedViews,
  UpdateHighlightedViews,
} from './update-highlighted-views-command'
import { runUpdateSelectedViews, UpdateSelectedViews } from './update-selected-views-command'
import { runWildcardPatch, WildcardPatch } from './wildcard-patch-command'
import { abstractEquals } from "../../../core/shared/equality-utils"
enablePatches()
export interface CommandFunctionResult {
  editorStatePatch: Array<Patch>
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
  patches: Array<Patch>,
  commands: Array<CanvasCommand>,
  transient: TransientOrNot,
): {
  editorState: EditorState
  editorStatePatches: Array<Patch>
  commandDescriptions: Array<CommandDescription>
} {
  const commandResult = foldCommands(editorState, patches, commands, transient)
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
  patches: Array<Patch>,
  commands: Array<CanvasCommand>,
  transient: TransientOrNot,
): {
  editorStatePatches: Array<Patch>
  commandDescriptions: Array<CommandDescription>
} {
  let workingEditorState: EditorState = applyPatches(editorState, patches)
  let workingCommandDescriptions: Array<CommandDescription> = []
  let statePatches: Array<Patch> = []

  for (const command of commands) {
    // Allow every command if this is a transient fold, otherwise only allow commands that are not transient.
    if (transient === 'transient' || command.transient === 'permanent') {
      // Run the command with our current states.
      const commandResult = runCanvasCommand(workingEditorState, command)
      // Capture values from the result.
      const statePatch = commandResult.editorStatePatch
      // Apply the update to the editor state.
      workingEditorState = applyPatches(workingEditorState, statePatch)
      // Collate the patches.
      statePatches.push(...statePatch)
      workingCommandDescriptions.push({
        description: commandResult.commandDescription,
        transient: command.transient === 'transient',
      })
    }
  }

  // for (const command of commands) {
  //     // Allow every command if this is a transient fold, otherwise only allow commands that are not transient.
  //   if (transient === 'transient' || command.transient === 'permanent') {
  //     // Run the command with our current states.
  //     const commandResult = runCanvasCommand(patchedState, command)
  //     // Capture values from the result.
  //     const statePatch = commandResult.editorStatePatch
  //     statePatches.push(...stateP)
      
  //     // Collate the patches.
  //     workingCommandDescriptions.push({
  //       description: commandResult.commandDescription,
  //       transient: command.transient === 'transient',
  //     })
  //   }
  // }

  return {
    editorStatePatches: statePatches,
    commandDescriptions: workingCommandDescriptions,
  }
}

export function applyStatePatches(
  editorState: EditorState,
  priorPatchedState: EditorState,
  patches: Array<Patch>,
): EditorState {
  if (patches.length === 0) {
    return editorState
  } else {
    const myEq = (a: any, b: any) => abstractEquals(a, b, true, myEq)
    const diff = myEq(priorPatchedState, applyPatches(editorState, patches))
    console.log("DIFF", priorPatchedState, diff)
    return keepDeepReferenceEqualityIfPossible(
      priorPatchedState,
      applyPatches(editorState, patches),
    )
  }
}

