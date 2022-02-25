import update from 'immutability-helper'
import { ElementPath } from '../../../core/shared/project-file-types'
import { keepDeepReferenceEqualityIfPossible } from '../../../utils/react-performance'
import { EditorState, EditorStatePatch } from '../../editor/store/editor-state'
import { CommandDescription } from '../canvas-strategies/interaction-state'

export interface PathMapping {
  from: ElementPath
  to: ElementPath
}

export type PathMappings = Array<PathMapping>

export interface CommandFunctionResult {
  editorStatePatch: EditorStatePatch
  pathMappings: PathMappings
  commandDescription: string
}

export type CommandFunction<T> = (
  editorState: EditorState,
  pathMappings: PathMappings,
  command: T,
) => CommandFunctionResult

export type TransientOrNot = 'transient' | 'permanent'

export interface BaseCommand {
  transient: TransientOrNot
}

export interface WildcardPatch extends BaseCommand {
  type: 'WILDCARD_PATCH'
  patch: EditorStatePatch
}

export function wildcardPatch(transient: TransientOrNot, patch: EditorStatePatch): WildcardPatch {
  return {
    type: 'WILDCARD_PATCH',
    transient: transient,
    patch: patch,
  }
}

export interface StrategySwitched extends BaseCommand {
  type: 'STRATEGY_SWITCHED'
  reason: 'automatic' | 'user-input'
  newStrategy: string
  accumulatedCommands: boolean
  dataReset: boolean
  previousFitness: number
  newFitness: number
}

export function strategySwitched(
  reason: 'automatic' | 'user-input',
  newStrategy: string,
  accumulatedCommands: boolean,
  dataReset: boolean,
  previousFitness: number,
  newFitness: number,
): StrategySwitched {
  return {
    type: 'STRATEGY_SWITCHED',
    transient: 'transient',
    reason,
    newStrategy,
    accumulatedCommands,
    dataReset,
    previousFitness,
    newFitness,
  }
}

export type CanvasCommand = WildcardPatch | StrategySwitched

export const runWildcardPatch: CommandFunction<WildcardPatch> = (
  editorState: EditorState,
  pathMappings: PathMappings,
  command: WildcardPatch,
) => {
  return {
    editorStatePatch: command.patch,
    pathMappings: pathMappings,
    commandDescription: `Wildcard Patch: ${JSON.stringify(command.patch, null, 2)}`,
  }
}

function runStrategySwitchedCommand(
  pathMappings: PathMappings,
  command: StrategySwitched,
): CommandFunctionResult {
  let commandDescription: string = `Strategy switched to ${command.newStrategy} ${
    command.reason === 'automatic'
      ? `automatically (fitness ${command.previousFitness} -> ${command.newFitness})`
      : 'by user input'
  }. ${command.dataReset ? 'Interaction data reset.' : ''}`

  return {
    editorStatePatch: {},
    pathMappings: pathMappings,
    commandDescription: commandDescription,
  }
}

export const runCanvasCommand: CommandFunction<CanvasCommand> = (
  editorState: EditorState,
  pathMappings: PathMappings,
  command: CanvasCommand,
) => {
  switch (command.type) {
    case 'WILDCARD_PATCH':
      return runWildcardPatch(editorState, pathMappings, command)
    case 'STRATEGY_SWITCHED':
      return runStrategySwitchedCommand(pathMappings, command)
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
  let workingPathMappings: PathMappings = []
  let workingCommandDescriptions: Array<CommandDescription> = []
  for (const command of commands) {
    // Allow every command if this is a transient fold, otherwise only allow commands that are not transient.
    if (transient === 'transient' || command.transient === 'permanent') {
      // Run the command with our current states.
      const commandResult = runCanvasCommand(workingEditorState, workingPathMappings, command)
      // Capture values from the result.
      const statePatch = commandResult.editorStatePatch
      workingPathMappings = commandResult.pathMappings
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
