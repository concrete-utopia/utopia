import update, { Spec } from 'immutability-helper'
import { applyUtopiaJSXComponentsChanges } from '../../../core/model/project-file-utils'
import { drop } from '../../../core/shared/array-utils'
import { TopLevelElement, UtopiaJSXComponent } from '../../../core/shared/element-template'
import { Imports, RevisionsState } from '../../../core/shared/project-file-types'
import { keepDeepReferenceEqualityIfPossible } from '../../../utils/react-performance'
import {
  getProjectContentKeyPathElements,
  ProjectContentFile,
  ProjectContentsTree,
  ProjectContentTreeRoot,
} from '../../assets'
import { EditorState, EditorStatePatch } from '../../editor/store/editor-state'
import { CommandDescription } from '../canvas-strategies/interaction-state'
import { AdjustCssLengthProperty, runAdjustCssLengthProperty } from './adjust-css-length-command'
import { AdjustNumberProperty, runAdjustNumberProperty } from './adjust-number-command'
import { ConvertToAbsolute, runConvertToAbsolute } from './convert-to-absolute-command'
import { mergePatches } from './merge-patches'
import { ReorderElement, runReorderElement } from './reorder-element-command'
import { ReparentElement, runReparentElement } from './reparent-element-command'
import { runSetSnappingGuidelines, SetSnappingGuidelines } from './set-snapping-guidelines-command'
import { runStrategySwitchedCommand, StrategySwitched } from './strategy-switched-command'
import {
  runUpdateHighlightedViews,
  UpdateHighlightedViews,
} from './update-highlighted-views-command'
import { runUpdateSelectedViews, UpdateSelectedViews } from './update-selected-views-command'
import { runWildcardPatch, WildcardPatch } from './wildcard-patch-command'
import { runSetCssLengthProperty, SetCssLengthProperty } from './set-css-length-command'
import { EditorStateKeepDeepEquality } from '../../editor/store/store-deep-equality-instances'
import { runShowOutlineHighlight, ShowOutlineHighlight } from './show-outline-highlight-command'
import { runSetCursor, SetCursorCommand } from './set-cursor-command'
import {
  runSetElementsToRerender,
  SetElementsToRerenderCommand,
} from './set-elements-to-rerender-command'

export interface CommandFunctionResult {
  editorStatePatches: Array<EditorStatePatch>
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
  | ConvertToAbsolute
  | SetCssLengthProperty
  | ReorderElement
  | ShowOutlineHighlight
  | SetCursorCommand
  | SetElementsToRerenderCommand

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
    case 'CONVERT_TO_ABSOLUTE':
      return runConvertToAbsolute(editorState, command)
    case 'SET_CSS_LENGTH_PROPERTY':
      return runSetCssLengthProperty(editorState, command)
    case 'REORDER_ELEMENT':
      return runReorderElement(editorState, command)
    case 'SHOW_OUTLINE_HIGHLIGHT':
      return runShowOutlineHighlight(editorState, command)
    case 'SET_CURSOR_COMMAND':
      return runSetCursor(editorState, command)
    case 'SET_ELEMENTS_TO_RERENDER_COMMAND':
      return runSetElementsToRerender(editorState, command)
    default:
      const _exhaustiveCheck: never = command
      throw new Error(`Unhandled canvas command ${JSON.stringify(command)}`)
  }
}

export function foldAndApplyCommandsSimple(
  editorState: EditorState,
  commands: Array<CanvasCommand>,
): EditorState {
  const updatedEditorState = commands.reduce((workingEditorState, command) => {
    const patches = runCanvasCommand(workingEditorState, command)
    return updateEditorStateWithPatches(workingEditorState, patches.editorStatePatches)
  }, editorState)

  return updatedEditorState
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
      const statePatch = commandResult.editorStatePatches
      // Apply the update to the editor state.
      workingEditorState = updateEditorStateWithPatches(workingEditorState, statePatch)
      // Collate the patches.
      statePatches.push(...statePatch)
      if (shouldAccumulatePatches) {
        accumulatedPatches.push(...statePatch)
      }
      workingCommandDescriptions.push({
        description: commandResult.commandDescription,
        transient: command.transient === 'transient',
      })
    }
  }

  commandsToAccumulate.forEach((command) => runCommand(command, true))
  commands.forEach((command) => runCommand(command, false))

  if (statePatches.length === 0) {
    workingEditorState = editorState
  } else {
    workingEditorState = EditorStateKeepDeepEquality(priorPatchedState, workingEditorState).value
  }

  return {
    editorState: workingEditorState,
    accumulatedPatches: mergePatches(accumulatedPatches),
    commandDescriptions: workingCommandDescriptions,
  }
}

export function updateEditorStateWithPatches(
  state: EditorState,
  patches: Array<EditorStatePatch>,
): EditorState {
  return patches.reduce((acc, curr) => {
    return update(acc, curr)
  }, state)
}

export function getPatchForComponentChange(
  topLevelElements: Array<TopLevelElement>,
  newUtopiaComponents: Array<UtopiaJSXComponent>,
  imports: Imports,
  filePath: string,
): EditorStatePatch {
  const updatedTopLevelElements = applyUtopiaJSXComponentsChanges(
    topLevelElements,
    newUtopiaComponents,
  )
  const projectContentFilePatch: Spec<ProjectContentFile> = {
    content: {
      fileContents: {
        revisionsState: {
          $set: RevisionsState.ParsedAhead,
        },
        parsed: {
          topLevelElements: {
            $set: updatedTopLevelElements,
          },
          imports: {
            $set: imports,
          },
        },
      },
    },
  }
  // ProjectContentTreeRoot is a bit awkward to patch.
  const pathElements = getProjectContentKeyPathElements(filePath)
  if (pathElements.length === 0) {
    throw new Error('Invalid path length.')
  }
  const remainderPath = drop(1, pathElements)
  const projectContentsTreePatch: Spec<ProjectContentsTree> = remainderPath.reduceRight(
    (working: Spec<ProjectContentsTree>, pathPart: string) => {
      return {
        children: {
          [pathPart]: working,
        },
      }
    },
    projectContentFilePatch,
  )

  // Finally patch the last part of the path in.
  const projectContentTreeRootPatch: Spec<ProjectContentTreeRoot> = {
    [pathElements[0]]: projectContentsTreePatch,
  }

  return {
    projectContents: projectContentTreeRootPatch,
  }
}
