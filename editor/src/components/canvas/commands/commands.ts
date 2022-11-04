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
  AppendElementsToRerenderCommand,
  runAppendElementsToRerender,
  runSetElementsToRerender,
  SetElementsToRerenderCommand,
} from './set-elements-to-rerender-command'
import { DuplicateElement, runDuplicateElement } from './duplicate-element-command'
import { runUpdateFunctionCommand, UpdateFunctionCommand } from './update-function-command'
import { runPushIntendedBounds, PushIntendedBounds } from './push-intended-bounds-command'
import { DeleteProperties, runDeleteProperties } from './delete-properties-command'
import { AddImportsToFile, runAddImportsToFile } from './add-imports-to-file-command'
import { runSetProperty, SetProperty } from './set-property-command'
import {
  runAddToReparentedToPaths,
  AddToReparentedToPaths,
} from './add-to-reparented-to-paths-command'
import {
  InsertElementInsertionSubject,
  runInsertElementInsertionSubject,
} from './insert-element-insertion-subject'
import { AddElement, runAddElement } from './add-element-command'
import { runUpdatePropIfExists, UpdatePropIfExists } from './update-prop-if-exists-command'
import { HighlightElementsCommand, runHighlightElementsCommand } from './highlight-element-command'
import { InteractionLifecycle } from '../canvas-strategies/canvas-strategy-types'
import { runShowReorderIndicator, ShowReorderIndicator } from './show-reorder-indicator-command'

export interface CommandFunctionResult {
  editorStatePatches: Array<EditorStatePatch>
  commandDescription: string
}

export type CommandFunction<T> = (editorState: EditorState, command: T) => CommandFunctionResult

export type WhenToRun = 'mid-interaction' | 'always' | 'on-complete'

export interface BaseCommand {
  whenToRun: WhenToRun
}

export type CanvasCommand =
  | WildcardPatch
  | UpdateFunctionCommand
  | StrategySwitched
  | AdjustNumberProperty
  | AdjustCssLengthProperty
  | ReparentElement
  | DuplicateElement
  | UpdateSelectedViews
  | UpdateHighlightedViews
  | SetSnappingGuidelines
  | ConvertToAbsolute
  | SetCssLengthProperty
  | ReorderElement
  | ShowOutlineHighlight
  | ShowReorderIndicator
  | SetCursorCommand
  | SetElementsToRerenderCommand
  | AppendElementsToRerenderCommand
  | PushIntendedBounds
  | DeleteProperties
  | SetProperty
  | UpdatePropIfExists
  | AddImportsToFile
  | AddToReparentedToPaths
  | InsertElementInsertionSubject
  | AddElement
  | HighlightElementsCommand

export const runCanvasCommand = (
  editorState: EditorState,
  command: CanvasCommand,
  commandLifecycle: InteractionLifecycle,
): CommandFunctionResult => {
  switch (command.type) {
    case 'WILDCARD_PATCH':
      return runWildcardPatch(editorState, command)
    case 'UPDATE_FUNCTION_COMMAND':
      return runUpdateFunctionCommand(editorState, command, commandLifecycle)
    case 'STRATEGY_SWITCHED':
      return runStrategySwitchedCommand(command)
    case 'ADJUST_NUMBER_PROPERTY':
      return runAdjustNumberProperty(editorState, command)
    case 'ADJUST_CSS_LENGTH_PROPERTY':
      return runAdjustCssLengthProperty(editorState, command)
    case 'REPARENT_ELEMENT':
      return runReparentElement(editorState, command)
    case 'DUPLICATE_ELEMENT':
      return runDuplicateElement(editorState, command)
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
    case 'SHOW_REORDER_INDICATOR':
      return runShowReorderIndicator(editorState, command)
    case 'SET_CURSOR_COMMAND':
      return runSetCursor(editorState, command)
    case 'SET_ELEMENTS_TO_RERENDER_COMMAND':
      return runSetElementsToRerender(editorState, command)
    case 'APPEND_ELEMENTS_TO_RERENDER_COMMAND':
      return runAppendElementsToRerender(editorState, command)
    case 'PUSH_INTENDED_BOUNDS':
      return runPushIntendedBounds(editorState, command)
    case 'DELETE_PROPERTIES':
      return runDeleteProperties(editorState, command)
    case 'SET_PROPERTY':
      return runSetProperty(editorState, command)
    case 'UPDATE_PROP_IF_EXISTS':
      return runUpdatePropIfExists(editorState, command)
    case 'ADD_IMPORTS_TO_FILE':
      return runAddImportsToFile(editorState, command)
    case 'ADD_TO_REPARENTED_TO_PATHS':
      return runAddToReparentedToPaths(editorState, command)
    case 'INSERT_ELEMENT_INSERTION_SUBJECT':
      return runInsertElementInsertionSubject(editorState, command)
    case 'ADD_ELEMENT':
      return runAddElement(editorState, command)
    case 'HIGHLIGHT_ELEMENTS_COMMAND':
      return runHighlightElementsCommand(editorState, command)
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
    const patches = runCanvasCommand(workingEditorState, command, 'end-interaction')
    return updateEditorStateWithPatches(workingEditorState, patches.editorStatePatches)
  }, editorState)

  return updatedEditorState
}

export function foldAndApplyCommandsInner(
  editorState: EditorState,
  commandsToAccumulate: Array<CanvasCommand>,
  commands: Array<CanvasCommand>,
  commandLifecycle: InteractionLifecycle,
): {
  statePatches: EditorStatePatch[]
  updatedEditorState: EditorState
  commandDescriptions: CommandDescription[]
} {
  let statePatches: Array<EditorStatePatch> = []
  let workingEditorState: EditorState = editorState
  let workingCommandDescriptions: Array<CommandDescription> = []

  const runCommand = (command: CanvasCommand, shouldAccumulatePatches: boolean) => {
    let shouldRunCommand: boolean
    if (commandLifecycle === 'mid-interaction') {
      shouldRunCommand = command.whenToRun === 'always' || command.whenToRun === 'mid-interaction'
    } else {
      shouldRunCommand = command.whenToRun === 'always' || command.whenToRun === 'on-complete'
    }

    if (shouldRunCommand) {
      // Run the command with our current states.
      const commandResult = runCanvasCommand(workingEditorState, command, commandLifecycle)
      // Capture values from the result.
      const statePatch = commandResult.editorStatePatches
      // Apply the update to the editor state.
      workingEditorState = updateEditorStateWithPatches(workingEditorState, statePatch)
      // Collate the patches.
      statePatches.push(...statePatch)
      workingCommandDescriptions.push({
        description: commandResult.commandDescription,
        transient: command.whenToRun === 'mid-interaction',
      })
    }
  }

  commandsToAccumulate.forEach((command) => runCommand(command, true))
  commands.forEach((command) => runCommand(command, false))

  return {
    statePatches: statePatches,
    updatedEditorState: workingEditorState,
    commandDescriptions: workingCommandDescriptions,
  }
}

export function foldAndApplyCommands(
  editorState: EditorState,
  priorPatchedState: EditorState,
  commandsToAccumulate: Array<CanvasCommand>,
  commands: Array<CanvasCommand>,
  commandLifecycle: InteractionLifecycle,
): {
  editorState: EditorState
  commandDescriptions: Array<CommandDescription>
} {
  const { statePatches, updatedEditorState, commandDescriptions } = foldAndApplyCommandsInner(
    editorState,
    commandsToAccumulate,
    commands,
    commandLifecycle,
  )

  let workingEditorState = updatedEditorState
  if (statePatches.length === 0) {
    workingEditorState = editorState
  } else {
    workingEditorState = EditorStateKeepDeepEquality(priorPatchedState, workingEditorState).value
  }

  return {
    editorState: workingEditorState,
    commandDescriptions: commandDescriptions,
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
