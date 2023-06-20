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
import {
  AdjustCssLengthProperties,
  runAdjustCssLengthProperties,
} from './adjust-css-length-command'
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
import {
  runPushIntendedBoundsAndUpdateGroups,
  PushIntendedBoundsAndUpdateGroups,
} from './push-intended-bounds-and-update-groups-command'
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
import {
  ConvertCssPercentToPx,
  runConvertCssPercentToPx,
} from './convert-css-percent-to-px-command'
import { HideInNavigatorCommand, runHideInNavigatorCommand } from './hide-in-navigator-command'
import { runShowToastCommand, ShowToastCommand } from './show-toast-command'
import {
  AddContainLayoutIfNeeded,
  runAddContainLayoutIfNeeded,
} from './add-contain-layout-if-needed-command'
import { RearrangeChildren, runRearrangeChildren } from './rearrange-children-command'
import { DeleteElement, runDeleteElement } from './delete-element-command'
import { runWrapInContainerCommand, WrapInContainerCommand } from './wrap-in-container-command'
import { patchProjectContentsWithParsedFile } from './patch-utils'

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
  | AdjustCssLengthProperties
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
  | PushIntendedBoundsAndUpdateGroups
  | DeleteProperties
  | SetProperty
  | UpdatePropIfExists
  | AddImportsToFile
  | AddToReparentedToPaths
  | InsertElementInsertionSubject
  | AddElement
  | HighlightElementsCommand
  | ConvertCssPercentToPx
  | HideInNavigatorCommand
  | ShowToastCommand
  | AddContainLayoutIfNeeded
  | RearrangeChildren
  | DeleteElement
  | WrapInContainerCommand

export function runCanvasCommand(
  editorState: EditorState,
  command: CanvasCommand,
  commandLifecycle: InteractionLifecycle,
): CommandFunctionResult {
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
      return runAdjustCssLengthProperties(editorState, command)
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
    case 'PUSH_INTENDED_BOUNDS_AND_UPDATE_GROUPS':
      return runPushIntendedBoundsAndUpdateGroups(editorState, command, commandLifecycle)
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
    case 'CONVERT_CSS_PERCENT_TO_PX':
      return runConvertCssPercentToPx(editorState, command)
    case 'HIDE_IN_NAVIGATOR_COMMAND':
      return runHideInNavigatorCommand(editorState, command)
    case 'SHOW_TOAST_COMMAND':
      return runShowToastCommand(editorState, command, commandLifecycle)
    case 'ADD_CONTAIN_LAYOUT_IF_NEEDED':
      return runAddContainLayoutIfNeeded(editorState, command)
    case 'REARRANGE_CHILDREN':
      return runRearrangeChildren(editorState, command)
    case 'DELETE_ELEMENT':
      return runDeleteElement(editorState, command)
    case 'WRAP_IN_CONTAINER':
      return runWrapInContainerCommand(editorState, command)
    default:
      const _exhaustiveCheck: never = command
      throw new Error(`Unhandled canvas command ${JSON.stringify(command)}`)
  }
}

export function foldAndApplyCommandsSimple(
  editorState: EditorState,
  commands: Array<CanvasCommand>,
): EditorState {
  const updatedEditorState = commands
    .filter((c) => c.whenToRun === 'always' || c.whenToRun === 'on-complete')
    .reduce((workingEditorState, command) => {
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

  function runCommand(command: CanvasCommand, shouldAccumulatePatches: boolean): void {
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

  const priorPatchedStateWithCurrentSpyValues: EditorState = {
    ...priorPatchedState,

    // List of parts of the editor state that we already know changed from the last frame, and are not usually affected by Commands
    _currentAllElementProps_KILLME: editorState._currentAllElementProps_KILLME,
    jsxMetadata: editorState.jsxMetadata,
    domMetadata: editorState.domMetadata,
    spyMetadata: editorState.spyMetadata,
    canvas: {
      ...priorPatchedState.canvas,
      interactionSession: editorState.canvas.interactionSession,
    },
  }

  let workingEditorState = updatedEditorState
  if (statePatches.length === 0) {
    workingEditorState = editorState
  } else {
    workingEditorState = EditorStateKeepDeepEquality(
      priorPatchedStateWithCurrentSpyValues,
      workingEditorState,
    ).value
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

  return patchProjectContentsWithParsedFile(filePath, {
    topLevelElements: {
      $set: updatedTopLevelElements,
    },
    imports: {
      $set: imports,
    },
  })
}
