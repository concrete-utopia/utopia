import update from 'immutability-helper'
import { applyUtopiaJSXComponentsChanges } from '../../../core/model/project-file-utils'
import type { TopLevelElement, UtopiaJSXComponent } from '../../../core/shared/element-template'
import type { Imports } from '../../../core/shared/project-file-types'
import type { EditorState, EditorStatePatch } from '../../editor/store/editor-state'
import type { CommandDescription } from '../canvas-strategies/interaction-state'
import type { AdjustCssLengthProperties } from './adjust-css-length-command'
import { runAdjustCssLengthProperties } from './adjust-css-length-command'
import type { AdjustNumberProperty } from './adjust-number-command'
import { runAdjustNumberProperty } from './adjust-number-command'
import type { ConvertToAbsolute } from './convert-to-absolute-command'
import { runConvertToAbsolute } from './convert-to-absolute-command'
import type { ReorderElement } from './reorder-element-command'
import { runReorderElement } from './reorder-element-command'
import type { ReparentElement } from './reparent-element-command'
import { runReparentElement } from './reparent-element-command'
import type { SetSnappingGuidelines } from './set-snapping-guidelines-command'
import { runSetSnappingGuidelines } from './set-snapping-guidelines-command'
import type { StrategySwitched } from './strategy-switched-command'
import { runStrategySwitchedCommand } from './strategy-switched-command'
import type { UpdateHighlightedViews } from './update-highlighted-views-command'
import { runUpdateHighlightedViews } from './update-highlighted-views-command'
import type { UpdateSelectedViews } from './update-selected-views-command'
import { runUpdateSelectedViews } from './update-selected-views-command'
import type { WildcardPatch } from './wildcard-patch-command'
import { runWildcardPatch } from './wildcard-patch-command'
import type { SetCssLengthProperty } from './set-css-length-command'
import { runSetCssLengthProperty } from './set-css-length-command'
import { EditorStateKeepDeepEquality } from '../../editor/store/store-deep-equality-instances'
import type { ShowOutlineHighlight } from './show-outline-highlight-command'
import { runShowOutlineHighlight } from './show-outline-highlight-command'
import type { SetCursorCommand } from './set-cursor-command'
import { runSetCursor } from './set-cursor-command'
import type {
  AppendElementsToRerenderCommand,
  SetElementsToRerenderCommand,
} from './set-elements-to-rerender-command'
import {
  runAppendElementsToRerender,
  runSetElementsToRerender,
} from './set-elements-to-rerender-command'
import type { DuplicateElement } from './duplicate-element-command'
import { runDuplicateElement } from './duplicate-element-command'
import type { UpdateFunctionCommand } from './update-function-command'
import { runUpdateFunctionCommand } from './update-function-command'
import type { DeleteProperties } from './delete-properties-command'
import { runDeleteProperties } from './delete-properties-command'
import type { AddImportsToFile } from './add-imports-to-file-command'
import { runAddImportsToFile } from './add-imports-to-file-command'
import type { UpdateBulkProperties, SetProperty } from './set-property-command'
import { runBulkUpdateProperties, runSetProperty } from './set-property-command'
import type { AddToReparentedToPaths } from './add-to-reparented-to-paths-command'
import { runAddToReparentedToPaths } from './add-to-reparented-to-paths-command'
import type { InsertElementInsertionSubject } from './insert-element-insertion-subject'
import { runInsertElementInsertionSubject } from './insert-element-insertion-subject'
import type { AddElement } from './add-element-command'
import { runAddElement } from './add-element-command'
import type { UpdatePropIfExists } from './update-prop-if-exists-command'
import { runUpdatePropIfExists } from './update-prop-if-exists-command'
import type { HighlightElementsCommand } from './highlight-element-command'
import { runHighlightElementsCommand } from './highlight-element-command'
import type { InteractionLifecycle } from '../canvas-strategies/canvas-strategy-types'
import type { ShowReorderIndicator } from './show-reorder-indicator-command'
import { runShowReorderIndicator } from './show-reorder-indicator-command'
import type { ConvertCssPercentToPx } from './convert-css-percent-to-px-command'
import { runConvertCssPercentToPx } from './convert-css-percent-to-px-command'
import type { HideInNavigatorCommand } from './hide-in-navigator-command'
import { runHideInNavigatorCommand } from './hide-in-navigator-command'
import type { ShowToastCommand } from './show-toast-command'
import { runShowToastCommand } from './show-toast-command'
import type { AddContainLayoutIfNeeded } from './add-contain-layout-if-needed-command'
import { runAddContainLayoutIfNeeded } from './add-contain-layout-if-needed-command'
import type { RearrangeChildren } from './rearrange-children-command'
import { runRearrangeChildren } from './rearrange-children-command'
import type { DeleteElement } from './delete-element-command'
import { runDeleteElement } from './delete-element-command'
import type { WrapInContainerCommand } from './wrap-in-container-command'
import { runWrapInContainerCommand } from './wrap-in-container-command'
import { patchProjectContentsWithParsedFile } from './patch-utils'
import type { AddElements } from './add-elements-command'
import { runAddElements } from './add-elements-command'
import type { QueueTrueUpElement } from './queue-true-up-command'
import { runQueueTrueUpElement } from './queue-true-up-command'
import type { PushIntendedBoundsAndUpdateGroups } from './push-intended-bounds-and-update-groups-command'
import { runPushIntendedBoundsAndUpdateGroups } from './push-intended-bounds-and-update-groups-command'
import type { PushIntendedBoundsAndUpdateHuggingElements } from './push-intended-bounds-and-update-hugging-elements-command'
import { runPushIntendedBoundsAndUpdateHuggingElements } from './push-intended-bounds-and-update-hugging-elements-command'
import { runSetActiveFrames, type SetActiveFrames } from './set-active-frames-command'
import {
  runShowGridControlsCommand,
  type ShowGridControlsCommand,
} from './show-grid-controls-command'
import type { UpdateClassList } from './update-class-list-command'
import { runUpdateClassList } from './update-class-list-command'

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
  | PushIntendedBoundsAndUpdateHuggingElements
  | DeleteProperties
  | SetProperty
  | UpdatePropIfExists
  | AddImportsToFile
  | AddToReparentedToPaths
  | InsertElementInsertionSubject
  | AddElement
  | AddElements
  | HighlightElementsCommand
  | ConvertCssPercentToPx
  | HideInNavigatorCommand
  | ShowToastCommand
  | AddContainLayoutIfNeeded
  | RearrangeChildren
  | DeleteElement
  | WrapInContainerCommand
  | QueueTrueUpElement
  | SetActiveFrames
  | UpdateBulkProperties
  | ShowGridControlsCommand
  | UpdateClassList

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
    case 'PUSH_INTENDED_BOUNDS_AND_UPDATE_HUGGING_ELEMENTS':
      return runPushIntendedBoundsAndUpdateHuggingElements(editorState, command)
    case 'DELETE_PROPERTIES':
      return runDeleteProperties(editorState, command)
    case 'SET_PROPERTY':
      return runSetProperty(editorState, command)
    case 'UPDATE_BULK_PROPERTIES':
      return runBulkUpdateProperties(editorState, command)
    case 'UPDATE_PROP_IF_EXISTS':
      return runUpdatePropIfExists(editorState, command)
    case 'ADD_IMPORTS_TO_FILE':
      return runAddImportsToFile(editorState, command)
    case 'ADD_TO_REPARENTED_TO_PATHS':
      return runAddToReparentedToPaths(command, commandLifecycle)
    case 'INSERT_ELEMENT_INSERTION_SUBJECT':
      return runInsertElementInsertionSubject(editorState, command)
    case 'ADD_ELEMENT':
      return runAddElement(editorState, command)
    case 'ADD_ELEMENTS':
      return runAddElements(editorState, command, commandLifecycle)
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
    case 'QUEUE_TRUE_UP_ELEMENT':
      return runQueueTrueUpElement(editorState, command)
    case 'SET_ACTIVE_FRAMES':
      return runSetActiveFrames(editorState, command)
    case 'SHOW_GRID_CONTROLS':
      return runShowGridControlsCommand(editorState, command)
    case 'UPDATE_CLASS_LIST':
      return runUpdateClassList(editorState, command)
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
    currentAllElementProps: editorState.currentAllElementProps,
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
