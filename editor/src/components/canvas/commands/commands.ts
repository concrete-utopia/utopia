import update, { Spec } from 'immutability-helper'
import { applyUtopiaJSXComponentsChanges } from '../../../core/model/project-file-utils'
import { drop } from '../../../core/shared/array-utils'
import type { TopLevelElement, UtopiaJSXComponent } from '../../../core/shared/element-template'
import type { Imports } from '../../../core/shared/project-file-types'
import { RevisionsState } from '../../../core/shared/project-file-types'
import { keepDeepReferenceEqualityIfPossible } from '../../../utils/react-performance'
import {
  getProjectContentKeyPathElements,
  ProjectContentFile,
  ProjectContentsTree,
  ProjectContentTreeRoot,
} from '../../assets'
import type {
  EditorState,
  EditorStatePatch,
  ReparentedPathsLookup,
} from '../../editor/store/editor-state'
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
import type { PushIntendedBoundsAndUpdateGroups } from './push-intended-bounds-and-update-groups-command'
import { runPushIntendedBoundsAndUpdateGroups } from './push-intended-bounds-and-update-groups-command'
import type { DeleteProperties } from './delete-properties-command'
import { runDeleteProperties } from './delete-properties-command'
import type { AddImportsToFile } from './add-imports-to-file-command'
import { runAddImportsToFile } from './add-imports-to-file-command'
import type { SetProperty } from './set-property-command'
import { runSetProperty } from './set-property-command'
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
import type { QueueGroupTrueUp } from './queue-group-true-up-command'
import { runQueueGroupTrueUp } from './queue-group-true-up-command'

export interface CommandState {
  reparentedPathsLookup: ReparentedPathsLookup
}

export interface CommandFunctionResult {
  editorStatePatches: Array<EditorStatePatch>
  commandState: CommandState
  commandDescription: string
}

export type CommandFunction<T> = (
  editorState: EditorState,
  command: T,
  commandState: CommandState,
) => CommandFunctionResult

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
  | AddElements
  | HighlightElementsCommand
  | ConvertCssPercentToPx
  | HideInNavigatorCommand
  | ShowToastCommand
  | AddContainLayoutIfNeeded
  | RearrangeChildren
  | DeleteElement
  | WrapInContainerCommand
  | QueueGroupTrueUp

export function runCanvasCommand(
  editorState: EditorState,
  command: CanvasCommand,
  commandState: CommandState,
  commandLifecycle: InteractionLifecycle,
): CommandFunctionResult {
  switch (command.type) {
    case 'WILDCARD_PATCH':
      return runWildcardPatch(editorState, command, commandState)
    case 'UPDATE_FUNCTION_COMMAND':
      return runUpdateFunctionCommand(editorState, command, commandState, commandLifecycle)
    case 'STRATEGY_SWITCHED':
      return runStrategySwitchedCommand(command, commandState)
    case 'ADJUST_NUMBER_PROPERTY':
      return runAdjustNumberProperty(editorState, command, commandState)
    case 'ADJUST_CSS_LENGTH_PROPERTY':
      return runAdjustCssLengthProperties(editorState, command, commandState)
    case 'REPARENT_ELEMENT':
      return runReparentElement(editorState, command, commandState)
    case 'DUPLICATE_ELEMENT':
      return runDuplicateElement(editorState, command, commandState)
    case 'UPDATE_SELECTED_VIEWS':
      return runUpdateSelectedViews(editorState, command, commandState)
    case 'UPDATE_HIGHLIGHTED_VIEWS':
      return runUpdateHighlightedViews(editorState, command, commandState)
    case 'SET_SNAPPING_GUIDELINES':
      return runSetSnappingGuidelines(editorState, command, commandState)
    case 'CONVERT_TO_ABSOLUTE':
      return runConvertToAbsolute(editorState, command, commandState)
    case 'SET_CSS_LENGTH_PROPERTY':
      return runSetCssLengthProperty(editorState, command, commandState)
    case 'REORDER_ELEMENT':
      return runReorderElement(editorState, command, commandState)
    case 'SHOW_OUTLINE_HIGHLIGHT':
      return runShowOutlineHighlight(editorState, command, commandState)
    case 'SHOW_REORDER_INDICATOR':
      return runShowReorderIndicator(editorState, command, commandState)
    case 'SET_CURSOR_COMMAND':
      return runSetCursor(editorState, command, commandState)
    case 'SET_ELEMENTS_TO_RERENDER_COMMAND':
      return runSetElementsToRerender(editorState, command, commandState)
    case 'APPEND_ELEMENTS_TO_RERENDER_COMMAND':
      return runAppendElementsToRerender(editorState, command, commandState)
    case 'PUSH_INTENDED_BOUNDS_AND_UPDATE_GROUPS':
      return runPushIntendedBoundsAndUpdateGroups(
        editorState,
        command,
        commandState,
        commandLifecycle,
      )
    case 'DELETE_PROPERTIES':
      return runDeleteProperties(editorState, command, commandState)
    case 'SET_PROPERTY':
      return runSetProperty(editorState, command, commandState)
    case 'UPDATE_PROP_IF_EXISTS':
      return runUpdatePropIfExists(editorState, command, commandState)
    case 'ADD_IMPORTS_TO_FILE':
      return runAddImportsToFile(editorState, command, commandState)
    case 'ADD_TO_REPARENTED_TO_PATHS':
      return runAddToReparentedToPaths(editorState, command, commandState)
    case 'INSERT_ELEMENT_INSERTION_SUBJECT':
      return runInsertElementInsertionSubject(editorState, command, commandState)
    case 'ADD_ELEMENT':
      return runAddElement(editorState, command, commandState)
    case 'ADD_ELEMENTS':
      return runAddElements(editorState, command, commandState)
    case 'HIGHLIGHT_ELEMENTS_COMMAND':
      return runHighlightElementsCommand(editorState, command, commandState)
    case 'CONVERT_CSS_PERCENT_TO_PX':
      return runConvertCssPercentToPx(editorState, command, commandState)
    case 'HIDE_IN_NAVIGATOR_COMMAND':
      return runHideInNavigatorCommand(editorState, command, commandState)
    case 'SHOW_TOAST_COMMAND':
      return runShowToastCommand(editorState, command, commandState, commandLifecycle)
    case 'ADD_CONTAIN_LAYOUT_IF_NEEDED':
      return runAddContainLayoutIfNeeded(editorState, command, commandState)
    case 'REARRANGE_CHILDREN':
      return runRearrangeChildren(editorState, command, commandState)
    case 'DELETE_ELEMENT':
      return runDeleteElement(editorState, command, commandState)
    case 'WRAP_IN_CONTAINER':
      return runWrapInContainerCommand(editorState, command, commandState)
    case 'QUEUE_GROUP_TRUE_UP':
      return runQueueGroupTrueUp(editorState, command, commandState)
    default:
      const _exhaustiveCheck: never = command
      throw new Error(`Unhandled canvas command ${JSON.stringify(command)}`)
  }
}

export function foldAndApplyCommandsSimple(
  editorState: EditorState,
  commands: Array<CanvasCommand>,
): EditorState {
  const startingCommandState: CommandState = { reparentedPathsLookup: {} }
  const updatedEditorState = commands
    .filter((c) => c.whenToRun === 'always' || c.whenToRun === 'on-complete')
    .reduce(
      ({ workingEditorState, workingCommandState }, command) => {
        const { editorStatePatches, commandState } = runCanvasCommand(
          workingEditorState,
          command,
          workingCommandState,
          'end-interaction',
        )

        return {
          workingEditorState: updateEditorStateWithPatches(workingEditorState, editorStatePatches),
          workingCommandState: commandState,
        }
      },
      { workingEditorState: editorState, workingCommandState: startingCommandState },
    ).workingEditorState

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
  let workingCommandState: CommandState = { reparentedPathsLookup: {} }

  function runCommand(command: CanvasCommand, shouldAccumulatePatches: boolean): void {
    let shouldRunCommand: boolean
    if (commandLifecycle === 'mid-interaction') {
      shouldRunCommand = command.whenToRun === 'always' || command.whenToRun === 'mid-interaction'
    } else {
      shouldRunCommand = command.whenToRun === 'always' || command.whenToRun === 'on-complete'
    }

    if (shouldRunCommand) {
      // Run the command with our current states.
      const commandResult = runCanvasCommand(
        workingEditorState,
        command,
        workingCommandState,
        commandLifecycle,
      )
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
      workingCommandState = commandResult.commandState
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
