import type { ElementPath, PropertyPath } from 'utopia-shared/src/types'
import { mapDropNulls, safeIndex } from '../../../core/shared/array-utils'
import type { CanvasCommand } from '../../canvas/commands/commands'
import type { EditorAction } from '../action-types'
import { isFromVSCodeAction } from './actions-from-vscode'

export function isTransientAction(action: EditorAction): boolean {
  switch (action.action) {
    case 'CLEAR_INTERACTION_SESSION':
      return !action.applyChanges

    case 'MERGE_WITH_PREV_UNDO':
      return action.actions.every(isTransientAction)

    case 'SHOW_DROP_TARGET_HINT':
    case 'HIDE_DROP_TARGET_HINT':
    case 'CLOSE_POPUP':
    case 'OPEN_POPUP':
    case 'ZOOM':
    case 'ZOOMUI':
    case 'SHOW_CONTEXT_MENU':
    case 'UPDATE_KEYS_PRESSED':
    case 'UPDATE_MOUSE_BUTTONS_PRESSED':
    case 'SET_SELECTION_CONTROLS_VISIBILITY':
    case 'SCROLL_CANVAS':
    case 'POSITION_CANVAS':
    case 'SET_FOCUS':
    case 'UNDO':
    case 'REDO':
    case 'CLEAR_SELECTION':
    case 'CANVAS_ACTION':
    case 'TRANSIENT_ACTIONS':
    case 'UPDATE_EDITOR_MODE':
    case 'SWITCH_EDITOR_MODE':
    case 'INSERT_IMAGE_INTO_UI':
    case 'SET_PANEL_VISIBILITY':
    case 'TOGGLE_FOCUSED_OMNIBOX_TAB':
    case 'TOGGLE_PANE':
    case 'COPY_SELECTION_TO_CLIPBOARD':
    case 'COPY_PROPERTIES':
    case 'OPEN_TEXT_EDITOR':
    case 'CLOSE_TEXT_EDITOR':
    case 'SET_LEFT_MENU_TAB':
    case 'SET_LEFT_MENU_EXPANDED':
    case 'SET_RIGHT_MENU_TAB':
    case 'SET_RIGHT_MENU_EXPANDED':
    case 'TOGGLE_COLLAPSE':
    case 'ADD_COLLAPSED_VIEWS':
    case 'ADD_TOAST':
    case 'REMOVE_TOAST':
    case 'SET_HIGHLIGHTED_VIEWS':
    case 'CLEAR_HIGHLIGHTED_VIEWS':
    case 'SET_HOVERED_VIEWS':
    case 'CLEAR_HOVERED_VIEWS':
    case 'HIDE_MODAL':
    case 'SHOW_MODAL':
    case 'TOGGLE_INTERFACEDESIGNER_ADDITIONAL_CONTROLS':
    case 'SET_CURSOR_OVERLAY':
    case 'SET_NAVIGATOR_RENAMING_TARGET':
    case 'REDRAW_OLD_CANVAS_CONTROLS':
    case 'UPDATE_FRAME_DIMENSIONS':
    case 'SET_STORED_FONT_SETTINGS':
    case 'SELECT_ALL_SIBLINGS':
    case 'SET_PROJECT_ID':
    case 'SET_CODE_EDITOR_VISIBILITY':
    case 'OPEN_CODE_EDITOR':
    case 'CLOSE_DESIGNER_FILE':
    case 'UPDATE_CODE_RESULT_CACHE':
    case 'SET_CODE_EDITOR_BUILD_ERRORS':
    case 'SET_CODE_EDITOR_LINT_ERRORS':
    case 'SET_CODE_EDITOR_COMPONENT_DESCRIPTOR_ERRORS':
    case 'SAVE_DOM_REPORT':
    case 'UPDATE_METADATA_IN_EDITOR_STATE':
    case 'RUN_DOM_WALKER':
    case 'SET_FILEBROWSER_RENAMING_TARGET':
    case 'UPDATE_DUPLICATION_STATE':
    case 'CLEAR_IMAGE_FILE_BLOB':
    case 'CLEAR_PARSE_OR_PRINT_IN_FLIGHT':
    case 'UPDATE_FROM_WORKER':
    case 'SELECT_COMPONENTS':
    case 'TOGGLE_CANVAS_IS_LIVE':
    case 'RENAME_PROP_KEY':
    case 'SET_SAFE_MODE':
    case 'SET_SAVE_ERROR':
    case 'REMOVE_FROM_NODE_MODULES_CONTENTS':
    case 'UPDATE_NODE_MODULES_CONTENTS':
    case 'START_CHECKPOINT_TIMER':
    case 'SET_PACKAGE_STATUS':
    case 'SET_SHORTCUT':
    case 'UPDATE_PROPERTY_CONTROLS_INFO':
    case 'SEND_LINTER_REQUEST_MESSAGE':
    case 'MARK_VSCODE_BRIDGE_READY':
    case 'SELECT_FROM_FILE_AND_POSITION':
    case 'SEND_CODE_EDITOR_INITIALISATION':
    case 'SET_FOCUSED_ELEMENT':
    case 'SCROLL_TO_ELEMENT':
    case 'SCROLL_TO_POSITION':
    case 'SET_SCROLL_ANIMATION':
    case 'SET_FOLLOW_SELECTION_ENABLED':
    case 'UPDATE_CONFIG_FROM_VSCODE':
    case 'SET_LOGIN_STATE':
    case 'SET_GITHUB_STATE':
    case 'SET_USER_CONFIGURATION':
    case 'RESET_CANVAS':
    case 'SET_FILEBROWSER_DROPTARGET':
    case 'SET_FORKED_FROM_PROJECT_ID':
    case 'SET_CURRENT_THEME':
    case 'FOCUS_CLASS_NAME_INPUT':
    case 'FOCUS_FORMULA_BAR':
    case 'UPDATE_FORMULA_BAR_MODE':
    case 'SET_PROP_TRANSIENT':
    case 'CLEAR_TRANSIENT_PROPS':
    case 'DECREMENT_RESIZE_OPTIONS_SELECTED_INDEX':
    case 'INCREMENT_RESIZE_OPTIONS_SELECTED_INDEX':
    case 'SET_RESIZE_OPTIONS_TARGET_OPTIONS':
    case 'OPEN_CODE_EDITOR_FILE':
    case 'HIDE_VSCODE_LOADING_SCREEN':
    case 'SET_INDEXED_DB_FAILED':
    case 'FORCE_PARSE_FILE':
    case 'CREATE_INTERACTION_SESSION':
    case 'UPDATE_INTERACTION_SESSION':
    case 'UPDATE_DRAG_INTERACTION_DATA':
    case 'SET_USERS_PREFERRED_STRATEGY':
    case 'SET_ELEMENTS_TO_RERENDER':
    case 'TOGGLE_SELECTION_LOCK':
    case 'UPDATE_GITHUB_OPERATIONS':
    case 'SET_REFRESHING_DEPENDENCIES':
    case 'UPDATE_GITHUB_DATA':
    case 'REMOVE_FILE_CONFLICT':
    case 'CLEAR_POST_ACTION_SESSION':
    case 'START_POST_ACTION_SESSION':
    case 'TRUNCATE_HISTORY':
    case 'UPDATE_PROJECT_SERVER_STATE':
    case 'SET_COMMENT_FILTER_MODE':
    case 'SET_FORKING':
    case 'SET_COLLABORATORS':
    case 'EXTRACT_PROPERTY_CONTROLS_FROM_DESCRIPTOR_FILES':
    case 'SET_SHARING_DIALOG_OPEN':
    case 'RESET_ONLINE_STATE':
    case 'INCREASE_ONLINE_STATE_FAILURE_COUNT':
    case 'SET_ERROR_BOUNDARY_HANDLING':
    case 'SET_IMPORT_WIZARD_OPEN':
    case 'UPDATE_IMPORT_OPERATIONS':
    case 'UPDATE_IMPORT_STATUS':
    case 'UPDATE_PROJECT_REQUIREMENTS':
      return true

    case 'TRUE_UP_ELEMENTS':
    case 'EXECUTE_POST_ACTION_MENU_CHOICE':
    case 'NEW':
    case 'LOAD':
    case 'ATOMIC':
    case 'DELETE_SELECTED':
    case 'DELETE_VIEW':
    case 'UNSET_PROPERTY':
    case 'INSERT_JSX_ELEMENT':
    case 'REPLACE_JSX_ELEMENT':
    case 'INSERT_ATTRIBUTE_OTHER_JAVASCRIPT_INTO_ELEMENT':
    case 'MOVE_SELECTED_TO_BACK':
    case 'MOVE_SELECTED_TO_FRONT':
    case 'MOVE_SELECTED_BACKWARD':
    case 'MOVE_SELECTED_FORWARD':
    case 'SET_Z_INDEX':
    case 'DUPLICATE_SELECTED':
    case 'DUPLICATE_SPECIFIC_ELEMENTS':
    case 'RENAME_COMPONENT':
    case 'PASTE_PROPERTIES':
    case 'TOGGLE_PROPERTY':
    case 'deprecated_TOGGLE_ENABLED_PROPERTY':
    case 'RESET_PINS':
    case 'WRAP_IN_ELEMENT':
    case 'UNWRAP_ELEMENTS':
    case 'SET_CANVAS_FRAMES':
    case 'SET_PROJECT_NAME':
    case 'SET_PROJECT_DESCRIPTION':
    case 'ALIGN_SELECTED_VIEWS':
    case 'DISTRIBUTE_SELECTED_VIEWS':
    case 'TOGGLE_HIDDEN':
    case 'TOGGLE_DATA_CAN_CONDENSE':
    case 'UPDATE_FILE_PATH':
    case 'UPDATE_REMIX_ROUTE':
    case 'ADD_FOLDER':
    case 'DELETE_FILE':
    case 'DELETE_FILE_FROM_VSCODE':
    case 'DELETE_FILE_FROM_COLLABORATION':
    case 'ADD_TEXT_FILE':
    case 'ADD_NEW_PAGE':
    case 'ADD_NEW_FEATURED_ROUTE':
    case 'REMOVE_FEATURED_ROUTE':
    case 'UPDATE_FILE':
    case 'UPDATE_PROJECT_CONTENTS':
    case 'UPDATE_BRANCH_CONTENTS':
    case 'UPDATE_GITHUB_SETTINGS':
    case 'UPDATE_FROM_CODE_EDITOR':
    case 'SET_MAIN_UI_FILE':
    case 'SET_PROP':
    case 'SAVE_CURRENT_FILE':
    case 'UPDATE_JSX_ELEMENT_NAME':
    case 'ADD_IMPORTS':
    case 'SET_ASPECT_RATIO_LOCK':
    case 'INSERT_DROPPED_IMAGE':
    case 'UPDATE_PACKAGE_JSON':
    case 'FINISH_CHECKPOINT_TIMER':
    case 'ADD_MISSING_DIMENSIONS':
    case 'UPDATE_TEXT':
    case 'INSERT_INSERTABLE':
    case 'ADD_TAILWIND_CONFIG':
    case 'RUN_ESCAPE_HATCH':
    case 'SET_IMAGE_DRAG_SESSION_STATE':
    case 'UPDATE_AGAINST_GITHUB':
    case 'APPLY_COMMANDS':
    case 'UPDATE_COLOR_SWATCHES':
    case 'SET_CONDITIONAL_OVERRIDDEN_CONDITION':
    case 'SET_MAP_COUNT_OVERRIDE':
    case 'SWITCH_CONDITIONAL_BRANCHES':
    case 'UPDATE_CONIDTIONAL_EXPRESSION':
    case 'CUT_SELECTION_TO_CLIPBOARD':
    case 'UPDATE_TOP_LEVEL_ELEMENTS_FROM_COLLABORATION_UPDATE':
    case 'UPDATE_EXPORTS_DETAIL_FROM_COLLABORATION_UPDATE':
    case 'UPDATE_IMPORTS_FROM_COLLABORATION_UPDATE':
    case 'UPDATE_CODE_FROM_COLLABORATION_UPDATE':
    case 'REPLACE_MAPPED_ELEMENT':
    case 'REPLACE_ELEMENT_IN_SCOPE':
      return false
    case 'SAVE_ASSET':
      return (
        action.imageDetails?.afterSave.type === 'SAVE_IMAGE_DO_NOTHING' ||
        action.imageDetails?.afterSave.type === 'SAVE_IMAGE_SWITCH_MODE'
      )
    default:
      const _exhaustiveCheck: never = action
      throw new Error(`Unknown action ${JSON.stringify(action)}`)
  }
}

export function isUndoOrRedo(action: EditorAction): boolean {
  switch (action.action) {
    case 'TRANSIENT_ACTIONS':
      return action.transientActions.some(isUndoOrRedo)
    case 'ATOMIC':
    case 'MERGE_WITH_PREV_UNDO':
      return action.actions.some(isUndoOrRedo)
    case 'UNDO':
    case 'REDO':
      return true
    default:
      return false
  }
}

export function isParsedModelUpdate(action: EditorAction): boolean {
  switch (action.action) {
    case 'TRANSIENT_ACTIONS':
      return action.transientActions.some(isParsedModelUpdate)
    case 'ATOMIC':
    case 'MERGE_WITH_PREV_UNDO':
      return action.actions.some(isParsedModelUpdate)
    case 'UPDATE_FROM_WORKER':
      return action.updates.some((update) => update.type === 'WORKER_PARSED_UPDATE')
    default:
      return false
  }
}

export function isFromVSCode(action: EditorAction): boolean {
  switch (action.action) {
    case 'TRANSIENT_ACTIONS':
      return action.transientActions.some(isFromVSCode)
    case 'ATOMIC':
    case 'MERGE_WITH_PREV_UNDO':
      return action.actions.some(isFromVSCode)
    default:
      return isFromVSCodeAction(action)
  }
}

export function isClearInteractionSession(action: EditorAction): boolean {
  switch (action.action) {
    case 'TRANSIENT_ACTIONS':
      return action.transientActions.some(isClearInteractionSession)
    case 'ATOMIC':
    case 'MERGE_WITH_PREV_UNDO':
      return action.actions.some(isClearInteractionSession)
    case 'CLEAR_INTERACTION_SESSION':
      return true
    default:
      return false
  }
}

export function isCreateOrUpdateInteractionSession(action: EditorAction): boolean {
  switch (action.action) {
    case 'TRANSIENT_ACTIONS':
      return action.transientActions.some(isCreateOrUpdateInteractionSession)
    case 'ATOMIC':
    case 'MERGE_WITH_PREV_UNDO':
      return action.actions.some(isCreateOrUpdateInteractionSession)
    case 'CREATE_INTERACTION_SESSION':
    case 'UPDATE_INTERACTION_SESSION':
      return true
    default:
      return false
  }
}

export function shouldApplyClearInteractionSessionResult(action: EditorAction): boolean {
  switch (action.action) {
    case 'TRANSIENT_ACTIONS':
      return action.transientActions.some(shouldApplyClearInteractionSessionResult)
    case 'ATOMIC':
    case 'MERGE_WITH_PREV_UNDO':
      return action.actions.some(shouldApplyClearInteractionSessionResult)
    case 'CLEAR_INTERACTION_SESSION':
      return action.applyChanges
    default:
      return false
  }
}

export function isWorkerUpdate(action: EditorAction): boolean {
  return (
    action.action === 'UPDATE_FROM_WORKER' ||
    (action.action === 'MERGE_WITH_PREV_UNDO' && checkAnyWorkerUpdates(action.actions))
  )
}

export function checkAnyWorkerUpdates(actions: ReadonlyArray<EditorAction>): boolean {
  return actions.some(isWorkerUpdate)
}

export function onlyActionIsWorkerParsedUpdate(actions: ReadonlyArray<EditorAction>): boolean {
  const firstAction = safeIndex(actions, 0)
  if (firstAction == null || actions.length != 1) {
    return false
  } else {
    return (
      (firstAction.action === 'UPDATE_FROM_WORKER' &&
        firstAction.updates.some((update) => update.type === 'WORKER_PARSED_UPDATE')) ||
      (firstAction.action === 'MERGE_WITH_PREV_UNDO' &&
        onlyActionIsWorkerParsedUpdate(firstAction.actions))
    )
  }
}

function simpleStringifyAction(action: EditorAction, indentation: number): string {
  switch (action.action) {
    case 'TRANSIENT_ACTIONS':
      return `TRANSIENT_ACTIONS: ${simpleStringifyActions(
        action.transientActions,
        indentation + 1,
      )}`
    case 'ATOMIC':
      return `ATOMIC: ${simpleStringifyActions(action.actions, indentation + 1)}`
    case 'MERGE_WITH_PREV_UNDO':
      return `MERGE_WITH_PREV_UNDO: ${simpleStringifyActions(action.actions, indentation + 1)}`
    default:
      return action.action
  }
}

export function simpleStringifyActions(
  actions: ReadonlyArray<EditorAction>,
  indentation: number = 1,
): string {
  const spacing = '  '.repeat(indentation)
  const spacingBeforeClose = '  '.repeat(indentation - 1)
  return `[\n${spacing}${actions
    .map((a) => simpleStringifyAction(a, indentation))
    .join(`,\n${spacing}`)}\n${spacingBeforeClose}]`
}

export function getElementsToNormalizeFromCommands(commands: CanvasCommand[]): ElementPath[] {
  return mapDropNulls((command) => {
    switch (command.type) {
      case 'ADJUST_CSS_LENGTH_PROPERTY':
      case 'SET_CSS_LENGTH_PROPERTY':
      case 'CONVERT_CSS_PERCENT_TO_PX':
      case 'CONVERT_TO_ABSOLUTE':
        return command.target
      case 'ADD_CONTAIN_LAYOUT_IF_NEEDED':
      case 'SET_PROPERTY':
      case 'UPDATE_BULK_PROPERTIES':
        return command.element
      default:
        return null
    }
  }, commands)
}

export function getElementsToNormalizeFromActions(actions: EditorAction[]): ElementPath[] {
  return actions.flatMap((action) => {
    switch (action.action) {
      case 'APPLY_COMMANDS':
        return getElementsToNormalizeFromCommands(action.commands)
      // TODO: extends this switch when we add support to non-canvas
      // command-based edits
      default:
        return []
    }
  })
}

export interface PropertiesWithElementPath {
  elementPath: ElementPath
  properties: PropertyPath[]
}

export function getPropertiesToRemoveFromCommands(
  commands: CanvasCommand[],
): PropertiesWithElementPath[] {
  return mapDropNulls((command) => {
    switch (command.type) {
      case 'DELETE_PROPERTIES':
        return { elementPath: command.element, properties: command.properties }
      case 'UPDATE_BULK_PROPERTIES':
        return {
          elementPath: command.element,
          properties: mapDropNulls(
            (p) => (p.type === 'DELETE' ? p.path : null),
            command.properties,
          ),
        }
      default:
        return null
    }
  }, commands)
}

export function getPropertiesToRemoveFromActions(
  actions: EditorAction[],
): PropertiesWithElementPath[] {
  return actions.flatMap((action) => {
    switch (action.action) {
      case 'APPLY_COMMANDS':
        return getPropertiesToRemoveFromCommands(action.commands)
      default:
        return []
    }
  })
}
