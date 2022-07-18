import { EditorAction } from '../action-types'

export function isTransientAction(action: EditorAction): boolean {
  switch (action.action) {
    case 'CLEAR_DRAG_STATE':
    case 'CLEAR_INTERACTION_SESSION':
      return !action.applyChanges

    case 'DROP_TARGET_HINT':
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
    case 'RESIZE_LEFTPANE':
    case 'CREATE_DRAG_STATE':
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
    case 'OPEN_TEXT_EDITOR':
    case 'CLOSE_TEXT_EDITOR':
    case 'SET_LEFT_MENU_TAB':
    case 'SET_LEFT_MENU_EXPANDED':
    case 'SET_RIGHT_MENU_TAB':
    case 'SET_RIGHT_MENU_EXPANDED':
    case 'TOGGLE_COLLAPSE':
    case 'ADD_TOAST':
    case 'REMOVE_TOAST':
    case 'SET_HIGHLIGHTED_VIEW':
    case 'CLEAR_HIGHLIGHTED_VIEWS':
    case 'HIDE_MODAL':
    case 'SHOW_MODAL':
    case 'RESIZE_INTERFACEDESIGNER_CODEPANE':
    case 'TOGGLE_INTERFACEDESIGNER_CODEEDITOR':
    case 'TOGGLE_INTERFACEDESIGNER_ADDITIONAL_CONTROLS':
    case 'SET_CURSOR_OVERLAY':
    case 'SET_NAVIGATOR_RENAMING_TARGET':
    case 'REDRAW_OLD_CANVAS_CONTROLS':
    case 'UPDATE_FRAME_DIMENSIONS':
    case 'SET_STORED_FONT_SETTINGS':
    case 'SELECT_ALL_SIBLINGS':
    case 'SET_PROJECT_ID':
    case 'SET_CODE_EDITOR_VISIBILITY':
    case 'UPDATE_PREVIEW_CONNECTED':
    case 'SEND_PREVIEW_MODEL':
    case 'CLOSE_DESIGNER_FILE':
    case 'UPDATE_CODE_RESULT_CACHE':
    case 'SET_CODE_EDITOR_BUILD_ERRORS':
    case 'SET_CODE_EDITOR_LINT_ERRORS':
    case 'SAVE_DOM_REPORT':
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
    case 'SET_SCROLL_ANIMATION':
    case 'SET_FOLLOW_SELECTION_ENABLED':
    case 'UPDATE_CONFIG_FROM_VSCODE':
    case 'SET_LOGIN_STATE':
    case 'RESET_CANVAS':
    case 'SET_FILEBROWSER_DROPTARGET':
    case 'SET_FORKED_FROM_PROJECT_ID':
    case 'SET_CURRENT_THEME':
    case 'FOCUS_CLASS_NAME_INPUT':
    case 'FOCUS_FORMULA_BAR':
    case 'UPDATE_FORMULA_BAR_MODE':
    case 'OPEN_FLOATING_INSERT_MENU':
    case 'CLOSE_FLOATING_INSERT_MENU':
    case 'SET_PROP_TRANSIENT':
    case 'CLEAR_TRANSIENT_PROPS':
    case 'SET_INSPECTOR_LAYOUT_SECTION_HOVERED':
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
      return true

    case 'NEW':
    case 'LOAD':
    case 'ATOMIC':
    case 'DELETE_SELECTED':
    case 'DELETE_VIEW':
    case 'UNSET_PROPERTY':
    case 'INSERT_SCENE':
    case 'INSERT_JSX_ELEMENT':
    case 'MOVE_SELECTED_TO_BACK':
    case 'MOVE_SELECTED_TO_FRONT':
    case 'MOVE_SELECTED_BACKWARD':
    case 'MOVE_SELECTED_FORWARD':
    case 'SET_Z_INDEX':
    case 'DUPLICATE_SELECTED':
    case 'DUPLICATE_SPECIFIC_ELEMENTS':
    case 'NAVIGATOR_REORDER':
    case 'RENAME_COMPONENT':
    case 'PASTE_JSX_ELEMENTS':
    case 'TOGGLE_PROPERTY':
    case 'deprecated_TOGGLE_ENABLED_PROPERTY':
    case 'RESET_PINS':
    case 'WRAP_IN_VIEW':
    case 'WRAP_IN_ELEMENT':
    case 'UNWRAP_GROUP_OR_VIEW':
    case 'SET_CANVAS_FRAMES':
    case 'SET_PROJECT_NAME':
    case 'SET_PROJECT_DESCRIPTION':
    case 'REGENERATE_THUMBNAIL':
    case 'UPDATE_THUMBNAIL_GENERATED':
    case 'ALIGN_SELECTED_VIEWS':
    case 'DISTRIBUTE_SELECTED_VIEWS':
    case 'TOGGLE_HIDDEN':
    case 'UPDATE_FILE_PATH':
    case 'ADD_FOLDER':
    case 'DELETE_FILE':
    case 'ADD_TEXT_FILE':
    case 'UPDATE_FILE':
    case 'UPDATE_FROM_CODE_EDITOR':
    case 'SET_MAIN_UI_FILE':
    case 'SET_PROP':
    case 'SET_PROP_WITH_ELEMENT_PATH':
    case 'SWITCH_LAYOUT_SYSTEM':
    case 'SAVE_CURRENT_FILE':
    case 'UPDATE_JSX_ELEMENT_NAME':
    case 'ADD_IMPORTS':
    case 'SET_ASPECT_RATIO_LOCK':
    case 'INSERT_DROPPED_IMAGE':
    case 'UPDATE_PACKAGE_JSON':
    case 'FINISH_CHECKPOINT_TIMER':
    case 'ADD_MISSING_DIMENSIONS':
    case 'ADD_STORYBOARD_FILE':
    case 'UPDATE_CHILD_TEXT':
    case 'INSERT_INSERTABLE':
    case 'ADD_TAILWIND_CONFIG':
    case 'RUN_ESCAPE_HATCH':
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
      return action.actions.some(isFromVSCode)
    case 'UPDATE_FROM_CODE_EDITOR':
    case 'SEND_LINTER_REQUEST_MESSAGE':
      return true
    default:
      return false
  }
}

export function isClearInteractionSession(action: EditorAction): boolean {
  switch (action.action) {
    case 'TRANSIENT_ACTIONS':
      return action.transientActions.some(isClearInteractionSession)
    case 'ATOMIC':
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
      return action.actions.some(shouldApplyClearInteractionSessionResult)
    case 'CLEAR_INTERACTION_SESSION':
      return action.applyChanges
    default:
      return false
  }
}
