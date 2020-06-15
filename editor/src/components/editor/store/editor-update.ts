import { EditorState, DerivedState } from './editor-state'
import { EditorAction, LoginState, EditorDispatch } from '../action-types'
import { UPDATE_FNS } from '../actions/actions'

import { StateHistory } from '../history'
import {
  setClipboardData,
  createClipboardDataFromSelectionNewWorld,
} from '../../../utils/clipboard'
import { UtopiaTsWorkers } from '../../../core/workers/common/worker-types'

export function runLocalEditorAction(
  state: EditorState,
  derivedState: DerivedState,
  loginState: LoginState,
  workers: UtopiaTsWorkers,
  action: EditorAction,
  stateHistory: StateHistory,
  dispatch: EditorDispatch,
): EditorState {
  switch (action.action) {
    case 'SET_CANVAS_FRAMES':
      return UPDATE_FNS.SET_CANVAS_FRAMES(action, state, derivedState)
    case 'ALIGN_SELECTED_VIEWS':
      return UPDATE_FNS.ALIGN_SELECTED_VIEWS(action, state, derivedState)
    case 'DISTRIBUTE_SELECTED_VIEWS':
      return UPDATE_FNS.DISTRIBUTE_SELECTED_VIEWS(action, state, derivedState)
    case 'SAVE_ASSET':
      return UPDATE_FNS.SAVE_ASSET(action, state, derivedState, dispatch, loginState)
    default:
      return runSimpleLocalEditorAction(
        state,
        derivedState,
        loginState,
        workers,
        action,
        stateHistory,
        dispatch,
      )
  }
}

export function runSimpleLocalEditorAction(
  state: EditorState,
  derivedState: DerivedState,
  loginState: LoginState,
  workers: UtopiaTsWorkers,
  action: EditorAction,
  stateHistory: StateHistory,
  dispatch: EditorDispatch,
): EditorState {
  switch (action.action) {
    case 'NEW':
      return UPDATE_FNS.NEW(action, state, workers)
    case 'LOAD':
      return UPDATE_FNS.LOAD(action, state)
    case 'DUPLICATE_SELECTED':
      return UPDATE_FNS.DUPLICATE_SELECTED(state, dispatch)
    case 'UPDATE_DUPLICATION_STATE':
      return UPDATE_FNS.UPDATE_DUPLICATION_STATE(action, state)
    case 'MOVE_SELECTED_TO_BACK':
      return UPDATE_FNS.MOVE_SELECTED_TO_BACK(state)
    case 'MOVE_SELECTED_TO_FRONT':
      return UPDATE_FNS.MOVE_SELECTED_TO_FRONT(state)
    case 'MOVE_SELECTED_BACKWARD':
      return UPDATE_FNS.MOVE_SELECTED_BACKWARD(state)
    case 'MOVE_SELECTED_FORWARD':
      return UPDATE_FNS.MOVE_SELECTED_FORWARD(state)
    case 'NAVIGATOR_REORDER':
      return UPDATE_FNS.NAVIGATOR_REORDER(action, state, derivedState)
    case 'UNSET_PROPERTY':
      return UPDATE_FNS.UNSET_PROPERTY(action, state, dispatch)
    case 'UNDO':
      return UPDATE_FNS.UNDO(state, stateHistory)
    case 'REDO':
      return UPDATE_FNS.REDO(state, stateHistory)
    case 'SELECT_COMPONENTS':
      return UPDATE_FNS.SELECT_COMPONENTS(action, state, dispatch)
    case 'CLEAR_SELECTION':
      return UPDATE_FNS.CLEAR_SELECTION(state)
    case 'SELECT_ALL_SIBLINGS':
      return UPDATE_FNS.SELECT_ALL_SIBLINGS(action, state, derivedState)
    case 'UPDATE_EDITOR_MODE':
      return UPDATE_FNS.UPDATE_EDITOR_MODE(action, state)
    case 'SWITCH_EDITOR_MODE':
      return UPDATE_FNS.SWITCH_EDITOR_MODE(action, state, derivedState)
    case 'TOGGLE_HIDDEN':
      return UPDATE_FNS.TOGGLE_HIDDEN(action, state)
    case 'RENAME_COMPONENT':
      return UPDATE_FNS.RENAME_COMPONENT(action, state)
    case 'INSERT_SCENE':
      return UPDATE_FNS.INSERT_SCENE(action, state)
    case 'INSERT_JSX_ELEMENT':
      return UPDATE_FNS.INSERT_JSX_ELEMENT(action, state)
    case 'SET_PANEL_VISIBILITY':
      return UPDATE_FNS.SET_PANEL_VISIBILITY(action, state)
    case 'TOGGLE_PANE':
      return UPDATE_FNS.TOGGLE_PANE(action, state)
    case 'RESIZE_INTERFACEDESIGNER_CODEPANE':
      return UPDATE_FNS.RESIZE_INTERFACEDESIGNER_CODEPANE(action, state, dispatch)
    case 'TOGGLE_INTERFACEDESIGNER_CODEEDITOR':
      return UPDATE_FNS.TOGGLE_INTERFACEDESIGNER_CODEEDITOR(action, state, dispatch)
    case 'TOGGLE_INTERFACEDESIGNER_LAYOUT_REVERSED':
      return UPDATE_FNS.TOGGLE_INTERFACEDESIGNER_LAYOUT_REVERSED(action, state)
    case 'TOGGLE_INTERFACEDESIGNER_ADDITIONAL_CONTROLS':
      return UPDATE_FNS.TOGGLE_INTERFACEDESIGNER_ADDITIONAL_CONTROLS(action, state)
    case 'OPEN_POPUP':
      return UPDATE_FNS.OPEN_POPUP(action, state)
    case 'CLOSE_POPUP':
      return UPDATE_FNS.CLOSE_POPUP(action, state)
    case 'PASTE_JSX_ELEMENTS':
      return UPDATE_FNS.PASTE_JSX_ELEMENTS(action, state, dispatch)
    case 'COPY_SELECTION_TO_CLIPBOARD':
      // side effect 😟
      setClipboardData(createClipboardDataFromSelectionNewWorld(state, derivedState))
      return UPDATE_FNS.COPY_SELECTION_TO_CLIPBOARD(action, state, dispatch)
    case 'OPEN_TEXT_EDITOR':
      return UPDATE_FNS.OPEN_TEXT_EDITOR(action, state)
    case 'CLOSE_TEXT_EDITOR':
      return UPDATE_FNS.CLOSE_TEXT_EDITOR(action, state)
    case 'SET_LEFT_MENU_TAB':
      return UPDATE_FNS.SET_LEFT_MENU_TAB(action, state)
    case 'SET_LEFT_MENU_EXPANDED':
      return UPDATE_FNS.SET_LEFT_MENU_EXPANDED(action, state)
    case 'SET_RIGHT_MENU_TAB':
      return UPDATE_FNS.SET_RIGHT_MENU_TAB(action, state)
    case 'SET_RIGHT_MENU_EXPANDED':
      return UPDATE_FNS.SET_RIGHT_MENU_EXPANDED(action, state)
    case 'TOGGLE_COLLAPSE':
      return UPDATE_FNS.TOGGLE_COLLAPSE(action, state)
    case 'PUSH_TOAST':
      return UPDATE_FNS.PUSH_TOAST(action, state, dispatch)
    case 'POP_TOAST':
      return UPDATE_FNS.POP_TOAST(action, state)
    case 'SET_HIGHLIGHTED_VIEW':
      return UPDATE_FNS.SET_HIGHLIGHTED_VIEW(action, state)
    case 'CLEAR_HIGHLIGHTED_VIEWS':
      return UPDATE_FNS.CLEAR_HIGHLIGHTED_VIEWS(action, state)
    case 'UPDATE_KEYS_PRESSED':
      return UPDATE_FNS.UPDATE_KEYS_PRESSED(action, state)
    case 'HIDE_MODAL':
      return UPDATE_FNS.HIDE_MODAL(action, state)
    case 'SHOW_MODAL':
      return UPDATE_FNS.SHOW_MODAL(action, state)
    case 'RESET_PINS':
      return UPDATE_FNS.RESET_PINS(action, state, dispatch)
    case 'SET_CURSOR_OVERLAY':
      return UPDATE_FNS.SET_CURSOR_OVERLAY(action, state)
    case 'SET_CANVAS_ANIMATIONS_ENABLED':
      return UPDATE_FNS.SET_CANVAS_ANIMATIONS_ENABLED(action, state)
    case 'SET_Z_INDEX':
      return UPDATE_FNS.SET_Z_INDEX(action, state, derivedState)
    case 'UPDATE_FRAME_DIMENSIONS':
      return UPDATE_FNS.UPDATE_FRAME_DIMENSIONS(action, state, derivedState)
    case 'SET_NAVIGATOR_RENAMING_TARGET':
      return UPDATE_FNS.SET_NAVIGATOR_RENAMING_TARGET(action, state)
    case 'SET_STORED_FONT_SETTINGS':
      return UPDATE_FNS.SET_STORED_FONT_SETTINGS(action, state)
    case 'SET_PROJECT_ID':
      return UPDATE_FNS.SET_PROJECT_ID(action, state, dispatch)
    case 'UPDATE_CODE_RESULT_CACHE':
      return UPDATE_FNS.UPDATE_CODE_RESULT_CACHE(action, state)
    case 'SET_CODE_EDITOR_VISIBILITY':
      return UPDATE_FNS.SET_CODE_EDITOR_VISIBILITY(action, state)
    case 'SET_PROJECT_NAME':
      return UPDATE_FNS.SET_PROJECT_NAME(action, state)
    case 'REGENERATE_THUMBNAIL':
      return UPDATE_FNS.REGENERATE_THUMBNAIL(action, state, dispatch)
    case 'UPDATE_THUMBNAIL_GENERATED':
      return UPDATE_FNS.UPDATE_THUMBNAIL_GENERATED(action, state)
    case 'UPDATE_PREVIEW_CONNECTED':
      return UPDATE_FNS.UPDATE_PREVIEW_CONNECTED(action, state)
    case 'SET_HIGHLIGHTS_ENABLED':
      return UPDATE_FNS.SET_HIGHLIGHTS_ENABLED(action, state)
    case 'SHOW_CONTEXT_MENU':
      return UPDATE_FNS.SHOW_CONTEXT_MENU(action, state)
    case 'DUPLICATE_SPECIFIC_ELEMENTS':
      return UPDATE_FNS.DUPLICATE_SPECIFIC_ELEMENTS(action, state, dispatch)
    case 'SEND_PREVIEW_MODEL':
      return UPDATE_FNS.SEND_PREVIEW_MODEL(action, state)
    case 'UPDATE_FILE_PATH':
      return UPDATE_FNS.UPDATE_FILE_PATH(action, state, loginState, dispatch)
    case 'SET_FOCUS':
      return UPDATE_FNS.SET_FOCUS(action, state)
    case 'RESIZE_LEFTPANE':
      return UPDATE_FNS.RESIZE_LEFTPANE(action, state)
    case 'OPEN_FILE':
      return UPDATE_FNS.OPEN_EDITOR_TAB(action, state)
    case 'CLOSE_FILE':
      return UPDATE_FNS.CLOSE_FILE(action, state)
    case 'REORDER_OPEN_FILES':
      return UPDATE_FNS.REORDER_EDITOR_TABS(action, state)
    case 'UPDATE_FILE':
      return UPDATE_FNS.UPDATE_FILE(action, state, dispatch)
    case 'UPDATE_FROM_WORKER':
      return UPDATE_FNS.UPDATE_FROM_WORKER(action, state, derivedState)
    case 'CLEAR_PARSE_OR_PRINT_IN_FLIGHT':
      return UPDATE_FNS.CLEAR_PARSE_OR_PRINT_IN_FLIGHT(action, state)
    case 'ADD_FOLDER':
      return UPDATE_FNS.ADD_FOLDER(action, state)
    case 'DELETE_FILE':
      return UPDATE_FNS.DELETE_FILE(action, state, derivedState, loginState)
    case 'ADD_CODE_FILE':
      return UPDATE_FNS.ADD_CODE_FILE(action, state)
    case 'ADD_UI_JS_FILE':
      return UPDATE_FNS.ADD_UI_JS_FILE(action, state)
    case 'SET_MAIN_UI_FILE':
      return UPDATE_FNS.SET_MAIN_UI_FILE_OLDWORLD(action, state)
    case 'SET_CODE_EDITOR_BUILD_ERRORS':
      return UPDATE_FNS.SET_CODE_EDITOR_BUILD_ERRORS(action, state)
    case 'SET_CODE_EDITOR_LINT_ERRORS':
      return UPDATE_FNS.SET_CODE_EDITOR_LINT_ERRORS(action, state)
    case 'SAVE_DOM_REPORT':
      return UPDATE_FNS.SAVE_DOM_REPORT(action, state)
    case 'SET_PROP':
      return UPDATE_FNS.SET_PROP(action, state)
    case 'SET_PROP_WITH_ELEMENT_PATH':
      return UPDATE_FNS.SET_PROP_WITH_ELEMENT_PATH(action, state)
    case 'SET_FILEBROWSER_RENAMING_TARGET':
      return UPDATE_FNS.SET_FILEBROWSER_RENAMING_TARGET(action, state)
    case 'TOGGLE_PROPERTY':
      return UPDATE_FNS.TOGGLE_PROPERTY(action, state, derivedState)
    case 'SWITCH_LAYOUT_SYSTEM':
      return UPDATE_FNS.SWITCH_LAYOUT_SYSTEM(action, state)
    case 'CLEAR_IMAGE_FILE_BLOB':
      return UPDATE_FNS.CLEAR_IMAGE_FILE_BLOB(action, state)
    case 'SAVE_CURRENT_FILE':
      return UPDATE_FNS.SAVE_CURRENT_FILE(action, state)
    case 'DELETE_VIEW':
      return UPDATE_FNS.DELETE_VIEW(action, state, dispatch)
    case 'DELETE_VIEWS':
      return UPDATE_FNS.DELETE_VIEWS(action, state, dispatch)
    case 'DELETE_SELECTED':
      return UPDATE_FNS.DELETE_SELECTED(action, state, dispatch)
    case 'WRAP_IN_VIEW':
      return UPDATE_FNS.WRAP_IN_VIEW(action, state, derivedState, dispatch)
    case 'UNWRAP_GROUP_OR_VIEW':
      return UPDATE_FNS.UNWRAP_GROUP_OR_VIEW(action, state, dispatch)
    case 'INSERT_IMAGE_INTO_UI':
      return UPDATE_FNS.INSERT_IMAGE_INTO_UI(action, state, derivedState)
    case 'SET_SCENE_PROP':
      return UPDATE_FNS.SET_SCENE_PROP(action, state)
    case 'UNSET_SCENE_PROP':
      return UPDATE_FNS.UNSET_SCENE_PROP(action, state)
    case 'WRAP_IN_LAYOUTABLE':
      return UPDATE_FNS.WRAP_IN_LAYOUTABLE(action, state)
    case 'UNWRAP_LAYOUTABLE':
      return UPDATE_FNS.UNWRAP_LAYOUTABLE(action, state)
    case 'UPDATE_JSX_ELEMENT_NAME':
      return UPDATE_FNS.UPDATE_JSX_ELEMENT_NAME(action, state)
    case 'SET_ASPECT_RATIO_LOCK':
      return UPDATE_FNS.SET_ASPECT_RATIO_LOCK(action, state)
    case 'SAVE_CURSOR_POSITION':
      return UPDATE_FNS.SAVE_CURSOR_POSITION(action, state)
    case 'TOGGLE_CANVAS_IS_LIVE':
      return UPDATE_FNS.TOGGLE_CANVAS_IS_LIVE(state, derivedState)
    case 'RENAME_PROP_KEY':
      return UPDATE_FNS.RENAME_PROP_KEY(action, state)
    case 'SET_CODE_EDITOR_THEME':
      return UPDATE_FNS.SET_CODE_EDITOR_THEME(action, state)
    case 'SET_SAFE_MODE':
      return UPDATE_FNS.SET_SAFE_MODE(action, state)
    case 'SET_SAVE_ERROR':
      return UPDATE_FNS.SET_SAVE_ERROR(action, state)
    case 'INSERT_DROPPED_IMAGE':
      return UPDATE_FNS.INSERT_DROPPED_IMAGE(action, state)
    case 'RESET_PROP_TO_DEFAULT':
      return UPDATE_FNS.RESET_PROP_TO_DEFAULT(action, state)
    case 'UPDATE_NODE_MODULES_CONTENTS':
      return UPDATE_FNS.UPDATE_NODE_MODULES_CONTENTS(action, state)
    case 'UPDATE_PACKAGE_JSON':
      return UPDATE_FNS.UPDATE_PACKAGE_JSON(action, state)
    case 'START_CHECKPOINT_TIMER':
      return UPDATE_FNS.START_CHECKPOINT_TIMER(action, state, dispatch)
    case 'FINISH_CHECKPOINT_TIMER':
      return UPDATE_FNS.FINISH_CHECKPOINT_TIMER(action, state)
    default:
      return state
  }
}
