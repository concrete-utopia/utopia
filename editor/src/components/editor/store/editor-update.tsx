import { EditorState, DerivedState, UserState } from './editor-state'
import { EditorAction, EditorDispatch } from '../action-types'
import { UPDATE_FNS } from '../actions/actions'

import { StateHistory } from '../history'
import { setClipboardData, createClipboardDataFromSelection } from '../../../utils/clipboard'
import { UtopiaTsWorkers } from '../../../core/workers/common/worker-types'
import { UiJsxCanvasContextData } from '../../canvas/ui-jsx-canvas'
import type { BuiltInDependencies } from '../../../core/es-modules/package-manager/built-in-dependencies-list'
import { getAllUniqueUids } from '../../../core/model/element-template-utils'
import { removePathsWithDeadUIDs } from '../../../core/shared/element-path'

export function runLocalEditorAction(
  state: EditorState,
  derivedState: DerivedState,
  userState: UserState,
  workers: UtopiaTsWorkers,
  action: EditorAction,
  stateHistory: StateHistory,
  dispatch: EditorDispatch,
  spyCollector: UiJsxCanvasContextData,
  builtInDependencies: BuiltInDependencies,
): EditorState {
  switch (action.action) {
    case 'SET_CANVAS_FRAMES':
      return UPDATE_FNS.SET_CANVAS_FRAMES(action, state, derivedState)
    case 'ALIGN_SELECTED_VIEWS':
      return UPDATE_FNS.ALIGN_SELECTED_VIEWS(action, state, derivedState)
    case 'DISTRIBUTE_SELECTED_VIEWS':
      return UPDATE_FNS.DISTRIBUTE_SELECTED_VIEWS(action, state, derivedState)
    case 'SAVE_ASSET':
      return UPDATE_FNS.SAVE_ASSET(action, state, derivedState, dispatch, userState)
    default:
      return runSimpleLocalEditorAction(
        state,
        derivedState,
        userState,
        workers,
        action,
        stateHistory,
        dispatch,
        spyCollector,
        builtInDependencies,
      )
  }
}

export function runSimpleLocalEditorAction(
  state: EditorState,
  derivedState: DerivedState,
  userState: UserState,
  workers: UtopiaTsWorkers,
  action: EditorAction,
  stateHistory: StateHistory,
  dispatch: EditorDispatch,
  spyCollector: UiJsxCanvasContextData,
  builtInDependencies: BuiltInDependencies,
): EditorState {
  switch (action.action) {
    case 'NEW':
      return UPDATE_FNS.NEW(action, state, workers, dispatch)
    case 'LOAD':
      return UPDATE_FNS.LOAD(action, state, dispatch)
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
    case 'TOGGLE_FOCUSED_OMNIBOX_TAB':
      return UPDATE_FNS.TOGGLE_FOCUSED_OMNIBOX_TAB(state)
    case 'TOGGLE_PANE':
      return UPDATE_FNS.TOGGLE_PANE(action, state)
    case 'RESIZE_INTERFACEDESIGNER_CODEPANE':
      return UPDATE_FNS.RESIZE_INTERFACEDESIGNER_CODEPANE(action, state, dispatch)
    case 'TOGGLE_INTERFACEDESIGNER_CODEEDITOR':
      return UPDATE_FNS.TOGGLE_INTERFACEDESIGNER_CODEEDITOR(action, state, dispatch)
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
      setClipboardData(createClipboardDataFromSelection(state))
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
    case 'ADD_TOAST':
      return UPDATE_FNS.ADD_TOAST(action, state, dispatch)
    case 'REMOVE_TOAST':
      return UPDATE_FNS.REMOVE_TOAST(action, state)
    case 'SET_HIGHLIGHTED_VIEW':
      return UPDATE_FNS.SET_HIGHLIGHTED_VIEW(action, state)
    case 'CLEAR_HIGHLIGHTED_VIEWS':
      return UPDATE_FNS.CLEAR_HIGHLIGHTED_VIEWS(action, state)
    case 'UPDATE_KEYS_PRESSED':
      return UPDATE_FNS.UPDATE_KEYS_PRESSED(action, state)
    case 'UPDATE_MOUSE_BUTTONS_PRESSED':
      return UPDATE_FNS.UPDATE_MOUSE_BUTTONS_PRESSED(action, state)
    case 'HIDE_MODAL':
      return UPDATE_FNS.HIDE_MODAL(action, state)
    case 'SHOW_MODAL':
      return UPDATE_FNS.SHOW_MODAL(action, state)
    case 'RESET_PINS':
      return UPDATE_FNS.RESET_PINS(action, state, dispatch)
    case 'SET_CURSOR_OVERLAY':
      return UPDATE_FNS.SET_CURSOR_OVERLAY(action, state)
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
    case 'SET_PROJECT_DESCRIPTION':
      return UPDATE_FNS.SET_PROJECT_DESCRIPTION(action, state)
    case 'REGENERATE_THUMBNAIL':
      return UPDATE_FNS.REGENERATE_THUMBNAIL(action, state, dispatch)
    case 'UPDATE_THUMBNAIL_GENERATED':
      return UPDATE_FNS.UPDATE_THUMBNAIL_GENERATED(action, state)
    case 'UPDATE_PREVIEW_CONNECTED':
      return UPDATE_FNS.UPDATE_PREVIEW_CONNECTED(action, state)
    case 'SHOW_CONTEXT_MENU':
      return UPDATE_FNS.SHOW_CONTEXT_MENU(action, state)
    case 'DUPLICATE_SPECIFIC_ELEMENTS':
      return UPDATE_FNS.DUPLICATE_SPECIFIC_ELEMENTS(action, state, dispatch)
    case 'SEND_PREVIEW_MODEL':
      return UPDATE_FNS.SEND_PREVIEW_MODEL(action, state)
    case 'UPDATE_FILE_PATH':
      return UPDATE_FNS.UPDATE_FILE_PATH(action, state, userState, dispatch)
    case 'SET_FOCUS':
      return UPDATE_FNS.SET_FOCUS(action, state)
    case 'RESIZE_LEFTPANE':
      return UPDATE_FNS.RESIZE_LEFTPANE(action, state)
    case 'OPEN_CODE_EDITOR_FILE':
      return UPDATE_FNS.OPEN_CODE_EDITOR_FILE(action, state)
    case 'UPDATE_FILE':
      return UPDATE_FNS.UPDATE_FILE(action, state, dispatch, builtInDependencies)
    case 'UPDATE_FROM_WORKER':
      return UPDATE_FNS.UPDATE_FROM_WORKER(action, state)
    case 'UPDATE_FROM_CODE_EDITOR':
      return UPDATE_FNS.UPDATE_FROM_CODE_EDITOR(action, state, dispatch, builtInDependencies)
    case 'CLEAR_PARSE_OR_PRINT_IN_FLIGHT':
      return UPDATE_FNS.CLEAR_PARSE_OR_PRINT_IN_FLIGHT(action, state)
    case 'ADD_FOLDER':
      return UPDATE_FNS.ADD_FOLDER(action, state)
    case 'DELETE_FILE':
      return UPDATE_FNS.DELETE_FILE(action, state, derivedState, userState)
    case 'ADD_TEXT_FILE':
      return UPDATE_FNS.ADD_TEXT_FILE(action, state)
    case 'SET_MAIN_UI_FILE':
      return UPDATE_FNS.SET_MAIN_UI_FILE_OLDWORLD(action, state)
    case 'SET_CODE_EDITOR_BUILD_ERRORS':
      return UPDATE_FNS.SET_CODE_EDITOR_BUILD_ERRORS(action, state)
    case 'SET_CODE_EDITOR_LINT_ERRORS':
      return UPDATE_FNS.SET_CODE_EDITOR_LINT_ERRORS(action, state)
    case 'SAVE_DOM_REPORT':
      return UPDATE_FNS.SAVE_DOM_REPORT(action, state, spyCollector)
    case 'SET_PROP':
      return UPDATE_FNS.SET_PROP(action, state)
    case 'SET_PROP_WITH_ELEMENT_PATH':
      return UPDATE_FNS.SET_PROP_WITH_ELEMENT_PATH(action, state)
    case 'SET_FILEBROWSER_RENAMING_TARGET':
      return UPDATE_FNS.SET_FILEBROWSER_RENAMING_TARGET(action, state)
    case 'TOGGLE_PROPERTY':
      return UPDATE_FNS.TOGGLE_PROPERTY(action, state)
    case 'SWITCH_LAYOUT_SYSTEM':
      return UPDATE_FNS.SWITCH_LAYOUT_SYSTEM(action, state)
    case 'CLEAR_IMAGE_FILE_BLOB':
      return UPDATE_FNS.CLEAR_IMAGE_FILE_BLOB(action, state)
    case 'SAVE_CURRENT_FILE':
      return UPDATE_FNS.SAVE_CURRENT_FILE(action, state)
    case 'DELETE_VIEW':
      return UPDATE_FNS.DELETE_VIEW(action, state, dispatch)
    case 'DELETE_SELECTED':
      return UPDATE_FNS.DELETE_SELECTED(action, state, derivedState, dispatch)
    case 'WRAP_IN_VIEW':
      return UPDATE_FNS.WRAP_IN_VIEW(action, state, derivedState, dispatch)
    case 'WRAP_IN_ELEMENT':
      return UPDATE_FNS.WRAP_IN_ELEMENT(action, state, derivedState, dispatch)
    case 'OPEN_FLOATING_INSERT_MENU':
      return UPDATE_FNS.OPEN_FLOATING_INSERT_MENU(action, state)
    case 'UNWRAP_GROUP_OR_VIEW':
      return UPDATE_FNS.UNWRAP_GROUP_OR_VIEW(action, state, derivedState, dispatch)
    case 'INSERT_IMAGE_INTO_UI':
      return UPDATE_FNS.INSERT_IMAGE_INTO_UI(action, state, derivedState)
    case 'UPDATE_JSX_ELEMENT_NAME':
      return UPDATE_FNS.UPDATE_JSX_ELEMENT_NAME(action, state)
    case 'ADD_IMPORTS':
      return UPDATE_FNS.ADD_IMPORTS(action, state)
    case 'SET_ASPECT_RATIO_LOCK':
      return UPDATE_FNS.SET_ASPECT_RATIO_LOCK(action, state)
    case 'TOGGLE_CANVAS_IS_LIVE':
      return UPDATE_FNS.TOGGLE_CANVAS_IS_LIVE(state, derivedState)
    case 'RENAME_PROP_KEY':
      return UPDATE_FNS.RENAME_PROP_KEY(action, state)
    case 'SET_SAFE_MODE':
      return UPDATE_FNS.SET_SAFE_MODE(action, state)
    case 'SET_SAVE_ERROR':
      return UPDATE_FNS.SET_SAVE_ERROR(action, state)
    case 'INSERT_DROPPED_IMAGE':
      return UPDATE_FNS.INSERT_DROPPED_IMAGE(action, state)
    case 'REMOVE_FROM_NODE_MODULES_CONTENTS':
      return UPDATE_FNS.REMOVE_FROM_NODE_MODULES_CONTENTS(
        action,
        state,
        dispatch,
        builtInDependencies,
      )
    case 'UPDATE_NODE_MODULES_CONTENTS':
      return UPDATE_FNS.UPDATE_NODE_MODULES_CONTENTS(action, state, dispatch, builtInDependencies)
    case 'UPDATE_PACKAGE_JSON':
      return UPDATE_FNS.UPDATE_PACKAGE_JSON(action, state)
    case 'START_CHECKPOINT_TIMER':
      return UPDATE_FNS.START_CHECKPOINT_TIMER(action, state, dispatch)
    case 'FINISH_CHECKPOINT_TIMER':
      return UPDATE_FNS.FINISH_CHECKPOINT_TIMER(action, state)
    case 'ADD_MISSING_DIMENSIONS':
      return UPDATE_FNS.ADD_MISSING_DIMENSIONS(action, state)
    case 'SET_PACKAGE_STATUS':
      return UPDATE_FNS.SET_PACKAGE_STATUS(action, state)
    case 'UPDATE_PROPERTY_CONTROLS_INFO':
      return UPDATE_FNS.UPDATE_PROPERTY_CONTROLS_INFO(action, state)
    case 'ADD_STORYBOARD_FILE':
      return UPDATE_FNS.ADD_STORYBOARD_FILE(action, state)
    case 'UPDATE_CHILD_TEXT':
      return UPDATE_FNS.UPDATE_CHILD_TEXT(action, state)
    case 'SELECT_FROM_FILE_AND_POSITION':
      return UPDATE_FNS.SELECT_FROM_FILE_AND_POSITION(action, state, derivedState, dispatch)
    case 'SEND_LINTER_REQUEST_MESSAGE':
      // side effect ☢️
      workers.sendLinterRequestMessage(action.filePath, action.content)
      return state
    case 'MARK_VSCODE_BRIDGE_READY':
      return UPDATE_FNS.MARK_VSCODE_BRIDGE_READY(action, state)
    case 'SET_FOCUSED_ELEMENT':
      return UPDATE_FNS.SET_FOCUSED_ELEMENT(action, state)
    case 'SCROLL_TO_ELEMENT':
      return UPDATE_FNS.SCROLL_TO_ELEMENT(action, state, dispatch)
    case 'SET_SCROLL_ANIMATION':
      return UPDATE_FNS.SET_SCROLL_ANIMATION(action, state, dispatch)
    case 'UPDATE_CONFIG_FROM_VSCODE':
      return UPDATE_FNS.UPDATE_CONFIG_FROM_VSCODE(action, state)
    case 'SET_FOLLOW_SELECTION_ENABLED':
      return UPDATE_FNS.SET_FOLLOW_SELECTION_ENABLED(action, state)
    case 'RESET_CANVAS':
      return UPDATE_FNS.RESET_CANVAS(action, state)
    case 'SET_FILEBROWSER_DROPTARGET':
      return UPDATE_FNS.SET_FILEBROWSER_DROPTARGET(action, state)
    case 'SET_FORKED_FROM_PROJECT_ID':
      return UPDATE_FNS.SET_FORKED_FROM_PROJECT_ID(action, state)
    case 'SET_CURRENT_THEME':
      return UPDATE_FNS.SET_CURRENT_THEME(action, state)
    case 'FOCUS_CLASS_NAME_INPUT':
      return UPDATE_FNS.FOCUS_CLASS_NAME_INPUT(state)
    case 'FOCUS_FORMULA_BAR':
      return UPDATE_FNS.FOCUS_FORMULA_BAR(state)
    case 'UPDATE_FORMULA_BAR_MODE':
      return UPDATE_FNS.UPDATE_FORMULA_BAR_MODE(action, state)
    case 'CLOSE_FLOATING_INSERT_MENU':
      return UPDATE_FNS.CLOSE_FLOATING_INSERT_MENU(action, state)
    case 'INSERT_INSERTABLE':
      return UPDATE_FNS.INSERT_INSERTABLE(action, state)
    case 'SET_PROP_TRANSIENT':
      return UPDATE_FNS.SET_PROP_TRANSIENT(action, state)
    case 'CLEAR_TRANSIENT_PROPS':
      return UPDATE_FNS.CLEAR_TRANSIENT_PROPS(action, state)
    case 'ADD_TAILWIND_CONFIG':
      return UPDATE_FNS.ADD_TAILWIND_CONFIG(action, state, dispatch, builtInDependencies)
    case 'SET_INSPECTOR_LAYOUT_SECTION_HOVERED':
      return UPDATE_FNS.SET_INSPECTOR_LAYOUT_SECTION_HOVERED(action, state)
    case 'DECREMENT_RESIZE_OPTIONS_SELECTED_INDEX':
      return UPDATE_FNS.DECREMENT_RESIZE_OPTIONS_SELECTED_INDEX(state)
    case 'INCREMENT_RESIZE_OPTIONS_SELECTED_INDEX':
      return UPDATE_FNS.INCREMENT_RESIZE_OPTIONS_SELECTED_INDEX(state)
    case 'SET_RESIZE_OPTIONS_TARGET_OPTIONS':
      return UPDATE_FNS.SET_RESIZE_OPTIONS_TARGET_OPTIONS(action, state)
    case 'SEND_CODE_EDITOR_INITIALISATION':
      return UPDATE_FNS.SEND_CODE_EDITOR_INITIALISATION(action, state)
    case 'HIDE_VSCODE_LOADING_SCREEN':
      return UPDATE_FNS.HIDE_VSCODE_LOADING_SCREEN(action, state)
    case 'SET_INDEXED_DB_FAILED':
      return UPDATE_FNS.SET_INDEXED_DB_FAILED(action, state)
    case 'FORCE_PARSE_FILE':
      return UPDATE_FNS.FORCE_PARSE_FILE(action, state)
    case 'RUN_ESCAPE_HATCH':
      return UPDATE_FNS.RUN_ESCAPE_HATCH(action, state)
    default:
      return state
  }
}
