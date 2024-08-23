import type {
  EditorState,
  DerivedState,
  UserState,
  EditorStoreUnpatched,
  CollaborativeEditingSupport,
} from './editor-state'
import type {
  EditorAction,
  EditorDispatch,
  ExecutePostActionMenuChoice,
  IncreaseOnlineStateFailureCount,
  LoginState,
  ResetOnlineState,
  StartPostActionSession,
  UpdateProjectServerState,
} from '../action-types'
import { UPDATE_FNS, restoreEditorState } from '../actions/actions'

import type { StateHistory } from '../history'
import type { UtopiaTsWorkers } from '../../../core/workers/common/worker-types'
import type { UiJsxCanvasContextData } from '../../canvas/ui-jsx-canvas'
import type { BuiltInDependencies } from '../../../core/es-modules/package-manager/built-in-dependencies-list'
import { foldAndApplyCommandsSimple } from '../../canvas/commands/commands'
import type { ProjectServerState } from './project-server-state'
import { isTransientAction } from '../actions/action-utils'
import { allowedToEditProject } from './collaborative-editing'
import { InitialOnlineState } from '../online-status'
import type { Optic } from '../../../core/shared/optics/optics'
import { fromField } from '../../../core/shared/optics/optic-creators'
import { modify } from '../../../core/shared/optics/optic-utilities'
import { ProjectServerStateKeepDeepEquality } from './store-deep-equality-instances'

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
  collaborativeEditingSupport: CollaborativeEditingSupport,
  serverState: ProjectServerState,
): EditorState {
  switch (action.action) {
    case 'SET_CANVAS_FRAMES':
      return UPDATE_FNS.SET_CANVAS_FRAMES(action, state)
    case 'ALIGN_SELECTED_VIEWS':
      return UPDATE_FNS.ALIGN_SELECTED_VIEWS(action, state)
    case 'DISTRIBUTE_SELECTED_VIEWS':
      return UPDATE_FNS.DISTRIBUTE_SELECTED_VIEWS(action, state)
    case 'SAVE_ASSET':
      return UPDATE_FNS.SAVE_ASSET(action, state, dispatch, userState)
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
        collaborativeEditingSupport,
        serverState,
      )
  }
}

export function gatedActions<T>(
  action: EditorAction,
  loginState: LoginState,
  serverState: ProjectServerState,
  editorState: EditorState,
  defaultValue: T,
  updateCallback: () => T,
): T {
  // Determine if this is a collaboration update, which is to say it's an update
  // which is only for viewers in a collaboration scenario.
  let isCollaborationUpdate: boolean = false
  let alwaysRun: boolean = false
  switch (action.action) {
    case 'UPDATE_TOP_LEVEL_ELEMENTS_FROM_COLLABORATION_UPDATE':
    case 'UPDATE_EXPORTS_DETAIL_FROM_COLLABORATION_UPDATE':
    case 'UPDATE_IMPORTS_FROM_COLLABORATION_UPDATE':
    case 'UPDATE_CODE_FROM_COLLABORATION_UPDATE':
    case 'DELETE_FILE_FROM_COLLABORATION':
      isCollaborationUpdate = true
      break
    case 'UPDATE_FILE':
      isCollaborationUpdate = action.fromCollaboration
      break
    case 'UPDATE_FROM_WORKER':
      alwaysRun = true
      break
    default:
      break
  }

  const canEditProject = allowedToEditProject(loginState, serverState)

  if (alwaysRun) {
    // If it should always run.
    return updateCallback()
  } else if (isCollaborationUpdate && canEditProject) {
    // If this action is something that would've originated with an owner,
    // it shouldn't run on an owner.
    return defaultValue
  } else if (!isTransientAction(action) && !canEditProject && !isCollaborationUpdate) {
    // If this is a change that will modify the project contents, the current user
    // is not the owner and it's not a collaboration update (those intended directly
    // for viewers in a collaboration session) do not run the action.
    return defaultValue
  } else {
    // Otherwise, run the action as a fallback.
    return updateCallback()
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
  collaborativeEditingSupport: CollaborativeEditingSupport,
  serverState: ProjectServerState,
): EditorState {
  switch (action.action) {
    case 'NEW':
      return UPDATE_FNS.NEW(action, state, workers, dispatch)
    case 'LOAD':
      return UPDATE_FNS.LOAD(action, state, dispatch, collaborativeEditingSupport)
    case 'DUPLICATE_SELECTED':
      return UPDATE_FNS.DUPLICATE_SELECTED(state)
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
    case 'UNSET_PROPERTY':
      return UPDATE_FNS.UNSET_PROPERTY(action, state)
    case 'UNDO':
      return UPDATE_FNS.UNDO(state, stateHistory)
    case 'REDO':
      return UPDATE_FNS.REDO(state, stateHistory)
    case 'SELECT_COMPONENTS':
      return UPDATE_FNS.SELECT_COMPONENTS(action, state, dispatch)
    case 'CLEAR_SELECTION':
      return UPDATE_FNS.CLEAR_SELECTION(state, derivedState)
    case 'SELECT_ALL_SIBLINGS':
      return UPDATE_FNS.SELECT_ALL_SIBLINGS(action, state, derivedState)
    case 'UPDATE_EDITOR_MODE':
      return UPDATE_FNS.UPDATE_EDITOR_MODE(action, state)
    case 'SWITCH_EDITOR_MODE':
      return UPDATE_FNS.SWITCH_EDITOR_MODE(action, state, userState)
    case 'TOGGLE_HIDDEN':
      return UPDATE_FNS.TOGGLE_HIDDEN(action, state)
    case 'TOGGLE_DATA_CAN_CONDENSE':
      return UPDATE_FNS.TOGGLE_DATA_CAN_CONDENSE(action, state)
    case 'RENAME_COMPONENT':
      return UPDATE_FNS.RENAME_COMPONENT(action, state)
    case 'INSERT_JSX_ELEMENT':
      return UPDATE_FNS.INSERT_JSX_ELEMENT(action, state)
    case 'REPLACE_JSX_ELEMENT':
      return UPDATE_FNS.REPLACE_JSX_ELEMENT(action, state)
    case 'INSERT_ATTRIBUTE_OTHER_JAVASCRIPT_INTO_ELEMENT':
      return UPDATE_FNS.INSERT_ATTRIBUTE_OTHER_JAVASCRIPT_INTO_ELEMENT(action, state)
    case 'SET_PANEL_VISIBILITY':
      return UPDATE_FNS.SET_PANEL_VISIBILITY(action, state)
    case 'TOGGLE_FOCUSED_OMNIBOX_TAB':
      return UPDATE_FNS.TOGGLE_FOCUSED_OMNIBOX_TAB(state)
    case 'TOGGLE_PANE':
      return UPDATE_FNS.TOGGLE_PANE(action, state)
    case 'TOGGLE_INTERFACEDESIGNER_ADDITIONAL_CONTROLS':
      return UPDATE_FNS.TOGGLE_INTERFACEDESIGNER_ADDITIONAL_CONTROLS(action, state)
    case 'OPEN_POPUP':
      return UPDATE_FNS.OPEN_POPUP(action, state)
    case 'CLOSE_POPUP':
      return UPDATE_FNS.CLOSE_POPUP(action, state)
    case 'PASTE_PROPERTIES':
      return UPDATE_FNS.PASTE_PROPERTIES(action, state)
    case 'COPY_SELECTION_TO_CLIPBOARD':
      return UPDATE_FNS.COPY_SELECTION_TO_CLIPBOARD(state, builtInDependencies)
    case 'CUT_SELECTION_TO_CLIPBOARD':
      return UPDATE_FNS.CUT_SELECTION_TO_CLIPBOARD(state, dispatch, builtInDependencies)
    case 'COPY_PROPERTIES':
      return UPDATE_FNS.COPY_PROPERTIES(action, state)
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
    case 'ADD_COLLAPSED_VIEWS':
      return UPDATE_FNS.ADD_COLLAPSED_VIEWS(action, state)
    case 'ADD_TOAST':
      return UPDATE_FNS.ADD_TOAST(action, state)
    case 'SET_REFRESHING_DEPENDENCIES':
      return UPDATE_FNS.SET_REFRESHING_DEPENDENCIES(action, state)
    case 'UPDATE_GITHUB_OPERATIONS':
      return UPDATE_FNS.UPDATE_GITHUB_OPERATIONS(action, state)
    case 'REMOVE_TOAST':
      return UPDATE_FNS.REMOVE_TOAST(action, state)
    case 'SET_HIGHLIGHTED_VIEWS':
      return UPDATE_FNS.SET_HIGHLIGHTED_VIEWS(action, state)
    case 'SET_HOVERED_VIEWS':
      return UPDATE_FNS.SET_HOVERED_VIEWS(action, state)
    case 'CLEAR_HIGHLIGHTED_VIEWS':
      return UPDATE_FNS.CLEAR_HIGHLIGHTED_VIEWS(action, state)
    case 'CLEAR_HOVERED_VIEWS':
      return UPDATE_FNS.CLEAR_HOVERED_VIEWS(action, state)
    case 'UPDATE_KEYS_PRESSED':
      return UPDATE_FNS.UPDATE_KEYS_PRESSED(action, state)
    case 'UPDATE_MOUSE_BUTTONS_PRESSED':
      return UPDATE_FNS.UPDATE_MOUSE_BUTTONS_PRESSED(action, state)
    case 'HIDE_MODAL':
      return UPDATE_FNS.HIDE_MODAL(action, state)
    case 'SHOW_MODAL':
      return UPDATE_FNS.SHOW_MODAL(action, state)
    case 'RESET_PINS':
      return UPDATE_FNS.RESET_PINS(action, state)
    case 'SET_CURSOR_OVERLAY':
      return UPDATE_FNS.SET_CURSOR_OVERLAY(action, state)
    case 'SET_Z_INDEX':
      return UPDATE_FNS.SET_Z_INDEX(action, state)
    case 'UPDATE_FRAME_DIMENSIONS':
      return UPDATE_FNS.UPDATE_FRAME_DIMENSIONS(action, state)
    case 'SET_NAVIGATOR_RENAMING_TARGET':
      return UPDATE_FNS.SET_NAVIGATOR_RENAMING_TARGET(action, state)
    case 'SET_STORED_FONT_SETTINGS':
      return UPDATE_FNS.SET_STORED_FONT_SETTINGS(action, state)
    case 'SET_PROJECT_ID':
      return UPDATE_FNS.SET_PROJECT_ID(action, state)
    case 'UPDATE_CODE_RESULT_CACHE':
      return UPDATE_FNS.UPDATE_CODE_RESULT_CACHE(action, state)
    case 'SET_CODE_EDITOR_VISIBILITY':
      return UPDATE_FNS.SET_CODE_EDITOR_VISIBILITY(action, state)
    case 'OPEN_CODE_EDITOR':
      return UPDATE_FNS.OPEN_CODE_EDITOR(state)
    case 'SET_PROJECT_NAME':
      return UPDATE_FNS.SET_PROJECT_NAME(action, state)
    case 'SET_PROJECT_DESCRIPTION':
      return UPDATE_FNS.SET_PROJECT_DESCRIPTION(action, state)
    case 'UPDATE_PREVIEW_CONNECTED':
      return UPDATE_FNS.UPDATE_PREVIEW_CONNECTED(action, state)
    case 'SHOW_CONTEXT_MENU':
      return UPDATE_FNS.SHOW_CONTEXT_MENU(action, state)
    case 'DUPLICATE_SPECIFIC_ELEMENTS':
      return UPDATE_FNS.DUPLICATE_SPECIFIC_ELEMENTS(action, state, dispatch)
    case 'SEND_PREVIEW_MODEL':
      return UPDATE_FNS.SEND_PREVIEW_MODEL(action, state)
    case 'UPDATE_FILE_PATH':
      return UPDATE_FNS.UPDATE_FILE_PATH(action, state, userState)
    case 'UPDATE_REMIX_ROUTE':
      return UPDATE_FNS.UPDATE_REMIX_ROUTE(action, state, userState)
    case 'SET_FOCUS':
      return UPDATE_FNS.SET_FOCUS(action, state)
    case 'OPEN_CODE_EDITOR_FILE':
      return UPDATE_FNS.OPEN_CODE_EDITOR_FILE(action, state)
    case 'UPDATE_FILE':
      return UPDATE_FNS.UPDATE_FILE(action, state, dispatch, builtInDependencies)
    case 'UPDATE_PROJECT_CONTENTS':
      return UPDATE_FNS.UPDATE_PROJECT_CONTENTS(action, state)
    case 'UPDATE_BRANCH_CONTENTS':
      return UPDATE_FNS.UPDATE_BRANCH_CONTENTS(action, state)
    case 'UPDATE_GITHUB_SETTINGS':
      return UPDATE_FNS.UPDATE_GITHUB_SETTINGS(action, state)
    case 'UPDATE_GITHUB_DATA':
      return UPDATE_FNS.UPDATE_GITHUB_DATA(action, state)
    case 'REMOVE_FILE_CONFLICT':
      return UPDATE_FNS.REMOVE_FILE_CONFLICT(action, state)
    case 'UPDATE_FROM_WORKER':
      return UPDATE_FNS.UPDATE_FROM_WORKER(action, state, userState)
    case 'UPDATE_FROM_CODE_EDITOR':
      return UPDATE_FNS.UPDATE_FROM_CODE_EDITOR(
        action,
        state,
        dispatch,
        builtInDependencies,
        serverState,
      )
    case 'CLEAR_PARSE_OR_PRINT_IN_FLIGHT':
      return UPDATE_FNS.CLEAR_PARSE_OR_PRINT_IN_FLIGHT(action, state)
    case 'ADD_FOLDER':
      return UPDATE_FNS.ADD_FOLDER(action, state)
    case 'DELETE_FILE':
    case 'DELETE_FILE_FROM_VSCODE':
    case 'DELETE_FILE_FROM_COLLABORATION':
      return UPDATE_FNS.DELETE_FILE(action, state, derivedState, userState)
    case 'ADD_TEXT_FILE':
      return UPDATE_FNS.ADD_TEXT_FILE(action, state)
    case 'ADD_NEW_PAGE':
      return UPDATE_FNS.ADD_NEW_PAGE(action, state)
    case 'ADD_NEW_FEATURED_ROUTE':
      return UPDATE_FNS.ADD_NEW_FEATURED_ROUTE(action, state)
    case 'REMOVE_FEATURED_ROUTE':
      return UPDATE_FNS.REMOVE_FEATURED_ROUTE(action, state)
    case 'SET_MAIN_UI_FILE':
      return UPDATE_FNS.SET_MAIN_UI_FILE_OLDWORLD(action, state)
    case 'SET_CODE_EDITOR_BUILD_ERRORS':
      return UPDATE_FNS.SET_CODE_EDITOR_BUILD_ERRORS(action, state)
    case 'SET_CODE_EDITOR_LINT_ERRORS':
      return UPDATE_FNS.SET_CODE_EDITOR_LINT_ERRORS(action, state)
    case 'SET_CODE_EDITOR_COMPONENT_DESCRIPTOR_ERRORS':
      return UPDATE_FNS.SET_CODE_EDITOR_COMPONENT_DESCRIPTOR_ERRORS(action, state)
    case 'SAVE_DOM_REPORT':
      return UPDATE_FNS.SAVE_DOM_REPORT(action, state, spyCollector)
    case 'TRUE_UP_ELEMENTS':
      return UPDATE_FNS.TRUE_UP_ELEMENTS(state)
    case 'SET_PROP':
      return UPDATE_FNS.SET_PROP(action, state)
    case 'SET_FILEBROWSER_RENAMING_TARGET':
      return UPDATE_FNS.SET_FILEBROWSER_RENAMING_TARGET(action, state)
    case 'TOGGLE_PROPERTY':
      return UPDATE_FNS.TOGGLE_PROPERTY(action, state)
    case 'CLEAR_IMAGE_FILE_BLOB':
      return UPDATE_FNS.CLEAR_IMAGE_FILE_BLOB(action, state)
    case 'SAVE_CURRENT_FILE':
      return UPDATE_FNS.SAVE_CURRENT_FILE(action, state)
    case 'DELETE_VIEW':
      return UPDATE_FNS.DELETE_VIEW(action, state)
    case 'DELETE_SELECTED':
      return UPDATE_FNS.DELETE_SELECTED(state, dispatch)
    case 'WRAP_IN_ELEMENT':
      return UPDATE_FNS.WRAP_IN_ELEMENT(action, state)
    case 'UNWRAP_ELEMENTS':
      return UPDATE_FNS.UNWRAP_ELEMENTS(action, state, builtInDependencies)
    case 'INSERT_IMAGE_INTO_UI':
      return UPDATE_FNS.INSERT_IMAGE_INTO_UI(action, state, userState)
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
    case 'SELECT_FROM_FILE_AND_POSITION':
      return UPDATE_FNS.SELECT_FROM_FILE_AND_POSITION(action, state, dispatch)
    case 'SEND_LINTER_REQUEST_MESSAGE':
      // side effect ☢️
      workers.sendLinterRequestMessage(action.filePath, action.content)
      return state
    case 'MARK_VSCODE_BRIDGE_READY':
      return UPDATE_FNS.MARK_VSCODE_BRIDGE_READY(action, state)
    case 'SET_FOCUSED_ELEMENT':
      return UPDATE_FNS.SET_FOCUSED_ELEMENT(action, state, derivedState)
    case 'SCROLL_TO_ELEMENT':
      return UPDATE_FNS.SCROLL_TO_ELEMENT(action, state, dispatch)
    case 'SCROLL_TO_POSITION':
      return UPDATE_FNS.SCROLL_TO_POSITION(action, state, dispatch)
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
    case 'FOCUS_CLASS_NAME_INPUT':
      return UPDATE_FNS.FOCUS_CLASS_NAME_INPUT(state)
    case 'FOCUS_FORMULA_BAR':
      return UPDATE_FNS.FOCUS_FORMULA_BAR(state)
    case 'UPDATE_FORMULA_BAR_MODE':
      return UPDATE_FNS.UPDATE_FORMULA_BAR_MODE(action, state)
    case 'INSERT_INSERTABLE':
      return UPDATE_FNS.INSERT_INSERTABLE(action, state)
    case 'SET_PROP_TRANSIENT':
      return UPDATE_FNS.SET_PROP_TRANSIENT(action, state)
    case 'CLEAR_TRANSIENT_PROPS':
      return UPDATE_FNS.CLEAR_TRANSIENT_PROPS(action, state)
    case 'ADD_TAILWIND_CONFIG':
      return UPDATE_FNS.ADD_TAILWIND_CONFIG(action, state, dispatch, builtInDependencies)
    case 'DECREMENT_RESIZE_OPTIONS_SELECTED_INDEX':
      return UPDATE_FNS.DECREMENT_RESIZE_OPTIONS_SELECTED_INDEX(state)
    case 'INCREMENT_RESIZE_OPTIONS_SELECTED_INDEX':
      return UPDATE_FNS.INCREMENT_RESIZE_OPTIONS_SELECTED_INDEX(state)
    case 'SET_RESIZE_OPTIONS_TARGET_OPTIONS':
      return UPDATE_FNS.SET_RESIZE_OPTIONS_TARGET_OPTIONS(action, state)
    case 'SEND_CODE_EDITOR_INITIALISATION':
      return UPDATE_FNS.SEND_CODE_EDITOR_INITIALISATION(action, state, userState)
    case 'HIDE_VSCODE_LOADING_SCREEN':
      return UPDATE_FNS.HIDE_VSCODE_LOADING_SCREEN(action, state)
    case 'SET_INDEXED_DB_FAILED':
      return UPDATE_FNS.SET_INDEXED_DB_FAILED(action, state)
    case 'FORCE_PARSE_FILE':
      return UPDATE_FNS.FORCE_PARSE_FILE(action, state)
    case 'RUN_ESCAPE_HATCH':
      return UPDATE_FNS.RUN_ESCAPE_HATCH(action, state, builtInDependencies)
    case 'TOGGLE_SELECTION_LOCK':
      return UPDATE_FNS.TOGGLE_SELECTION_LOCK(action, state)
    case 'UPDATE_AGAINST_GITHUB':
      return UPDATE_FNS.UPDATE_AGAINST_GITHUB(action, state)
    case 'SET_IMAGE_DRAG_SESSION_STATE':
      return UPDATE_FNS.SET_FILE_BROWSER_DRAG_STATE(action, state)
    case 'APPLY_COMMANDS':
      return UPDATE_FNS.APPLY_COMMANDS(action, state)
    case 'UPDATE_COLOR_SWATCHES':
      return UPDATE_FNS.UPDATE_COLOR_SWATCHES(action, state)
    case 'SET_CONDITIONAL_OVERRIDDEN_CONDITION':
      return UPDATE_FNS.SET_CONDITIONAL_OVERRIDDEN_CONDITION(action, state)
    case 'SET_MAP_COUNT_OVERRIDE':
      return UPDATE_FNS.SET_MAP_COUNT_OVERRIDE(action, state)
    case 'UPDATE_CONIDTIONAL_EXPRESSION':
      return UPDATE_FNS.UPDATE_CONDITIONAL_EXPRESSION(action, state)
    case 'SWITCH_CONDITIONAL_BRANCHES':
      return UPDATE_FNS.SWITCH_CONDITIONAL_BRANCHES(action, state)
    case 'UPDATE_TOP_LEVEL_ELEMENTS_FROM_COLLABORATION_UPDATE':
      return UPDATE_FNS.UPDATE_TOP_LEVEL_ELEMENTS_FROM_COLLABORATION_UPDATE(action, state)
    case 'UPDATE_EXPORTS_DETAIL_FROM_COLLABORATION_UPDATE':
      return UPDATE_FNS.UPDATE_EXPORTS_DETAIL_FROM_COLLABORATION_UPDATE(action, state)
    case 'UPDATE_IMPORTS_FROM_COLLABORATION_UPDATE':
      return UPDATE_FNS.UPDATE_IMPORTS_FROM_COLLABORATION_UPDATE(action, state)
    case 'UPDATE_CODE_FROM_COLLABORATION_UPDATE':
      return UPDATE_FNS.UPDATE_CODE_FROM_COLLABORATION_UPDATE(
        action,
        state,
        dispatch,
        builtInDependencies,
      )
    case 'SET_COMMENT_FILTER_MODE':
      return UPDATE_FNS.SET_SHOW_RESOLVED_THREADS(action, state)
    case 'SET_FORKING':
      return UPDATE_FNS.SET_FORKING(action, state)
    case 'SET_COLLABORATORS':
      return UPDATE_FNS.SET_COLLABORATORS(action, state)
    case 'EXTRACT_PROPERTY_CONTROLS_FROM_DESCRIPTOR_FILES':
      return UPDATE_FNS.EXTRACT_PROPERTY_CONTROLS_FROM_DESCRIPTOR_FILES(
        action,
        state,
        workers,
        dispatch,
      )
    case 'SET_SHARING_DIALOG_OPEN':
      return UPDATE_FNS.SET_SHARING_DIALOG_OPEN(action, state)
    case 'REPLACE_MAPPED_ELEMENT':
      return UPDATE_FNS.REPLACE_MAPPED_ELEMENT(action, state)
    case 'REPLACE_ELEMENT_IN_SCOPE':
      return UPDATE_FNS.REPLACE_ELEMENT_IN_SCOPE(action, state)
    case 'SET_ERROR_BOUNDARY_HANDLING':
      return UPDATE_FNS.SET_ERROR_BOUNDARY_HANDLING(action, state)
    default:
      return state
  }
}

export function runExecuteWithPostActionMenuAction(
  action: ExecutePostActionMenuChoice,
  working: EditorStoreUnpatched,
): EditorStoreUnpatched {
  if (working.postActionInteractionSession == null) {
    throw new Error('no post-action session in progress')
  }

  const editorState = restoreEditorState(
    working.unpatchedEditor,
    working.postActionInteractionSession.editorStateSnapshot,
  )

  const commands = action.choice.run(
    editorState,
    working.postActionInteractionSession.derivedStateSnapshot,
    working.builtInDependencies,
  )

  if (commands == null) {
    return working
  }

  const newEditorState = foldAndApplyCommandsSimple(editorState, commands)
  return {
    ...working,
    unpatchedEditor: newEditorState,
    postActionInteractionSession: {
      ...working.postActionInteractionSession,
      activeChoiceId: action.choice.id,
    },
    history: working.postActionInteractionSession.historySnapshot,
  }
}

export function runExecuteStartPostActionMenuAction(
  action: StartPostActionSession,
  working: EditorStoreUnpatched,
): EditorStoreUnpatched {
  return {
    ...working,
    postActionInteractionSession: {
      historySnapshot: working.history,
      activeChoiceId: null,
      postActionMenuData: action.data,
      editorStateSnapshot: working.unpatchedEditor,
      derivedStateSnapshot: working.unpatchedDerived,
    },
  }
}

export function runClearPostActionSession(working: EditorStoreUnpatched): EditorStoreUnpatched {
  return {
    ...working,
    postActionInteractionSession: null,
  }
}

export function runUpdateProjectServerState(
  working: EditorStoreUnpatched,
  action: UpdateProjectServerState,
): EditorStoreUnpatched {
  const projectServerStateKeepDeepResult = ProjectServerStateKeepDeepEquality(
    working.projectServerState,
    {
      ...working.projectServerState,
      ...action.serverState,
    },
  )
  if (projectServerStateKeepDeepResult.areEqual) {
    return working
  } else {
    return {
      ...working,
      projectServerState: projectServerStateKeepDeepResult.value,
    }
  }
}

export function runResetOnlineState(
  working: EditorStoreUnpatched,
  _action: ResetOnlineState,
): EditorStoreUnpatched {
  return {
    ...working,
    onlineState: InitialOnlineState,
  }
}

export const editorStateRunningFailureCountOptic: Optic<EditorStoreUnpatched, number> = fromField<
  EditorStoreUnpatched,
  'onlineState'
>('onlineState').compose(fromField('runningFailureCount'))

export function runIncreaseOnlineStateFailureCount(
  working: EditorStoreUnpatched,
  _action: IncreaseOnlineStateFailureCount,
): EditorStoreUnpatched {
  return modify(editorStateRunningFailureCountOptic, (count) => count + 1, working)
}
