import {
  JSExpression,
  JSXElement,
  JSXElementName,
  ElementInstanceMetadataMap,
  SettableLayoutSystem,
  JSXElementChild,
  JSXConditionalExpression,
  JSXFragment,
} from '../../core/shared/element-template'
import { KeysPressed, Key } from '../../utils/keyboard'
import { IndexPosition } from '../../utils/utils'
import { CanvasRectangle, Size, WindowPoint, CanvasPoint } from '../../core/shared/math-utils'
import {
  CanvasAction,
  CSSCursor,
  PinOrFlexFrameChange,
  SelectionLocked,
} from '../canvas/canvas-types'
import { EditorPane, EditorPanel, ResizeLeftPane, SetFocus } from '../common/actions'
import {
  ProjectFile,
  PropertyPath,
  StaticElementPathPart,
  ElementPath,
  NodeModules,
  Imports,
  ParsedTextFile,
  HighlightBoundsForUids,
  ImageFile,
} from '../../core/shared/project-file-types'
import { CodeResultCache, PropertyControlsInfo } from '../custom-code/code-file'
import { ElementContextMenuInstance } from '../element-context-menu'
import { FontSettings } from '../inspector/common/css-utils'
import { CSSTarget } from '../inspector/sections/header-section/target-selector'
import { LocalNavigatorAction } from '../navigator/actions/index'
import { Mode } from './editor-modes'
import type {
  RequestedNpmDependency,
  PackageStatusMap,
  PackageStatus,
} from '../../core/shared/npm-dependency-types'
import {
  ImageDragSessionState,
  DuplicationState,
  EditorState,
  ElementsToRerender,
  ErrorMessages,
  FloatingInsertMenuState,
  GithubRepo,
  GithubState,
  LeftMenuTab,
  ModalDialog,
  OriginalFrame,
  PersistentModel,
  ProjectGithubSettings,
  RightMenuTab,
  StoredEditorState,
  GithubOperation,
  FileChecksums,
  GithubData,
  UserConfiguration,
  ThemeSetting,
  ColorSwatch,
  NavigatorEntry,
} from './store/editor-state'
import { Notice } from '../common/notice'
import { UtopiaVSCodeConfig } from 'utopia-vscode-common'
import type { LoginState } from '../../common/user'
import { InsertableComponent, StylePropOption } from '../shared/project-components'
import { LayoutTargetableProp } from '../../core/layout/layout-helpers-new'
import { BuildType } from '../../core/workers/common/worker-types'
import { ProjectContentTreeRoot } from '../assets'
import { GithubOperationType } from './actions/action-creators'
import { CanvasCommand } from '../canvas/commands/commands'
import { InsertionPath } from './store/insertion-path'
import { TextProp } from '../text-editor/text-editor'
import { ElementPathTrees } from '../../core/shared/element-path-tree'
export { isLoggedIn, loggedInUser, notLoggedIn } from '../../common/user'
export type { LoginState, UserDetails } from '../../common/user'

export interface PropertyTarget {
  propertyPath: PropertyPath
}

export interface ProjectListing {
  id: string
  title: string
  createdAt: string
  modifiedAt: string
  thumbnail: string
}

export function projectListing(
  id: string,
  title: string,
  createdAt: string,
  modifiedAt: string,
  thumbnail: string,
): ProjectListing {
  return {
    id: id,
    title: title,
    createdAt: createdAt,
    modifiedAt: modifiedAt,
    thumbnail: thumbnail,
  }
}

export type EditorModel = EditorState

export type MoveRowBefore = {
  type: 'MOVE_ROW_BEFORE'
  target: ElementPath
}

export type MoveRowAfter = {
  type: 'MOVE_ROW_AFTER'
  target: ElementPath
}

export type ReparentRow = {
  type: 'REPARENT_ROW'
  target: ElementPath
  indexPosition: IndexPosition
}

export type DropTarget = MoveRowBefore | MoveRowAfter | ReparentRow

export type NavigatorReorder = {
  action: 'NAVIGATOR_REORDER'
  dragSources: Array<ElementPath>
  targetParent: ElementPath
  indexPosition: IndexPosition
  canvasViewportCenter: CanvasPoint
}

export type RenameComponent = {
  action: 'RENAME_COMPONENT'
  target: ElementPath
  name: string | null
}

export type ClearSelection = {
  action: 'CLEAR_SELECTION'
}

export interface InsertJSXElement {
  action: 'INSERT_JSX_ELEMENT'
  jsxElement: JSXElement
  parent: ElementPath | null
  importsToAdd: Imports
}

export type DeleteSelected = {
  action: 'DELETE_SELECTED'
}

export type DeleteView = {
  action: 'DELETE_VIEW'
  target: ElementPath
}

export type SelectComponents = {
  action: 'SELECT_COMPONENTS'
  target: Array<ElementPath>
  addToSelection: boolean
}

export type UpdateEditorMode = {
  action: 'UPDATE_EDITOR_MODE'
  mode: Mode
}

export type SwitchEditorMode = {
  action: 'SWITCH_EDITOR_MODE'
  mode: Mode
  unlessMode?: 'select' | 'live' | 'insert' | 'textEdit'
}

export interface ToggleCanvasIsLive {
  action: 'TOGGLE_CANVAS_IS_LIVE'
}

export type ToggleHidden = {
  action: 'TOGGLE_HIDDEN'
  targets: Array<ElementPath>
}

export type UnsetProperty = {
  action: 'UNSET_PROPERTY'
  element: ElementPath
  property: PropertyPath
}

export type SetProperty = {
  action: 'SET_PROPERTY'
  element: ElementPath
  property: PropertyPath
  value: JSExpression
}

export type SetCanvasFrames = {
  action: 'SET_CANVAS_FRAMES'
  framesAndTargets: Array<PinOrFlexFrameChange>
  keepChildrenGlobalCoords: boolean
  originalFrames: Array<OriginalFrame> | null
}

// TODO Pull into actions.ts?
export type Canvas = {
  action: 'CANVAS_ACTION'
  canvasAction: CanvasAction
}

export type DuplicateSelected = {
  action: 'DUPLICATE_SELECTED'
}

export interface DuplicateSpecificElements {
  action: 'DUPLICATE_SPECIFIC_ELEMENTS'
  paths: Array<ElementPath>
}

export interface UpdateDuplicationState {
  action: 'UPDATE_DUPLICATION_STATE'
  duplicationState: DuplicationState | null
}

export type MoveSelectedToBack = {
  action: 'MOVE_SELECTED_TO_BACK'
}

export type MoveSelectedToFront = {
  action: 'MOVE_SELECTED_TO_FRONT'
}

export type MoveSelectedBackward = {
  action: 'MOVE_SELECTED_BACKWARD'
}

export type MoveSelectedForward = {
  action: 'MOVE_SELECTED_FORWARD'
}

export type SetZIndex = {
  action: 'SET_Z_INDEX'
  target: ElementPath
  indexPosition: IndexPosition
}

export type TransientActions = {
  action: 'TRANSIENT_ACTIONS'
  transientActions: Array<EditorAction>
  elementsToRerender: Array<ElementPath> | null
}

// This is a wrapper action which changes the undo behavior for the included actions.
// When you wrap actions in this, dispatching them will not create a new undo step.
// Instead of that the effects of the actions will be merged into the previous undo step.
// (Practically the previous undo snapshot will be replaced with the result of these actions.)
export type MergeWithPrevUndo = {
  action: 'MERGE_WITH_PREV_UNDO'
  actions: Array<EditorAction>
}

export type Atomic = {
  action: 'ATOMIC'
  actions: Array<EditorAction>
}

export interface NewProject {
  action: 'NEW'
  nodeModules: NodeModules
  packageResult: PackageStatusMap
  persistentModel: PersistentModel
  codeResultCache: CodeResultCache
}

export type Load = {
  action: 'LOAD'
  model: PersistentModel
  nodeModules: NodeModules
  packageResult: PackageStatusMap
  codeResultCache: CodeResultCache
  title: string
  projectId: string
  storedState: StoredEditorState | null
  safeMode: boolean
}

export type Undo = {
  action: 'UNDO'
}

export type Redo = {
  action: 'REDO'
}

export type SetPanelVisibility = {
  action: 'SET_PANEL_VISIBILITY'
  target: EditorPanel | EditorPane
  visible: boolean
}

export type ToggleFocusedOmniboxTab = {
  action: 'TOGGLE_FOCUSED_OMNIBOX_TAB'
}

export type TogglePane = {
  action: 'TOGGLE_PANE'
  target: EditorPanel | EditorPane
}

export type ToggleInterfaceDesignerAdditionalControls = {
  action: 'TOGGLE_INTERFACEDESIGNER_ADDITIONAL_CONTROLS'
}

export type ResizeInterfaceDesignerCodePane = {
  action: 'RESIZE_INTERFACEDESIGNER_CODEPANE'
  deltaCodePaneWidth: number
}

export type ToggleInterfaceDesignerCodeEditor = {
  action: 'TOGGLE_INTERFACEDESIGNER_CODEEDITOR'
}

export interface OpenPopup {
  action: 'OPEN_POPUP'
  popupId: string
}

export interface ClosePopup {
  action: 'CLOSE_POPUP'
}

export interface ElementPaste {
  element: JSXElementChild
  importsToAdd: Imports
  originalElementPath: ElementPath
}

export interface PasteJSXElements {
  action: 'PASTE_JSX_ELEMENTS'
  elements: Array<ElementPaste>
  targetOriginalContextMetadata: ElementInstanceMetadataMap
  targetOriginalElementPathTree: ElementPathTrees
  canvasViewportCenter: CanvasPoint
}

export interface CopySelectionToClipboard {
  action: 'COPY_SELECTION_TO_CLIPBOARD'
}

export interface CutSelectionToClipboard {
  action: 'CUT_SELECTION_TO_CLIPBOARD'
}

export interface CopyProperties {
  action: 'COPY_PROPERTIES'
}
export interface PasteProperties {
  action: 'PASTE_PROPERTIES'
  type: 'style' | 'layout'
}
export interface PasteToReplace {
  action: 'PASTE_TO_REPLACE'
}

export interface SetProjectID {
  action: 'SET_PROJECT_ID'
  id: string
}

export interface SetForkedFromProjectID {
  action: 'SET_FORKED_FROM_PROJECT_ID'
  id: string | null
}

export interface OpenTextEditor {
  action: 'OPEN_TEXT_EDITOR'
  target: ElementPath
  mousePosition: WindowPoint | null
}

export interface CloseTextEditor {
  action: 'CLOSE_TEXT_EDITOR'
}

export interface SetLeftMenuTab {
  action: 'SET_LEFT_MENU_TAB'
  tab: LeftMenuTab
}

export interface SetLeftMenuExpanded {
  action: 'SET_LEFT_MENU_EXPANDED'
  expanded: boolean
}

export interface SetRightMenuTab {
  action: 'SET_RIGHT_MENU_TAB'
  tab: RightMenuTab
}

export interface SetRightMenuExpanded {
  action: 'SET_RIGHT_MENU_EXPANDED'
  expanded: boolean
}

export interface ToggleCollapse {
  action: 'TOGGLE_COLLAPSE'
  target: ElementPath
}

export interface AddToast {
  action: 'ADD_TOAST'
  // FIXME: This contains React.ReactChild and is likely not serializable.
  toast: Notice
}

export interface RemoveToast {
  action: 'REMOVE_TOAST'
  id: string
}

export interface SetHighlightedViews {
  action: 'SET_HIGHLIGHTED_VIEWS'
  targets: ElementPath[]
}

export interface SetHoveredViews {
  action: 'SET_HOVERED_VIEWS'
  targets: ElementPath[]
}

export interface ClearHighlightedViews {
  action: 'CLEAR_HIGHLIGHTED_VIEWS'
}
export interface ClearHoveredViews {
  action: 'CLEAR_HOVERED_VIEWS'
}

export type UpdateKeysPressed = {
  action: 'UPDATE_KEYS_PRESSED'
  keys: KeysPressed
}

export interface UpdateMouseButtonsPressed {
  action: 'UPDATE_MOUSE_BUTTONS_PRESSED'
  added: number | null
  removed: number | null
}

export type HideModal = {
  action: 'HIDE_MODAL'
}

export type ShowModal = {
  action: 'SHOW_MODAL'
  modal: ModalDialog
}

export interface SaveImageSwitchMode {
  type: 'SAVE_IMAGE_SWITCH_MODE'
}

export interface SaveImageDoNothing {
  type: 'SAVE_IMAGE_DO_NOTHING'
}

export interface SaveImageInsertWith {
  type: 'SAVE_IMAGE_INSERT_WITH'
  parentPath: InsertionPath | null
  frame: CanvasRectangle
  multiplier: number
}

export interface SaveImageReplace {
  type: 'SAVE_IMAGE_REPLACE'
}

export type SaveCurrentFile = {
  action: 'SAVE_CURRENT_FILE'
}

export interface SaveImageDetails {
  imageSize: Size | null
  afterSave: SaveImageSwitchMode | SaveImageDoNothing | SaveImageInsertWith | SaveImageReplace
}

export type SaveAsset = {
  action: 'SAVE_ASSET'
  fileName: string
  fileType: string
  base64: string
  hash: number
  imageDetails: SaveImageDetails | null
}

export type ResetPins = {
  action: 'RESET_PINS'
  target: ElementPath
}

export interface WrapInElement {
  action: 'WRAP_IN_ELEMENT'
  targets: ElementPath[]
  whatToWrapWith: {
    element: JSXElement | JSXConditionalExpression | JSXFragment
    importsToAdd: Imports
  }
}

export interface OpenFloatingInsertMenu {
  action: 'OPEN_FLOATING_INSERT_MENU'
  mode: FloatingInsertMenuState
}

export interface CloseFloatingInsertMenu {
  action: 'CLOSE_FLOATING_INSERT_MENU'
}

export interface UnwrapElement {
  action: 'UNWRAP_ELEMENT'
  target: ElementPath
}

export interface UpdateFrameDimensions {
  action: 'UPDATE_FRAME_DIMENSIONS'
  element: ElementPath
  width: number
  height: number
}

export interface SetNavigatorRenamingTarget {
  action: 'SET_NAVIGATOR_RENAMING_TARGET'
  target: ElementPath | null
}

export interface RedrawOldCanvasControls {
  action: 'REDRAW_OLD_CANVAS_CONTROLS'
}

export interface SetStoredFontSettings {
  action: 'SET_STORED_FONT_SETTINGS'
  fontSettings: FontSettings
}

export interface SelectAllSiblings {
  action: 'SELECT_ALL_SIBLINGS'
}

export interface UpdateCodeResultCache {
  action: 'UPDATE_CODE_RESULT_CACHE'
  codeResultCache: CodeResultCache
  buildType: BuildType
}

export interface SetCodeEditorVisibility {
  action: 'SET_CODE_EDITOR_VISIBILITY'
  value: boolean
}

export interface SetProjectName {
  action: 'SET_PROJECT_NAME'
  name: string
}

export interface SetProjectDescription {
  action: 'SET_PROJECT_DESCRIPTION'
  description: string
}

export interface RegenerateThumbnail {
  action: 'REGENERATE_THUMBNAIL'
}

export interface UpdateThumbnailGenerated {
  action: 'UPDATE_THUMBNAIL_GENERATED'
  timestamp: number
}

export interface UpdatePreviewConnected {
  action: 'UPDATE_PREVIEW_CONNECTED'
  connected: boolean
}

export interface AlignSelectedViews {
  action: 'ALIGN_SELECTED_VIEWS'
  alignment: Alignment
}

export interface DistributeSelectedViews {
  action: 'DISTRIBUTE_SELECTED_VIEWS'
  distribution: Distribution
}

export interface ShowContextMenu {
  action: 'SHOW_CONTEXT_MENU'
  menuName: ElementContextMenuInstance
  event: MouseEvent
}

export interface SetCursorOverlay {
  action: 'SET_CURSOR_OVERLAY'
  cursor: CSSCursor | null
}

export interface SendPreviewModel {
  action: 'SEND_PREVIEW_MODEL'
}

export interface UpdateFilePath {
  action: 'UPDATE_FILE_PATH'
  oldPath: string
  newPath: string
}

export interface OpenCodeEditorFile {
  action: 'OPEN_CODE_EDITOR_FILE'
  filename: string
  forceShowCodeEditor: boolean
}

export interface CloseDesignerFile {
  action: 'CLOSE_DESIGNER_FILE'
  filename: string
}

export interface UpdateFile {
  action: 'UPDATE_FILE'
  filePath: string
  file: ProjectFile
  addIfNotInFiles: boolean
}

export interface UpdateProjectContents {
  action: 'UPDATE_PROJECT_CONTENTS'
  contents: ProjectContentTreeRoot
}

export interface UpdateBranchContents {
  action: 'UPDATE_BRANCH_CONTENTS'
  contents: ProjectContentTreeRoot | null
}

export interface UpdateGithubSettings {
  action: 'UPDATE_GITHUB_SETTINGS'
  settings: Partial<ProjectGithubSettings>
}

export interface UpdateGithubData {
  action: 'UPDATE_GITHUB_DATA'
  data: Partial<GithubData>
}

export interface RemoveFileConflict {
  action: 'REMOVE_FILE_CONFLICT'
  path: string
}

export interface WorkerParsedUpdate {
  type: 'WORKER_PARSED_UPDATE'
  filePath: string
  parsed: ParsedTextFile
  versionNumber: number
}

export interface WorkerCodeAndParsedUpdate {
  type: 'WORKER_CODE_AND_PARSED_UPDATE'
  filePath: string
  code: string
  parsed: ParsedTextFile
  versionNumber: number
}

export interface UpdateFromWorker {
  action: 'UPDATE_FROM_WORKER'
  updates: Array<WorkerParsedUpdate | WorkerCodeAndParsedUpdate>
}

export interface UpdateFromCodeEditor {
  action: 'UPDATE_FROM_CODE_EDITOR'
  filePath: string
  savedContent: string
  unsavedContent: string | null
}

export interface ClearParseOrPrintInFlight {
  action: 'CLEAR_PARSE_OR_PRINT_IN_FLIGHT'
}

export interface ClearImageFileBlob {
  action: 'CLEAR_IMAGE_FILE_BLOB'
  uiFilePath: string
  elementID: string
}

export interface AddFolder {
  action: 'ADD_FOLDER'
  parentPath: string
  fileName: string
}

export interface DeleteFile {
  action: 'DELETE_FILE'
  filename: string
}

export interface AddTextFile {
  action: 'ADD_TEXT_FILE'
  fileName: string
  parentPath: string
}

export interface SetMainUIFile {
  action: 'SET_MAIN_UI_FILE'
  uiFile: string
}

export interface SetCodeEditorBuildErrors {
  action: 'SET_CODE_EDITOR_BUILD_ERRORS'
  buildErrors: ErrorMessages
}

export interface SetCodeEditorLintErrors {
  action: 'SET_CODE_EDITOR_LINT_ERRORS'
  lintErrors: ErrorMessages
}

export interface SendLinterRequestMessage {
  action: 'SEND_LINTER_REQUEST_MESSAGE'
  filePath: string
  content: string
}

export interface SaveDOMReport {
  action: 'SAVE_DOM_REPORT'
  elementMetadata: ElementInstanceMetadataMap
  cachedPaths: Array<ElementPath>
  invalidatedPaths: Array<string>
}

export interface SetProp {
  action: 'SET_PROP'
  target: ElementPath
  propertyPath: PropertyPath
  value: JSExpression
}

export interface SetPropWithElementPath {
  action: 'SET_PROP_WITH_ELEMENT_PATH'
  target: StaticElementPathPart
  propertyPath: PropertyPath
  value: JSExpression
}

export interface SetFilebrowserRenamingTarget {
  action: 'SET_FILEBROWSER_RENAMING_TARGET'
  filename: string | null
}

export interface ToggleProperty {
  action: 'TOGGLE_PROPERTY'
  target: ElementPath
  // FIXME: This will cause problems with multi-user editing.
  togglePropValue: (element: JSXElement) => JSXElement
}

export interface DEPRECATEDToggleEnabledProperty {
  action: 'deprecated_TOGGLE_ENABLED_PROPERTY'
  target: ElementPath
  // FIXME: This will cause problems with multi-user editing.
  togglePropValue: (element: JSXElement) => JSXElement
}

export type TextFormattingType = 'bold' | 'italic' | 'underline'

export interface SwitchLayoutSystem {
  action: 'SWITCH_LAYOUT_SYSTEM'
  layoutSystem: SettableLayoutSystem
  propertyTarget: ReadonlyArray<string>
}

export interface InsertImageIntoUI {
  action: 'INSERT_IMAGE_INTO_UI'
  imagePath: string
}

export interface UpdateJSXElementName {
  action: 'UPDATE_JSX_ELEMENT_NAME'
  target: ElementPath
  elementName: { type: 'JSX_ELEMENT'; name: JSXElementName } | { type: 'JSX_FRAGMENT' }
  importsToAdd: Imports
}

export interface SetConditionalOverriddenCondition {
  action: 'SET_CONDITIONAL_OVERRIDDEN_CONDITION'
  target: ElementPath
  condition: boolean | null
}

export interface UpdateConditionalExpression {
  action: 'UPDATE_CONIDTIONAL_EXPRESSION'
  target: ElementPath
  expression: string
}

export interface AddImports {
  action: 'ADD_IMPORTS'
  target: ElementPath
  importsToAdd: Imports
}

export interface SetAspectRatioLock {
  action: 'SET_ASPECT_RATIO_LOCK'
  target: ElementPath
  locked: boolean
}

export interface RenameStyleSelector {
  action: 'RENAME_PROP_KEY'
  target: ElementPath
  cssTargetPath: CSSTarget
  value: Array<string>
}

export interface SetSafeMode {
  action: 'SET_SAFE_MODE'
  value: boolean
}

export interface SetSaveError {
  action: 'SET_SAVE_ERROR'
  value: boolean
}

export interface InsertDroppedImage {
  action: 'INSERT_DROPPED_IMAGE'
  image: ImageFile
  path: string
  position: CanvasPoint
}

export interface RemoveFromNodeModulesContents {
  action: 'REMOVE_FROM_NODE_MODULES_CONTENTS'
  modulesToRemove: Array<string>
}

export interface UpdateNodeModulesContents {
  action: 'UPDATE_NODE_MODULES_CONTENTS'
  contentsToAdd: NodeModules
}

export interface UpdatePackageJson {
  action: 'UPDATE_PACKAGE_JSON'
  dependencies: Array<RequestedNpmDependency>
}

export interface StartCheckpointTimer {
  action: 'START_CHECKPOINT_TIMER'
}

export interface FinishCheckpointTimer {
  action: 'FINISH_CHECKPOINT_TIMER'
}

export interface AddMissingDimensions {
  action: 'ADD_MISSING_DIMENSIONS'
  existingSize: CanvasRectangle
  target: ElementPath
}

export interface SetPackageStatus {
  action: 'SET_PACKAGE_STATUS'
  packageName: string
  status: PackageStatus
}

export interface SetShortcut {
  action: 'SET_SHORTCUT'
  shortcutName: string
  newKey: Key
}

export interface UpdatePropertyControlsInfo {
  action: 'UPDATE_PROPERTY_CONTROLS_INFO'
  propertyControlsInfo: PropertyControlsInfo
  moduleNamesOrPathsToDelete: Array<string>
}

export interface AddStoryboardFile {
  action: 'ADD_STORYBOARD_FILE'
}

export interface UpdateText {
  action: 'UPDATE_TEXT'
  target: ElementPath
  text: string
  textProp: TextProp
}

export interface MarkVSCodeBridgeReady {
  action: 'MARK_VSCODE_BRIDGE_READY'
  ready: boolean
}

export interface SelectFromFileAndPosition {
  action: 'SELECT_FROM_FILE_AND_POSITION'
  filePath: string
  line: number
  column: number
}

export interface SendCodeEditorInitialisation {
  action: 'SEND_CODE_EDITOR_INITIALISATION'
}

export interface SetFocusedElement {
  action: 'SET_FOCUSED_ELEMENT'
  focusedElementPath: ElementPath | null
}

export interface ScrollToElement {
  action: 'SCROLL_TO_ELEMENT'
  target: ElementPath
  keepScrollPositionIfVisible: boolean
}

export interface SetScrollAnimation {
  action: 'SET_SCROLL_ANIMATION'
  value: boolean
}

export interface SetFollowSelectionEnabled {
  action: 'SET_FOLLOW_SELECTION_ENABLED'
  value: boolean
}

export interface UpdateConfigFromVSCode {
  action: 'UPDATE_CONFIG_FROM_VSCODE'
  config: UtopiaVSCodeConfig
}

export interface SetLoginState {
  action: 'SET_LOGIN_STATE'
  loginState: LoginState
}

export interface SetGithubState {
  action: 'SET_GITHUB_STATE'
  githubState: GithubState
}

export interface SetUserConfiguration {
  action: 'SET_USER_CONFIGURATION'
  userConfiguration: UserConfiguration
}

export interface UpdateGithubOperations {
  action: 'UPDATE_GITHUB_OPERATIONS'
  operation: GithubOperation
  type: GithubOperationType
}

export interface SetRefreshingDependencies {
  action: 'SET_REFRESHING_DEPENDENCIES'
  value: boolean
}

export interface ResetCanvas {
  action: 'RESET_CANVAS'
}

export interface SetFilebrowserDropTarget {
  action: 'SET_FILEBROWSER_DROPTARGET'
  target: string | null
}

export interface SetCurrentTheme {
  action: 'SET_CURRENT_THEME'
  theme: ThemeSetting
}

export interface FocusClassNameInput {
  action: 'FOCUS_CLASS_NAME_INPUT'
}

export interface FocusFormulaBar {
  action: 'FOCUS_FORMULA_BAR'
}

export interface UpdateFormulaBarMode {
  action: 'UPDATE_FORMULA_BAR_MODE'
  value: 'css' | 'content'
}

export interface InsertInsertable {
  action: 'INSERT_INSERTABLE'
  insertionPath: InsertionPath | null
  toInsert: InsertableComponent
  styleProps: StylePropOption
  indexPosition: IndexPosition | null
}

export interface SetPropTransient {
  action: 'SET_PROP_TRANSIENT'
  target: ElementPath
  propertyPath: PropertyPath
  value: JSExpression
}

export interface ClearTransientProps {
  action: 'CLEAR_TRANSIENT_PROPS'
}

export interface AddTailwindConfig {
  action: 'ADD_TAILWIND_CONFIG'
}

export interface SetInspectorLayoutSectionHovered {
  action: 'SET_INSPECTOR_LAYOUT_SECTION_HOVERED'
  hovered: boolean
}

export interface DecrementResizeOptionsSelectedIndex {
  action: 'DECREMENT_RESIZE_OPTIONS_SELECTED_INDEX'
}

export interface IncrementResizeOptionsSelectedIndex {
  action: 'INCREMENT_RESIZE_OPTIONS_SELECTED_INDEX'
}

export interface SetResizeOptionsTargetOptions {
  action: 'SET_RESIZE_OPTIONS_TARGET_OPTIONS'
  propertyTargetOptions: Array<LayoutTargetableProp>
  index: number | null
}

export interface HideVSCodeLoadingScreen {
  action: 'HIDE_VSCODE_LOADING_SCREEN'
}

export interface SetIndexedDBFailed {
  action: 'SET_INDEXED_DB_FAILED'
  indexedDBFailed: boolean
}

export interface ForceParseFile {
  action: 'FORCE_PARSE_FILE'
  filePath: string
}

export interface RunEscapeHatch {
  action: 'RUN_ESCAPE_HATCH'
  targets: Array<ElementPath>
}

export interface SetElementsToRerender {
  action: 'SET_ELEMENTS_TO_RERENDER'
  value: ElementsToRerender
}

export type ToggleSelectionLock = {
  action: 'TOGGLE_SELECTION_LOCK'
  targets: Array<ElementPath>
  newValue: SelectionLocked
}

export interface UpdateAgainstGithub {
  action: 'UPDATE_AGAINST_GITHUB'
  branchLatestContent: ProjectContentTreeRoot
  specificCommitContent: ProjectContentTreeRoot
  latestCommit: string
}

export interface SetImageDragSessionState {
  action: 'SET_IMAGE_DRAG_SESSION_STATE'
  imageDragSessionState: ImageDragSessionState
}

export interface ApplyCommandsAction {
  action: 'APPLY_COMMANDS'
  commands: CanvasCommand[]
}

export interface UpdateColorSwatches {
  action: 'UPDATE_COLOR_SWATCHES'
  colorSwatches: Array<ColorSwatch>
}

export interface SwitchConditionalBranches {
  action: 'SWITCH_CONDITIONAL_BRANCHES'
  target: ElementPath
}

export type EditorAction =
  | ClearSelection
  | InsertJSXElement
  | DeleteSelected
  | DeleteView
  | UpdateEditorMode
  | SwitchEditorMode
  | SelectComponents
  | UnsetProperty
  | SetProperty
  | Canvas
  | DuplicateSelected
  | MoveSelectedToBack
  | MoveSelectedToFront
  | MoveSelectedBackward
  | MoveSelectedForward
  | SetZIndex
  | TransientActions
  | MergeWithPrevUndo
  | Atomic
  | NewProject
  | Load
  | CanvasAction
  | ShowContextMenu
  | LocalNavigatorAction
  | Undo
  | Redo
  | ToggleHidden
  | RenameComponent
  | NavigatorReorder
  | SetPanelVisibility
  | ToggleFocusedOmniboxTab
  | TogglePane
  | ClosePopup
  | OpenPopup
  | PasteJSXElements
  | CopySelectionToClipboard
  | CutSelectionToClipboard
  | CopyProperties
  | PasteProperties
  | PasteToReplace
  | SetProjectID
  | SetForkedFromProjectID
  | OpenTextEditor
  | CloseTextEditor
  | SetLeftMenuTab
  | SetLeftMenuExpanded
  | SetRightMenuTab
  | SetRightMenuExpanded
  | ToggleCollapse
  | AddToast
  | RemoveToast
  | SetHighlightedViews
  | ClearHighlightedViews
  | SetHoveredViews
  | ClearHoveredViews
  | UpdateKeysPressed
  | UpdateMouseButtonsPressed
  | HideModal
  | ShowModal
  | ResizeInterfaceDesignerCodePane
  | ToggleInterfaceDesignerCodeEditor
  | ToggleInterfaceDesignerAdditionalControls
  | SaveCurrentFile
  | SaveAsset
  | ResetPins
  | WrapInElement
  | OpenFloatingInsertMenu
  | CloseFloatingInsertMenu
  | UnwrapElement
  | SetNavigatorRenamingTarget
  | RedrawOldCanvasControls
  | UpdateFrameDimensions
  | SetStoredFontSettings
  | SetCanvasFrames
  | SelectAllSiblings
  | UpdateCodeResultCache
  | SetCodeEditorVisibility
  | SetProjectName
  | SetProjectDescription
  | RegenerateThumbnail
  | UpdateThumbnailGenerated
  | UpdatePreviewConnected
  | AlignSelectedViews
  | DistributeSelectedViews
  | SetCursorOverlay
  | DuplicateSpecificElements
  | UpdateDuplicationState
  | SendPreviewModel
  | UpdateFilePath
  | OpenCodeEditorFile
  | CloseDesignerFile
  | UpdateFile
  | UpdateProjectContents
  | UpdateGithubSettings
  | UpdateGithubData
  | RemoveFileConflict
  | UpdateFromWorker
  | UpdateFromCodeEditor
  | ClearParseOrPrintInFlight
  | ClearImageFileBlob
  | AddFolder
  | DeleteFile
  | AddTextFile
  | SetMainUIFile
  | SetCodeEditorBuildErrors
  | SetCodeEditorLintErrors
  | SendLinterRequestMessage
  | SaveDOMReport
  | SetProp
  | SetPropWithElementPath
  | SetFilebrowserRenamingTarget
  | ToggleProperty
  | DEPRECATEDToggleEnabledProperty
  | SwitchLayoutSystem
  | InsertImageIntoUI
  | SetFocus
  | ResizeLeftPane
  | SetAspectRatioLock
  | UpdateJSXElementName
  | AddImports
  | ToggleCanvasIsLive
  | RenameStyleSelector
  | SetSafeMode
  | SetSaveError
  | InsertDroppedImage
  | RemoveFromNodeModulesContents
  | UpdateNodeModulesContents
  | UpdatePackageJson
  | StartCheckpointTimer
  | FinishCheckpointTimer
  | AddMissingDimensions
  | SetPackageStatus
  | SetShortcut
  | UpdatePropertyControlsInfo
  | AddStoryboardFile
  | UpdateText
  | MarkVSCodeBridgeReady
  | SelectFromFileAndPosition
  | SendCodeEditorInitialisation
  | SetFocusedElement
  | ScrollToElement
  | SetScrollAnimation
  | SetFollowSelectionEnabled
  | UpdateConfigFromVSCode
  | SetLoginState
  | SetGithubState
  | SetUserConfiguration
  | ResetCanvas
  | SetFilebrowserDropTarget
  | SetCurrentTheme
  | FocusClassNameInput
  | FocusFormulaBar
  | UpdateFormulaBarMode
  | InsertInsertable
  | SetPropTransient
  | ClearTransientProps
  | AddTailwindConfig
  | SetInspectorLayoutSectionHovered
  | DecrementResizeOptionsSelectedIndex
  | IncrementResizeOptionsSelectedIndex
  | SetResizeOptionsTargetOptions
  | HideVSCodeLoadingScreen
  | SetIndexedDBFailed
  | ForceParseFile
  | RunEscapeHatch
  | SetElementsToRerender
  | ToggleSelectionLock
  | UpdateAgainstGithub
  | SetImageDragSessionState
  | UpdateGithubOperations
  | UpdateBranchContents
  | SetRefreshingDependencies
  | ApplyCommandsAction
  | UpdateColorSwatches
  | SetConditionalOverriddenCondition
  | SwitchConditionalBranches
  | UpdateConditionalExpression

export type DispatchPriority =
  | 'everyone'
  | 'canvas'
  | 'inspector'
  | 'leftpane'
  | 'topmenu'
  | 'contextmenu'
  | 'noone'
export type EditorDispatch = (
  actions: ReadonlyArray<EditorAction>,
  priority?: DispatchPriority,
) => void

export type DebugDispatch = (
  actions: ReadonlyArray<EditorAction>,
  priority?: DispatchPriority,
) => {
  entireUpdateFinished: Promise<any>
}

interface EditorDispatchScratchPad {
  addActions: (action: Array<EditorAction>) => void
}

export const usingDispatch = (
  dispatch: EditorDispatch,
  run: (builder: EditorDispatchScratchPad) => void,
): void => {
  let scratchPad: Array<EditorAction> = []
  run({ addActions: (actions: Array<EditorAction>) => (scratchPad = [...scratchPad, ...actions]) })
  dispatch(scratchPad)
}

export type Alignment = 'left' | 'hcenter' | 'right' | 'top' | 'vcenter' | 'bottom'
export type Distribution = 'horizontal' | 'vertical'
