import type {
  JSExpression,
  JSXElement,
  JSXElementName,
  ElementInstanceMetadataMap,
  JSXElementChild,
  JSXConditionalExpression,
  JSXFragment,
  TopLevelElement,
  JSExpressionOtherJavaScript,
} from '../../core/shared/element-template'
import type { KeysPressed, Key } from '../../utils/keyboard'
import type { IndexPosition } from '../../utils/utils'
import type { CanvasRectangle, Size, WindowPoint, CanvasPoint } from '../../core/shared/math-utils'
import type {
  CanvasAction,
  CSSCursor,
  PinOrFlexFrameChange,
  SelectionLocked,
} from '../canvas/canvas-types'
import type { EditorPane, EditorPanel, SetFocus } from '../common/actions'
import type {
  ProjectFile,
  PropertyPath,
  ElementPath,
  NodeModules,
  Imports,
  ParsedTextFile,
  ImageFile,
  ExportDetail,
  ImportDetails,
} from '../../core/shared/project-file-types'
import type { CodeResultCache, PropertyControlsInfo } from '../custom-code/code-file'
import type { ElementContextMenuInstance } from '../element-context-menu'
import type { FontSettings } from '../inspector/common/css-utils'
import type { CSSTarget } from '../inspector/sections/header-section/target-selector'
import type { LocalNavigatorAction } from '../navigator/actions/index'
import type { Mode } from './editor-modes'
import type {
  RequestedNpmDependency,
  PackageStatusMap,
  PackageStatus,
} from '../../core/shared/npm-dependency-types'
import type {
  ImageDragSessionState,
  DuplicationState,
  EditorState,
  ElementsToRerender,
  ErrorMessages,
  FloatingInsertMenuState,
  GithubState,
  LeftMenuTab,
  ModalDialog,
  OriginalFrame,
  PersistentModel,
  ProjectGithubSettings,
  RightMenuTab,
  StoredEditorState,
  GithubOperation,
  GithubData,
  UserConfiguration,
  ThemeSetting,
  ColorSwatch,
  PostActionMenuData,
} from './store/editor-state'
import type { Notice } from '../common/notice'
import type { LoginState } from '../../common/user'
import type { InsertableComponent, StylePropOption } from '../shared/project-components'
import type { LayoutTargetableProp } from '../../core/layout/layout-helpers-new'
import type { BuildType } from '../../core/workers/common/worker-types'
import type { ProjectContentTreeRoot } from '../assets'
import type { GithubOperationType } from './actions/action-creators'
import type { CanvasCommand } from '../canvas/commands/commands'
import type { InsertionPath } from './store/insertion-path'
import type { TextProp } from '../text-editor/text-editor'
import type { PostActionChoice } from '../canvas/canvas-strategies/post-action-options/post-action-options'
import type { FromVSCodeAction } from './actions/actions-from-vscode'
import type { ProjectServerState } from './store/project-server-state'
import type { SetHuggingParentToFixed } from '../canvas/canvas-strategies/strategies/convert-to-absolute-and-move-strategy'
import type { MapLike } from 'typescript'
import type { CommentFilterMode } from '../inspector/sections/comment-section'
import type { Collaborator } from '../../core/shared/multiplayer'
import type { PageTemplate } from '../canvas/remix/remix-utils'
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

export type RenameComponent = {
  action: 'RENAME_COMPONENT'
  target: ElementPath
  name: string | null
}

export type ClearSelection = {
  action: 'CLEAR_SELECTION'
}

export type ReplaceTarget = { type: 'replace-target' }
export type ReplaceKeepChildrenAndStyleTarget = { type: 'replace-target-keep-children-and-style' }
export type InsertAsChildTarget = { type: 'insert-as-child'; indexPosition?: IndexPosition }

export interface InsertJSXElement {
  action: 'INSERT_JSX_ELEMENT'
  jsxElement: JSXElement
  target: ElementPath | null
  importsToAdd: Imports
  indexPosition: IndexPosition | null
}

export interface ReplaceJSXElement {
  action: 'REPLACE_JSX_ELEMENT'
  jsxElement: JSXElement
  target: ElementPath
  importsToAdd: Imports
  behaviour: ReplaceKeepChildrenAndStyleTarget | ReplaceTarget
}

export interface ReplaceMappedElement {
  action: 'REPLACE_MAPPED_ELEMENT'
  jsxElement: JSXElement
  target: ElementPath
  importsToAdd: Imports
}

export type ElementReplacementPath =
  | {
      type: 'replace-child-with-uid'
      uid: string
      replaceWith: JSXElementChild
    }
  | { type: 'update-map-expression'; valueToMap: JSExpression }
  | { type: 'replace-property-value'; propertyPath: PropertyPath; replaceWith: JSExpression }

export interface ReplaceElementInScope {
  action: 'REPLACE_ELEMENT_IN_SCOPE'
  target: ElementPath
  replacementPath: ElementReplacementPath
}

export interface InsertAttributeOtherJavascriptIntoElement {
  action: 'INSERT_ATTRIBUTE_OTHER_JAVASCRIPT_INTO_ELEMENT'
  expression: JSExpressionOtherJavaScript
  parent: ElementPath
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
  duplicateNameMap?: Map<string, string>
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

export interface AddCollapsedViews {
  action: 'ADD_COLLAPSED_VIEWS'
  collapsedViews: ElementPath[]
}

export interface AddToast {
  action: 'ADD_TOAST'
  // FIXME: This contains React.ReactChild and is likely not serializable.
  toast: Notice
}

export interface SetForking {
  action: 'SET_FORKING'
  forking: boolean
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

export interface SaveAsset {
  action: 'SAVE_ASSET'
  fileName: string
  fileType: string
  base64: string
  hash: number
  imageDetails: SaveImageDetails | null
  gitBlobSha: string
}

export type ResetPins = {
  action: 'RESET_PINS'
  target: ElementPath
}

export interface WrapInElementWith {
  element: JSXElement | JSXConditionalExpression | JSXFragment
  importsToAdd: Imports
}

export interface WrapInElement {
  action: 'WRAP_IN_ELEMENT'
  targets: ElementPath[]
  whatToWrapWith: WrapInElementWith
}

export interface OpenFloatingInsertMenu {
  action: 'OPEN_FLOATING_INSERT_MENU'
  mode: FloatingInsertMenuState
}

export interface CloseFloatingInsertMenu {
  action: 'CLOSE_FLOATING_INSERT_MENU'
}

export interface UnwrapElements {
  action: 'UNWRAP_ELEMENTS'
  targets: ElementPath[]
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

export interface OpenCodeEditor {
  action: 'OPEN_CODE_EDITOR'
}

export interface SetProjectName {
  action: 'SET_PROJECT_NAME'
  name: string
}

export interface SetProjectDescription {
  action: 'SET_PROJECT_DESCRIPTION'
  description: string
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

export interface UpdateRemixRoute {
  action: 'UPDATE_REMIX_ROUTE'
  oldPath: string
  newPath: string
  oldRoute: string
  newRoute: string
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
  fromCollaboration: boolean
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

export type WorkerUpdate = WorkerParsedUpdate | WorkerCodeAndParsedUpdate

export interface UpdateFromWorker {
  action: 'UPDATE_FROM_WORKER'
  updates: Array<WorkerUpdate>
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

export interface DeleteFileFromCollaboration {
  action: 'DELETE_FILE_FROM_COLLABORATION'
  filename: string
}

export interface AddTextFile {
  action: 'ADD_TEXT_FILE'
  fileName: string
  parentPath: string
}

export interface AddNewPage {
  action: 'ADD_NEW_PAGE'
  parentPath: string
  template: PageTemplate
  newPageName: string
}

export interface AddNewFeaturedRoute {
  action: 'ADD_NEW_FEATURED_ROUTE'
  featuredRoute: string
}

export interface RemoveFeaturedRoute {
  action: 'REMOVE_FEATURED_ROUTE'
  routeToRemove: string
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

export interface SetCodeEditorComponentDescriptorErrors {
  action: 'SET_CODE_EDITOR_COMPONENT_DESCRIPTOR_ERRORS'
  componentDescriptorErrors: ErrorMessages
}

export interface SaveDOMReport {
  action: 'SAVE_DOM_REPORT'
  elementMetadata: ElementInstanceMetadataMap
  cachedPaths: Array<ElementPath>
  invalidatedPaths: Array<string>
}

export interface RunDOMWalker {
  action: 'RUN_DOM_WALKER'
}

export interface TrueUpElements {
  action: 'TRUE_UP_ELEMENTS'
}

export interface SetProp {
  action: 'SET_PROP'
  target: ElementPath
  propertyPath: PropertyPath
  value: JSExpression
  importsToAdd: Imports
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

export interface SetMapCountOverride {
  action: 'SET_MAP_COUNT_OVERRIDE'
  target: ElementPath
  value: number | null
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
}

export interface UpdateText {
  action: 'UPDATE_TEXT'
  target: ElementPath
  text: string
  textProp: TextProp
}

export interface TruncateHistory {
  action: 'TRUNCATE_HISTORY'
}

export interface SetFocusedElement {
  action: 'SET_FOCUSED_ELEMENT'
  focusedElementPath: ElementPath | null
}

export type ScrollToElementBehaviour = 'keep-scroll-position-if-visible' | 'to-center'

export interface ScrollToElement {
  action: 'SCROLL_TO_ELEMENT'
  target: ElementPath
  behaviour: ScrollToElementBehaviour
}

export interface ScrollToPosition {
  action: 'SCROLL_TO_POSITION'
  target: CanvasRectangle
  behaviour: ScrollToElementBehaviour
}

export interface SetScrollAnimation {
  action: 'SET_SCROLL_ANIMATION'
  value: boolean
}

export interface SetFollowSelectionEnabled {
  action: 'SET_FOLLOW_SELECTION_ENABLED'
  value: boolean
}

export interface SetLoginState {
  action: 'SET_LOGIN_STATE'
  loginState: LoginState
}

export interface SetGithubState {
  action: 'SET_GITHUB_STATE'
  githubState: Partial<GithubState>
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

export interface ForceParseFile {
  action: 'FORCE_PARSE_FILE'
  filePath: string
}

export interface RunEscapeHatch {
  action: 'RUN_ESCAPE_HATCH'
  targets: Array<ElementPath>
  setHuggingParentToFixed: SetHuggingParentToFixed
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

export interface ExecutePostActionMenuChoice {
  action: 'EXECUTE_POST_ACTION_MENU_CHOICE'
  choice: PostActionChoice
}

export interface StartPostActionSession {
  action: 'START_POST_ACTION_SESSION'
  data: PostActionMenuData
}

export interface ClearPostActionSession {
  action: 'CLEAR_POST_ACTION_SESSION'
}

export interface UpdateProjectServerState {
  action: 'UPDATE_PROJECT_SERVER_STATE'
  serverState: Partial<ProjectServerState>
}

export interface UpdateTopLevelElementsFromCollaborationUpdate {
  action: 'UPDATE_TOP_LEVEL_ELEMENTS_FROM_COLLABORATION_UPDATE'
  fullPath: string
  topLevelElements: Array<TopLevelElement>
}

export interface UpdateExportsDetailFromCollaborationUpdate {
  action: 'UPDATE_EXPORTS_DETAIL_FROM_COLLABORATION_UPDATE'
  fullPath: string
  exportsDetail: Array<ExportDetail>
}

export interface UpdateImportsFromCollaborationUpdate {
  action: 'UPDATE_IMPORTS_FROM_COLLABORATION_UPDATE'
  fullPath: string
  imports: MapLike<ImportDetails>
}

export interface UpdateCodeFromCollaborationUpdate {
  action: 'UPDATE_CODE_FROM_COLLABORATION_UPDATE'
  fullPath: string
  code: string
}

export interface SetCommentFilterMode {
  action: 'SET_COMMENT_FILTER_MODE'
  commentFilterMode: CommentFilterMode
}

export interface SetCollaborators {
  action: 'SET_COLLABORATORS'
  collaborators: Collaborator[]
}

export interface ExtractPropertyControlsFromDescriptorFiles {
  action: 'EXTRACT_PROPERTY_CONTROLS_FROM_DESCRIPTOR_FILES'
  paths: string[]
}

export interface SetSharingDialogOpen {
  action: 'SET_SHARING_DIALOG_OPEN'
  open: boolean
}

export interface ResetOnlineState {
  action: 'RESET_ONLINE_STATE'
}

export interface IncreaseOnlineStateFailureCount {
  action: 'INCREASE_ONLINE_STATE_FAILURE_COUNT'
}

export type EditorAction =
  | ClearSelection
  | InsertJSXElement
  | ReplaceJSXElement
  | ReplaceMappedElement
  | ReplaceElementInScope
  | InsertAttributeOtherJavascriptIntoElement
  | DeleteSelected
  | DeleteView
  | UpdateEditorMode
  | SwitchEditorMode
  | SelectComponents
  | UnsetProperty
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
  | SetPanelVisibility
  | ToggleFocusedOmniboxTab
  | TogglePane
  | ClosePopup
  | OpenPopup
  | CopySelectionToClipboard
  | CutSelectionToClipboard
  | CopyProperties
  | PasteProperties
  | SetProjectID
  | SetForkedFromProjectID
  | OpenTextEditor
  | CloseTextEditor
  | SetLeftMenuTab
  | SetLeftMenuExpanded
  | SetRightMenuTab
  | SetRightMenuExpanded
  | ToggleCollapse
  | AddCollapsedViews
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
  | ToggleInterfaceDesignerAdditionalControls
  | SaveCurrentFile
  | SaveAsset
  | ResetPins
  | WrapInElement
  | OpenFloatingInsertMenu
  | CloseFloatingInsertMenu
  | UnwrapElements
  | SetNavigatorRenamingTarget
  | RedrawOldCanvasControls
  | UpdateFrameDimensions
  | SetStoredFontSettings
  | SetCanvasFrames
  | SelectAllSiblings
  | UpdateCodeResultCache
  | SetCodeEditorVisibility
  | OpenCodeEditor
  | SetProjectName
  | SetProjectDescription
  | UpdatePreviewConnected
  | AlignSelectedViews
  | DistributeSelectedViews
  | SetCursorOverlay
  | DuplicateSpecificElements
  | UpdateDuplicationState
  | SendPreviewModel
  | UpdateFilePath
  | UpdateRemixRoute
  | OpenCodeEditorFile
  | CloseDesignerFile
  | UpdateFile
  | UpdateProjectContents
  | UpdateGithubSettings
  | UpdateGithubData
  | RemoveFileConflict
  | UpdateFromWorker
  | ClearParseOrPrintInFlight
  | ClearImageFileBlob
  | AddFolder
  | DeleteFile
  | DeleteFileFromCollaboration
  | AddTextFile
  | AddNewPage
  | AddNewFeaturedRoute
  | RemoveFeaturedRoute
  | SetMainUIFile
  | SetCodeEditorBuildErrors
  | SetCodeEditorLintErrors
  | SetCodeEditorComponentDescriptorErrors
  | SaveDOMReport
  | RunDOMWalker
  | TrueUpElements
  | SetProp
  | SetFilebrowserRenamingTarget
  | ToggleProperty
  | DEPRECATEDToggleEnabledProperty
  | InsertImageIntoUI
  | SetFocus
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
  | UpdateText
  | SetFocusedElement
  | ScrollToElement
  | ScrollToPosition
  | SetScrollAnimation
  | SetFollowSelectionEnabled
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
  | DecrementResizeOptionsSelectedIndex
  | IncrementResizeOptionsSelectedIndex
  | SetResizeOptionsTargetOptions
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
  | SetMapCountOverride
  | SwitchConditionalBranches
  | UpdateConditionalExpression
  | ExecutePostActionMenuChoice
  | ClearPostActionSession
  | StartPostActionSession
  | FromVSCodeAction
  | TruncateHistory
  | UpdateProjectServerState
  | UpdateTopLevelElementsFromCollaborationUpdate
  | UpdateExportsDetailFromCollaborationUpdate
  | UpdateImportsFromCollaborationUpdate
  | UpdateCodeFromCollaborationUpdate
  | SetCommentFilterMode
  | SetForking
  | SetCollaborators
  | ExtractPropertyControlsFromDescriptorFiles
  | SetSharingDialogOpen
  | ResetOnlineState
  | IncreaseOnlineStateFailureCount

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
