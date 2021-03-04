import { LayoutSystem, NormalisedFrame } from 'utopia-api'
import {
  ElementInstanceMetadata,
  JSXAttribute,
  JSXElement,
  JSXElementName,
  JSXMetadata,
  SettableLayoutSystem,
} from '../../core/shared/element-template'
import { KeysPressed, Key } from '../../utils/keyboard'
import { IndexPosition } from '../../utils/utils'
import { CanvasRectangle, Size, WindowPoint, CanvasPoint } from '../../core/shared/math-utils'
import { CanvasAction, CSSCursor, PinOrFlexFrameChange } from '../canvas/canvas-types'
import { CodeEditorTheme } from '../code-editor/code-editor-themes'
import { CursorPosition } from '../code-editor/code-editor-utils'
import { EditorPane, EditorPanel, ResizeLeftPane, SetFocus } from '../common/actions'
import {
  InstancePath,
  LayoutWrapper,
  ProjectFile,
  PropertyPath,
  ScenePath,
  StaticElementPath,
  TemplatePath,
  TextFile,
  NodeModules,
  Imports,
} from '../../core/shared/project-file-types'
import { CodeResultCache, PropertyControlsInfo } from '../custom-code/code-file'
import { ElementContextMenuInstance } from '../element-context-menu'
import { FontSettings } from '../inspector/common/css-utils'
import { CSSTarget } from '../inspector/sections/header-section/target-selector'
import { LocalNavigatorAction } from '../navigator/actions/index'
import { LeftMenuTab } from '../navigator/left-pane'
import { RightMenuTab } from '../canvas/right-menu'
import { Mode } from './editor-modes'
import type {
  RequestedNpmDependency,
  PackageStatusMap,
  PackageStatus,
} from '../../core/shared/npm-dependency-types'
import {
  DuplicationState,
  EditorState,
  ErrorMessages,
  ModalDialog,
  OriginalFrame,
  PersistentModel,
  StoredEditorState,
} from './store/editor-state'
import { Notice } from '../common/notice'
import { BuildType } from '../../core/workers/ts/ts-worker'
import type { EditorTab } from './store/editor-tabs'
import { ContextMenuInnerProps } from '../../uuiui-deps'
export { isLoggedIn, loggedInUser, LoginState, notLoggedIn, UserDetails } from '../../common/user'

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

export type EditorModel = EditorState

export type MoveRowBefore = {
  type: 'MOVE_ROW_BEFORE'
  target: TemplatePath
}

export type MoveRowAfter = {
  type: 'MOVE_ROW_AFTER'
  target: TemplatePath
}

export type ReparentRow = {
  type: 'REPARENT_ROW'
  target: TemplatePath
}

export type ReparentToIndex = {
  type: 'REPARENT_TO_INDEX'
  target: TemplatePath
  index: number
}

export type DropTarget = MoveRowBefore | MoveRowAfter | ReparentRow | ReparentToIndex

export type NavigatorReorder = {
  action: 'NAVIGATOR_REORDER'
  dragSources: Array<TemplatePath>
  dropTarget: DropTarget
}

export type RenameComponent = {
  action: 'RENAME_COMPONENT'
  target: TemplatePath
  name: string | null
}

export type ClearSelection = {
  action: 'CLEAR_SELECTION'
}

export interface InsertScene {
  action: 'INSERT_SCENE'
  frame: CanvasRectangle
}

export interface SetSceneProp {
  action: 'SET_SCENE_PROP'
  scenePath: ScenePath
  propertyPath: PropertyPath
  value: JSXAttribute
}

export interface UnsetSceneProp {
  action: 'UNSET_SCENE_PROP'
  scenePath: ScenePath
  propertyPath: PropertyPath
}

export interface InsertJSXElement {
  action: 'INSERT_JSX_ELEMENT'
  jsxElement: JSXElement
  parent: TemplatePath | null
  importsToAdd: Imports
}

export type DeleteSelected = {
  action: 'DELETE_SELECTED'
}

export type DeleteView = {
  action: 'DELETE_VIEW'
  target: TemplatePath
}

export type DeleteViews = {
  action: 'DELETE_VIEWS'
  targets: Array<TemplatePath>
}

export type SelectComponents = {
  action: 'SELECT_COMPONENTS'
  target: Array<TemplatePath>
  addToSelection: boolean
}

export type UpdateEditorMode = {
  action: 'UPDATE_EDITOR_MODE'
  mode: Mode
}

export type SwitchEditorMode = {
  action: 'SWITCH_EDITOR_MODE'
  mode: Mode
}

export interface ToggleCanvasIsLive {
  action: 'TOGGLE_CANVAS_IS_LIVE'
}

export type ToggleHidden = {
  action: 'TOGGLE_HIDDEN'
  targets: Array<TemplatePath>
}

export type UnsetProperty = {
  action: 'UNSET_PROPERTY'
  element: InstancePath
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
  paths: Array<TemplatePath>
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
  target: TemplatePath
  indexPosition: IndexPosition
}

export type TransientActions = {
  action: 'TRANSIENT_ACTIONS'
  transientActions: Array<EditorAction>
}

export type Atomic = {
  action: 'ATOMIC'
  actions: Array<EditorAction>
}

export type NewProject = {
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

export interface PasteJSXElements {
  action: 'PASTE_JSX_ELEMENTS'
  elements: JSXElement[]
  originalTemplatePaths: TemplatePath[]
  targetOriginalContextMetadata: JSXMetadata
}

export interface CopySelectionToClipboard {
  action: 'COPY_SELECTION_TO_CLIPBOARD'
}

export interface SetProjectID {
  action: 'SET_PROJECT_ID'
  id: string
}

export interface OpenTextEditor {
  action: 'OPEN_TEXT_EDITOR'
  target: InstancePath
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
  target: TemplatePath
}

export interface AddToast {
  action: 'ADD_TOAST'
  toast: Notice
}

export interface RemoveToast {
  action: 'REMOVE_TOAST'
  id: string
}

export interface SetHighlightedView {
  action: 'SET_HIGHLIGHTED_VIEW'
  target: TemplatePath
}

export interface ClearHighlightedViews {
  action: 'CLEAR_HIGHLIGHTED_VIEWS'
}

export type UpdateKeysPressed = {
  action: 'UPDATE_KEYS_PRESSED'
  keys: KeysPressed
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
  parentPath: TemplatePath | null
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
  hash: string
  imageDetails: SaveImageDetails | null
}

export type ResetPins = {
  action: 'RESET_PINS'
  target: InstancePath
}

export interface WrapInView {
  action: 'WRAP_IN_VIEW'
  targets: TemplatePath[]
  layoutSystem: LayoutSystem
}

export interface UnwrapGroupOrView {
  action: 'UNWRAP_GROUP_OR_VIEW'
  target: TemplatePath
  onlyForGroups: boolean
}

export interface SetCanvasAnimationsEnabled {
  action: 'SET_CANVAS_ANIMATIONS_ENABLED'
  value: boolean
}

export interface UpdateFrameDimensions {
  action: 'UPDATE_FRAME_DIMENSIONS'
  element: TemplatePath
  width: number
  height: number
}

export interface SetNavigatorRenamingTarget {
  action: 'SET_NAVIGATOR_RENAMING_TARGET'
  target: TemplatePath | null
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

export interface SetHighlightsEnabled {
  action: 'SET_HIGHLIGHTS_ENABLED'
  value: boolean
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
  props: ContextMenuInnerProps | null
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

export interface OpenEditorTab {
  action: 'OPEN_FILE'
  editorTab: EditorTab
  cursorPosition: CursorPosition | null
}

export interface CloseEditorTab {
  action: 'CLOSE_FILE'
  editorTab: EditorTab
}

export interface ReorderEditorTabs {
  action: 'REORDER_OPEN_FILES'
  editorTab: EditorTab
  newIndex: number
}

export interface UpdateFile {
  action: 'UPDATE_FILE'
  filePath: string
  file: ProjectFile
  addIfNotInFiles: boolean
}

export interface UpdateFromWorker {
  action: 'UPDATE_FROM_WORKER'
  filePath: string
  file: TextFile
  codeOrModel: 'Code' | 'Model'
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
  elementMetadata: Array<ElementInstanceMetadata>
}

export interface SetProp {
  action: 'SET_PROP'
  target: InstancePath
  propertyPath: PropertyPath
  value: JSXAttribute
}

export interface SetPropWithElementPath {
  action: 'SET_PROP_WITH_ELEMENT_PATH'
  target: StaticElementPath
  propertyPath: PropertyPath
  value: JSXAttribute
}

export interface SetFilebrowserRenamingTarget {
  action: 'SET_FILEBROWSER_RENAMING_TARGET'
  filename: string | null
}

export interface ToggleProperty {
  action: 'TOGGLE_PROPERTY'
  target: InstancePath
  // FIXME: This will cause problems with multi-user editing.
  togglePropValue: (element: JSXElement) => JSXElement
}

export interface DEPRECATEDToggleEnabledProperty {
  action: 'deprecated_TOGGLE_ENABLED_PROPERTY'
  target: InstancePath
  // FIXME: This will cause problems with multi-user editing.
  togglePropValue: (element: JSXElement) => JSXElement
}

export type TextFormattingType = 'bold' | 'italic' | 'underline'

export interface SwitchLayoutSystem {
  action: 'SWITCH_LAYOUT_SYSTEM'
  layoutSystem: SettableLayoutSystem
}

export interface InsertImageIntoUI {
  action: 'INSERT_IMAGE_INTO_UI'
  imagePath: string
}

export interface WrapInLayoutable {
  action: 'WRAP_IN_LAYOUTABLE'
  target: InstancePath
  wrapper: LayoutWrapper
}

export interface UnwrapLayoutable {
  action: 'UNWRAP_LAYOUTABLE'
  target: InstancePath
}

export interface UpdateJSXElementName {
  action: 'UPDATE_JSX_ELEMENT_NAME'
  target: InstancePath
  elementName: JSXElementName
}

export interface SetAspectRatioLock {
  action: 'SET_ASPECT_RATIO_LOCK'
  target: InstancePath
  locked: boolean
}

export interface RenameStyleSelector {
  action: 'RENAME_PROP_KEY'
  target: InstancePath
  cssTargetPath: CSSTarget
  value: Array<string>
}

export interface SetCodeEditorTheme {
  action: 'SET_CODE_EDITOR_THEME'
  value: CodeEditorTheme
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
  imagePath: string
  position: CanvasPoint
}

export interface ResetPropToDefault {
  action: 'RESET_PROP_TO_DEFAULT'
  target: TemplatePath
  path: PropertyPath | null
}

export interface UpdateNodeModulesContents {
  action: 'UPDATE_NODE_MODULES_CONTENTS'
  contentsToAdd: NodeModules
  buildType: BuildType
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
  target: InstancePath
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

export interface PropertyControlsIFrameReady {
  action: 'PROPERTY_CONTROLS_IFRAME_READY'
}

export interface AddStoryboardFile {
  action: 'ADD_STORYBOARD_FILE'
}

export interface UpdateChildText {
  action: 'UPDATE_CHILD_TEXT'
  target: InstancePath
  text: string
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

export type EditorAction =
  | ClearSelection
  | InsertScene
  | InsertJSXElement
  | DeleteSelected
  | DeleteView
  | DeleteViews
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
  | TogglePane
  | ClosePopup
  | OpenPopup
  | PasteJSXElements
  | CopySelectionToClipboard
  | SetProjectID
  | OpenTextEditor
  | CloseTextEditor
  | SetLeftMenuTab
  | SetLeftMenuExpanded
  | SetRightMenuTab
  | SetRightMenuExpanded
  | ToggleCollapse
  | AddToast
  | RemoveToast
  | SetHighlightedView
  | ClearHighlightedViews
  | UpdateKeysPressed
  | HideModal
  | ShowModal
  | ResizeInterfaceDesignerCodePane
  | ToggleInterfaceDesignerCodeEditor
  | ToggleInterfaceDesignerAdditionalControls
  | SaveCurrentFile
  | SaveAsset
  | ResetPins
  | WrapInView
  | UnwrapGroupOrView
  | SetCanvasAnimationsEnabled
  | SetNavigatorRenamingTarget
  | RedrawOldCanvasControls
  | UpdateFrameDimensions
  | SetStoredFontSettings
  | SetCanvasFrames
  | SelectAllSiblings
  | UpdateCodeResultCache
  | SetCodeEditorVisibility
  | SetProjectName
  | RegenerateThumbnail
  | UpdateThumbnailGenerated
  | UpdatePreviewConnected
  | SetHighlightsEnabled
  | AlignSelectedViews
  | DistributeSelectedViews
  | SetCursorOverlay
  | DuplicateSpecificElements
  | UpdateDuplicationState
  | SendPreviewModel
  | UpdateFilePath
  | OpenEditorTab
  | CloseEditorTab
  | ReorderEditorTabs
  | UpdateFile
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
  | SetSceneProp
  | UnsetSceneProp
  | SetFocus
  | ResizeLeftPane
  | WrapInLayoutable
  | UnwrapLayoutable
  | SetAspectRatioLock
  | UpdateJSXElementName
  | ToggleCanvasIsLive
  | RenameStyleSelector
  | SetCodeEditorTheme
  | SetSafeMode
  | SetSaveError
  | InsertDroppedImage
  | ResetPropToDefault
  | UpdateNodeModulesContents
  | UpdatePackageJson
  | StartCheckpointTimer
  | FinishCheckpointTimer
  | AddMissingDimensions
  | SetPackageStatus
  | SetShortcut
  | UpdatePropertyControlsInfo
  | PropertyControlsIFrameReady
  | AddStoryboardFile
  | UpdateChildText
  | MarkVSCodeBridgeReady
  | SelectFromFileAndPosition
  | SendCodeEditorInitialisation

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

export type Alignment = 'left' | 'hcenter' | 'right' | 'top' | 'vcenter' | 'bottom'
export type Distribution = 'horizontal' | 'vertical'
