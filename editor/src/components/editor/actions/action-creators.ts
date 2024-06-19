import type { LoginState } from '../../../common/user'
import type { LayoutTargetableProp } from '../../../core/layout/layout-helpers-new'
import type {
  JSExpression,
  JSXElement,
  JSXElementName,
  ElementInstanceMetadataMap,
  JSXElementChild,
  TopLevelElement,
} from '../../../core/shared/element-template'
import type {
  CanvasPoint,
  CanvasRectangle,
  Size,
  WindowPoint,
} from '../../../core/shared/math-utils'
import type {
  PackageStatus,
  RequestedNpmDependency,
} from '../../../core/shared/npm-dependency-types'
import type {
  Imports,
  NodeModules,
  ParsedTextFile,
  ProjectFile,
  PropertyPath,
  ElementPath,
  ImageFile,
  ExportDetail,
} from '../../../core/shared/project-file-types'
import type { BuildType } from '../../../core/workers/common/worker-types'
import type { Key, KeysPressed } from '../../../utils/keyboard'
import type { IndexPosition } from '../../../utils/utils'
import type { CSSCursor } from '../../../uuiui-deps'
import type { ProjectContentTreeRoot } from '../../assets'
import CanvasActions from '../../canvas/canvas-actions'
import type { PinOrFlexFrameChange, SelectionLocked } from '../../canvas/canvas-types'
import type { CanvasCommand } from '../../canvas/commands/commands'
import type { EditorPane, EditorPanel } from '../../common/actions'
import type { Notice } from '../../common/notice'
import type { CodeResultCache, PropertyControlsInfo } from '../../custom-code/code-file'
import type { ElementContextMenuInstance } from '../../element-context-menu'
import type { FontSettings } from '../../inspector/common/css-utils'
import type { CSSTarget } from '../../inspector/sections/header-section/target-selector'
import type { InsertableComponent, StylePropOption } from '../../shared/project-components'
import type {
  AddFolder,
  AddMissingDimensions,
  AddTextFile,
  Alignment,
  AlignSelectedViews,
  Atomic,
  ClearHighlightedViews,
  ClearImageFileBlob,
  ClearParseOrPrintInFlight,
  ClosePopup,
  CloseTextEditor,
  CopySelectionToClipboard,
  DeleteFile,
  DeleteView,
  DistributeSelectedViews,
  Distribution,
  DuplicateSelected,
  DuplicateSpecificElements,
  EditorAction,
  EditorDispatch,
  FinishCheckpointTimer,
  HideModal,
  InsertDroppedImage,
  InsertImageIntoUI,
  InsertJSXElement,
  MoveSelectedBackward,
  MoveSelectedForward,
  MoveSelectedToBack,
  MoveSelectedToFront,
  OpenCodeEditorFile,
  OpenPopup,
  OpenTextEditor,
  AddToast,
  RemoveToast,
  Redo,
  RedrawOldCanvasControls,
  RenameStyleSelector,
  ResetPins,
  SaveAsset,
  SaveCurrentFile,
  SaveDOMReport,
  SaveImageDetails,
  SaveImageDoNothing,
  SaveImageInsertWith,
  SaveImageReplace,
  SaveImageSwitchMode,
  SelectAllSiblings,
  SelectComponents,
  SendPreviewModel,
  SetAspectRatioLock,
  SetCanvasFrames,
  SetCodeEditorBuildErrors,
  SetCodeEditorLintErrors,
  SetCodeEditorVisibility,
  SetCursorOverlay,
  SetFilebrowserRenamingTarget,
  SetHighlightedViews,
  SetLeftMenuExpanded,
  SetLeftMenuTab,
  SetMainUIFile,
  SetNavigatorRenamingTarget,
  SetPackageStatus,
  SetPanelVisibility,
  SetProjectID,
  SetProjectName,
  SetProjectDescription,
  SetProp,
  SetRightMenuExpanded,
  SetRightMenuTab,
  SetSafeMode,
  SetSaveError,
  SetShortcut,
  SetStoredFontSettings,
  SetZIndex,
  ShowContextMenu,
  ShowModal,
  StartCheckpointTimer,
  SwitchEditorMode,
  ToggleCanvasIsLive,
  ToggleCollapse,
  ToggleHidden,
  ToggleInterfaceDesignerAdditionalControls,
  TogglePane,
  ToggleProperty,
  TransientActions,
  Undo,
  UnsetProperty,
  UnwrapElements,
  UpdateText,
  UpdateCodeResultCache,
  UpdateDuplicationState,
  UpdateEditorMode,
  UpdateFile,
  UpdateFilePath,
  UpdateFrameDimensions,
  UpdateFromWorker,
  UpdateJSXElementName,
  UpdateKeysPressed,
  UpdateNodeModulesContents,
  UpdatePackageJson,
  UpdatePreviewConnected,
  UpdatePropertyControlsInfo,
  CloseDesignerFile,
  SetFocusedElement,
  AddImports,
  ScrollToElement,
  WorkerParsedUpdate,
  SetScrollAnimation,
  SetFollowSelectionEnabled,
  SetLoginState,
  ResetCanvas,
  SetFilebrowserDropTarget,
  SetForkedFromProjectID,
  SetCurrentTheme,
  FocusFormulaBar,
  UpdateFormulaBarMode,
  InsertInsertable,
  ToggleFocusedOmniboxTab,
  SetPropTransient,
  ClearTransientProps,
  AddTailwindConfig,
  FocusClassNameInput,
  WrapInElement,
  DecrementResizeOptionsSelectedIndex,
  IncrementResizeOptionsSelectedIndex,
  SetResizeOptionsTargetOptions,
  ForceParseFile,
  RemoveFromNodeModulesContents,
  RunEscapeHatch,
  UpdateMouseButtonsPressed,
  ToggleSelectionLock,
  ElementPaste,
  SetGithubState,
  UpdateProjectContents,
  UpdateGithubSettings,
  SetImageDragSessionState as SetDragSessionState,
  UpdateGithubOperations,
  UpdateBranchContents,
  UpdateAgainstGithub,
  UpdateGithubData,
  RemoveFileConflict,
  SetRefreshingDependencies,
  SetUserConfiguration,
  SetHoveredViews,
  ClearHoveredViews,
  ApplyCommandsAction,
  WorkerCodeAndParsedUpdate,
  UpdateColorSwatches,
  PasteProperties,
  CopyProperties,
  MergeWithPrevUndo,
  SetConditionalOverriddenCondition,
  SwitchConditionalBranches,
  UpdateConditionalExpression,
  CutSelectionToClipboard,
  ExecutePostActionMenuChoice,
  StartPostActionSession,
  ClearPostActionSession,
  ScrollToElementBehaviour,
  OpenCodeEditor,
  SetMapCountOverride,
  TruncateHistory,
  RunDOMWalker,
  WrapInElementWith,
  ScrollToPosition,
  UpdateProjectServerState,
  UpdateTopLevelElementsFromCollaborationUpdate,
  DeleteFileFromCollaboration,
  UpdateExportsDetailFromCollaborationUpdate,
  UpdateImportsFromCollaborationUpdate,
  UpdateCodeFromCollaborationUpdate,
  SetCommentFilterMode,
  SetForking,
  SetCollaborators,
  ExtractPropertyControlsFromDescriptorFiles,
  SetSharingDialogOpen,
  SetCodeEditorComponentDescriptorErrors,
  AddNewPage,
  UpdateRemixRoute,
  AddNewFeaturedRoute,
  RemoveFeaturedRoute,
  ResetOnlineState,
  IncreaseOnlineStateFailureCount,
  AddCollapsedViews,
  ReplaceMappedElement,
  ReplaceTarget,
  InsertAsChildTarget,
  ReplaceKeepChildrenAndStyleTarget,
  WrapTarget,
  ReplaceElementInScope,
  ElementReplacementPath,
  ReplaceJSXElement,
  ToggleDataCanCondense,
} from '../action-types'
import type { InsertionSubjectWrapper, Mode } from '../editor-modes'
import { EditorModes, insertionSubject } from '../editor-modes'
import type {
  ImageDragSessionState,
  DuplicationState,
  ErrorMessages,
  GithubState,
  LeftMenuTab,
  ModalDialog,
  OriginalFrame,
  ProjectGithubSettings,
  RightMenuTab,
  GithubOperation,
  GithubData,
  UserConfiguration,
  ThemeSetting,
  ColorSwatch,
  PostActionMenuData,
} from '../store/editor-state'
import type { InsertionPath } from '../store/insertion-path'
import type { TextProp } from '../../text-editor/text-editor'
import type { PostActionChoice } from '../../canvas/canvas-strategies/post-action-options/post-action-options'
import type { ProjectServerState } from '../store/project-server-state'
import type { SetHuggingParentToFixed } from '../../canvas/canvas-strategies/strategies/convert-to-absolute-and-move-strategy'
import type { CommentFilterMode } from '../../inspector/sections/comment-section'
import type { Collaborator } from '../../../core/shared/multiplayer'
import type { PageTemplate } from '../../canvas/remix/remix-utils'
import type { Bounds } from 'utopia-vscode-common'

export function clearSelection(): EditorAction {
  return {
    action: 'CLEAR_SELECTION',
  }
}

export const replaceTarget: ReplaceTarget = { type: 'replace-target' }
export const wrapTarget: WrapTarget = { type: 'wrap-target' }
export const replaceKeepChildrenAndStyleTarget: ReplaceKeepChildrenAndStyleTarget = {
  type: 'replace-target-keep-children-and-style',
}
export function insertAsChildTarget(indexPosition?: IndexPosition): InsertAsChildTarget {
  return { type: 'insert-as-child', indexPosition: indexPosition }
}

export function insertJSXElement(
  element: JSXElement,
  target: ElementPath | null,
  importsToAdd: Imports,
  indexPosition?: IndexPosition,
): InsertJSXElement {
  return {
    action: 'INSERT_JSX_ELEMENT',
    jsxElement: element,
    target: target,
    importsToAdd: importsToAdd,
    indexPosition: indexPosition ?? null,
  }
}

export function replaceJSXElement(
  element: JSXElement,
  target: ElementPath,
  importsToAdd: Imports,
  behaviour: ReplaceKeepChildrenAndStyleTarget | ReplaceTarget,
): ReplaceJSXElement {
  return {
    action: 'REPLACE_JSX_ELEMENT',
    jsxElement: element,
    target: target,
    importsToAdd: importsToAdd,
    behaviour: behaviour,
  }
}

export function replaceMappedElement(
  element: JSXElement,
  target: ElementPath,
  importsToAdd: Imports,
): ReplaceMappedElement {
  return {
    action: 'REPLACE_MAPPED_ELEMENT',
    jsxElement: element,
    target: target,
    importsToAdd: importsToAdd,
  }
}

export function replaceElementInScope(
  target: ElementPath,
  replacementPath: ElementReplacementPath,
): ReplaceElementInScope {
  return {
    action: 'REPLACE_ELEMENT_IN_SCOPE',
    target: target,
    replacementPath: replacementPath,
  }
}

export function deleteView(target: ElementPath): DeleteView {
  return {
    action: 'DELETE_VIEW',
    target: target,
  }
}

export function deleteSelected(): EditorAction {
  return {
    action: 'DELETE_SELECTED',
  }
}

export function unsetProperty(element: ElementPath, property: PropertyPath): UnsetProperty {
  return {
    action: 'UNSET_PROPERTY',
    element: element,
    property: property,
  }
}

export function toggleHidden(targets: Array<ElementPath> = []): ToggleHidden {
  return {
    action: 'TOGGLE_HIDDEN',
    targets: targets,
  }
}

export function toggleDataCanCondense(targets: Array<ElementPath>): ToggleDataCanCondense {
  return {
    action: 'TOGGLE_DATA_CAN_CONDENSE',
    targets: targets,
  }
}

export function transientActions(
  actions: Array<EditorAction>,
  elementsToRerender: Array<ElementPath> | null = null,
): TransientActions {
  return {
    action: 'TRANSIENT_ACTIONS',
    transientActions: actions,
    elementsToRerender: elementsToRerender,
  }
}

export function mergeWithPrevUndo(actions: Array<EditorAction>): MergeWithPrevUndo {
  return {
    action: 'MERGE_WITH_PREV_UNDO',
    actions: actions,
  }
}

export function selectComponents(
  target: Array<ElementPath>,
  addToSelection: boolean,
): SelectComponents {
  return {
    action: 'SELECT_COMPONENTS',
    target: target,
    addToSelection: addToSelection,
  }
}

export function updateEditorMode(mode: Mode): UpdateEditorMode {
  return {
    action: 'UPDATE_EDITOR_MODE',
    mode: mode,
  }
}

export function switchEditorMode(
  mode: Mode,
  unlessMode?: 'select' | 'live' | 'insert' | 'textEdit',
): SwitchEditorMode {
  return {
    action: 'SWITCH_EDITOR_MODE',
    mode: mode,
    unlessMode: unlessMode,
  }
}

export function duplicateSelected(): DuplicateSelected {
  return {
    action: 'DUPLICATE_SELECTED',
  }
}

export function duplicateSpecificElements(paths: Array<ElementPath>): DuplicateSpecificElements {
  return {
    action: 'DUPLICATE_SPECIFIC_ELEMENTS',
    paths: paths,
  }
}

export function updateDuplicationState(
  duplicationState: DuplicationState | null,
): UpdateDuplicationState {
  return {
    action: 'UPDATE_DUPLICATION_STATE',
    duplicationState: duplicationState,
  }
}

export function setCanvasFrames(
  framesAndTargets: Array<PinOrFlexFrameChange>,
  keepChildrenGlobalCoords: boolean,
  originalFrames: Array<OriginalFrame> | null = null,
): SetCanvasFrames {
  return {
    action: 'SET_CANVAS_FRAMES',
    framesAndTargets: framesAndTargets,
    keepChildrenGlobalCoords: keepChildrenGlobalCoords,
    originalFrames: originalFrames,
  }
}

export function setPanelVisibility(
  target: EditorPanel | EditorPane,
  visible: boolean,
): SetPanelVisibility {
  return {
    action: 'SET_PANEL_VISIBILITY',
    target: target,
    visible: visible,
  }
}

export function toggleFocusedOmniboxTab(): ToggleFocusedOmniboxTab {
  return {
    action: 'TOGGLE_FOCUSED_OMNIBOX_TAB',
  }
}

export function togglePanel(panel: EditorPanel | EditorPane): TogglePane {
  return {
    action: 'TOGGLE_PANE',
    target: panel,
  }
}

export function openPopup(popupId: string): OpenPopup {
  return {
    action: 'OPEN_POPUP',
    popupId: popupId,
  }
}

export function closePopup(): ClosePopup {
  return {
    action: 'CLOSE_POPUP',
  }
}

export function elementPaste(
  element: JSXElementChild,
  importsToAdd: Imports,
  originalElementPath: ElementPath,
  duplicateNameMap?: Map<string, string>,
): ElementPaste {
  return {
    element: element,
    importsToAdd: importsToAdd,
    originalElementPath: originalElementPath,
    duplicateNameMap: duplicateNameMap,
  }
}

export function copySelectionToClipboard(): CopySelectionToClipboard {
  return {
    action: 'COPY_SELECTION_TO_CLIPBOARD',
  }
}

export function cutSelectionToClipboard(): CutSelectionToClipboard {
  return {
    action: 'CUT_SELECTION_TO_CLIPBOARD',
  }
}

export function copyProperties(): CopyProperties {
  return {
    action: 'COPY_PROPERTIES',
  }
}

export function pasteProperties(type: 'style' | 'layout'): PasteProperties {
  return {
    action: 'PASTE_PROPERTIES',
    type: type,
  }
}

export function openTextEditor(
  target: ElementPath,
  mousePosition: WindowPoint | null, // if mousePosition is zero, the whole text will be selected
): OpenTextEditor {
  return {
    action: 'OPEN_TEXT_EDITOR',
    target: target,
    mousePosition: mousePosition,
  }
}

export function closeTextEditor(): CloseTextEditor {
  return {
    action: 'CLOSE_TEXT_EDITOR',
  }
}

export function toggleCollapse(target: ElementPath): ToggleCollapse {
  return {
    action: 'TOGGLE_COLLAPSE',
    target: target,
  }
}

export function addCollapsedViews(collapsedViews: ElementPath[]): AddCollapsedViews {
  return {
    action: 'ADD_COLLAPSED_VIEWS',
    collapsedViews: collapsedViews,
  }
}

export function enableInsertModeForJSXElement(
  element: JSXElement,
  uid: string,
  importsToAdd: Imports,
  size: Size | null,
  options?: {
    textEdit?: boolean
    wrapInContainer?: InsertionSubjectWrapper
  },
): SwitchEditorMode {
  return switchEditorMode(
    EditorModes.insertMode([
      insertionSubject(
        uid,
        element,
        size,
        importsToAdd,
        null,
        options?.textEdit ?? false,
        options?.wrapInContainer ?? null,
      ),
    ]),
  )
}

export function addToast(toastContent: Notice): AddToast {
  return {
    action: 'ADD_TOAST',
    toast: toastContent,
  }
}

export function removeToast(id: string): RemoveToast {
  return {
    action: 'REMOVE_TOAST',
    id: id,
  }
}

export function showToast(toastContent: Notice): AddToast {
  return addToast(toastContent)
}

export function setForking(forking: boolean): SetForking {
  return {
    action: 'SET_FORKING',
    forking: forking,
  }
}

let selectionControlTimer: any // TODO maybe this should live inside the editormodel
export function hideAndShowSelectionControls(dispatch: EditorDispatch): void {
  dispatch([CanvasActions.setSelectionControlsVisibility(false)], 'canvas')
  if (selectionControlTimer != null) {
    window.clearTimeout(selectionControlTimer)
  }
  selectionControlTimer = window.setTimeout(() => {
    dispatch([CanvasActions.setSelectionControlsVisibility(true)], 'canvas')
    selectionControlTimer = null
  }, 2000)
}

export function setLeftMenuTab(tab: LeftMenuTab): SetLeftMenuTab {
  return {
    action: 'SET_LEFT_MENU_TAB',
    tab: tab,
  }
}

export function setLeftMenuExpanded(expanded: boolean): SetLeftMenuExpanded {
  return {
    action: 'SET_LEFT_MENU_EXPANDED',
    expanded: expanded,
  }
}

export function setRightMenuTab(tab: RightMenuTab): SetRightMenuTab {
  return {
    action: 'SET_RIGHT_MENU_TAB',
    tab: tab,
  }
}

export function setRightMenuExpanded(expanded: boolean): SetRightMenuExpanded {
  return {
    action: 'SET_RIGHT_MENU_EXPANDED',
    expanded: expanded,
  }
}

export function setHighlightedView(target: ElementPath): SetHighlightedViews {
  return {
    action: 'SET_HIGHLIGHTED_VIEWS',
    targets: [target],
  }
}

export function setHighlightedViews(targets: ElementPath[]): SetHighlightedViews {
  return {
    action: 'SET_HIGHLIGHTED_VIEWS',
    targets: targets,
  }
}

export function setHoveredView(target: ElementPath): SetHoveredViews {
  return {
    action: 'SET_HOVERED_VIEWS',
    targets: [target],
  }
}

export function setHoveredViews(targets: ElementPath[]): SetHoveredViews {
  return {
    action: 'SET_HOVERED_VIEWS',
    targets: targets,
  }
}

export function clearHighlightedViews(): ClearHighlightedViews {
  return {
    action: 'CLEAR_HIGHLIGHTED_VIEWS',
  }
}

export function clearHoveredViews(): ClearHoveredViews {
  return {
    action: 'CLEAR_HOVERED_VIEWS',
  }
}

export function updateKeys(keys: KeysPressed): UpdateKeysPressed {
  return {
    action: 'UPDATE_KEYS_PRESSED',
    keys: keys,
  }
}

export function updateMouseButtonsPressed(
  added: number | null,
  removed: number | null,
): UpdateMouseButtonsPressed {
  return {
    action: 'UPDATE_MOUSE_BUTTONS_PRESSED',
    added: added,
    removed: removed,
  }
}

export function hideModal(): HideModal {
  return {
    action: 'HIDE_MODAL',
  }
}

export function showModal(modal: ModalDialog): ShowModal {
  return {
    action: 'SHOW_MODAL',
    modal: modal,
  }
}

export function toggleInterfaceDesignerAdditionalControls(): ToggleInterfaceDesignerAdditionalControls {
  return {
    action: 'TOGGLE_INTERFACEDESIGNER_ADDITIONAL_CONTROLS',
  }
}

export function saveImageSwitchMode(): SaveImageSwitchMode {
  return {
    type: 'SAVE_IMAGE_SWITCH_MODE',
  }
}

export function saveImageDoNothing(): SaveImageDoNothing {
  return {
    type: 'SAVE_IMAGE_DO_NOTHING',
  }
}

export function saveImageReplace(): SaveImageReplace {
  return {
    type: 'SAVE_IMAGE_REPLACE',
  }
}

export function saveImageInsertWith(
  parentPath: InsertionPath | null,
  frame: CanvasRectangle,
  multiplier: number,
): SaveImageInsertWith {
  return {
    type: 'SAVE_IMAGE_INSERT_WITH',
    parentPath: parentPath,
    frame: frame,
    multiplier: multiplier,
  }
}

export function saveCurrentFile(): SaveCurrentFile {
  return {
    action: 'SAVE_CURRENT_FILE',
  }
}

export function saveImageDetails(
  imageSize: Size | null,
  afterSave: SaveImageSwitchMode | SaveImageDoNothing | SaveImageInsertWith | SaveImageReplace,
): SaveImageDetails {
  return {
    imageSize: imageSize,
    afterSave: afterSave,
  }
}

export function saveAsset(
  fileName: string,
  fileType: string,
  base64: string,
  hash: number,
  imageDetails: SaveImageDetails | null,
  gitBlobSha: string,
): SaveAsset {
  return {
    action: 'SAVE_ASSET',
    fileName: fileName,
    fileType: fileType,
    base64: base64,
    hash: hash,
    imageDetails: imageDetails,
    gitBlobSha: gitBlobSha,
  }
}

export function resetPins(target: ElementPath): ResetPins {
  return {
    action: 'RESET_PINS',
    target: target,
  }
}

export function unwrapElements(targets: ElementPath[]): UnwrapElements {
  return {
    action: 'UNWRAP_ELEMENTS',
    targets: targets,
  }
}

export function wrapInElement(
  targets: Array<ElementPath>,
  whatToWrapWith: WrapInElementWith,
): WrapInElement {
  return {
    action: 'WRAP_IN_ELEMENT',
    targets: targets,
    whatToWrapWith: whatToWrapWith,
  }
}

export function setCursorOverlay(cursor: CSSCursor | null): SetCursorOverlay {
  return {
    action: 'SET_CURSOR_OVERLAY',
    cursor: cursor,
  }
}

export function setZIndex(target: ElementPath, index: number): SetZIndex {
  return {
    action: 'SET_Z_INDEX',
    target: target,
    indexPosition: {
      type: 'absolute',
      index: index,
    },
  }
}

export function moveSelectedBackward(): MoveSelectedBackward {
  return {
    action: 'MOVE_SELECTED_BACKWARD',
  }
}

export function moveSelectedToBack(): MoveSelectedToBack {
  return {
    action: 'MOVE_SELECTED_TO_BACK',
  }
}

export function moveSelectedForward(): MoveSelectedForward {
  return {
    action: 'MOVE_SELECTED_FORWARD',
  }
}

export function moveSelectedToFront(): MoveSelectedToFront {
  return {
    action: 'MOVE_SELECTED_TO_FRONT',
  }
}

export function updateFrameDimensions(
  element: ElementPath,
  width: number,
  height: number,
): UpdateFrameDimensions {
  return {
    action: 'UPDATE_FRAME_DIMENSIONS',
    element: element,
    width: width,
    height: height,
  }
}

export function setNavigatorRenamingTarget(target: ElementPath | null): SetNavigatorRenamingTarget {
  return {
    action: 'SET_NAVIGATOR_RENAMING_TARGET',
    target: target,
  }
}

export function redrawOldCanvasControls(): RedrawOldCanvasControls {
  return {
    action: 'REDRAW_OLD_CANVAS_CONTROLS',
  }
}

export function setStoredFontSettings(fontSettings: FontSettings): SetStoredFontSettings {
  return {
    action: 'SET_STORED_FONT_SETTINGS',
    fontSettings: fontSettings,
  }
}

export function setProjectID(id: string): SetProjectID {
  return {
    action: 'SET_PROJECT_ID',
    id: id,
  }
}

export function setForkedFromProjectID(id: string | null): SetForkedFromProjectID {
  return {
    action: 'SET_FORKED_FROM_PROJECT_ID',
    id: id,
  }
}

export function atomic(actions: Array<EditorAction>): Atomic {
  return {
    action: 'ATOMIC',
    actions: actions,
  }
}

export function selectAllSiblings(): SelectAllSiblings {
  return {
    action: 'SELECT_ALL_SIBLINGS',
  }
}

export function undo(): Undo {
  return {
    action: 'UNDO',
  }
}

export function redo(): Redo {
  return {
    action: 'REDO',
  }
}

export function updateCodeResultCache(
  codeResultCache: CodeResultCache,
  buildType: BuildType,
): UpdateCodeResultCache {
  return {
    action: 'UPDATE_CODE_RESULT_CACHE',
    codeResultCache: codeResultCache,
    buildType: buildType,
  }
}

export function setCodeEditorVisibility(value: boolean): SetCodeEditorVisibility {
  return {
    action: 'SET_CODE_EDITOR_VISIBILITY',
    value: value,
  }
}

export function openCodeEditor(): OpenCodeEditor {
  return {
    action: 'OPEN_CODE_EDITOR',
  }
}

export function setProjectName(projectName: string): SetProjectName {
  return {
    action: 'SET_PROJECT_NAME',
    name: projectName,
  }
}

export function setProjectDescription(projectDescription: string): SetProjectDescription {
  return {
    action: 'SET_PROJECT_DESCRIPTION',
    description: projectDescription,
  }
}

export function updatePreviewConnected(connected: boolean): UpdatePreviewConnected {
  return {
    action: 'UPDATE_PREVIEW_CONNECTED',
    connected: connected,
  }
}

export function alignSelectedViews(alignment: Alignment): AlignSelectedViews {
  return {
    action: 'ALIGN_SELECTED_VIEWS',
    alignment: alignment,
  }
}

export function distributeSelectedViews(distribution: Distribution): DistributeSelectedViews {
  return {
    action: 'DISTRIBUTE_SELECTED_VIEWS',
    distribution: distribution,
  }
}

export function showContextMenu(
  menuName: ElementContextMenuInstance,
  event: MouseEvent,
): ShowContextMenu {
  return {
    action: 'SHOW_CONTEXT_MENU',
    menuName: menuName,
    event: event,
  }
}

export function sendPreviewModel(): SendPreviewModel {
  return {
    action: 'SEND_PREVIEW_MODEL',
  }
}

export function updateFilePath(oldPath: string, newPath: string): UpdateFilePath {
  return {
    action: 'UPDATE_FILE_PATH',
    oldPath: oldPath,
    newPath: newPath,
  }
}

export function updateRemixRoute(
  oldPath: string,
  newPath: string,
  oldRoute: string,
  newRoute: string,
): UpdateRemixRoute {
  return {
    action: 'UPDATE_REMIX_ROUTE',
    oldPath: oldPath,
    newPath: newPath,
    oldRoute: oldRoute,
    newRoute: newRoute,
  }
}

export function deleteFile(filename: string): DeleteFile {
  return {
    action: 'DELETE_FILE',
    filename: filename,
  }
}

export function deleteFileFromCollaboration(filename: string): DeleteFileFromCollaboration {
  return {
    action: 'DELETE_FILE_FROM_COLLABORATION',
    filename: filename,
  }
}

export function addFolder(parentPath: string, fileName: string): AddFolder {
  return {
    action: 'ADD_FOLDER',
    parentPath: parentPath,
    fileName: fileName,
  }
}

export function openCodeEditorFile(
  filename: string,
  forceShowCodeEditor: boolean,
  bounds: Bounds | null = null,
): OpenCodeEditorFile {
  return {
    action: 'OPEN_CODE_EDITOR_FILE',
    filename: filename,
    forceShowCodeEditor: forceShowCodeEditor,
    bounds: bounds,
  }
}

export function closeDesignerFile(filename: string): CloseDesignerFile {
  return {
    action: 'CLOSE_DESIGNER_FILE',
    filename: filename,
  }
}

export function updateFile(
  filePath: string,
  file: ProjectFile,
  addIfNotInFiles: boolean,
): UpdateFile {
  return {
    action: 'UPDATE_FILE',
    filePath: filePath,
    file: file,
    addIfNotInFiles: addIfNotInFiles,
    fromCollaboration: false,
  }
}

export function updateFileFromCollaboration(
  filePath: string,
  file: ProjectFile,
  addIfNotInFiles: boolean,
): UpdateFile {
  return {
    action: 'UPDATE_FILE',
    filePath: filePath,
    file: file,
    addIfNotInFiles: addIfNotInFiles,
    fromCollaboration: true,
  }
}

export function updateProjectContents(contents: ProjectContentTreeRoot): UpdateProjectContents {
  return {
    action: 'UPDATE_PROJECT_CONTENTS',
    contents: contents,
  }
}

export function updateBranchContents(
  contents: ProjectContentTreeRoot | null,
): UpdateBranchContents {
  return {
    action: 'UPDATE_BRANCH_CONTENTS',
    contents: contents,
  }
}

export function updateGithubSettings(
  settings: Partial<ProjectGithubSettings>,
): UpdateGithubSettings {
  return {
    action: 'UPDATE_GITHUB_SETTINGS',
    settings: settings,
  }
}

export function updateGithubData(data: Partial<GithubData>): UpdateGithubData {
  return {
    action: 'UPDATE_GITHUB_DATA',
    data: data,
  }
}

export function removeFileConflict(path: string): RemoveFileConflict {
  return {
    action: 'REMOVE_FILE_CONFLICT',
    path: path,
  }
}

export function workerCodeAndParsedUpdate(
  filePath: string,
  code: string,
  parsed: ParsedTextFile,
  versionNumber: number,
): WorkerCodeAndParsedUpdate {
  return {
    type: 'WORKER_CODE_AND_PARSED_UPDATE',
    filePath: filePath,
    code: code,
    parsed: parsed,
    versionNumber: versionNumber,
  }
}

export function workerParsedUpdate(
  filePath: string,
  parsed: ParsedTextFile,
  versionNumber: number,
): WorkerParsedUpdate {
  return {
    type: 'WORKER_PARSED_UPDATE',
    filePath: filePath,
    parsed: parsed,
    versionNumber: versionNumber,
  }
}

export function updateFromWorker(
  updates: Array<WorkerParsedUpdate | WorkerCodeAndParsedUpdate>,
): UpdateFromWorker {
  return {
    action: 'UPDATE_FROM_WORKER',
    updates: updates,
  }
}

export function clearParseOrPrintInFlight(): ClearParseOrPrintInFlight {
  return {
    action: 'CLEAR_PARSE_OR_PRINT_IN_FLIGHT',
  }
}

export function clearImageFileBlob(uiFilePath: string, elementID: string): ClearImageFileBlob {
  return {
    action: 'CLEAR_IMAGE_FILE_BLOB',
    uiFilePath: uiFilePath,
    elementID: elementID,
  }
}

export function addTextFile(parentPath: string, fileName: string): AddTextFile {
  return {
    action: 'ADD_TEXT_FILE',
    fileName: fileName,
    parentPath: parentPath,
  }
}

export function addNewPage(
  parentPath: string,
  template: PageTemplate,
  newPageName: string,
): AddNewPage {
  return {
    action: 'ADD_NEW_PAGE',
    template: template,
    parentPath: parentPath,
    newPageName: newPageName,
  }
}

export function addNewFeaturedRoute(featuredRoute: string): AddNewFeaturedRoute {
  return {
    action: 'ADD_NEW_FEATURED_ROUTE',
    featuredRoute: featuredRoute,
  }
}

export function removeFeaturedRoute(routeToRemove: string): RemoveFeaturedRoute {
  return {
    action: 'REMOVE_FEATURED_ROUTE',
    routeToRemove: routeToRemove,
  }
}

export function setMainUIFile(uiFile: string): SetMainUIFile {
  return {
    action: 'SET_MAIN_UI_FILE',
    uiFile: uiFile,
  }
}

export function saveDOMReport(
  elementMetadata: ElementInstanceMetadataMap,
  cachedPaths: Array<ElementPath>,
  invalidatedPaths: Array<string>,
): SaveDOMReport {
  return {
    action: 'SAVE_DOM_REPORT',
    elementMetadata: elementMetadata,
    cachedPaths: cachedPaths,
    invalidatedPaths: invalidatedPaths,
  }
}

export function runDOMWalker(): RunDOMWalker {
  return {
    action: 'RUN_DOM_WALKER',
  }
}

/** WARNING: you probably don't want to use setProp, instead you should use a domain-specific action! */
export function setProp_UNSAFE(
  target: ElementPath,
  propertyPath: PropertyPath,
  value: JSExpression,
  importsToAdd: Imports = {},
): SetProp {
  return {
    action: 'SET_PROP',
    target: target,
    propertyPath: propertyPath,
    value: value,
    importsToAdd: importsToAdd,
  }
}

export function setPropTransient(
  target: ElementPath,
  propertyPath: PropertyPath,
  value: JSExpression,
): SetPropTransient {
  return {
    action: 'SET_PROP_TRANSIENT',
    target: target,
    propertyPath: propertyPath,
    value: value,
  }
}

export function clearTransientProps(): ClearTransientProps {
  return {
    action: 'CLEAR_TRANSIENT_PROPS',
  }
}

export function renamePropKey(
  target: ElementPath,
  cssTargetPath: CSSTarget,
  value: Array<string>,
): RenameStyleSelector {
  return {
    action: 'RENAME_PROP_KEY',
    target,
    cssTargetPath,
    value,
  }
}

export function setCodeEditorBuildErrors(buildErrors: ErrorMessages): SetCodeEditorBuildErrors {
  return {
    action: 'SET_CODE_EDITOR_BUILD_ERRORS',
    buildErrors: buildErrors,
  }
}

export function setCodeEditorLintErrors(lintErrors: ErrorMessages): SetCodeEditorLintErrors {
  return {
    action: 'SET_CODE_EDITOR_LINT_ERRORS',
    lintErrors: lintErrors,
  }
}

export function setCodeEditorComponentDescriptorErrors(
  componentDescriptorErrors: ErrorMessages,
): SetCodeEditorComponentDescriptorErrors {
  return {
    action: 'SET_CODE_EDITOR_COMPONENT_DESCRIPTOR_ERRORS',
    componentDescriptorErrors: componentDescriptorErrors,
  }
}

export function setFilebrowserRenamingTarget(
  filename: string | null,
): SetFilebrowserRenamingTarget {
  return {
    action: 'SET_FILEBROWSER_RENAMING_TARGET',
    filename: filename,
  }
}

export function toggleProperty(
  target: ElementPath,
  togglePropValue: (element: JSXElement) => JSXElement,
): ToggleProperty {
  return {
    action: 'TOGGLE_PROPERTY',
    target: target,
    togglePropValue: togglePropValue,
  }
}

export function insertImageIntoUI(imagePath: string): InsertImageIntoUI {
  return {
    action: 'INSERT_IMAGE_INTO_UI',
    imagePath: imagePath,
  }
}

export function updateJSXElementName(
  target: ElementPath,
  elementName: { type: 'JSX_ELEMENT'; name: JSXElementName } | { type: 'JSX_FRAGMENT' },
  importsToAdd: Imports,
): UpdateJSXElementName {
  return {
    action: 'UPDATE_JSX_ELEMENT_NAME',
    target: target,
    elementName: elementName,
    importsToAdd: importsToAdd,
  }
}

export function addImports(importsToAdd: Imports, target: ElementPath): AddImports {
  return {
    action: 'ADD_IMPORTS',
    target: target,
    importsToAdd: importsToAdd,
  }
}

export function setAspectRatioLock(target: ElementPath, locked: boolean): SetAspectRatioLock {
  return {
    action: 'SET_ASPECT_RATIO_LOCK',
    target: target,
    locked: locked,
  }
}

export function toggleCanvasIsLive(): ToggleCanvasIsLive {
  return {
    action: 'TOGGLE_CANVAS_IS_LIVE',
  }
}

export function setSafeMode(value: boolean): SetSafeMode {
  return {
    action: 'SET_SAFE_MODE',
    value: value,
  }
}

export function setSaveError(value: boolean): SetSaveError {
  return {
    action: 'SET_SAVE_ERROR',
    value: value,
  }
}

export function insertDroppedImage(
  image: ImageFile,
  path: string,
  position: CanvasPoint,
): InsertDroppedImage {
  return {
    action: 'INSERT_DROPPED_IMAGE',
    image: image,
    path: path,
    position: position,
  }
}

export function removeFromNodeModulesContents(
  modulesToRemove: Array<string>,
): RemoveFromNodeModulesContents {
  return {
    action: 'REMOVE_FROM_NODE_MODULES_CONTENTS',
    modulesToRemove: modulesToRemove,
  }
}

export function updateNodeModulesContents(contentsToAdd: NodeModules): UpdateNodeModulesContents {
  return {
    action: 'UPDATE_NODE_MODULES_CONTENTS',
    contentsToAdd: contentsToAdd,
  }
}

export function updatePackageJson(dependencies: Array<RequestedNpmDependency>): UpdatePackageJson {
  return {
    action: 'UPDATE_PACKAGE_JSON',
    dependencies: dependencies,
  }
}

export function startCheckpointTimer(): StartCheckpointTimer {
  return {
    action: 'START_CHECKPOINT_TIMER',
  }
}

export function finishCheckpointTimer(): FinishCheckpointTimer {
  return {
    action: 'FINISH_CHECKPOINT_TIMER',
  }
}

export function addMissingDimensions(
  target: ElementPath,
  existingSize: CanvasRectangle,
): AddMissingDimensions {
  return {
    action: 'ADD_MISSING_DIMENSIONS',
    existingSize: existingSize,
    target: target,
  }
}

export function setPackageStatus(packageName: string, status: PackageStatus): SetPackageStatus {
  return {
    action: 'SET_PACKAGE_STATUS',
    packageName: packageName,
    status: status,
  }
}

export function setShortcut(shortcutName: string, newKey: Key): SetShortcut {
  return {
    action: 'SET_SHORTCUT',
    shortcutName: shortcutName,
    newKey: newKey,
  }
}

export function updatePropertyControlsInfo(
  propertyControlsInfo: PropertyControlsInfo,
): UpdatePropertyControlsInfo {
  return {
    action: 'UPDATE_PROPERTY_CONTROLS_INFO',
    propertyControlsInfo: propertyControlsInfo,
  }
}

export function updateText(target: ElementPath, text: string, textProp: TextProp): UpdateText {
  return {
    action: 'UPDATE_TEXT',
    target: target,
    text: text,
    textProp: textProp,
  }
}

export function truncateHistory(): TruncateHistory {
  return {
    action: 'TRUNCATE_HISTORY',
  }
}

export function setFocusedElement(
  focusedElementElementPath: ElementPath | null,
): SetFocusedElement {
  return {
    action: 'SET_FOCUSED_ELEMENT',
    focusedElementPath: focusedElementElementPath,
  }
}

export function scrollToElement(
  focusedElementElementPath: ElementPath,
  behaviour: ScrollToElementBehaviour,
): ScrollToElement {
  return {
    action: 'SCROLL_TO_ELEMENT',
    target: focusedElementElementPath,
    behaviour: behaviour,
  }
}

export function scrollToPosition(
  target: CanvasRectangle,
  behaviour: ScrollToElementBehaviour,
): ScrollToPosition {
  return {
    action: 'SCROLL_TO_POSITION',
    target: target,
    behaviour: behaviour,
  }
}

export function setScrollAnimation(value: boolean): SetScrollAnimation {
  return {
    action: 'SET_SCROLL_ANIMATION',
    value: value,
  }
}

export function setFollowSelectionEnabled(value: boolean): SetFollowSelectionEnabled {
  return {
    action: 'SET_FOLLOW_SELECTION_ENABLED',
    value: value,
  }
}

export function setLoginState(loginState: LoginState): SetLoginState {
  return {
    action: 'SET_LOGIN_STATE',
    loginState: loginState,
  }
}

export function setGithubState(githubState: Partial<GithubState>): SetGithubState {
  return {
    action: 'SET_GITHUB_STATE',
    githubState: githubState,
  }
}

export function setUserConfiguration(userConfiguration: UserConfiguration): SetUserConfiguration {
  return {
    action: 'SET_USER_CONFIGURATION',
    userConfiguration: userConfiguration,
  }
}

export type GithubOperationType = 'add' | 'remove'

export function updateGithubOperations(
  operation: GithubOperation,
  type: GithubOperationType,
): UpdateGithubOperations {
  return {
    action: 'UPDATE_GITHUB_OPERATIONS',
    operation: operation,
    type: type,
  }
}

export function setRefreshingDependencies(value: boolean): SetRefreshingDependencies {
  return {
    action: 'SET_REFRESHING_DEPENDENCIES',
    value: value,
  }
}

export function resetCanvas(): ResetCanvas {
  return {
    action: 'RESET_CANVAS',
  }
}

export function setFilebrowserDropTarget(target: string | null): SetFilebrowserDropTarget {
  return {
    action: 'SET_FILEBROWSER_DROPTARGET',
    target: target,
  }
}

export function setCurrentTheme(theme: ThemeSetting): SetCurrentTheme {
  return {
    action: 'SET_CURRENT_THEME',
    theme: theme,
  }
}

export function focusClassNameInput(): FocusClassNameInput {
  return {
    action: 'FOCUS_CLASS_NAME_INPUT',
  }
}

export function focusFormulaBar(): FocusFormulaBar {
  return {
    action: 'FOCUS_FORMULA_BAR',
  }
}

export function updateFormulaBarMode(value: 'css' | 'content'): UpdateFormulaBarMode {
  return {
    action: 'UPDATE_FORMULA_BAR_MODE',
    value: value,
  }
}

export function insertInsertable(
  insertionPath: InsertionPath | null,
  toInsert: InsertableComponent,
  styleProps: StylePropOption,
  indexPosition: IndexPosition | null,
): InsertInsertable {
  return {
    action: 'INSERT_INSERTABLE',
    insertionPath: insertionPath,
    toInsert: toInsert,
    styleProps: styleProps,
    indexPosition: indexPosition,
  }
}

export function addTailwindConfig(): AddTailwindConfig {
  return {
    action: 'ADD_TAILWIND_CONFIG',
  }
}

export function decrementResizeOptionsSelectedIndex(): DecrementResizeOptionsSelectedIndex {
  return {
    action: 'DECREMENT_RESIZE_OPTIONS_SELECTED_INDEX',
  }
}

export function incrementResizeOptionsSelectedIndex(): IncrementResizeOptionsSelectedIndex {
  return {
    action: 'INCREMENT_RESIZE_OPTIONS_SELECTED_INDEX',
  }
}

export function setResizeOptionsTargetOptions(
  propertyTargetOptions: Array<LayoutTargetableProp>,
  index: number | null,
): SetResizeOptionsTargetOptions {
  return {
    action: 'SET_RESIZE_OPTIONS_TARGET_OPTIONS',
    propertyTargetOptions: propertyTargetOptions,
    index: index,
  }
}

export function forceParseFile(filePath: string): ForceParseFile {
  return {
    action: 'FORCE_PARSE_FILE',
    filePath: filePath,
  }
}

export function runEscapeHatch(
  targets: Array<ElementPath>,
  setHuggingParentToFixed: SetHuggingParentToFixed,
): RunEscapeHatch {
  return {
    action: 'RUN_ESCAPE_HATCH',
    targets: targets,
    setHuggingParentToFixed: setHuggingParentToFixed,
  }
}

export function toggleSelectionLock(
  targets: Array<ElementPath>,
  newValue: SelectionLocked,
): ToggleSelectionLock {
  return {
    action: 'TOGGLE_SELECTION_LOCK',
    targets: targets,
    newValue: newValue,
  }
}

export function updateAgainstGithub(
  branchLatestContent: ProjectContentTreeRoot,
  specificCommitContent: ProjectContentTreeRoot,
  latestCommit: string,
): UpdateAgainstGithub {
  return {
    action: 'UPDATE_AGAINST_GITHUB',
    branchLatestContent: branchLatestContent,
    specificCommitContent: specificCommitContent,
    latestCommit: latestCommit,
  }
}

export function setImageDragSessionState(
  imageDragSessionState: ImageDragSessionState,
): SetDragSessionState {
  return {
    action: 'SET_IMAGE_DRAG_SESSION_STATE',
    imageDragSessionState: imageDragSessionState,
  }
}

export function applyCommandsAction(commands: CanvasCommand[]): ApplyCommandsAction {
  return {
    action: 'APPLY_COMMANDS',
    commands: commands,
  }
}

export function updateColorSwatches(colorSwatches: Array<ColorSwatch>): UpdateColorSwatches {
  return {
    action: 'UPDATE_COLOR_SWATCHES',
    colorSwatches: colorSwatches,
  }
}

export function setConditionalOverriddenCondition(
  target: ElementPath,
  condition: boolean | null,
): SetConditionalOverriddenCondition {
  return {
    action: 'SET_CONDITIONAL_OVERRIDDEN_CONDITION',
    target: target,
    condition: condition,
  }
}

export function setMapCountOverride(
  target: ElementPath,
  value: number | null,
): SetMapCountOverride {
  return {
    action: 'SET_MAP_COUNT_OVERRIDE',
    target: target,
    value: value,
  }
}

export function updateConditionalExpression(
  target: ElementPath,
  expression: string,
): UpdateConditionalExpression {
  return {
    action: 'UPDATE_CONIDTIONAL_EXPRESSION',
    target: target,
    expression: expression,
  }
}

export function switchConditionalBranches(target: ElementPath): SwitchConditionalBranches {
  return {
    action: 'SWITCH_CONDITIONAL_BRANCHES',
    target: target,
  }
}

export function executePostActionMenuChoice(choice: PostActionChoice): ExecutePostActionMenuChoice {
  return {
    action: 'EXECUTE_POST_ACTION_MENU_CHOICE',
    choice: choice,
  }
}

export function startPostActionSession(data: PostActionMenuData): StartPostActionSession {
  return {
    action: 'START_POST_ACTION_SESSION',
    data: data,
  }
}

export function clearPostActionData(): ClearPostActionSession {
  return {
    action: 'CLEAR_POST_ACTION_SESSION',
  }
}

export function updateProjectServerState(
  projectServerState: Partial<ProjectServerState>,
): UpdateProjectServerState {
  return {
    action: 'UPDATE_PROJECT_SERVER_STATE',
    serverState: projectServerState,
  }
}

export function updateTopLevelElementsFromCollaborationUpdate(
  fullPath: string,
  topLevelElements: Array<TopLevelElement>,
): UpdateTopLevelElementsFromCollaborationUpdate {
  return {
    action: 'UPDATE_TOP_LEVEL_ELEMENTS_FROM_COLLABORATION_UPDATE',
    fullPath: fullPath,
    topLevelElements: topLevelElements,
  }
}

export function updateExportsDetailFromCollaborationUpdate(
  fullPath: string,
  exportsDetail: Array<ExportDetail>,
): UpdateExportsDetailFromCollaborationUpdate {
  return {
    action: 'UPDATE_EXPORTS_DETAIL_FROM_COLLABORATION_UPDATE',
    fullPath: fullPath,
    exportsDetail: exportsDetail,
  }
}

export function updateImportsFromCollaborationUpdate(
  fullPath: string,
  imports: Imports,
): UpdateImportsFromCollaborationUpdate {
  return {
    action: 'UPDATE_IMPORTS_FROM_COLLABORATION_UPDATE',
    fullPath: fullPath,
    imports: imports,
  }
}

export function updateCodeFromCollaborationUpdate(
  fullPath: string,
  code: string,
): UpdateCodeFromCollaborationUpdate {
  return {
    action: 'UPDATE_CODE_FROM_COLLABORATION_UPDATE',
    fullPath: fullPath,
    code: code,
  }
}

export function setCommentFilterMode(commentFilterMode: CommentFilterMode): SetCommentFilterMode {
  return {
    action: 'SET_COMMENT_FILTER_MODE',
    commentFilterMode: commentFilterMode,
  }
}

export function setCollaborators(collaborators: Collaborator[]): SetCollaborators {
  return {
    action: 'SET_COLLABORATORS',
    collaborators: collaborators,
  }
}

export function extractPropertyControlsFromDescriptorFiles(
  paths: string[],
): ExtractPropertyControlsFromDescriptorFiles {
  return {
    action: 'EXTRACT_PROPERTY_CONTROLS_FROM_DESCRIPTOR_FILES',
    paths: paths,
  }
}

export function setSharingDialogOpen(open: boolean): SetSharingDialogOpen {
  return {
    action: 'SET_SHARING_DIALOG_OPEN',
    open: open,
  }
}

export function resetOnlineState(): ResetOnlineState {
  return {
    action: 'RESET_ONLINE_STATE',
  }
}

export function increaseOnlineStateFailureCount(): IncreaseOnlineStateFailureCount {
  return {
    action: 'INCREASE_ONLINE_STATE_FAILURE_COUNT',
  }
}
