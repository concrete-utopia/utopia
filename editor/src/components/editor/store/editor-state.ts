import * as json5 from 'json5'
import { findJSXElementAtPath, MetadataUtils } from '../../../core/model/element-metadata-utils'
import {
  ElementInstanceMetadata,
  ElementInstanceMetadataMap,
  getElementsByUIDFromTopLevelElements,
  isUtopiaJSXComponent,
  JSXElement,
  JSXElementChild,
  TopLevelElement,
  UtopiaJSXComponent,
  isJSXElement,
  emptyJsxMetadata,
  JSXAttribute,
  walkElements,
} from '../../../core/shared/element-template'
import {
  insertJSXElementChild,
  removeJSXElementChild,
  transformJSXComponentAtPath,
  getUtopiaID,
  findJSXElementAtStaticPath,
} from '../../../core/model/element-template-utils'
import {
  correctProjectContentsPath,
  getOrDefaultScenes,
  getUtopiaJSXComponentsFromSuccess,
  saveTextFileContents,
  getHighlightBoundsFromParseResult,
  updateFileContents,
  getHighlightBoundsForProject,
  applyUtopiaJSXComponentsChanges,
  applyToAllUIJSFiles,
} from '../../../core/model/project-file-utils'
import type { ErrorMessage, ErrorMessageSeverity } from '../../../core/shared/error-messages'
import type { PackageStatus, PackageStatusMap } from '../../../core/shared/npm-dependency-types'
import {
  Imports,
  ParseSuccess,
  ProjectFile,
  RevisionsState,
  ElementPath,
  TextFile,
  isTextFile,
  StaticElementPath,
  NodeModules,
  foldParsedTextFile,
  textFileContents,
  isParseSuccess,
  codeFile,
  isParseFailure,
  isParsedTextFile,
  HighlightBoundsForUids,
  HighlightBoundsWithFile,
  PropertyPath,
  HighlightBoundsWithFileForUids,
  parseSuccess,
} from '../../../core/shared/project-file-types'
import { diagnosticToErrorMessage } from '../../../core/workers/ts/ts-utils'
import {
  ExportsInfo,
  MultiFileBuildResult,
  UtopiaTsWorkers,
} from '../../../core/workers/common/worker-types'
import {
  bimapEither,
  Either,
  foldEither,
  isLeft,
  isRight,
  left,
  mapEither,
  right,
} from '../../../core/shared/either'
import { KeysPressed } from '../../../utils/keyboard'
import { keepDeepReferenceEqualityIfPossible } from '../../../utils/react-performance'
import Utils, { IndexPosition } from '../../../utils/utils'
import {
  CanvasPoint,
  CanvasRectangle,
  CanvasVector,
  LocalRectangle,
  WindowPoint,
} from '../../../core/shared/math-utils'
import {
  addFileToProjectContents,
  ensureDirectoriesExist,
  getContentsTreeFileFromElements,
  getContentsTreeFileFromString,
  ProjectContentTreeRoot,
  transformContentsTree,
  walkContentsTree,
} from '../../assets'
import {
  CanvasFrameAndTarget,
  CanvasModel,
  CSSCursor,
  DragState,
  FrameAndTarget,
  HigherOrderControl,
} from '../../canvas/canvas-types'
import {
  getParseSuccessOrTransientForFilePath,
  produceCanvasTransientState,
} from '../../canvas/canvas-utils'
import { CursorPosition } from '../../code-editor/code-editor-utils'
import { EditorPanel } from '../../common/actions/index'
import {
  CodeResultCache,
  generateCodeResultCache,
  normalisePathSuccessOrThrowError,
  normalisePathToUnderlyingTarget,
  PropertyControlsInfo,
  ResolveFn,
} from '../../custom-code/code-file'
import { convertModeToSavedMode, EditorModes, Mode, PersistedMode } from '../editor-modes'
import { FontSettings } from '../../inspector/common/css-utils'
import { DebugDispatch, EditorDispatch, LoginState, ProjectListing } from '../action-types'
import { CURRENT_PROJECT_VERSION } from '../actions/migrations/migrations'
import { StateHistory } from '../history'
import {
  BakedInStoryboardVariableName,
  getStoryboardElementPath,
} from '../../../core/model/scene-utils'

import {
  toUid,
  toString,
  dynamicPathToStaticPath,
  staticElementPath,
} from '../../../core/shared/element-path'

import { Notice } from '../../common/notice'
import { emptyComplexMap, ComplexMap, addToComplexMap } from '../../../utils/map'
import * as friendlyWords from 'friendly-words'
import { fastForEach } from '../../../core/shared/utils'
import { ShortcutConfiguration } from '../shortcut-definitions'
import { loginNotYetKnown, notLoggedIn } from '../../../common/user'
import { immediatelyResolvableDependenciesWithEditorRequirements } from '../npm-dependency/npm-dependency'
import {
  DerivedStateKeepDeepEquality,
  ElementInstanceMetadataMapKeepDeepEquality,
} from './store-deep-equality-instances'
import { forceNotNull } from '../../../core/shared/optional-utils'
import * as EP from '../../../core/shared/element-path'
import { defaultConfig, UtopiaVSCodeConfig } from 'utopia-vscode-common'

import * as OPI from 'object-path-immutable'
import { MapLike } from 'typescript'
import { pick } from '../../../core/shared/object-utils'
import { LayoutTargetableProp } from '../../../core/layout/layout-helpers-new'
import { atomWithPubSub } from '../../../core/shared/atom-with-pub-sub'

import { v4 as UUID } from 'uuid'
import { PersistenceMachine } from '../persistence/persistence'
import type { BuiltInDependencies } from '../../../core/es-modules/package-manager/built-in-dependencies-list'
import { memoize } from '../../../core/shared/memoize'
import { emptySet } from '../../../core/shared/set-utils'
import { DefaultThirdPartyControlDefinitions } from '../../../core/third-party/third-party-controls'
import { Spec } from 'immutability-helper'
import { InteractionSession, StrategyState } from '../../canvas/canvas-strategies/interaction-state'
import { GuidelineWithSnappingVectorAndPointsOfRelevance } from '../../canvas/guideline'
import { MouseButtonsPressed } from '../../../utils/mouse'
import { UTOPIA_LABEL_KEY } from '../../../core/model/utopia-constants'
import { FileResult } from '../../../core/shared/file-utils'
import { GithubFileStatus } from '../../../core/shared/github'

const ObjectPathImmutable: any = OPI

export enum LeftMenuTab {
  UIInsert = 'ui-insert',
  Contents = 'contents',
  Settings = 'settings',
  Github = 'github',
}

export const LeftPaneMinimumWidth = 5

export const LeftPaneDefaultWidth = 260

export const MenuBarWidth = 44

const DefaultNavigatorWidth = 280
export const NavigatorWidthAtom = atomWithPubSub({
  key: 'NavigatorWidthAtom',
  defaultValue: DefaultNavigatorWidth,
})

export enum RightMenuTab {
  Insert = 'insert',
  Inspector = 'inspector',
}

// TODO: this should just contain an NpmDependency and a status
export interface DependencyPackageDetails {
  name: string
  version: string | null
  status: PackageStatus
}

export const DefaultPackagesList: Array<DependencyPackageDetails> = [
  {
    name: 'react',
    version: '16.13.1',
    status: 'default-package',
  },
  {
    name: 'react-dom',
    version: '16.13.1',
    status: 'default-package',
  },
  {
    name: 'utopia-api',
    version: '0.4.1',
    status: 'default-package',
  },
]

export const StoryboardFilePath: string = '/utopia/storyboard.js'

export interface OriginalPath {
  originalTP: ElementPath
  currentTP: ElementPath
}

export function originalPath(originalTP: ElementPath, currentTP: ElementPath): OriginalPath {
  return {
    originalTP: originalTP,
    currentTP: currentTP,
  }
}

export interface UserConfiguration {
  shortcutConfig: ShortcutConfiguration | null
}

export function emptyUserConfiguration(): UserConfiguration {
  return {
    shortcutConfig: null,
  }
}

export interface GithubState {
  authenticated: boolean
}

export interface UserState extends UserConfiguration {
  loginState: LoginState
  githubState: GithubState
}

export type GithubOperation =
  | { name: 'commish' }
  | { name: 'listBranches' }
  | { name: 'loadBranch'; branchName: string; githubRepo: GithubRepo }
  | { name: 'loadRepositories' }
  | { name: 'updateAgainstBranch' }

export function githubOperationPrettyName(op: GithubOperation): string {
  switch (op.name) {
    case 'commish':
      return 'Saving'
    case 'listBranches':
      return 'Listing branches'
    case 'loadBranch':
      return 'Loading branch'
    case 'loadRepositories':
      return 'Loading Repositories'
    case 'updateAgainstBranch':
      return 'Updating'
    default:
      const _exhaustiveCheck: never = op
      return 'Unknown operation' // this should never happen
  }
}

export function isGithubLoadingBranch(
  operations: Array<GithubOperation>,
  branchName: string,
  repo: GithubRepo | null,
): boolean {
  return operations.some(
    (o) =>
      o.name === 'loadBranch' &&
      o.branchName === branchName &&
      o.githubRepo.owner === repo?.owner &&
      o.githubRepo.repository === repo?.repository,
  )
}

export function isGithubCommishing(operations: Array<GithubOperation>): boolean {
  return operations.some((o) => o.name === 'commish')
}

export function isGithubLoadingRepositories(operations: Array<GithubOperation>): boolean {
  return operations.some((operation) => operation.name === 'loadRepositories')
}

export function isGithubUpdating(operations: Array<GithubOperation>): boolean {
  return operations.some((operation) => operation.name === 'updateAgainstBranch')
}

export const defaultUserState: UserState = {
  loginState: loginNotYetKnown,
  shortcutConfig: {},
  githubState: {
    authenticated: false,
  },
}

type EditorStoreShared = {
  strategyState: StrategyState
  history: StateHistory
  userState: UserState
  workers: UtopiaTsWorkers
  persistence: PersistenceMachine
  dispatch: EditorDispatch
  builtInDependencies: BuiltInDependencies
  alreadySaved: boolean
}

export type EditorStoreFull = EditorStoreShared & {
  unpatchedEditor: EditorState
  patchedEditor: EditorState
  unpatchedDerived: DerivedState
  patchedDerived: DerivedState
}

export type EditorStorePatched = EditorStoreShared & {
  editor: EditorState
  derived: DerivedState
}

export type EditorStoreUnpatched = Omit<EditorStoreFull, 'patchedEditor' | 'patchedDerived'>

export function patchedStoreFromFullStore(store: EditorStoreFull): EditorStorePatched {
  return {
    ...store,
    editor: store.patchedEditor,
    derived: store.patchedDerived,
  }
}

export interface FileDeleteModal {
  type: 'file-delete'
  filePath: string
}

export function fileDeleteModal(filePath: string): FileDeleteModal {
  return {
    type: 'file-delete',
    filePath: filePath,
  }
}

export interface FileRevertModal {
  type: 'file-revert'
  filePath: string
  status: GithubFileStatus | null
}

export function fileRevertModal(
  filePath: string,
  status: GithubFileStatus | null,
): FileRevertModal {
  return {
    type: 'file-revert',
    filePath: filePath,
    status: status,
  }
}

export interface FileRevertAllModal {
  type: 'file-revert-all'
}

export function fileRevertAllModal(): FileRevertAllModal {
  return {
    type: 'file-revert-all',
  }
}

export interface FileUploadInfo {
  fileResult: FileResult
  targetPath: string
}

export function fileUploadInfo(fileResult: FileResult, targetPath: string): FileUploadInfo {
  return {
    fileResult: fileResult,
    targetPath: targetPath,
  }
}

export interface FileOverwriteModal {
  type: 'file-overwrite'
  files: Array<FileUploadInfo>
}

export function fileOverwriteModal(files: Array<FileUploadInfo>): FileOverwriteModal {
  return {
    type: 'file-overwrite',
    files: files,
  }
}

export type ModalDialog =
  | FileDeleteModal
  | FileOverwriteModal
  | FileRevertModal
  | FileRevertAllModal

export type CursorImportanceLevel = 'fixed' | 'mouseOver' // only one fixed cursor can exist, mouseover is a bit less important
export interface CursorStackItem {
  id: string
  importance: CursorImportanceLevel
  cursor: CSSCursor
}

export function cursorStackItem(
  id: string,
  importance: CursorImportanceLevel,
  cursor: CSSCursor,
): CursorStackItem {
  return {
    id: id,
    importance: importance,
    cursor: cursor,
  }
}

export type CursorStack = Array<CursorStackItem>
export interface CanvasCursor {
  fixed: CursorStackItem | null
  mouseOver: CursorStack
}

export function canvasCursor(fixed: CursorStackItem | null, mouseOver: CursorStack): CanvasCursor {
  return {
    fixed: fixed,
    mouseOver: mouseOver,
  }
}

export interface DuplicationState {
  duplicateRoots: Array<OriginalPath>
}

export function duplicationState(duplicateRoots: Array<OriginalPath>): DuplicationState {
  return {
    duplicateRoots: duplicateRoots,
  }
}

export interface ImageBlob {
  base64: string
}

export function imageBlob(base64: string): ImageBlob {
  return { base64: base64 }
}

export type UIFileBase64Blobs = { [key: string]: ImageBlob }

export type CanvasBase64Blobs = { [key: string]: UIFileBase64Blobs }

export type ErrorMessages = { [filename: string]: Array<ErrorMessage> }

export interface ConsoleLog {
  method: string
  data: Array<any>
}

export interface DesignerFile {
  filename: string
}

export function designerFile(filename: string): DesignerFile {
  return {
    filename: filename,
  }
}

export type Theme = 'light' | 'dark'

export type DropTargetType = 'before' | 'after' | 'reparent' | null

export interface DropTargetHint {
  target: ElementPath | null
  type: DropTargetType
}

export interface NavigatorState {
  minimised: boolean
  dropTargetHint: DropTargetHint
  collapsedViews: ElementPath[]
  renamingTarget: ElementPath | null
  highlightedTargets: Array<ElementPath>
}

export interface FloatingInsertMenuStateClosed {
  insertMenuMode: 'closed'
}

export function floatingInsertMenuStateClosed(): FloatingInsertMenuStateClosed {
  return {
    insertMenuMode: 'closed',
  }
}

export interface FloatingInsertMenuStateInsert {
  insertMenuMode: 'insert'
  parentPath: ElementPath | null
  indexPosition: IndexPosition | null
}

export function floatingInsertMenuStateInsert(
  parentPath: ElementPath | null,
  indexPosition: IndexPosition | null,
): FloatingInsertMenuStateInsert {
  return {
    insertMenuMode: 'insert',
    parentPath: parentPath,
    indexPosition: indexPosition,
  }
}

export interface FloatingInsertMenuStateConvert {
  insertMenuMode: 'convert'
}

export function floatingInsertMenuStateConvert(): FloatingInsertMenuStateConvert {
  return {
    insertMenuMode: 'convert',
  }
}

export interface FloatingInsertMenuStateWrap {
  insertMenuMode: 'wrap'
}

export function floatingInsertMenuStateWrap(): FloatingInsertMenuStateWrap {
  return {
    insertMenuMode: 'wrap',
  }
}

export type FloatingInsertMenuState =
  | FloatingInsertMenuStateClosed
  | FloatingInsertMenuStateInsert
  | FloatingInsertMenuStateConvert
  | FloatingInsertMenuStateWrap

export interface ResizeOptions {
  propertyTargetOptions: Array<LayoutTargetableProp>
  propertyTargetSelectedIndex: number
}

export function resizeOptions(
  propertyTargetOptions: Array<LayoutTargetableProp>,
  propertyTargetSelectedIndex: number,
): ResizeOptions {
  return {
    propertyTargetOptions: propertyTargetOptions,
    propertyTargetSelectedIndex: propertyTargetSelectedIndex,
  }
}

export interface VSCodeBridgeIdDefault {
  type: 'VSCODE_BRIDGE_ID_DEFAULT'
  defaultID: string
}

export function vsCodeBridgeIdDefault(defaultID: string): VSCodeBridgeIdDefault {
  return {
    type: 'VSCODE_BRIDGE_ID_DEFAULT',
    defaultID: defaultID,
  }
}

export interface VSCodeBridgeIdProjectId {
  type: 'VSCODE_BRIDGE_ID_PROJECT_ID'
  projectID: string
}

export function vsCodeBridgeIdProjectId(projectID: string): VSCodeBridgeIdProjectId {
  return {
    type: 'VSCODE_BRIDGE_ID_PROJECT_ID',
    projectID: projectID,
  }
}

export type VSCodeBridgeId = VSCodeBridgeIdDefault | VSCodeBridgeIdProjectId

export function getUnderlyingVSCodeBridgeID(bridgeId: VSCodeBridgeId): string {
  switch (bridgeId.type) {
    case 'VSCODE_BRIDGE_ID_DEFAULT':
      return bridgeId.defaultID
    case 'VSCODE_BRIDGE_ID_PROJECT_ID':
      return bridgeId.projectID
    default:
      const _exhaustiveCheck: never = bridgeId
      throw new Error(`Unhandled type ${JSON.stringify(bridgeId)}`)
  }
}

export interface EditorStateNodeModules {
  skipDeepFreeze: true // when we evaluate the code files we plan to mutate the content with the eval result
  files: NodeModules
  projectFilesBuildResults: MultiFileBuildResult
  packageStatus: PackageStatusMap
}

export function editorStateNodeModules(
  skipDeepFreeze: true,
  files: NodeModules,
  projectFilesBuildResults: MultiFileBuildResult,
  packageStatus: PackageStatusMap,
): EditorStateNodeModules {
  return {
    skipDeepFreeze: skipDeepFreeze,
    files: files,
    projectFilesBuildResults: projectFilesBuildResults,
    packageStatus: packageStatus,
  }
}

export interface EditorStateLeftMenu {
  selectedTab: LeftMenuTab
  expanded: boolean
  paneWidth: number
}

export function editorStateLeftMenu(
  selectedTab: LeftMenuTab,
  expanded: boolean,
  paneWidth: number,
): EditorStateLeftMenu {
  return {
    selectedTab: selectedTab,
    expanded: expanded,
    paneWidth: paneWidth,
  }
}

export interface EditorStateRightMenu {
  selectedTab: RightMenuTab
  expanded: boolean
}

export function editorStateRightMenu(
  selectedTab: RightMenuTab,
  expanded: boolean,
): EditorStateRightMenu {
  return {
    selectedTab: selectedTab,
    expanded: expanded,
  }
}

export interface EditorStateInterfaceDesigner {
  codePaneWidth: number
  codePaneVisible: boolean
  restorableCodePaneWidth: number
  additionalControls: boolean
}

export function editorStateInterfaceDesigner(
  codePaneWidth: number,
  codePaneVisible: boolean,
  restorableCodePaneWidth: number,
  additionalControls: boolean,
): EditorStateInterfaceDesigner {
  return {
    codePaneWidth: codePaneWidth,
    codePaneVisible: codePaneVisible,
    restorableCodePaneWidth: restorableCodePaneWidth,
    additionalControls: additionalControls,
  }
}

export interface EditorStateCanvasTextEditor {
  elementPath: ElementPath
  triggerMousePosition: WindowPoint | null
}

export function editorStateCanvasTextEditor(
  elementPath: ElementPath,
  triggerMousePosition: WindowPoint | null,
): EditorStateCanvasTextEditor {
  return {
    elementPath: elementPath,
    triggerMousePosition: triggerMousePosition,
  }
}

export interface EditorStateCanvasTransientProperty {
  elementPath: ElementPath
  attributesToUpdate: { [key: string]: JSXAttribute }
}

export function editorStateCanvasTransientProperty(
  elementPath: ElementPath,
  attributesToUpdate: { [key: string]: JSXAttribute },
): EditorStateCanvasTransientProperty {
  return {
    elementPath: elementPath,
    attributesToUpdate: attributesToUpdate,
  }
}

export interface EditorStateCanvasControls {
  // this is where we can put props for the strategy controls
  snappingGuidelines: Array<GuidelineWithSnappingVectorAndPointsOfRelevance>
  outlineHighlights: Array<CanvasRectangle>
  strategyIntendedBounds: Array<CanvasFrameAndTarget>
  flexReparentTargetLines: Array<CanvasRectangle>
  parentHighlightPaths: Array<ElementPath> | null
  reparentedToPaths: Array<ElementPath>
}

export function editorStateCanvasControls(
  snappingGuidelines: Array<GuidelineWithSnappingVectorAndPointsOfRelevance>,
  outlineHighlights: Array<CanvasRectangle>,
  strategyIntendedBounds: Array<CanvasFrameAndTarget>,
  flexReparentTargetLines: Array<CanvasRectangle>,
  parentHighlightPaths: Array<ElementPath> | null,
  reparentedToPaths: Array<ElementPath>,
): EditorStateCanvasControls {
  return {
    snappingGuidelines: snappingGuidelines,
    outlineHighlights: outlineHighlights,
    strategyIntendedBounds: strategyIntendedBounds,
    flexReparentTargetLines: flexReparentTargetLines,
    parentHighlightPaths: parentHighlightPaths,
    reparentedToPaths: reparentedToPaths,
  }
}

export type ElementsToRerender = Array<ElementPath> | 'rerender-all-elements'

export interface EditorStateCanvas {
  elementsToRerender: ElementsToRerender
  visible: boolean
  dragState: DragState | null
  interactionSession: InteractionSession | null
  scale: number
  snappingThreshold: number
  realCanvasOffset: CanvasVector
  roundedCanvasOffset: CanvasVector
  textEditor: EditorStateCanvasTextEditor | null
  selectionControlsVisible: boolean
  cursor: CSSCursor | null
  duplicationState: DuplicationState | null
  base64Blobs: CanvasBase64Blobs
  mountCount: number
  canvasContentInvalidateCount: number
  domWalkerInvalidateCount: number
  openFile: DesignerFile | null
  scrollAnimation: boolean
  transientProperties: { [key: string]: EditorStateCanvasTransientProperty } | null
  resizeOptions: ResizeOptions
  domWalkerAdditionalElementsToUpdate: Array<ElementPath>
  controls: EditorStateCanvasControls
}

export function editorStateCanvas(
  elementsToRerender: Array<ElementPath> | 'rerender-all-elements',
  visible: boolean,
  dragState: DragState | null,
  interactionSession: InteractionSession | null,
  scale: number,
  snappingThreshold: number,
  realCanvasOffset: CanvasVector,
  roundedCanvasOffset: CanvasVector,
  textEditor: EditorStateCanvasTextEditor | null,
  selectionControlsVisible: boolean,
  cursor: CSSCursor | null,
  dupeState: DuplicationState | null,
  base64Blobs: CanvasBase64Blobs,
  mountCount: number,
  canvasContentInvalidateCount: number,
  domWalkerInvalidateCount: number,
  openFile: DesignerFile | null,
  scrollAnimation: boolean,
  transientProperties: MapLike<EditorStateCanvasTransientProperty> | null,
  resizeOpts: ResizeOptions,
  domWalkerAdditionalElementsToUpdate: Array<ElementPath>,
  controls: EditorStateCanvasControls,
): EditorStateCanvas {
  return {
    elementsToRerender: elementsToRerender,
    visible: visible,
    dragState: dragState,
    interactionSession: interactionSession,
    scale: scale,
    snappingThreshold: snappingThreshold,
    realCanvasOffset: realCanvasOffset,
    roundedCanvasOffset: roundedCanvasOffset,
    textEditor: textEditor,
    selectionControlsVisible: selectionControlsVisible,
    cursor: cursor,
    duplicationState: dupeState,
    base64Blobs: base64Blobs,
    mountCount: mountCount,
    canvasContentInvalidateCount: canvasContentInvalidateCount,
    domWalkerInvalidateCount: domWalkerInvalidateCount,
    openFile: openFile,
    scrollAnimation: scrollAnimation,
    transientProperties: transientProperties,
    resizeOptions: resizeOpts,
    domWalkerAdditionalElementsToUpdate: domWalkerAdditionalElementsToUpdate,
    controls: controls,
  }
}

export interface EditorStateInspector {
  visible: boolean
  classnameFocusCounter: number
  layoutSectionHovered: boolean
}

export function editorStateInspector(
  visible: boolean,
  classnameFocusCounter: number,
  layoutSectionHovered: boolean,
): EditorStateInspector {
  return {
    visible: visible,
    classnameFocusCounter: classnameFocusCounter,
    layoutSectionHovered: layoutSectionHovered,
  }
}

export interface DraggedImageProperties {
  width: number
  height: number
  src: string
}

interface NotDragging {
  type: 'NOT_DRAGGING'
}

interface DraggingFromFS {
  type: 'DRAGGING_FROM_FS'
}

export interface DraggingFromSidebar {
  type: 'DRAGGING_FROM_SIDEBAR'
  draggedImageProperties: DraggedImageProperties | null
}

export type ImageDragSessionState = NotDragging | DraggingFromFS | DraggingFromSidebar

export function notDragging(): NotDragging {
  return { type: 'NOT_DRAGGING' }
}

export function draggingFromFS(): DraggingFromFS {
  return { type: 'DRAGGING_FROM_FS' }
}

export function draggingFromSidebar(
  draggedImage: DraggedImageProperties | null,
): DraggingFromSidebar {
  return {
    type: 'DRAGGING_FROM_SIDEBAR',
    draggedImageProperties: draggedImage,
  }
}

export function draggedImageProperties(
  width: number,
  height: number,
  src: string,
): DraggedImageProperties {
  return {
    width,
    height,
    src,
  }
}

export interface EditorStateFileBrowser {
  minimised: boolean
  renamingTarget: string | null
  dropTarget: string | null
}

export function editorStateFileBrowser(
  minimised: boolean,
  renamingTarget: string | null,
  dropTarget: string | null,
): EditorStateFileBrowser {
  return {
    minimised: minimised,
    renamingTarget: renamingTarget,
    dropTarget: dropTarget,
  }
}

export interface EditorStateDependencyList {
  minimised: boolean
}

export function editorStateDependencyList(minimised: boolean): EditorStateDependencyList {
  return {
    minimised: minimised,
  }
}

export interface EditorStateGenericExternalResources {
  minimised: boolean
}

export function editorStateGenericExternalResources(
  minimised: boolean,
): EditorStateGenericExternalResources {
  return {
    minimised: minimised,
  }
}

export interface EditorStateGoogleFontsResources {
  minimised: boolean
}

export function editorStateGoogleFontsResources(
  minimised: boolean,
): EditorStateGoogleFontsResources {
  return {
    minimised: minimised,
  }
}

export interface EditorStateProjectSettings {
  minimised: boolean
}

export function editorStateProjectSettings(minimised: boolean): EditorStateProjectSettings {
  return {
    minimised: minimised,
  }
}

export interface EditorStateTopMenu {
  formulaBarMode: 'css' | 'content'
  formulaBarFocusCounter: number
}

export function editorStateTopMenu(
  formulaBarMode: 'css' | 'content',
  formulaBarFocusCounter: number,
): EditorStateTopMenu {
  return {
    formulaBarMode: formulaBarMode,
    formulaBarFocusCounter: formulaBarFocusCounter,
  }
}

export interface EditorStatePreview {
  visible: boolean
  connected: boolean
}

export function editorStatePreview(visible: boolean, connected: boolean): EditorStatePreview {
  return {
    visible: visible,
    connected: connected,
  }
}

export interface EditorStateHome {
  visible: boolean
}

export function editorStateHome(visible: boolean): EditorStateHome {
  return {
    visible: visible,
  }
}

export interface EditorStateCodeEditorErrors {
  buildErrors: ErrorMessages
  lintErrors: ErrorMessages
}

export function editorStateCodeEditorErrors(
  buildErrors: ErrorMessages,
  lintErrors: ErrorMessages,
): EditorStateCodeEditorErrors {
  return {
    buildErrors: buildErrors,
    lintErrors: lintErrors,
  }
}

export type ElementProps = { [key: string]: any }

export type AllElementProps = { [path: string]: ElementProps }

export type LockedElements = {
  simpleLock: Array<ElementPath>
  hierarchyLock: Array<ElementPath>
}

export interface GithubRepo {
  owner: string
  repository: string
}

export function githubRepo(owner: string, repository: string): GithubRepo {
  return {
    owner: owner,
    repository: repository,
  }
}

export interface ProjectGithubSettings {
  targetRepository: GithubRepo | null
  originCommit: string | null
  branchName: string | null
}

export function projectGithubSettings(
  targetRepository: GithubRepo | null,
  originCommit: string | null,
  branchName: string | null,
): ProjectGithubSettings {
  return {
    targetRepository: targetRepository,
    originCommit: originCommit,
    branchName: branchName,
  }
}

export type GithubChecksums = { [filename: string]: string } // key = filename, value = sha1 hash of the file

// FIXME We need to pull out ProjectState from here
export interface EditorState {
  id: string | null
  vscodeBridgeId: VSCodeBridgeId
  forkedFromProjectId: string | null
  appID: string | null
  projectName: string
  projectDescription: string
  projectVersion: number
  isLoaded: boolean
  spyMetadata: ElementInstanceMetadataMap // this is coming from the canvas spy report.
  domMetadata: ElementInstanceMetadataMap // this is coming from the dom walking report.
  jsxMetadata: ElementInstanceMetadataMap // this is a merged result of the two above.
  projectContents: ProjectContentTreeRoot
  branchContents: ProjectContentTreeRoot | null
  codeResultCache: CodeResultCache
  propertyControlsInfo: PropertyControlsInfo
  nodeModules: EditorStateNodeModules
  selectedViews: Array<ElementPath>
  highlightedViews: Array<ElementPath>
  hiddenInstances: Array<ElementPath>
  displayNoneInstances: Array<ElementPath>
  warnedInstances: Array<ElementPath>
  lockedElements: LockedElements
  mode: Mode
  focusedPanel: EditorPanel | null
  keysPressed: KeysPressed
  mouseButtonsPressed: MouseButtonsPressed
  openPopupId: string | null
  toasts: ReadonlyArray<Notice>
  cursorStack: CanvasCursor
  leftMenu: EditorStateLeftMenu
  rightMenu: EditorStateRightMenu
  interfaceDesigner: EditorStateInterfaceDesigner
  canvas: EditorStateCanvas
  floatingInsertMenu: FloatingInsertMenuState
  inspector: EditorStateInspector
  fileBrowser: EditorStateFileBrowser
  dependencyList: EditorStateDependencyList
  genericExternalResources: EditorStateGenericExternalResources
  googleFontsResources: EditorStateGoogleFontsResources
  projectSettings: EditorStateProjectSettings
  navigator: NavigatorState
  topmenu: EditorStateTopMenu
  preview: EditorStatePreview
  home: EditorStateHome
  lastUsedFont: FontSettings | null
  modal: ModalDialog | null
  localProjectList: Array<ProjectListing>
  projectList: Array<ProjectListing>
  showcaseProjects: Array<ProjectListing>
  codeEditingEnabled: boolean
  codeEditorErrors: EditorStateCodeEditorErrors
  thumbnailLastGenerated: number
  pasteTargetsToIgnore: ElementPath[]
  parseOrPrintInFlight: boolean
  safeMode: boolean
  saveError: boolean
  vscodeBridgeReady: boolean
  vscodeReady: boolean
  focusedElementPath: ElementPath | null
  config: UtopiaVSCodeConfig
  theme: Theme
  vscodeLoadingScreenVisible: boolean
  indexedDBFailed: boolean
  forceParseFiles: Array<string>
  allElementProps: AllElementProps // the final, resolved, static props value for each element. // This is the counterpart of jsxMetadata. we only update allElementProps when we update jsxMetadata
  _currentAllElementProps_KILLME: AllElementProps // This is the counterpart of domMetadata and spyMetadata. we update _currentAllElementProps_KILLME every time we update domMetadata/spyMetadata
  githubSettings: ProjectGithubSettings
  imageDragSessionState: ImageDragSessionState
  githubOperations: Array<GithubOperation>
  githubChecksums: GithubChecksums | null
}

export function editorState(
  id: string | null,
  vscodeBridgeId: VSCodeBridgeId,
  forkedFromProjectId: string | null,
  appID: string | null,
  projectName: string,
  projectDescription: string,
  projectVersion: number,
  isLoaded: boolean,
  spyMetadata: ElementInstanceMetadataMap,
  domMetadata: ElementInstanceMetadataMap,
  jsxMetadata: ElementInstanceMetadataMap,
  projectContents: ProjectContentTreeRoot,
  codeResultCache: CodeResultCache,
  propertyControlsInfo: PropertyControlsInfo,
  nodeModules: EditorStateNodeModules,
  selectedViews: Array<ElementPath>,
  highlightedViews: Array<ElementPath>,
  hiddenInstances: Array<ElementPath>,
  displayNoneInstances: Array<ElementPath>,
  warnedInstances: Array<ElementPath>,
  lockedElements: LockedElements,
  mode: Mode,
  focusedPanel: EditorPanel | null,
  keysPressed: KeysPressed,
  mouseButtonsPressed: MouseButtonsPressed,
  openPopupId: string | null,
  toasts: ReadonlyArray<Notice>,
  cursorStack: CanvasCursor,
  leftMenu: EditorStateLeftMenu,
  rightMenu: EditorStateRightMenu,
  interfaceDesigner: EditorStateInterfaceDesigner,
  canvas: EditorStateCanvas,
  floatingInsertMenu: FloatingInsertMenuState,
  inspector: EditorStateInspector,
  fileBrowser: EditorStateFileBrowser,
  dependencyList: EditorStateDependencyList,
  genericExternalResources: EditorStateGenericExternalResources,
  googleFontsResources: EditorStateGoogleFontsResources,
  projectSettings: EditorStateProjectSettings,
  editorStateNavigator: NavigatorState,
  topmenu: EditorStateTopMenu,
  preview: EditorStatePreview,
  home: EditorStateHome,
  lastUsedFont: FontSettings | null,
  modal: ModalDialog | null,
  localProjectList: Array<ProjectListing>,
  projectList: Array<ProjectListing>,
  showcaseProjects: Array<ProjectListing>,
  codeEditingEnabled: boolean,
  codeEditorErrors: EditorStateCodeEditorErrors,
  thumbnailLastGenerated: number,
  pasteTargetsToIgnore: ElementPath[],
  parseOrPrintInFlight: boolean,
  safeMode: boolean,
  saveError: boolean,
  vscodeBridgeReady: boolean,
  vscodeReady: boolean,
  focusedElementPath: ElementPath | null,
  config: UtopiaVSCodeConfig,
  theme: Theme,
  vscodeLoadingScreenVisible: boolean,
  indexedDBFailed: boolean,
  forceParseFiles: Array<string>,
  allElementProps: AllElementProps,
  _currentAllElementProps_KILLME: AllElementProps,
  githubSettings: ProjectGithubSettings,
  imageDragSessionState: ImageDragSessionState,
  githubOperations: Array<GithubOperation>,
  githubChecksums: GithubChecksums | null,
  branchContents: ProjectContentTreeRoot | null,
): EditorState {
  return {
    id: id,
    vscodeBridgeId: vscodeBridgeId,
    forkedFromProjectId: forkedFromProjectId,
    appID: appID,
    projectName: projectName,
    projectDescription: projectDescription,
    projectVersion: projectVersion,
    isLoaded: isLoaded,
    spyMetadata: spyMetadata,
    domMetadata: domMetadata,
    jsxMetadata: jsxMetadata,
    projectContents: projectContents,
    branchContents: branchContents,
    codeResultCache: codeResultCache,
    propertyControlsInfo: propertyControlsInfo,
    nodeModules: nodeModules,
    selectedViews: selectedViews,
    highlightedViews: highlightedViews,
    hiddenInstances: hiddenInstances,
    displayNoneInstances: displayNoneInstances,
    warnedInstances: warnedInstances,
    lockedElements: lockedElements,
    mode: mode,
    focusedPanel: focusedPanel,
    keysPressed: keysPressed,
    mouseButtonsPressed: mouseButtonsPressed,
    openPopupId: openPopupId,
    toasts: toasts,
    cursorStack: cursorStack,
    leftMenu: leftMenu,
    rightMenu: rightMenu,
    interfaceDesigner: interfaceDesigner,
    canvas: canvas,
    floatingInsertMenu: floatingInsertMenu,
    inspector: inspector,
    fileBrowser: fileBrowser,
    dependencyList: dependencyList,
    genericExternalResources: genericExternalResources,
    googleFontsResources: googleFontsResources,
    projectSettings: projectSettings,
    navigator: editorStateNavigator,
    topmenu: topmenu,
    preview: preview,
    home: home,
    lastUsedFont: lastUsedFont,
    modal: modal,
    localProjectList: localProjectList,
    projectList: projectList,
    showcaseProjects: showcaseProjects,
    codeEditingEnabled: codeEditingEnabled,
    codeEditorErrors: codeEditorErrors,
    thumbnailLastGenerated: thumbnailLastGenerated,
    pasteTargetsToIgnore: pasteTargetsToIgnore,
    parseOrPrintInFlight: parseOrPrintInFlight,
    safeMode: safeMode,
    saveError: saveError,
    vscodeBridgeReady: vscodeBridgeReady,
    vscodeReady: vscodeReady,
    focusedElementPath: focusedElementPath,
    config: config,
    theme: theme,
    vscodeLoadingScreenVisible: vscodeLoadingScreenVisible,
    indexedDBFailed: indexedDBFailed,
    forceParseFiles: forceParseFiles,
    allElementProps: allElementProps,
    _currentAllElementProps_KILLME: _currentAllElementProps_KILLME,
    githubSettings: githubSettings,
    imageDragSessionState: imageDragSessionState,
    githubOperations: githubOperations,
    githubChecksums: githubChecksums,
  }
}

export const StoredStateVersion = 1

export interface StoredEditorState {
  version: number
  selectedViews: Array<ElementPath>
  mode: PersistedMode | null
}

export function storedEditorStateFromEditorState(editor: EditorState): StoredEditorState {
  return {
    version: StoredStateVersion,
    selectedViews: editor.selectedViews,
    mode: convertModeToSavedMode(editor.mode),
  }
}

export function mergeStoredEditorStateIntoEditorState(
  storedEditorState: StoredEditorState | null,
  editor: EditorState,
): EditorState {
  if (storedEditorState == null) {
    return editor
  } else {
    return {
      ...editor,
      selectedViews: storedEditorState.selectedViews,
      mode: storedEditorState.mode ?? EditorModes.selectMode(),
    }
  }
}

export function getOpenFilename(model: EditorState): string | null {
  return model.canvas.openFile?.filename ?? null
}

export function getOpenFile(model: EditorState): ProjectFile | null {
  const openFile = getOpenFilename(model)
  if (openFile == null) {
    return null
  } else {
    return getContentsTreeFileFromString(model.projectContents, openFile)
  }
}

export function getFileForName(filePath: string, model: EditorState): ProjectFile | null {
  return getContentsTreeFileFromString(model.projectContents, filePath)
}

export function getOpenTextFileKey(model: EditorState): string | null {
  const openFilename = getOpenFilename(model)
  if (openFilename == null) {
    return null
  } else {
    const projectFile = getContentsTreeFileFromString(model.projectContents, openFilename)
    if (isTextFile(projectFile)) {
      return openFilename
    } else {
      return null
    }
  }
}

export function getOpenTextFile(model: EditorState): TextFile | null {
  const openFile = getOpenFile(model)
  if (openFile == null) {
    return null
  } else {
    if (isTextFile(openFile)) {
      return openFile
    } else {
      return null
    }
  }
}

export function getOpenUIJSFileKey(model: EditorState): string | null {
  const openFilename = getOpenFilename(model)
  if (openFilename == null) {
    return null
  } else {
    const projectFile = getContentsTreeFileFromString(model.projectContents, openFilename)
    if (isParsedTextFile(projectFile)) {
      return openFilename
    } else {
      return null
    }
  }
}

export function isOpenFileUiJs(model: EditorState): boolean {
  const openFile = getOpenFile(model)
  return openFile != null && isParsedTextFile(openFile)
}

export function getOpenUIJSFile(model: EditorState): TextFile | null {
  const openFilename = getOpenFilename(model)
  if (openFilename == null) {
    return null
  } else {
    const projectFile = getContentsTreeFileFromString(model.projectContents, openFilename)
    if (isParsedTextFile(projectFile)) {
      return projectFile
    } else {
      return null
    }
  }
}

export interface SimpleParseSuccess {
  imports: Imports
  utopiaComponents: Array<UtopiaJSXComponent>
}

export function simpleParseSuccess(
  imports: Imports,
  utopiaComponents: Array<UtopiaJSXComponent>,
): SimpleParseSuccess {
  return {
    imports: imports,
    utopiaComponents: utopiaComponents,
  }
}

export function modifyParseSuccessWithSimple(
  transform: (s: SimpleParseSuccess) => SimpleParseSuccess,
  success: ParseSuccess,
): ParseSuccess {
  const oldSimpleParseSuccess: SimpleParseSuccess = {
    imports: success.imports,
    utopiaComponents: getUtopiaJSXComponentsFromSuccess(success),
  }
  const newSimpleParseSuccess: SimpleParseSuccess = transform(oldSimpleParseSuccess)
  const newTopLevelElements = applyUtopiaJSXComponentsChanges(
    success.topLevelElements,
    newSimpleParseSuccess.utopiaComponents,
  )
  return parseSuccess(
    newSimpleParseSuccess.imports,
    newTopLevelElements,
    {},
    success.jsxFactoryFunction,
    success.combinedTopLevelArbitraryBlock,
    success.exportsDetail,
  )
}

export interface ParseSuccessAndEditorChanges<T> {
  parseSuccessTransform: (success: ParseSuccess) => ParseSuccess
  editorStateTransform: (editor: EditorState) => EditorState
  additionalData: T
}

export function modifyOpenParseSuccess(
  transform: (
    parseSuccess: ParseSuccess,
    underlying: StaticElementPath | null,
    underlyingFilePath: string,
  ) => ParseSuccess,
  model: EditorState,
): EditorState {
  return modifyUnderlyingTarget(
    null,
    forceNotNull('No open designer file.', model.canvas.openFile?.filename),
    model,
    (elem) => elem,
    transform,
  )
}

export function modifyOpenScenesAndJSXElements(
  transform: (utopiaComponents: Array<UtopiaJSXComponent>) => Array<UtopiaJSXComponent>,
  model: EditorState,
): EditorState {
  const successTransform = (success: ParseSuccess) => {
    const oldUtopiaJSXComponents = getUtopiaJSXComponentsFromSuccess(success)
    // Apply the transformation.
    const updatedResult = transform(oldUtopiaJSXComponents)

    const newTopLevelElements = applyUtopiaJSXComponentsChanges(
      success.topLevelElements,
      updatedResult,
    )

    return {
      ...success,
      topLevelElements: newTopLevelElements,
    }
  }
  return modifyOpenParseSuccess(successTransform, model)
}

export function modifyOpenJSXElements(
  transform: (utopiaComponents: Array<UtopiaJSXComponent>) => Array<UtopiaJSXComponent>,
  model: EditorState,
): EditorState {
  const successTransform = (success: ParseSuccess) => {
    const oldUtopiaJSXComponents = getUtopiaJSXComponentsFromSuccess(success)
    // Apply the transformation.
    const updatedUtopiaJSXComponents = transform(oldUtopiaJSXComponents)

    const newTopLevelElements = applyUtopiaJSXComponentsChanges(
      success.topLevelElements,
      updatedUtopiaJSXComponents,
    )

    return {
      ...success,
      topLevelElements: newTopLevelElements,
    }
  }
  return modifyOpenParseSuccess(successTransform, model)
}

export function modifyOpenJSXElementsAndMetadata(
  transform: (
    utopiaComponents: Array<UtopiaJSXComponent>,
    componentMetadata: ElementInstanceMetadataMap,
  ) => { components: Array<UtopiaJSXComponent>; componentMetadata: ElementInstanceMetadataMap },
  target: ElementPath,
  model: EditorState,
): EditorState {
  let workingMetadata: ElementInstanceMetadataMap = model.jsxMetadata
  const successTransform = (success: ParseSuccess) => {
    const oldUtopiaJSXComponents = getUtopiaJSXComponentsFromSuccess(success)
    // Apply the transformation.
    const transformResult = transform(oldUtopiaJSXComponents, model.jsxMetadata)
    workingMetadata = transformResult.componentMetadata

    const newTopLevelElements = applyUtopiaJSXComponentsChanges(
      success.topLevelElements,
      transformResult.components,
    )

    return {
      ...success,
      topLevelElements: newTopLevelElements,
    }
  }
  const beforeUpdatingMetadata = modifyUnderlyingForOpenFile(
    target,
    model,
    (elem) => elem,
    successTransform,
  )
  return {
    ...beforeUpdatingMetadata,
    jsxMetadata: workingMetadata,
  }
}

export function modifyOpenJsxElementAtPath(
  path: ElementPath,
  transform: (element: JSXElement) => JSXElement,
  model: EditorState,
): EditorState {
  return modifyUnderlyingTarget(
    path,
    forceNotNull('No open designer file.', model.canvas.openFile?.filename),
    model,
    transform,
  )
}

export function modifyOpenJsxElementAtStaticPath(
  path: StaticElementPath,
  transform: (element: JSXElement) => JSXElement,
  model: EditorState,
): EditorState {
  return modifyUnderlyingTarget(
    path,
    forceNotNull('No open designer file.', model.canvas.openFile?.filename),
    model,
    transform,
  )
}

function getImportedUtopiaJSXComponents(
  filePath: string,
  projectContents: ProjectContentTreeRoot,
  resolve: ResolveFn,
  pathsToFilter: string[],
): Array<UtopiaJSXComponent> {
  const file = getContentsTreeFileFromString(projectContents, filePath)
  if (isTextFile(file) && isParseSuccess(file.fileContents.parsed)) {
    const resolvedFilePaths = Object.keys(file.fileContents.parsed.imports)
      .map((toImport) => resolve(filePath, toImport))
      .filter(isRight)
      .map((r) => r.value)
      .filter((v) => !pathsToFilter.includes(v))

    return [
      ...getUtopiaJSXComponentsFromSuccess(file.fileContents.parsed),
      ...resolvedFilePaths.flatMap((path) =>
        getImportedUtopiaJSXComponents(path, projectContents, resolve, [
          ...pathsToFilter,
          ...resolvedFilePaths,
        ]),
      ),
    ]
  } else {
    return []
  }
}

export function getOpenUtopiaJSXComponentsFromStateMultifile(
  projectContents: ProjectContentTreeRoot,
  resolve: ResolveFn,
  openFilePath: string | null,
): Array<UtopiaJSXComponent> {
  if (openFilePath == null) {
    return []
  } else {
    return getImportedUtopiaJSXComponents(openFilePath, projectContents, resolve, [])
  }
}

export function getJSXComponentsAndImportsForPathFromState(
  path: ElementPath,
  model: EditorState,
  derived: DerivedState,
): {
  components: UtopiaJSXComponent[]
  imports: Imports
} {
  const storyboardFilePath = getOpenUIJSFileKey(model)
  if (storyboardFilePath == null) {
    return {
      components: [],
      imports: {},
    }
  }
  return getJSXComponentsAndImportsForPath(
    path,
    storyboardFilePath,
    model.projectContents,
    model.nodeModules.files,
    derived.transientState.filesState,
  )
}

export function getJSXComponentsAndImportsForPath(
  path: ElementPath,
  currentFilePath: string,
  projectContents: ProjectContentTreeRoot,
  nodeModules: NodeModules,
  transientFilesState: TransientFilesState | null,
): {
  underlyingFilePath: string
  components: UtopiaJSXComponent[]
  imports: Imports
} {
  const underlying = normalisePathToUnderlyingTarget(
    projectContents,
    nodeModules,
    currentFilePath,
    path,
  )
  const elementFilePath =
    underlying.type === 'NORMALISE_PATH_SUCCESS' ? underlying.filePath : currentFilePath
  const result = getParseSuccessOrTransientForFilePath(
    elementFilePath,
    projectContents,
    transientFilesState,
  )
  return {
    underlyingFilePath: elementFilePath,
    components: result.topLevelElements.filter(isUtopiaJSXComponent),
    imports: result.imports,
  }
}

function modifyOpenScenes_INTERNAL(
  transform: (topLevelElementsIncludingScene: UtopiaJSXComponent[]) => UtopiaJSXComponent[],
  model: EditorState,
): EditorState {
  return modifyOpenScenesAndJSXElements((componentsIncludingScenes) => {
    return transform(componentsIncludingScenes)
  }, model)
}

export function addNewScene(model: EditorState, newSceneElement: JSXElement): EditorState {
  return modifyOpenScenes_INTERNAL(
    (components) =>
      addSceneToJSXComponents(
        model.projectContents,
        model.canvas.openFile?.filename ?? null,
        components,
        newSceneElement,
      ),
    model,
  )
}

export function addSceneToJSXComponents(
  projectContents: ProjectContentTreeRoot,
  openFile: string | null,
  components: UtopiaJSXComponent[],
  newSceneElement: JSXElement,
): UtopiaJSXComponent[] {
  const storyoardComponentRootElement = components.find(
    (c) => c.name === BakedInStoryboardVariableName,
  )?.rootElement
  const storyboardComponentUID =
    storyoardComponentRootElement != null ? getUtopiaID(storyoardComponentRootElement) : null
  if (storyboardComponentUID != null) {
    const storyboardComponentElementPath = EP.elementPath([
      staticElementPath([storyboardComponentUID]),
    ])
    return insertJSXElementChild(
      projectContents,
      openFile,
      storyboardComponentElementPath,
      newSceneElement,
      components,
      null,
    )
  } else {
    return components
  }
}

const emptyImports: Imports = {}

export function removeElementAtPath(
  target: ElementPath,
  components: Array<UtopiaJSXComponent>,
): Array<UtopiaJSXComponent> {
  const staticTarget = EP.dynamicPathToStaticPath(target)
  if (staticTarget == null) {
    return components
  } else {
    return removeJSXElementChild(staticTarget, components)
  }
}

export function insertElementAtPath(
  projectContents: ProjectContentTreeRoot,
  openFile: string | null,
  targetParent: ElementPath | null,
  elementToInsert: JSXElementChild,
  components: Array<UtopiaJSXComponent>,
  indexPosition: IndexPosition | null,
): Array<UtopiaJSXComponent> {
  const staticTarget = targetParent == null ? null : EP.dynamicPathToStaticPath(targetParent)
  return insertJSXElementChild(
    projectContents,
    openFile,
    staticTarget,
    elementToInsert,
    components,
    indexPosition,
  )
}

export function transformElementAtPath(
  components: Array<UtopiaJSXComponent>,
  target: ElementPath,
  transform: (elem: JSXElement) => JSXElement,
): Array<UtopiaJSXComponent> {
  const staticTarget = EP.dynamicPathToStaticPath(target)
  if (staticTarget == null) {
    return components
  } else {
    return transformJSXComponentAtPath(components, staticTarget, transform)
  }
}

export interface TransientFileState {
  topLevelElementsIncludingScenes: Array<TopLevelElement>
  imports: Imports
}

export function transientFileState(
  topLevelElementsIncludingScenes: Array<TopLevelElement>,
  imports: Imports,
): TransientFileState {
  return {
    topLevelElementsIncludingScenes: topLevelElementsIncludingScenes,
    imports: imports,
  }
}

export type TransientFilesState = { [filepath: string]: TransientFileState }
export type EditorStatePatch = Spec<EditorState>

export interface TransientCanvasState {
  selectedViews: Array<ElementPath>
  highlightedViews: Array<ElementPath>
  filesState: TransientFilesState | null
  toastsToApply: ReadonlyArray<Notice>
}

export function transientCanvasState(
  selectedViews: Array<ElementPath>,
  highlightedViews: Array<ElementPath>,
  fileState: TransientFilesState | null,
  toastsToApply: ReadonlyArray<Notice>,
): TransientCanvasState {
  return {
    selectedViews: selectedViews,
    highlightedViews: highlightedViews,
    filesState: fileState,
    toastsToApply: toastsToApply,
  }
}

export function getMetadata(editor: EditorState): ElementInstanceMetadataMap {
  if (editor.canvas.dragState == null) {
    return editor.jsxMetadata
  } else {
    return editor.canvas.dragState.metadata
  }
}

export interface ElementWarnings {
  widthOrHeightZero: boolean
  absoluteWithUnpositionedParent: boolean
  dynamicSceneChildWidthHeightPercentage: boolean
}

export function elementWarnings(
  widthOrHeightZero: boolean,
  absoluteWithUnpositionedParent: boolean,
  dynamicSceneChildWidthHeightPercentage: boolean,
): ElementWarnings {
  return {
    widthOrHeightZero: widthOrHeightZero,
    absoluteWithUnpositionedParent: absoluteWithUnpositionedParent,
    dynamicSceneChildWidthHeightPercentage: dynamicSceneChildWidthHeightPercentage,
  }
}

export const defaultElementWarnings: ElementWarnings = {
  widthOrHeightZero: false,
  absoluteWithUnpositionedParent: false,
  dynamicSceneChildWidthHeightPercentage: false,
}

export interface DerivedState {
  navigatorTargets: Array<ElementPath>
  visibleNavigatorTargets: Array<ElementPath>
  controls: Array<HigherOrderControl>
  transientState: TransientCanvasState
  elementWarnings: ComplexMap<ElementPath, ElementWarnings>
}

function emptyDerivedState(editor: EditorState): DerivedState {
  return {
    navigatorTargets: [],
    visibleNavigatorTargets: [],
    controls: [],
    transientState: produceCanvasTransientState(editor.selectedViews, editor, false),
    elementWarnings: emptyComplexMap(),
  }
}

export interface PersistentModel {
  appID: string | null
  forkedFromProjectId: string | null
  projectVersion: number
  projectDescription: string
  projectContents: ProjectContentTreeRoot
  exportsInfo: Array<ExportsInfo>
  lastUsedFont: FontSettings | null
  hiddenInstances: Array<ElementPath>
  codeEditorErrors: {
    buildErrors: ErrorMessages
    lintErrors: ErrorMessages
  }
  fileBrowser: {
    minimised: boolean
  }
  dependencyList: {
    minimised: boolean
  }
  projectSettings: {
    minimised: boolean
  }
  navigator: {
    minimised: boolean
  }
  githubSettings: ProjectGithubSettings
  githubChecksums: GithubChecksums | null
  branchContents: ProjectContentTreeRoot | null
}

export function isPersistentModel(data: any): data is PersistentModel {
  return (
    data != null &&
    (data.projectContents as PersistentModel) != null &&
    (data.hiddenInstances as PersistentModel) != null
  )
}

export function mergePersistentModel(
  first: PersistentModel,
  second: PersistentModel,
): PersistentModel {
  return {
    appID: second.appID,
    forkedFromProjectId: second.forkedFromProjectId,
    projectVersion: second.projectVersion,
    projectDescription: second.projectDescription,
    projectContents: {
      ...first.projectContents,
      ...second.projectContents,
    },
    exportsInfo: second.exportsInfo,
    lastUsedFont: second.lastUsedFont,
    hiddenInstances: [...first.hiddenInstances, ...second.hiddenInstances],
    codeEditorErrors: second.codeEditorErrors,
    fileBrowser: {
      minimised: second.fileBrowser.minimised,
    },
    dependencyList: {
      minimised: second.dependencyList.minimised,
    },
    projectSettings: {
      minimised: second.projectSettings.minimised,
    },
    navigator: {
      minimised: second.navigator.minimised,
    },
    githubSettings: second.githubSettings,
    githubChecksums: second.githubChecksums,
    branchContents: second.branchContents,
  }
}

export function createNewProjectName(): string {
  const friendlyWordsPredicate =
    friendlyWords.predicates[Math.floor(Math.random() * friendlyWords.predicates.length)]
  const friendlyWordsObject =
    friendlyWords.objects[Math.floor(Math.random() * friendlyWords.objects.length)]
  return `${friendlyWordsPredicate}-${friendlyWordsObject}`
}

export const BaseSnappingThreshold = 5
export const BaseCanvasOffset = { x: 100, y: 60 } as CanvasPoint
export const BaseCanvasOffsetLeftPane = {
  x: BaseCanvasOffset.x + DefaultNavigatorWidth,
  y: BaseCanvasOffset.y,
} as CanvasPoint

export function createEditorState(dispatch: EditorDispatch): EditorState {
  return {
    id: null,
    vscodeBridgeId: vsCodeBridgeIdDefault(UUID()),
    forkedFromProjectId: null,
    appID: null,
    projectName: createNewProjectName(),
    projectDescription: 'Made with Utopia',
    projectVersion: CURRENT_PROJECT_VERSION,
    isLoaded: false,
    spyMetadata: emptyJsxMetadata,
    domMetadata: emptyJsxMetadata,
    jsxMetadata: emptyJsxMetadata,
    projectContents: {},
    codeResultCache: generateCodeResultCache({}, {}, [], {}, dispatch, {}, []),
    propertyControlsInfo: { ...DefaultThirdPartyControlDefinitions },
    nodeModules: {
      skipDeepFreeze: true,
      files: {},
      projectFilesBuildResults: {},
      packageStatus: {},
    },
    selectedViews: [],
    highlightedViews: [],
    hiddenInstances: [],
    displayNoneInstances: [],
    warnedInstances: [],
    lockedElements: {
      simpleLock: [],
      hierarchyLock: [],
    },
    mode: EditorModes.selectMode(),
    focusedPanel: 'canvas',
    keysPressed: {},
    mouseButtonsPressed: emptySet(),
    openPopupId: null,
    toasts: [],
    cursorStack: {
      fixed: null,
      mouseOver: [],
    },
    leftMenu: {
      selectedTab: LeftMenuTab.Contents,
      expanded: false,
      paneWidth: LeftPaneDefaultWidth,
    },
    rightMenu: {
      selectedTab: RightMenuTab.Inspector,
      expanded: true,
    },
    interfaceDesigner: {
      codePaneWidth: 500,
      codePaneVisible: true,
      restorableCodePaneWidth: 500,
      additionalControls: true,
    },
    canvas: {
      elementsToRerender: 'rerender-all-elements',
      dragState: null, // TODO change dragState if editorMode changes
      interactionSession: null,
      visible: true,
      scale: 1,
      snappingThreshold: BaseSnappingThreshold,
      realCanvasOffset: BaseCanvasOffsetLeftPane,
      roundedCanvasOffset: BaseCanvasOffsetLeftPane,
      textEditor: null,
      selectionControlsVisible: true,
      cursor: null,
      duplicationState: null,
      base64Blobs: {},
      mountCount: 0,
      canvasContentInvalidateCount: 0,
      domWalkerInvalidateCount: 0,
      openFile: {
        filename: StoryboardFilePath,
      },
      scrollAnimation: false,
      transientProperties: null,
      resizeOptions: {
        propertyTargetOptions: ['width', 'height'],
        propertyTargetSelectedIndex: 0,
      },
      domWalkerAdditionalElementsToUpdate: [],
      controls: {
        snappingGuidelines: [],
        outlineHighlights: [],
        strategyIntendedBounds: [],
        flexReparentTargetLines: [],
        parentHighlightPaths: null,
        reparentedToPaths: [],
      },
    },
    floatingInsertMenu: {
      insertMenuMode: 'closed',
    },
    inspector: {
      visible: true,
      classnameFocusCounter: 0,
      layoutSectionHovered: false,
    },
    dependencyList: {
      minimised: false,
    },
    genericExternalResources: {
      minimised: true,
    },
    googleFontsResources: {
      minimised: true,
    },
    projectSettings: {
      minimised: false,
    },
    fileBrowser: {
      minimised: false,
      dropTarget: null,
      renamingTarget: null,
    },
    navigator: {
      minimised: false,
      dropTargetHint: {
        target: null,
        type: null,
      },
      collapsedViews: [],
      renamingTarget: null,
      highlightedTargets: [],
    },
    topmenu: {
      formulaBarMode: 'content',
      formulaBarFocusCounter: 0,
    },
    preview: {
      visible: false,
      connected: false,
    },
    home: {
      visible: false,
    },
    lastUsedFont: null,
    modal: null,
    localProjectList: [],
    projectList: [],
    showcaseProjects: [],
    codeEditingEnabled: false,
    codeEditorErrors: {
      buildErrors: {},
      lintErrors: {},
    },
    thumbnailLastGenerated: 0,
    pasteTargetsToIgnore: [],
    parseOrPrintInFlight: false,
    safeMode: false,
    saveError: false,
    vscodeBridgeReady: false,
    vscodeReady: false,
    focusedElementPath: null,
    config: defaultConfig(),
    theme: 'light',
    vscodeLoadingScreenVisible: true,
    indexedDBFailed: false,
    forceParseFiles: [],
    allElementProps: {},
    _currentAllElementProps_KILLME: {},
    githubSettings: {
      targetRepository: null,
      originCommit: null,
      branchName: null,
    },
    imageDragSessionState: notDragging(),
    githubOperations: [],
    githubChecksums: null,
    branchContents: null,
  }
}

export type OriginalFrame = FrameAndTarget<LocalRectangle>

export interface OriginalCanvasAndLocalFrame {
  target: ElementPath
  frame?: LocalRectangle
  canvasFrame?: CanvasRectangle
}

function getElementWarningsInner(
  rootMetadata: ElementInstanceMetadataMap,
): ComplexMap<ElementPath, ElementWarnings> {
  let result: ComplexMap<ElementPath, ElementWarnings> = emptyComplexMap()
  Object.values(rootMetadata).forEach((elementMetadata) => {
    // Check to see if this element is collapsed in one dimension.
    const globalFrame = elementMetadata.globalFrame
    const widthOrHeightZero =
      globalFrame != null ? globalFrame.width === 0 || globalFrame.height === 0 : false

    // Identify if this element looks to be trying to position itself with "pins", but
    // the parent element isn't appropriately configured.
    let absoluteWithUnpositionedParent: boolean = false
    if (
      elementMetadata.specialSizeMeasurements.position === 'absolute' &&
      !elementMetadata.specialSizeMeasurements.immediateParentProvidesLayout
    ) {
      absoluteWithUnpositionedParent = true
    }

    // Build the warnings object and add it to the map.
    if (
      widthOrHeightZero !== defaultElementWarnings.widthOrHeightZero ||
      absoluteWithUnpositionedParent !== defaultElementWarnings.absoluteWithUnpositionedParent
    ) {
      const warnings: ElementWarnings = {
        widthOrHeightZero: widthOrHeightZero,
        absoluteWithUnpositionedParent: absoluteWithUnpositionedParent,
        dynamicSceneChildWidthHeightPercentage: false,
      }
      result = addToComplexMap(toString, result, elementMetadata.elementPath, warnings)
    }
  })
  return result
}

const getElementWarnings = memoize(getElementWarningsInner, { maxSize: 1 })

type CacheableDerivedState = {
  navigatorTargets: ElementPath[]
  visibleNavigatorTargets: ElementPath[]
  elementWarnings: ComplexMap<ElementPath, ElementWarnings>
}

function deriveCacheableStateInner(
  jsxMetadata: ElementInstanceMetadataMap,
  collapsedViews: ElementPath[],
): CacheableDerivedState {
  const { navigatorTargets, visibleNavigatorTargets } =
    MetadataUtils.createOrderedElementPathsFromElements(jsxMetadata, collapsedViews)

  const warnings = getElementWarnings(jsxMetadata)

  return {
    navigatorTargets: navigatorTargets,
    visibleNavigatorTargets: visibleNavigatorTargets,
    elementWarnings: warnings,
  }
}

const patchedDeriveCacheableState = memoize(deriveCacheableStateInner, { maxSize: 1 })
const unpatchedDeriveCacheableState = memoize(deriveCacheableStateInner, { maxSize: 1 })

export function deriveState(
  editor: EditorState,
  oldDerivedState: DerivedState | null,
  cacheKey: 'patched' | 'unpatched' = 'unpatched',
): DerivedState {
  const derivedState = oldDerivedState == null ? emptyDerivedState(editor) : oldDerivedState

  const deriveCacheableState =
    cacheKey === 'patched' ? patchedDeriveCacheableState : unpatchedDeriveCacheableState

  const {
    navigatorTargets,
    visibleNavigatorTargets,
    elementWarnings: warnings,
  } = deriveCacheableState(editor.jsxMetadata, editor.navigator.collapsedViews)

  const derived: DerivedState = {
    navigatorTargets: navigatorTargets,
    visibleNavigatorTargets: visibleNavigatorTargets,
    controls: derivedState.controls,
    transientState: produceCanvasTransientState(
      oldDerivedState?.transientState.selectedViews ?? editor.selectedViews,
      editor,
      true,
    ),
    elementWarnings: warnings,
  }

  const sanitizedDerivedState = DerivedStateKeepDeepEquality()(derivedState, derived).value

  return sanitizedDerivedState
}

export function createCanvasModelKILLME(
  editor: EditorState,
  derivedState: DerivedState,
): CanvasModel {
  return {
    controls: derivedState.controls,
    dragState: editor.canvas.dragState,
    keysPressed: editor.keysPressed,
    mouseButtonsPressed: editor.mouseButtonsPressed,
    mode: editor.mode,
    scale: editor.canvas.scale,
    highlightedviews: editor.highlightedViews,
    selectedViews: editor.selectedViews,
    canvasOffset: editor.canvas.roundedCanvasOffset,
    focusedPanel: editor.focusedPanel,
    editorState: editor,
  }
}

export function editorModelFromPersistentModel(
  persistentModel: PersistentModel,
  dispatch: EditorDispatch,
): EditorState {
  const editor: EditorState = {
    id: null,
    vscodeBridgeId: vsCodeBridgeIdDefault(UUID()),
    forkedFromProjectId: persistentModel.forkedFromProjectId,
    appID: persistentModel.appID ?? null,
    projectName: createNewProjectName(),
    projectDescription: persistentModel.projectDescription,
    projectVersion: persistentModel.projectVersion,
    isLoaded: false,
    spyMetadata: emptyJsxMetadata,
    domMetadata: emptyJsxMetadata,
    jsxMetadata: emptyJsxMetadata,
    codeResultCache: generateCodeResultCache(
      persistentModel.projectContents,
      {},
      [],
      {},
      dispatch,
      {},
      [],
    ),
    projectContents: persistentModel.projectContents,
    propertyControlsInfo: { ...DefaultThirdPartyControlDefinitions },
    nodeModules: {
      skipDeepFreeze: true,
      files: {},
      projectFilesBuildResults: {},
      packageStatus: {},
    },
    selectedViews: [],
    highlightedViews: [],
    hiddenInstances: persistentModel.hiddenInstances,
    displayNoneInstances: [],
    warnedInstances: [],
    lockedElements: {
      simpleLock: [],
      hierarchyLock: [],
    },
    mode: EditorModes.selectMode(),
    focusedPanel: 'canvas',
    keysPressed: {},
    mouseButtonsPressed: emptySet(),
    openPopupId: null,
    toasts: [],
    cursorStack: {
      fixed: null,
      mouseOver: [],
    },
    leftMenu: {
      selectedTab: LeftMenuTab.Contents,
      expanded: false,
      paneWidth: LeftPaneDefaultWidth,
    },
    rightMenu: {
      selectedTab: RightMenuTab.Inspector,
      expanded: true,
    },
    interfaceDesigner: {
      codePaneWidth: 500,
      codePaneVisible: true,
      restorableCodePaneWidth: 500,
      additionalControls: true,
    },
    canvas: {
      elementsToRerender: 'rerender-all-elements',
      dragState: null, // TODO change dragState if editorMode changes
      interactionSession: null,
      visible: true,
      scale: 1,
      snappingThreshold: BaseSnappingThreshold,
      realCanvasOffset: BaseCanvasOffsetLeftPane,
      roundedCanvasOffset: BaseCanvasOffsetLeftPane,
      textEditor: null,
      selectionControlsVisible: true,
      cursor: null,
      duplicationState: null,
      base64Blobs: {},
      mountCount: 0,
      canvasContentInvalidateCount: 0,
      domWalkerInvalidateCount: 0,
      openFile: {
        filename: StoryboardFilePath,
      },
      scrollAnimation: false,
      transientProperties: null,
      resizeOptions: {
        propertyTargetOptions: ['width', 'height'],
        propertyTargetSelectedIndex: 0,
      },
      domWalkerAdditionalElementsToUpdate: [],
      controls: {
        snappingGuidelines: [],
        outlineHighlights: [],
        strategyIntendedBounds: [],
        flexReparentTargetLines: [],
        parentHighlightPaths: null,
        reparentedToPaths: [],
      },
    },
    floatingInsertMenu: {
      insertMenuMode: 'closed',
    },
    inspector: {
      visible: true,
      classnameFocusCounter: 0,
      layoutSectionHovered: false,
    },
    dependencyList: persistentModel.dependencyList,
    genericExternalResources: {
      minimised: true,
    },
    googleFontsResources: {
      minimised: true,
    },
    projectSettings: persistentModel.projectSettings,
    topmenu: {
      formulaBarMode: 'content',
      formulaBarFocusCounter: 0,
    },
    preview: {
      visible: false,
      connected: false,
    },
    home: {
      visible: false,
    },
    lastUsedFont: persistentModel.lastUsedFont,
    modal: null,
    localProjectList: [],
    projectList: [],
    showcaseProjects: [],
    codeEditingEnabled: false,
    thumbnailLastGenerated: 0,
    pasteTargetsToIgnore: [],
    parseOrPrintInFlight: false,
    safeMode: false,
    saveError: false,
    navigator: {
      dropTargetHint: {
        target: null,
        type: null,
      },
      collapsedViews: [],
      renamingTarget: null,
      minimised: persistentModel.navigator.minimised,
      highlightedTargets: [],
    },
    fileBrowser: {
      renamingTarget: null,
      dropTarget: null,
      minimised: persistentModel.fileBrowser.minimised,
    },
    codeEditorErrors: persistentModel.codeEditorErrors,
    vscodeBridgeReady: false,
    vscodeReady: false,
    focusedElementPath: null,
    config: defaultConfig(),
    theme: 'light',
    vscodeLoadingScreenVisible: true,
    indexedDBFailed: false,
    forceParseFiles: [],
    allElementProps: {},
    _currentAllElementProps_KILLME: {},
    githubSettings: persistentModel.githubSettings,
    imageDragSessionState: notDragging(),
    githubOperations: [],
    githubChecksums: persistentModel.githubChecksums,
    branchContents: persistentModel.branchContents,
  }
  return editor
}

function removeParsedModelsFromProjectContents(
  projectContents: ProjectContentTreeRoot,
): ProjectContentTreeRoot {
  return applyToAllUIJSFiles(projectContents, (_: string, file: TextFile) => {
    return codeFile(file.fileContents.code, file.lastSavedContents?.code ?? null)
  })
}

export function persistentModelFromEditorModel(editor: EditorState): PersistentModel {
  return {
    appID: editor.appID,
    forkedFromProjectId: editor.forkedFromProjectId,
    projectVersion: editor.projectVersion,
    projectDescription: editor.projectDescription,
    projectContents: removeParsedModelsFromProjectContents(editor.projectContents),
    exportsInfo: editor.codeResultCache.exportsInfo,
    lastUsedFont: editor.lastUsedFont,
    hiddenInstances: editor.hiddenInstances,
    codeEditorErrors: editor.codeEditorErrors,
    fileBrowser: {
      minimised: editor.fileBrowser.minimised,
    },
    dependencyList: {
      minimised: editor.dependencyList.minimised,
    },
    projectSettings: {
      minimised: editor.projectSettings.minimised,
    },
    navigator: {
      minimised: editor.navigator.minimised,
    },
    githubSettings: editor.githubSettings,
    githubChecksums: editor.githubChecksums,
    branchContents: editor.branchContents,
  }
}

export function persistentModelForProjectContents(
  projectContents: ProjectContentTreeRoot,
): PersistentModel {
  return {
    appID: null,
    forkedFromProjectId: null,
    projectVersion: CURRENT_PROJECT_VERSION,
    projectDescription: '',
    projectContents: projectContents,
    exportsInfo: [],
    codeEditorErrors: {
      buildErrors: {},
      lintErrors: {},
    },
    lastUsedFont: null,
    hiddenInstances: [],
    fileBrowser: {
      minimised: false,
    },
    dependencyList: {
      minimised: false,
    },
    projectSettings: {
      minimised: false,
    },
    navigator: {
      minimised: false,
    },
    githubSettings: {
      targetRepository: null,
      originCommit: null,
      branchName: null,
    },
    githubChecksums: null,
    branchContents: null,
  }
}

const defaultDependencies = Utils.mapArrayToDictionary(
  DefaultPackagesList,
  (p) => p.name,
  (p) => p.version,
)

export const defaultIndexHtmlFilePath = 'public/index.html'

export const DefaultPackageJson = {
  name: 'Utopia Project',
  version: '0.1.0',
  utopia: {
    'main-ui': StoryboardFilePath.slice(1),
    html: defaultIndexHtmlFilePath,
    js: 'src/index.js',
  },
  dependencies: {
    ...defaultDependencies,
  },
}

export function packageJsonFileFromProjectContents(
  projectContents: ProjectContentTreeRoot,
): ProjectFile | null {
  return getContentsTreeFileFromString(projectContents, '/package.json')
}

export function getPackageJsonFromEditorState(editor: EditorState): Either<string, any> {
  const packageJsonFile = packageJsonFileFromProjectContents(editor.projectContents)
  if (packageJsonFile != null && isTextFile(packageJsonFile)) {
    const packageJsonContents = Utils.jsonParseOrNull(packageJsonFile.fileContents.code)
    return packageJsonContents != null
      ? right(packageJsonContents)
      : left('package.json parse error')
  } else {
    return left('No package.json file.')
  }
}

export function getMainUIFromModel(model: EditorState): string | null {
  const packageJsonContents = getPackageJsonFromEditorState(model)
  if (isRight(packageJsonContents)) {
    const mainUI = Utils.path(['utopia', 'main-ui'], packageJsonContents.value)
    // Make sure someone hasn't put something bizarro in there.
    if (typeof mainUI === 'string') {
      return mainUI
    }
  }
  return null
}

export function getIndexHtmlFileFromEditorState(editor: EditorState): Either<string, TextFile> {
  const parsedFilePath = mapEither(
    (contents) => contents?.utopia?.html,
    getPackageJsonFromEditorState(editor),
  )
  const filePath =
    isRight(parsedFilePath) && typeof parsedFilePath.value === 'string'
      ? parsedFilePath.value
      : 'public/index.html'
  const indexHtml = getContentsTreeFileFromString(editor.projectContents, `/${filePath}`)
  if (indexHtml != null && isTextFile(indexHtml)) {
    return right(indexHtml)
  } else {
    return left(`Can't find code file at ${filePath}`)
  }
}

export function updateMainUIInPackageJson(packageJson: string, mainUI: string): string {
  function updateDeps(parsedPackageJson: any): string {
    return JSON.stringify(
      ObjectPathImmutable.set(parsedPackageJson, ['utopia', 'main-ui'], mainUI),
      null,
      2,
    )
  }
  try {
    const parsedJSON = json5.parse(packageJson)
    return updateDeps(parsedJSON)
  } catch (error) {
    console.error('Error parsing package.json.', error)
    return updateDeps({})
  }
}

export function updatePackageJsonInEditorState(
  editor: EditorState,
  transformPackageJson: (packageJson: string) => string,
): EditorState {
  const packageJsonFile = packageJsonFileFromProjectContents(editor.projectContents)
  let updatedPackageJsonFile: TextFile
  if (packageJsonFile == null) {
    // Uh oh, there is no package.json file, so create a brand new one.
    updatedPackageJsonFile = codeFile(
      transformPackageJson(JSON.stringify(DefaultPackageJson)),
      null,
    )
  } else {
    if (isTextFile(packageJsonFile)) {
      // There is a package.json file, we should update it.
      updatedPackageJsonFile = codeFile(
        transformPackageJson(packageJsonFile.fileContents.code),
        null,
      )
    } else {
      // There is something else called package.json, we should bulldoze over it.
      updatedPackageJsonFile = codeFile(
        transformPackageJson(JSON.stringify(DefaultPackageJson)),
        null,
      )
    }
  }
  return {
    ...editor,
    projectContents: addFileToProjectContents(
      editor.projectContents,
      '/package.json',
      updatedPackageJsonFile,
    ),
  }
}

export function updateMainUIInEditorState(editor: EditorState, mainUI: string): EditorState {
  const transformPackageJson = (packageJson: string) => {
    return updateMainUIInPackageJson(packageJson, mainUI)
  }
  return updatePackageJsonInEditorState(editor, transformPackageJson)
}

export function areGeneratedElementsSelected(editor: EditorState): boolean {
  return areGeneratedElementsTargeted(editor.selectedViews, editor)
}

export function areGeneratedElementsTargeted(
  targets: Array<ElementPath>,
  editor: EditorState,
): boolean {
  return targets.some((target) => {
    return withUnderlyingTargetFromEditorState(target, editor, false, (success) => {
      const originType = MetadataUtils.getElementOriginType(
        getUtopiaJSXComponentsFromSuccess(success),
        target,
      )
      switch (originType) {
        case 'unknown-element':
        case 'generated-static-definition-present':
          return true
        default:
          return false
      }
    })
  })
}

export function getAllCodeEditorErrors(
  editor: EditorState,
  minimumSeverity: ErrorMessageSeverity,
  skipTsErrors: boolean,
): Array<ErrorMessage> {
  const allLintErrors = getAllLintErrors(editor)
  const allBuildErrors = getAllBuildErrors(editor)
  const errorsAndWarnings = skipTsErrors ? allLintErrors : [...allBuildErrors, ...allLintErrors]
  if (minimumSeverity === 'fatal') {
    return errorsAndWarnings.filter((error) => error.severity === 'fatal')
  } else if (minimumSeverity === 'error') {
    return errorsAndWarnings.filter(
      (error) => error.severity === 'fatal' || error.severity === 'error',
    )
  } else {
    return errorsAndWarnings
  }
}

export function getAllBuildErrors(editor: EditorState): Array<ErrorMessage> {
  return getAllErrorsFromFiles(editor.codeEditorErrors.buildErrors)
}

export function getAllLintErrors(editor: EditorState): Array<ErrorMessage> {
  return getAllErrorsFromFiles(editor.codeEditorErrors.lintErrors)
}

export function getAllErrorsFromFiles(errorsInFiles: ErrorMessages): Array<ErrorMessage> {
  return Object.keys(errorsInFiles).flatMap((filename) => errorsInFiles[filename] ?? [])
}

export function parseFailureAsErrorMessages(
  fileName: string | null,
  parseResult: TextFile | null,
): Array<ErrorMessage> {
  if (parseResult == null || !isParseFailure(parseResult.fileContents.parsed)) {
    return []
  } else {
    const parseFailure = parseResult.fileContents.parsed
    const fileNameString = fileName ?? ''
    let errors: Array<ErrorMessage> = []
    if (parseFailure.diagnostics != null && parseFailure.diagnostics.length > 0) {
      errors.push(...parseFailure.diagnostics)
    }
    errors.push(...parseFailure.errorMessages)
    if (parseFailure.errorMessage != null) {
      errors.push({
        codeSnippet: parseFailure.errorMessage,
        type: 'Parser Error', // TODO Check message and maybe add a better description field about what is utopia doing
        startLine: null,
        startColumn: null,
        endLine: null,
        endColumn: null,
        errorCode: '',
        fileName: fileNameString,
        message: 'Parse Failure',
        source: 'utopia-parser',
        severity: 'fatal',
        passTime: null,
      })
    }
    if (parseFailure.parsedJSONFailure != null) {
      errors.push({
        codeSnippet: parseFailure.parsedJSONFailure.codeSnippet,
        type: 'Parser Error', // TODO Check message and maybe add a better description field about what is utopia doing
        startLine: parseFailure.parsedJSONFailure.startLine,
        startColumn: parseFailure.parsedJSONFailure.startCol,
        endLine: parseFailure.parsedJSONFailure.endLine,
        endColumn: parseFailure.parsedJSONFailure.endCol,
        errorCode: '',
        fileName: fileNameString,
        message: `Parse Failure in canvasMetadata: ${parseFailure.parsedJSONFailure.reason}`,
        source: 'utopia-parser',
        severity: 'fatal',
        passTime: null,
      })
    }
    return errors
  }
}

export function reconstructJSXMetadata(editor: EditorState): ElementInstanceMetadataMap {
  const uiFile = getOpenUIJSFile(editor)
  if (uiFile == null) {
    return editor.jsxMetadata
  } else {
    return foldParsedTextFile(
      (_) => editor.jsxMetadata,
      (success) => {
        const elementsByUID = getElementsByUIDFromTopLevelElements(success.topLevelElements)
        const mergedMetadata = MetadataUtils.mergeComponentMetadata(
          elementsByUID,
          editor.spyMetadata,
          editor.domMetadata,
        )
        return ElementInstanceMetadataMapKeepDeepEquality(editor.jsxMetadata, mergedMetadata).value
      },
      (_) => editor.jsxMetadata,
      uiFile.fileContents.parsed,
    )
  }
}

export function getStoryboardElementPathFromEditorState(
  editor: EditorState,
): StaticElementPath | null {
  return getStoryboardElementPath(editor.projectContents, editor.canvas.openFile?.filename ?? null)
}

export function getHighlightBoundsForFile(
  editor: EditorState,
  fullPath: string,
): HighlightBoundsForUids | null {
  const file = getContentsTreeFileFromString(editor.projectContents, fullPath)
  if (isTextFile(file)) {
    if (isParseSuccess(file.fileContents.parsed)) {
      return getHighlightBoundsFromParseResult(file.fileContents.parsed)
    }
    if (file.lastParseSuccess != null) {
      return getHighlightBoundsFromParseResult(file.lastParseSuccess)
    }
  }
  return null
}

export function getHighlightBoundsForElementPath(
  path: ElementPath,
  editor: EditorState,
): HighlightBoundsWithFile | null {
  const staticPath = EP.dynamicPathToStaticPath(path)
  if (staticPath != null) {
    const highlightBounds = getHighlightBoundsForProject(editor.projectContents)
    if (highlightBounds != null) {
      const highlightedUID = toUid(staticPath)
      return highlightBounds[highlightedUID]
    }
  }

  return null
}

export function getHighlightBoundsForElementPaths(
  paths: Array<ElementPath>,
  editor: EditorState,
): HighlightBoundsWithFileForUids {
  const targetUIDs = paths.map((path) => toUid(EP.dynamicPathToStaticPath(path)))
  const projectHighlightBounds = getHighlightBoundsForProject(editor.projectContents)
  return pick(targetUIDs, projectHighlightBounds)
}

export function getElementPathsInBounds(
  line: number,
  parsedHighlightBounds: HighlightBoundsForUids | null,
  allElementPaths: Array<ElementPath>,
): Array<ElementPath> {
  if (parsedHighlightBounds == null) {
    return []
  } else {
    let highlightBounds = Object.values(parsedHighlightBounds).filter((bounds) => {
      return line >= bounds.startLine && line <= bounds.endLine
    })
    // Put the lowest possible start line first.
    highlightBounds.sort((a, b) => b.startLine - a.startLine)
    let paths: Array<ElementPath> = []
    if (highlightBounds.length > 0) {
      const target = highlightBounds[0].uid
      Utils.fastForEach(allElementPaths, (path) => {
        const staticPath = dynamicPathToStaticPath(path)
        const uid = staticPath != null ? toUid(staticPath) : null
        if (uid === target) {
          paths.push(path)
        }
      })
    }
    return paths
  }
}

export function modifyParseSuccessAtPath(
  filePath: string,
  editor: EditorState,
  modifyParseSuccess: (parseSuccess: ParseSuccess) => ParseSuccess,
): EditorState {
  const projectFile = getContentsTreeFileFromString(editor.projectContents, filePath)
  if (isTextFile(projectFile)) {
    const parsedFileContents = projectFile.fileContents.parsed
    if (isParseSuccess(parsedFileContents)) {
      const updatedParseSuccess = modifyParseSuccess(parsedFileContents)
      // Try to keep referential equality as much as possible.
      if (updatedParseSuccess === parsedFileContents) {
        return editor
      } else {
        const updatedFile = saveTextFileContents(
          projectFile,
          textFileContents(
            projectFile.fileContents.code,
            updatedParseSuccess,
            RevisionsState.ParsedAhead,
          ),
          false,
        )
        return {
          ...editor,
          projectContents: addFileToProjectContents(editor.projectContents, filePath, updatedFile),
        }
      }
    } else {
      throw new Error(`File ${filePath} is not currently parsed.`)
    }
  } else {
    throw new Error(`No text file found at ${filePath}`)
  }
}

export function modifyUnderlyingTarget(
  target: ElementPath | null,
  currentFilePath: string,
  editor: EditorState,
  modifyElement: (
    element: JSXElement,
    underlying: ElementPath,
    underlyingFilePath: string,
  ) => JSXElement = (element) => element,
  modifyParseSuccess: (
    parseSuccess: ParseSuccess,
    underlying: StaticElementPath | null,
    underlyingFilePath: string,
  ) => ParseSuccess = (success) => success,
): EditorState {
  const underlyingTarget = normalisePathToUnderlyingTarget(
    editor.projectContents,
    editor.nodeModules.files,
    currentFilePath,
    target,
  )
  const targetSuccess = normalisePathSuccessOrThrowError(underlyingTarget)

  function innerModifyParseSuccess(oldParseSuccess: ParseSuccess): ParseSuccess {
    // Apply the ParseSuccess level changes.
    let updatedParseSuccess: ParseSuccess = modifyParseSuccess(
      oldParseSuccess,
      targetSuccess.normalisedPath,
      targetSuccess.filePath,
    )

    // Apply the JSXElement level changes.
    const oldUtopiaJSXComponents = getUtopiaJSXComponentsFromSuccess(updatedParseSuccess)
    let elementModified: boolean = false
    let updatedUtopiaJSXComponents: Array<UtopiaJSXComponent>
    if (targetSuccess.normalisedPath == null) {
      updatedUtopiaJSXComponents = oldUtopiaJSXComponents
    } else {
      const nonNullNormalisedPath = targetSuccess.normalisedPath
      function innerModifyElement(element: JSXElement): JSXElement {
        const updatedElement = modifyElement(element, nonNullNormalisedPath, targetSuccess.filePath)
        elementModified = updatedElement !== element
        return updatedElement
      }
      updatedUtopiaJSXComponents = transformElementAtPath(
        oldUtopiaJSXComponents,
        targetSuccess.normalisedPath,
        innerModifyElement,
      )
    }
    // Try to keep the old structures where possible.
    if (elementModified) {
      const newTopLevelElements = applyUtopiaJSXComponentsChanges(
        updatedParseSuccess.topLevelElements,
        updatedUtopiaJSXComponents,
      )

      return {
        ...updatedParseSuccess,
        topLevelElements: newTopLevelElements,
      }
    } else {
      return updatedParseSuccess
    }
  }

  return modifyParseSuccessAtPath(targetSuccess.filePath, editor, innerModifyParseSuccess)
}

export function modifyUnderlyingForOpenFile(
  target: ElementPath | null,
  editor: EditorState,
  modifyElement: (
    element: JSXElement,
    underlying: ElementPath,
    underlyingFilePath: string,
  ) => JSXElement = (element) => element,
  modifyParseSuccess: (
    parseSuccess: ParseSuccess,
    underlying: StaticElementPath | null,
    underlyingFilePath: string,
  ) => ParseSuccess = (success) => success,
): EditorState {
  return modifyUnderlyingTarget(
    target,
    forceNotNull('Designer file should be open.', editor.canvas.openFile?.filename),
    editor,
    modifyElement,
    modifyParseSuccess,
  )
}

export function withUnderlyingTarget<T>(
  target: ElementPath | null | undefined,
  projectContents: ProjectContentTreeRoot,
  nodeModules: NodeModules,
  openFile: string | null | undefined,
  defaultValue: T,
  withTarget: (
    success: ParseSuccess,
    element: JSXElement,
    underlyingTarget: StaticElementPath,
    underlyingFilePath: string,
    underlyingDynamicTarget: ElementPath,
  ) => T,
): T {
  const underlyingTarget = normalisePathToUnderlyingTarget(
    projectContents,
    nodeModules,
    forceNotNull('Designer file should be open.', openFile),
    target ?? null,
  )

  if (
    underlyingTarget.type === 'NORMALISE_PATH_SUCCESS' &&
    underlyingTarget.normalisedPath != null &&
    underlyingTarget.normalisedDynamicPath != null
  ) {
    const parsed = underlyingTarget.textFile.fileContents.parsed
    if (isParseSuccess(parsed)) {
      const element = findJSXElementAtStaticPath(
        getUtopiaJSXComponentsFromSuccess(parsed),
        underlyingTarget.normalisedPath,
      )
      if (element != null) {
        return withTarget(
          parsed,
          element,
          underlyingTarget.normalisedPath,
          underlyingTarget.filePath,
          underlyingTarget.normalisedDynamicPath,
        )
      }
    }
  }

  return defaultValue
}

export function withUnderlyingTargetFromEditorState<T>(
  target: ElementPath | null,
  editor: EditorState,
  defaultValue: T,
  withTarget: (
    success: ParseSuccess,
    element: JSXElement,
    underlyingTarget: StaticElementPath,
    underlyingFilePath: string,
  ) => T,
): T {
  return withUnderlyingTarget(
    target,
    editor.projectContents,
    editor.nodeModules.files,
    editor.canvas.openFile?.filename ?? null,
    defaultValue,
    withTarget,
  )
}

export function forUnderlyingTargetFromEditorState(
  target: ElementPath | null,
  editor: EditorState,
  withTarget: (
    success: ParseSuccess,
    element: JSXElement,
    underlyingTarget: StaticElementPath,
    underlyingFilePath: string,
  ) => void,
): void {
  withUnderlyingTargetFromEditorState<any>(target, editor, {}, withTarget)
}

export function forUnderlyingTarget(
  target: ElementPath | null,
  projectContents: ProjectContentTreeRoot,
  nodeModules: NodeModules,
  openFile: string | null | undefined,
  withTarget: (
    success: ParseSuccess,
    element: JSXElement,
    underlyingTarget: StaticElementPath,
    underlyingFilePath: string,
  ) => void,
): void {
  withUnderlyingTarget<any>(target, projectContents, nodeModules, openFile, {}, withTarget)
}

export function getElementFromProjectContents(
  target: ElementPath | null,
  projectContents: ProjectContentTreeRoot,
  openFile: string | null | undefined,
): JSXElement | null {
  return withUnderlyingTarget(target, projectContents, {}, openFile, null, (_, element) => element)
}

export function getCurrentTheme(editor: EditorState): Theme {
  return editor.theme
}

export function getNewSceneName(editor: EditorState): string {
  const openFile = getOpenUIJSFile(editor)
  if (openFile != null) {
    if (isParseSuccess(openFile.fileContents.parsed)) {
      const success = openFile.fileContents.parsed
      function checkSceneNameExists(sceneN: number): string {
        let exists: boolean = false
        const sceneName = `Scene ${sceneN}`
        walkElements(success.topLevelElements, (elementChild) => {
          if (!exists && isJSXElement(elementChild)) {
            exists = elementChild.props.some(
              (prop) =>
                prop.type === 'JSX_ATTRIBUTES_ENTRY' &&
                prop.key === UTOPIA_LABEL_KEY &&
                prop.value.type === 'ATTRIBUTE_VALUE' &&
                prop.value.value === sceneName,
            )
          }
        })
        if (exists) {
          return checkSceneNameExists(sceneN + 1)
        } else {
          return sceneName
        }
      }
      return checkSceneNameExists(1)
    }
  }

  // Fallback.
  return 'New Scene'
}
