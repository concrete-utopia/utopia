import * as json5 from 'json5'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import {
  findJSXElementAtStaticPath,
  findJSXElementChildAtPath,
  removeJSXElement,
  transformJSXComponentAtPath,
} from '../../../core/model/element-template-utils'
import {
  type FilePathMappings,
  applyToAllUIJSFiles,
  applyUtopiaJSXComponentsChanges,
  getFilePathMappings,
  getHighlightBoundsForProject,
  getHighlightBoundsFromParseResult,
  getUtopiaJSXComponentsFromSuccess,
  saveTextFileContents,
} from '../../../core/model/project-file-utils'
import { getStoryboardElementPath } from '../../../core/model/scene-utils'
import type { Either } from '../../../core/shared/either'
import { forEachRight, isRight, left, mapEither, right } from '../../../core/shared/either'
import type {
  ElementInstanceMetadataMap,
  JSExpression,
  JSXConditionalExpression,
  JSXElement,
  JSXElementChild,
  JSXFragment,
  TopLevelElement,
  UtopiaJSXComponent,
} from '../../../core/shared/element-template'
import {
  emptyJsxMetadata,
  getElementsByUIDFromTopLevelElements,
  isJSExpression,
  isJSXConditionalExpression,
  isJSXElement,
  isJSXFragment,
  isUtopiaJSXComponent,
  walkElements,
} from '../../../core/shared/element-template'
import type { ErrorMessage, ErrorMessageSeverity } from '../../../core/shared/error-messages'
import type {
  CanvasPoint,
  CanvasRectangle,
  CanvasVector,
  LocalRectangle,
  WindowPoint,
} from '../../../core/shared/math-utils'
import { isFiniteRectangle, size } from '../../../core/shared/math-utils'
import type { PackageStatus, PackageStatusMap } from '../../../core/shared/npm-dependency-types'
import type {
  ElementPath,
  ElementPropertyPath,
  ExportDetail,
  HighlightBoundsForUids,
  HighlightBoundsWithFile,
  HighlightBoundsWithFileForUids,
  ImportDetails,
  Imports,
  NodeModules,
  ParseSuccess,
  ProjectFile,
  PropertyPath,
  StaticElementPath,
  TextFile,
} from '../../../core/shared/project-file-types'
import {
  RevisionsState,
  codeFile,
  foldParsedTextFile,
  isParseFailure,
  isParseSuccess,
  isParsedTextFile,
  isTextFile,
  parseSuccess,
  textFileContents,
} from '../../../core/shared/project-file-types'
import type {
  ExportsInfo,
  MultiFileBuildResult,
  UtopiaTsWorkers,
} from '../../../core/workers/common/worker-types'
import type { KeysPressed } from '../../../utils/keyboard'
import type { IndexPosition } from '../../../utils/utils'
import Utils from '../../../utils/utils'
import type { ProjectContentTreeRoot } from '../../assets'
import { packageJsonFileFromProjectContents } from '../../assets'
import {
  addFileToProjectContents,
  getProjectFileByFilePath,
  getProjectContentsChecksums,
} from '../../assets'
import type {
  CSSCursor,
  CanvasFrameAndTarget,
  CanvasModel,
  FrameAndTarget,
  HigherOrderControl,
} from '../../canvas/canvas-types'
import { getParseSuccessForFilePath } from '../../canvas/canvas-utils'
import type { EditorPanel } from '../../common/actions/index'
import type { CodeResultCache, PropertyControlsInfo, ResolveFn } from '../../custom-code/code-file'
import {
  generateCodeResultCache,
  normalisePathSuccessOrThrowError,
  normalisePathToUnderlyingTarget,
} from '../../custom-code/code-file'
import type { FontSettings } from '../../inspector/common/css-utils'
import type { EditorDispatch, LoginState, ProjectListing } from '../action-types'
import { CURRENT_PROJECT_VERSION } from '../actions/migrations/migrations'
import type { Mode, PersistedMode } from '../editor-modes'
import { EditorModes, convertModeToSavedMode } from '../editor-modes'
import type { StateHistory } from '../history'

import { dynamicPathToStaticPath, toUid } from '../../../core/shared/element-path'

import * as friendlyWords from 'friendly-words'
import type { UtopiaVSCodeConfig } from 'utopia-vscode-common'
import { defaultConfig } from 'utopia-vscode-common'
import { loginNotYetKnown } from '../../../common/user'
import * as EP from '../../../core/shared/element-path'
import { assertNever } from '../../../core/shared/utils'
import type { Notice } from '../../common/notice'
import type { ShortcutConfiguration } from '../shortcut-definitions'
import {
  DerivedStateKeepDeepEquality,
  ElementInstanceMetadataMapKeepDeepEquality,
  InvalidOverrideNavigatorEntryKeepDeepEquality,
  RenderPropNavigatorEntryKeepDeepEquality,
  RenderPropValueNavigatorEntryKeepDeepEquality,
  SyntheticNavigatorEntryKeepDeepEquality,
} from './store-deep-equality-instances'

import * as OPI from 'object-path-immutable'
import type { MapLike } from 'typescript'
import type { LayoutTargetableProp } from '../../../core/layout/layout-helpers-new'
import { atomWithPubSub } from '../../../core/shared/atom-with-pub-sub'
import { objectMap, pick } from '../../../core/shared/object-utils'

import type { Spec } from 'immutability-helper'
import { v4 as UUID } from 'uuid'
import { getNavigatorTargets } from '../../../components/navigator/navigator-utils'
import type { BuiltInDependencies } from '../../../core/es-modules/package-manager/built-in-dependencies-list'
import type { ConditionalCase } from '../../../core/model/conditionals'
import { UTOPIA_LABEL_KEY } from '../../../core/model/utopia-constants'
import type { FileResult } from '../../../core/shared/file-utils'
import type {
  GithubBranch,
  GithubFileChanges,
  GithubFileStatus,
  RepositoryEntry,
  TreeConflicts,
} from '../../../core/shared/github/helpers'
import type { ValueAtPath } from '../../../core/shared/jsx-attributes'
import { memoize } from '../../../core/shared/memoize'
import { fromTypeGuard } from '../../../core/shared/optics/optic-creators'
import type { Optic } from '../../../core/shared/optics/optics'
import { emptySet } from '../../../core/shared/set-utils'
import { getUtopiaID } from '../../../core/shared/uid-utils'
import { DefaultThirdPartyControlDefinitions } from '../../../core/third-party/third-party-controls'
import type { MouseButtonsPressed } from '../../../utils/mouse'
import type { Theme } from '../../../uuiui/styles/theme'
import { getPreferredColorScheme } from '../../../uuiui/styles/theme'
import type {
  InteractionSession,
  StrategyState,
} from '../../canvas/canvas-strategies/interaction-state'
import { treatElementAsFragmentLike } from '../../canvas/canvas-strategies/strategies/fragment-like-helpers'
import type { GuidelineWithSnappingVectorAndPointsOfRelevance } from '../../canvas/guideline'
import type { PersistenceMachine } from '../persistence/persistence'
import type { ThemeSubstate } from './store-hook-substore-types'
import type { ElementPathTrees } from '../../../core/shared/element-path-tree'
import type { CopyData, ElementPasteWithMetadata } from '../../../utils/clipboard'
import type { InvalidGroupState } from '../../canvas/canvas-strategies/strategies/group-helpers'
import {
  getGroupChildStateWithGroupMetadata,
  getGroupState,
  isInvalidGroupState,
  treatElementAsGroupLikeFromMetadata,
} from '../../canvas/canvas-strategies/strategies/group-helpers'
import type { RemixDerivedData, RemixDerivedDataFactory } from './remix-derived-data'
import type { ProjectServerState } from './project-server-state'
import type { ReparentTargetForPaste } from '../../canvas/canvas-strategies/strategies/reparent-utils'
import { GridMenuWidth } from '../../canvas/stored-layout'
import type { VariablesInScope } from '../../canvas/ui-jsx-canvas'
import type { ActiveFrame } from '../../canvas/commands/set-active-frames-command'
import { Y } from '../../../core/shared/yjs'
import { removeUnusedImportsForRemovedElement } from '../import-utils'
import { emptyImports } from '../../../core/workers/common/project-file-utils'
import type { CommentFilterMode } from '../../inspector/sections/comment-section'
import type { Collaborator } from '../../../core/shared/multiplayer'
import type { OnlineState } from '../online-status'
import type { NavigatorRow } from '../../navigator/navigator-row'

const ObjectPathImmutable: any = OPI

export enum LeftMenuTab {
  UIInsert = 'ui-insert',
  Project = 'project',
  Github = 'github',
  Navigator = 'navigator',
  Pages = 'pages',
}

export const DefaultNavigatorWidth = GridMenuWidth
export const CanvasSizeAtom = atomWithPubSub({
  key: 'CanvasSizeAtom',
  defaultValue: size(0, 0),
})

export enum RightMenuTab {
  Insert = 'insert',
  Inspector = 'inspector',
  Settings = 'settings',
  Comments = 'comments',
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
export const PlaygroundFilePath: string = '/src/playground.js'

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

const userProjectPermission = [
  'can_view',
  'can_fork',
  'can_play',
  'can_edit',
  'can_comment',
  'can_show_presence',
  'can_see_live_changes',
  'can_request_access',
  'can_manage',
] as const

type Permissions = (typeof userProjectPermission)[number]

export type UserPermissions = {
  [key in Permissions]: boolean
}

export function emptyUserPermissions(defaultValue: boolean): UserPermissions {
  return userProjectPermission.reduce((acc, permission) => {
    acc[permission] = defaultValue
    return acc
  }, {} as UserPermissions)
}

export interface UserConfiguration {
  shortcutConfig: ShortcutConfiguration | null
  themeConfig: ThemeSetting | null
}

export function emptyUserConfiguration(): UserConfiguration {
  return {
    shortcutConfig: null,
    themeConfig: DefaultTheme,
  }
}

export interface GithubState {
  authenticated: boolean
  gitRepoToLoad: GithubRepoWithBranch | null
}

export interface UserState extends UserConfiguration {
  loginState: LoginState
  githubState: GithubState
}

export interface GithubCommitAndPush {
  name: 'commitAndPush'
}

export interface GithubListBranches {
  name: 'listBranches'
}

export interface GithubLoadBranch {
  name: 'loadBranch'
  githubRepo: GithubRepo
  branchName: string | null
}

export interface GithubLoadRepositories {
  name: 'loadRepositories'
}

export interface GithubSearchRepository {
  name: 'searchRepository'
}

export interface GithubUpdateAgainstBranch {
  name: 'updateAgainstBranch'
}

export interface GithubListPullRequestsForBranch {
  name: 'listPullRequestsForBranch'
  githubRepo: GithubRepo
  branchName: string
}

export interface GithubSaveAsset {
  name: 'saveAsset'
  path: string
}

export type GithubOperation =
  | GithubCommitAndPush
  | GithubListBranches
  | GithubLoadBranch
  | GithubLoadRepositories
  | GithubUpdateAgainstBranch
  | GithubListPullRequestsForBranch
  | GithubSaveAsset
  | GithubSearchRepository

export function githubOperationLocksEditor(op: GithubOperation): boolean {
  switch (op.name) {
    case 'listBranches':
    case 'loadRepositories':
    case 'listPullRequestsForBranch':
      return false
    default:
      return true
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

export function isGithubCommitting(operations: Array<GithubOperation>): boolean {
  return operations.some((o) => o.name === 'commitAndPush')
}

export function isGithubLoadingRepositories(operations: Array<GithubOperation>): boolean {
  return operations.some((operation) => operation.name === 'loadRepositories')
}

export function isGithubListingBranches(operations: Array<GithubOperation>): boolean {
  return operations.some((operation) => operation.name === 'listBranches')
}

export function isGithubLoadingAnyBranch(operations: Array<GithubOperation>): boolean {
  return operations.some((operation) => operation.name === 'loadBranch')
}

export function isGithubUpdating(operations: Array<GithubOperation>): boolean {
  return operations.some((operation) => operation.name === 'updateAgainstBranch')
}

export function isGithubListingPullRequestsForBranch(
  operations: Array<GithubOperation>,
  repo: GithubRepo,
  branchName: string,
): boolean {
  return operations.some((operation) => {
    return (
      operation.name === 'listPullRequestsForBranch' &&
      githubRepoEquals(operation.githubRepo, repo) &&
      operation.branchName === branchName
    )
  })
}

export const defaultUserState: UserState = {
  loginState: loginNotYetKnown,
  shortcutConfig: {},
  themeConfig: 'system',
  githubState: {
    authenticated: false,
    gitRepoToLoad: null,
  },
}

export type CollabTextFileTopLevelElements = Y.Array<TopLevelElement>

export type CollabTextFileExportsDetail = Y.Array<ExportDetail>

export type CollabTextFileImports = Y.Map<ImportDetails>

export type CollabTextFile = Y.Map<
  | 'TEXT_FILE'
  | CollabTextFileTopLevelElements
  | CollabTextFileExportsDetail
  | CollabTextFileImports
  | string
>

export type CollabImageFile = Y.Map<'IMAGE_FILE' | string | number>

export type CollabAssetFile = Y.Map<'ASSET_FILE' | string>

export type CollabFile = CollabTextFile | CollabImageFile | CollabAssetFile

export interface CollaborativeEditingSupportSession {
  mergeDoc: Y.Doc
  projectContents: Y.Map<CollabFile>
}

export interface CollaborativeEditingSupport {
  session: CollaborativeEditingSupportSession | null
}

export function emptyCollaborativeEditingSupportSession(): CollaborativeEditingSupportSession {
  const doc = new Y.Doc()
  return {
    mergeDoc: doc,
    projectContents: doc.getMap('projectContents'),
  }
}

export function emptyCollaborativeEditingSupport(): CollaborativeEditingSupport {
  return {
    session: emptyCollaborativeEditingSupportSession(),
  }
}

export type EditorStoreShared = {
  postActionInteractionSession: PostActionMenuSession | null
  strategyState: StrategyState
  history: StateHistory
  userState: UserState
  workers: UtopiaTsWorkers
  persistence: PersistenceMachine
  builtInDependencies: BuiltInDependencies
  saveCountThisSession: number
  projectServerState: ProjectServerState
  collaborativeEditingSupport: CollaborativeEditingSupport
  onlineState: OnlineState
}

export type EditorStoreFull = EditorStoreShared & {
  unpatchedEditor: EditorState
  patchedEditor: EditorState
  unpatchedDerived: DerivedState
  patchedDerived: DerivedState
}

type StoreName = 'editor-store' | 'canvas-store' | 'low-priority-store'

export type EditorStorePatched = EditorStoreShared & {
  storeName: StoreName
  editor: EditorState
  derived: DerivedState
}

export type EditorStoreUnpatched = Omit<EditorStoreFull, 'patchedEditor' | 'patchedDerived'>

export function patchedStoreFromFullStore(
  store: EditorStoreFull,
  name: StoreName,
): EditorStorePatched {
  return {
    ...store,
    storeName: name,
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

export interface DisconnectGithubProjectModal {
  type: 'disconnect-github-project'
}

export function disconnectGithubProjectModal(branchName: string): DisconnectGithubProjectModal {
  return {
    type: 'disconnect-github-project',
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
  | DisconnectGithubProjectModal

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

export interface DesignerFile {
  filename: string
}

export function designerFile(filename: string): DesignerFile {
  return {
    filename: filename,
  }
}

export type ThemeSetting = 'light' | 'dark' | 'system'
export const DefaultTheme: ThemeSetting = 'system'

export type DropTargetType = 'before' | 'after' | 'reparent'

export interface DropTargetHint {
  displayAtEntry: NavigatorEntry
  targetParent: NavigatorEntry
  type: DropTargetType
  targetIndexPosition: IndexPosition
}

export interface NavigatorState {
  minimised: boolean
  dropTargetHint: DropTargetHint | null
  collapsedViews: ElementPath[]
  renamingTarget: ElementPath | null
  highlightedTargets: Array<ElementPath>
  hiddenInNavigator: Array<ElementPath>
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

export interface FloatingInsertMenuStateSwap {
  insertMenuMode: 'swap'
}

export function floatingInsertMenuStateSwap(): FloatingInsertMenuStateSwap {
  return {
    insertMenuMode: 'swap',
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
  | FloatingInsertMenuStateSwap
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

const UnderlyingVSCodeBridgeId = UUID()

export function getUnderlyingVSCodeBridgeID(): string {
  return UnderlyingVSCodeBridgeId
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
  visible: boolean
}

export function editorStateLeftMenu(
  selectedTab: LeftMenuTab,
  visible: boolean,
): EditorStateLeftMenu {
  return {
    selectedTab: selectedTab,
    visible: visible,
  }
}

export interface EditorStateRightMenu {
  selectedTab: RightMenuTab
  visible: boolean
}

export function editorStateRightMenu(
  selectedTab: RightMenuTab,
  visible: boolean,
): EditorStateRightMenu {
  return {
    selectedTab: selectedTab,
    visible: visible,
  }
}

export interface EditorStateInterfaceDesigner {
  codePaneVisible: boolean
  additionalControls: boolean
}

export function editorStateInterfaceDesigner(
  codePaneVisible: boolean,
  additionalControls: boolean,
): EditorStateInterfaceDesigner {
  return {
    codePaneVisible: codePaneVisible,
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
  attributesToUpdate: { [key: string]: JSExpression }
}

export function editorStateCanvasTransientProperty(
  elementPath: ElementPath,
  attributesToUpdate: { [key: string]: JSExpression },
): EditorStateCanvasTransientProperty {
  return {
    elementPath: elementPath,
    attributesToUpdate: attributesToUpdate,
  }
}

export function dragToMoveIndicatorFlags(
  showIndicator: boolean,
  dragType: DragToMoveIndicatorFlags['dragType'],
  reparent: DragToMoveIndicatorFlags['reparent'],
  ancestor: boolean,
): DragToMoveIndicatorFlags {
  return {
    showIndicator,
    dragType,
    reparent,
    ancestor,
  }
}

export const emptyDragToMoveIndicatorFlags = dragToMoveIndicatorFlags(false, 'none', 'none', false)
export interface DragToMoveIndicatorFlags {
  showIndicator: boolean
  dragType: 'absolute' | 'static' | 'none'
  reparent: 'same-component' | 'different-component' | 'none'
  ancestor: boolean
}

export interface EditorStateCanvasControls {
  // this is where we can put props for the strategy controls
  snappingGuidelines: Array<GuidelineWithSnappingVectorAndPointsOfRelevance>
  outlineHighlights: Array<CanvasRectangle>
  strategyIntendedBounds: Array<CanvasFrameAndTarget>
  flexReparentTargetLines: Array<CanvasRectangle>
  parentHighlightPaths: Array<ElementPath> | null
  reparentedToPaths: Array<ElementPath>
  dragToMoveIndicatorFlags: DragToMoveIndicatorFlags
  parentOutlineHighlight: ElementPath | null
}

export function editorStateCanvasControls(
  snappingGuidelines: Array<GuidelineWithSnappingVectorAndPointsOfRelevance>,
  outlineHighlights: Array<CanvasRectangle>,
  strategyIntendedBounds: Array<CanvasFrameAndTarget>,
  flexReparentTargetLines: Array<CanvasRectangle>,
  parentHighlightPaths: Array<ElementPath> | null,
  reparentedToPaths: Array<ElementPath>,
  dragToMoveIndicatorFlagsValue: DragToMoveIndicatorFlags,
  parentOutlineHighlight: ElementPath | null,
): EditorStateCanvasControls {
  return {
    snappingGuidelines: snappingGuidelines,
    outlineHighlights: outlineHighlights,
    strategyIntendedBounds: strategyIntendedBounds,
    flexReparentTargetLines: flexReparentTargetLines,
    parentHighlightPaths: parentHighlightPaths,
    reparentedToPaths: reparentedToPaths,
    dragToMoveIndicatorFlags: dragToMoveIndicatorFlagsValue,
    parentOutlineHighlight: parentOutlineHighlight,
  }
}

export type ElementsToRerender = Array<ElementPath> | 'rerender-all-elements'

export interface InternalClipboard {
  styleClipboard: Array<ValueAtPath>
  elements: Array<CopyData>
}

export function internalClipboard(
  styleClipboard: Array<ValueAtPath>,
  elements: Array<CopyData>,
): InternalClipboard {
  return {
    styleClipboard,
    elements,
  }
}

export interface EditorStateCanvas {
  elementsToRerender: ElementsToRerender
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
}

export function editorStateInspector(
  visible: boolean,
  classnameFocusCounter: number,
): EditorStateInspector {
  return {
    visible: visible,
    classnameFocusCounter: classnameFocusCounter,
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
  componentDescriptorErrors: ErrorMessages
}

export function editorStateCodeEditorErrors(
  buildErrors: ErrorMessages,
  lintErrors: ErrorMessages,
  componentDescriptorErrors: ErrorMessages,
): EditorStateCodeEditorErrors {
  return {
    buildErrors: buildErrors,
    lintErrors: lintErrors,
    componentDescriptorErrors: componentDescriptorErrors,
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

export type GithubRepoWithBranch = GithubRepo & { branch: string | null }

export function githubRepoFullName(repo: GithubRepo | null): string | null {
  if (repo == null) {
    return null
  }
  return `${repo.owner}/${repo.repository}`
}

export function githubRepoOwnerName(repo: GithubRepo | null): string | null {
  if (repo == null) {
    return null
  }
  return repo.owner
}

export function githubRepositoryName(repo: GithubRepo | null): string | null {
  if (repo == null) {
    return null
  }
  return repo.repository
}

export function githubRepo(owner: string, repository: string): GithubRepo {
  return {
    owner: owner,
    repository: repository,
  }
}

export function githubRepoEquals(a: GithubRepo | null, b: GithubRepo | null): boolean {
  return a?.owner === b?.owner && a?.repository === b?.repository
}

export interface PullRequest {
  title: string
  htmlURL: string
  number: number
}

export interface ProjectGithubSettings {
  targetRepository: GithubRepo | null
  originCommit: string | null
  branchName: string | null
  pendingCommit: string | null
  branchLoaded: boolean
}

export function projectGithubSettings(
  targetRepository: GithubRepo | null,
  originCommit: string | null,
  branchName: string | null,
  pendingCommit: string | null,
  branchLoaded: boolean,
): ProjectGithubSettings {
  return {
    targetRepository: targetRepository,
    originCommit: originCommit,
    branchName: branchName,
    pendingCommit: pendingCommit,
    branchLoaded: branchLoaded,
  }
}

export function emptyGithubSettings(): ProjectGithubSettings {
  return {
    targetRepository: null,
    originCommit: null,
    branchName: null,
    pendingCommit: null,
    branchLoaded: false,
  }
}

export interface GithubUser {
  login: string
  avatarURL: string
  htmlURL: string
  name: string | null
}

export interface GithubData {
  branches: Array<GithubBranch> | null
  userRepositories: Array<RepositoryEntry>
  publicRepositories: Array<RepositoryEntry>
  treeConflicts: TreeConflicts
  lastUpdatedAt: number | null
  upstreamChanges: GithubFileChanges | null
  currentBranchPullRequests: Array<PullRequest> | null
  githubUserDetails: GithubUser | null
  lastRefreshedCommit: string | null
}

export function emptyGithubData(): GithubData {
  return {
    branches: null,
    userRepositories: [],
    publicRepositories: [],
    treeConflicts: {},
    lastUpdatedAt: null,
    upstreamChanges: null,
    currentBranchPullRequests: null,
    githubUserDetails: null,
    lastRefreshedCommit: null,
  }
}

export function newGithubData(
  branches: Array<GithubBranch> | null,
  userRepositories: Array<RepositoryEntry>,
  publicRepositories: Array<RepositoryEntry>,
  treeConflicts: TreeConflicts,
  lastUpdatedAt: number | null,
  upstreamChanges: GithubFileChanges | null,
  currentBranchPullRequests: Array<PullRequest> | null,
  githubUserDetails: GithubUser | null,
  lastRefreshedCommit: string | null,
): GithubData {
  return {
    branches: branches,
    userRepositories: userRepositories,
    publicRepositories: publicRepositories,
    treeConflicts: treeConflicts,
    lastUpdatedAt: lastUpdatedAt,
    upstreamChanges: upstreamChanges,
    currentBranchPullRequests: currentBranchPullRequests,
    githubUserDetails: githubUserDetails,
    lastRefreshedCommit: lastRefreshedCommit,
  }
}

export type ColorSwatch = {
  id: string
  hex: string
}

export function newColorSwatch(id: string, hex: string): ColorSwatch {
  return {
    id: id,
    hex: hex,
  }
}

export interface FileWithChecksum {
  file: ProjectFile
  checksum: string
}

export type FileChecksums = { [filename: string]: string } // key = filename, value = sha1 hash of the file

export type FileChecksumsWithFile = { [filename: string]: FileWithChecksum }

export function fileChecksumsWithFileToFileChecksums(
  fileChecksums: FileChecksumsWithFile,
): FileChecksums {
  return objectMap((entry) => entry.checksum, fileChecksums)
}

export interface PastePostActionMenuData {
  type: 'PASTE'
  target: ReparentTargetForPaste
  dataWithPropsReplaced: ElementPasteWithMetadata | null
  dataWithPropsPreserved: ElementPasteWithMetadata
  pasteTargetsToIgnore: Array<ElementPath>
  targetOriginalPathTrees: ElementPathTrees
  originalAllElementProps: AllElementProps
  canvasViewportCenter: CanvasPoint
}

export interface PasteHerePostActionMenuData {
  type: 'PASTE_HERE'
  position: CanvasPoint
  internalClipboard: InternalClipboard
}

export interface PasteToReplacePostActionMenuData {
  type: 'PASTE_TO_REPLACE'
  targets: Array<ElementPath>
  internalClipboard: InternalClipboard
}
export interface NavigatorReparentPostActionMenuData {
  type: 'NAVIGATOR_REPARENT'
  dragSources: Array<ElementPath>
  targetParent: ElementPath
  indexPosition: IndexPosition
  canvasViewportCenter: CanvasPoint
  jsxMetadata: ElementInstanceMetadataMap
  allElementProps: AllElementProps
}

export type PostActionMenuData =
  | PastePostActionMenuData
  | PasteHerePostActionMenuData
  | PasteToReplacePostActionMenuData
  | NavigatorReparentPostActionMenuData

export interface PostActionMenuSession {
  activeChoiceId: string | null
  historySnapshot: StateHistory
  editorStateSnapshot: EditorState
  derivedStateSnapshot: DerivedState
  postActionMenuData: PostActionMenuData
}

export interface TrueUpGroupElementChanged {
  type: 'TRUE_UP_GROUP_ELEMENT_CHANGED'
  target: ElementPath
}

export function trueUpGroupElementChanged(target: ElementPath): TrueUpGroupElementChanged {
  return {
    type: 'TRUE_UP_GROUP_ELEMENT_CHANGED',
    target: target,
  }
}

export interface TrueUpChildrenOfGroupChanged {
  type: 'TRUE_UP_CHILDREN_OF_GROUP_CHANGED'
  targetParent: ElementPath
}

export function trueUpChildrenOfGroupChanged(
  targetParent: ElementPath,
): TrueUpChildrenOfGroupChanged {
  return {
    type: 'TRUE_UP_CHILDREN_OF_GROUP_CHANGED',
    targetParent: targetParent,
  }
}

export interface TrueUpHuggingElement {
  type: 'TRUE_UP_HUGGING_ELEMENT'
  target: ElementPath
  frame: CanvasRectangle
}

export function trueUpHuggingElement(
  target: ElementPath,
  frame: CanvasRectangle,
): TrueUpHuggingElement {
  return {
    type: 'TRUE_UP_HUGGING_ELEMENT',
    target: target,
    frame: frame,
  }
}

export type TrueUpTarget =
  | TrueUpGroupElementChanged
  | TrueUpChildrenOfGroupChanged
  | TrueUpHuggingElement

// FIXME We need to pull out ProjectState from here
export interface EditorState {
  id: string | null
  forkedFromProjectId: string | null
  appID: string | null
  projectName: string
  projectDescription: string
  projectVersion: number
  isLoaded: boolean
  trueUpElementsAfterDomWalkerRuns: Array<TrueUpTarget>
  spyMetadata: ElementInstanceMetadataMap // this is coming from the canvas spy report.
  domMetadata: ElementInstanceMetadataMap // this is coming from the dom walking report.
  jsxMetadata: ElementInstanceMetadataMap // this is a merged result of the two above.
  elementPathTree: ElementPathTrees
  projectContents: ProjectContentTreeRoot
  branchOriginContents: ProjectContentTreeRoot | null // The contents from the branch at the origin commit.
  codeResultCache: CodeResultCache
  propertyControlsInfo: PropertyControlsInfo
  nodeModules: EditorStateNodeModules
  selectedViews: Array<ElementPath>
  highlightedViews: Array<ElementPath>
  hoveredViews: Array<ElementPath>
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
  codeEditorErrors: EditorStateCodeEditorErrors
  thumbnailLastGenerated: number
  pasteTargetsToIgnore: ElementPath[]
  parseOrPrintInFlight: boolean
  previousParseOrPrintSkipped: boolean
  safeMode: boolean
  saveError: boolean
  vscodeBridgeReady: boolean
  vscodeReady: boolean
  focusedElementPath: ElementPath | null
  config: UtopiaVSCodeConfig
  vscodeLoadingScreenVisible: boolean
  indexedDBFailed: boolean
  forceParseFiles: Array<string>
  allElementProps: AllElementProps // the final, resolved, static props value for each element. // This is the counterpart of jsxMetadata. we only update allElementProps when we update jsxMetadata
  currentAllElementProps: AllElementProps // This is the counterpart of domMetadata and spyMetadata. we update currentAllElementProps every time we update domMetadata/spyMetadata
  variablesInScope: VariablesInScope // updated every time we update jsxMetadata, along the same lines as `allElementProps` and `currentAllElementProps`
  currentVariablesInScope: VariablesInScope // updated every time we update domMetadata/spyMetadata
  githubSettings: ProjectGithubSettings
  imageDragSessionState: ImageDragSessionState
  githubOperations: Array<GithubOperation>
  githubData: GithubData
  refreshingDependencies: boolean
  colorSwatches: Array<ColorSwatch>
  internalClipboard: InternalClipboard
  filesModifiedByAnotherUser: Array<string>
  activeFrames: ActiveFrame[]
  commentFilterMode: CommentFilterMode
  forking: boolean
  collaborators: Collaborator[]
  sharingDialogOpen: boolean
}

export function editorState(
  id: string | null,
  forkedFromProjectId: string | null,
  appID: string | null,
  projectName: string,
  projectDescription: string,
  projectVersion: number,
  isLoaded: boolean,
  trueUpElementsAfterDomWalkerRuns: Array<TrueUpTarget>,
  spyMetadata: ElementInstanceMetadataMap,
  domMetadata: ElementInstanceMetadataMap,
  jsxMetadata: ElementInstanceMetadataMap,
  elementPathTree: ElementPathTrees,
  projectContents: ProjectContentTreeRoot,
  codeResultCache: CodeResultCache,
  propertyControlsInfo: PropertyControlsInfo,
  nodeModules: EditorStateNodeModules,
  selectedViews: Array<ElementPath>,
  highlightedViews: Array<ElementPath>,
  hoveredViews: Array<ElementPath>,
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
  codeEditorErrors: EditorStateCodeEditorErrors,
  thumbnailLastGenerated: number,
  pasteTargetsToIgnore: ElementPath[],
  parseOrPrintInFlight: boolean,
  previousParseOrPrintSkipped: boolean,
  safeMode: boolean,
  saveError: boolean,
  vscodeBridgeReady: boolean,
  vscodeReady: boolean,
  focusedElementPath: ElementPath | null,
  config: UtopiaVSCodeConfig,
  vscodeLoadingScreenVisible: boolean,
  indexedDBFailed: boolean,
  forceParseFiles: Array<string>,
  allElementProps: AllElementProps,
  currentAllElementProps: AllElementProps,
  variablesInScope: VariablesInScope,
  currentVariablesInScope: VariablesInScope,
  githubSettings: ProjectGithubSettings,
  imageDragSessionState: ImageDragSessionState,
  githubOperations: Array<GithubOperation>,
  branchOriginContents: ProjectContentTreeRoot | null,
  githubData: GithubData,
  refreshingDependencies: boolean,
  colorSwatches: Array<ColorSwatch>,
  internalClipboardData: InternalClipboard,
  filesModifiedByAnotherUser: Array<string>,
  activeFrames: ActiveFrame[],
  commentFilterMode: CommentFilterMode,
  forking: boolean,
  collaborators: Collaborator[],
  sharingDialogOpen: boolean,
): EditorState {
  return {
    id: id,
    forkedFromProjectId: forkedFromProjectId,
    appID: appID,
    projectName: projectName,
    projectDescription: projectDescription,
    projectVersion: projectVersion,
    isLoaded: isLoaded,
    trueUpElementsAfterDomWalkerRuns: trueUpElementsAfterDomWalkerRuns,
    spyMetadata: spyMetadata,
    domMetadata: domMetadata,
    jsxMetadata: jsxMetadata,
    elementPathTree: elementPathTree,
    projectContents: projectContents,
    branchOriginContents: branchOriginContents,
    codeResultCache: codeResultCache,
    propertyControlsInfo: propertyControlsInfo,
    nodeModules: nodeModules,
    selectedViews: selectedViews,
    highlightedViews: highlightedViews,
    hoveredViews: hoveredViews,
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
    codeEditorErrors: codeEditorErrors,
    thumbnailLastGenerated: thumbnailLastGenerated,
    pasteTargetsToIgnore: pasteTargetsToIgnore,
    parseOrPrintInFlight: parseOrPrintInFlight,
    previousParseOrPrintSkipped: previousParseOrPrintSkipped,
    safeMode: safeMode,
    saveError: saveError,
    vscodeBridgeReady: vscodeBridgeReady,
    vscodeReady: vscodeReady,
    focusedElementPath: focusedElementPath,
    config: config,
    vscodeLoadingScreenVisible: vscodeLoadingScreenVisible,
    indexedDBFailed: indexedDBFailed,
    forceParseFiles: forceParseFiles,
    allElementProps: allElementProps,
    currentAllElementProps: currentAllElementProps,
    variablesInScope: variablesInScope,
    currentVariablesInScope: currentVariablesInScope,
    githubSettings: githubSettings,
    imageDragSessionState: imageDragSessionState,
    githubOperations: githubOperations,
    githubData: githubData,
    refreshingDependencies: refreshingDependencies,
    colorSwatches: colorSwatches,
    internalClipboard: internalClipboardData,
    filesModifiedByAnotherUser: filesModifiedByAnotherUser,
    activeFrames: activeFrames,
    commentFilterMode: commentFilterMode,
    forking: forking,
    collaborators: collaborators,
    sharingDialogOpen: sharingDialogOpen,
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
      mode: storedEditorState.mode ?? EditorModes.selectMode(null, false, 'none'),
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
    return getProjectFileByFilePath(model.projectContents, openFile)
  }
}

export function getFileForName(filePath: string, model: EditorState): ProjectFile | null {
  return getProjectFileByFilePath(model.projectContents, filePath)
}

export function getOpenTextFileKey(model: EditorState): string | null {
  const openFilename = getOpenFilename(model)
  if (openFilename == null) {
    return null
  } else {
    const projectFile = getProjectFileByFilePath(model.projectContents, openFilename)
    if (projectFile != null && isTextFile(projectFile)) {
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
    const projectFile = getProjectFileByFilePath(model.projectContents, openFilename)
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
    const projectFile = getProjectFileByFilePath(model.projectContents, openFilename)
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
    {},
  )
}

export interface ParseSuccessAndEditorChanges<T> {
  parseSuccessTransform: (success: ParseSuccess) => ParseSuccess
  editorStateTransform: (editor: EditorState) => EditorState
  additionalData: T
}

export function modifyOpenJsxElementAtPath(
  path: ElementPath,
  transform: (element: JSXElement) => JSXElement,
  model: EditorState,
): EditorState {
  return modifyOpenJsxElementOrConditionalAtPath(
    path,
    (element) => (isJSXElement(element) ? transform(element) : element),
    model,
  )
}

export function modifyOpenJsxElementOrConditionalAtPath(
  path: ElementPath,
  transform: (
    element: JSXElement | JSXConditionalExpression | JSXFragment,
  ) => JSXElement | JSXConditionalExpression | JSXFragment,
  model: EditorState,
): EditorState {
  return modifyUnderlyingTargetElement(
    path,
    model,
    (element) =>
      isJSXElement(element) || isJSXConditionalExpression(element) || isJSXFragment(element)
        ? transform(element)
        : element,
    defaultModifyParseSuccess,
  )
}

export function modifyOpenJsxChildAtPath(
  path: ElementPath,
  transform: (element: JSXElementChild) => JSXElementChild,
  model: EditorState,
): EditorState {
  return modifyUnderlyingTarget(
    path,
    model,
    (element) => transform(element),
    defaultModifyParseSuccess,
  )
}

function getImportedUtopiaJSXComponents(
  filePath: string,
  projectContents: ProjectContentTreeRoot,
  resolve: ResolveFn,
  pathsToFilter: string[],
): Array<UtopiaJSXComponent> {
  const file = getProjectFileByFilePath(projectContents, filePath)
  if (file != null && isTextFile(file) && isParseSuccess(file.fileContents.parsed)) {
    let resolvedFilePaths: Array<string> = []
    for (const toImport of Object.keys(file.fileContents.parsed.imports)) {
      const resolveResult = resolve(filePath, toImport)
      forEachRight(resolveResult, (path) => {
        if (!pathsToFilter.includes(path)) {
          resolvedFilePaths.push(path)
        }
      })
    }

    let result: Array<UtopiaJSXComponent> = []
    result.push(...getUtopiaJSXComponentsFromSuccess(file.fileContents.parsed))
    const newPathsToFilter = [...pathsToFilter, ...resolvedFilePaths]
    for (const resolvedFilePath of resolvedFilePaths) {
      const resolvedPathResult = getImportedUtopiaJSXComponents(
        resolvedFilePath,
        projectContents,
        resolve,
        newPathsToFilter,
      )
      result.push(...resolvedPathResult)
    }
    return result
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
  return getJSXComponentsAndImportsForPath(path, storyboardFilePath, model.projectContents)
}

function getJSXComponentsAndImportsForPath(
  path: ElementPath,
  currentFilePath: string,
  projectContents: ProjectContentTreeRoot,
): {
  underlyingFilePath: string
  components: UtopiaJSXComponent[]
  imports: Imports
} {
  const underlying = normalisePathToUnderlyingTarget(projectContents, path)
  const elementFilePath =
    underlying.type === 'NORMALISE_PATH_SUCCESS' ? underlying.filePath : currentFilePath
  const result = getParseSuccessForFilePath(elementFilePath, projectContents)
  return {
    underlyingFilePath: elementFilePath,
    components: result.topLevelElements.filter(isUtopiaJSXComponent),
    imports: result.imports,
  }
}

type RemoveElementResult = {
  components: Array<UtopiaJSXComponent>
  imports: Imports
}
export function removeElementAtPath(
  target: ElementPath,
  components: Array<UtopiaJSXComponent>,
  originalImports?: Imports,
): RemoveElementResult {
  const staticTarget = EP.dynamicPathToStaticPath(target)
  let resultImports = originalImports ?? emptyImports()
  if (staticTarget == null) {
    return { components, imports: resultImports }
  } else {
    const removedElement = findJSXElementAtStaticPath(components, staticTarget)
    const remainingComponents = removeJSXElement(staticTarget, components)
    if (removedElement != null) {
      resultImports =
        originalImports != null
          ? removeUnusedImportsForRemovedElement(
              removedElement,
              remainingComponents,
              originalImports,
            )
          : emptyImports()
    }
    return {
      components: remainingComponents,
      imports: resultImports,
    }
  }
}

export function transformElementAtPath(
  components: Array<UtopiaJSXComponent>,
  target: ElementPath,
  transform: (elem: JSXElementChild) => JSXElementChild,
): Array<UtopiaJSXComponent> {
  const staticTarget = EP.dynamicPathToStaticPath(target)
  if (staticTarget == null) {
    return components
  } else {
    return transformJSXComponentAtPath(components, staticTarget, transform)
  }
}

export function transformJSXElementAtPath(
  components: Array<UtopiaJSXComponent>,
  target: ElementPath,
  transform: (elem: JSXElement) => JSXElement,
): Array<UtopiaJSXComponent> {
  const staticTarget = EP.dynamicPathToStaticPath(target)
  if (staticTarget == null) {
    return components
  } else {
    return transformJSXComponentAtPath(components, staticTarget, (elem) => {
      if (isJSXElement(elem)) {
        return transform(elem)
      } else {
        return elem
      }
    })
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
  hoveredViews: Array<ElementPath>
  filesState: TransientFilesState | null
  toastsToApply: ReadonlyArray<Notice>
}

export function transientCanvasState(
  selectedViews: Array<ElementPath>,
  highlightedViews: Array<ElementPath>,
  hoveredViews: Array<ElementPath>,
  fileState: TransientFilesState | null,
  toastsToApply: ReadonlyArray<Notice>,
): TransientCanvasState {
  return {
    selectedViews: selectedViews,
    highlightedViews: highlightedViews,
    hoveredViews: hoveredViews,
    filesState: fileState,
    toastsToApply: toastsToApply,
  }
}

export function getMetadata(editor: {
  jsxMetadata: ElementInstanceMetadataMap
}): ElementInstanceMetadataMap {
  return editor.jsxMetadata
}

export interface ElementWarnings {
  widthOrHeightZero: boolean
  absoluteWithUnpositionedParent: boolean
  dynamicSceneChildWidthHeightPercentage: boolean
  invalidGroup: InvalidGroupState | null
  invalidGroupChild: InvalidGroupState | null
}

export function elementWarnings(
  widthOrHeightZero: boolean,
  absoluteWithUnpositionedParent: boolean,
  dynamicSceneChildWidthHeightPercentage: boolean,
  invalidGroup: InvalidGroupState | null,
  invalidGroupChild: InvalidGroupState | null,
): ElementWarnings {
  return {
    widthOrHeightZero: widthOrHeightZero,
    absoluteWithUnpositionedParent: absoluteWithUnpositionedParent,
    dynamicSceneChildWidthHeightPercentage: dynamicSceneChildWidthHeightPercentage,
    invalidGroup: invalidGroup,
    invalidGroupChild: invalidGroupChild,
  }
}

export const defaultElementWarnings: ElementWarnings = {
  widthOrHeightZero: false,
  absoluteWithUnpositionedParent: false,
  dynamicSceneChildWidthHeightPercentage: false,
  invalidGroup: null,
  invalidGroupChild: null,
}

export interface RegularNavigatorEntry {
  type: 'REGULAR'
  elementPath: ElementPath
}

export function regularNavigatorEntry(elementPath: ElementPath): RegularNavigatorEntry {
  return {
    type: 'REGULAR',
    elementPath: elementPath,
  }
}

export function regularNavigatorEntriesEqual(
  first: RegularNavigatorEntry,
  second: RegularNavigatorEntry,
): boolean {
  return EP.pathsEqual(first.elementPath, second.elementPath)
}
export interface ConditionalClauseNavigatorEntry {
  type: 'CONDITIONAL_CLAUSE'
  elementPath: ElementPath
  clause: ConditionalCase
}

export function conditionalClauseNavigatorEntry(
  elementPath: ElementPath,
  clause: ConditionalCase,
): ConditionalClauseNavigatorEntry {
  return {
    type: 'CONDITIONAL_CLAUSE',
    elementPath: elementPath,
    clause: clause,
  }
}

export function conditionalClauseNavigatorEntriesEqual(
  first: ConditionalClauseNavigatorEntry,
  second: ConditionalClauseNavigatorEntry,
): boolean {
  return EP.pathsEqual(first.elementPath, second.elementPath) && first.clause === second.clause
}

export interface SyntheticNavigatorEntry {
  type: 'SYNTHETIC'
  elementPath: ElementPath
  childOrAttribute: JSXElementChild
}

export function syntheticNavigatorEntry(
  elementPath: ElementPath,
  childOrAttribute: JSXElementChild,
): SyntheticNavigatorEntry {
  return {
    type: 'SYNTHETIC',
    elementPath: elementPath,
    childOrAttribute: childOrAttribute,
  }
}

interface RenderedAtPropertyPath {
  type: 'element-property-path'
  elementPropertyPath: ElementPropertyPath
}

interface RenderedAtChildNode {
  type: 'child-node'
  parentPath: ElementPath
  nodeUid: string
}

export type RenderedAt = RenderedAtPropertyPath | RenderedAtChildNode

export function renderedAtPropertyPath(
  elementPropertyPath: ElementPropertyPath,
): RenderedAtPropertyPath {
  return { type: 'element-property-path', elementPropertyPath: elementPropertyPath }
}

export function renderedAtChildNode(parentPath: ElementPath, nodeUid: string): RenderedAtChildNode {
  return { type: 'child-node', parentPath: parentPath, nodeUid: nodeUid }
}

export interface DataReferenceNavigatorEntry {
  type: 'DATA_REFERENCE'
  elementPath: ElementPath
  renderedAt: RenderedAt
  surroundingScope: ElementPath
  childOrAttribute: JSXElementChild
}

export function dataReferenceNavigatorEntry(
  elementPath: ElementPath,
  renderedAt: RenderedAt,
  surroundingScope: ElementPath,
  childOrAttribute: JSXElementChild,
): DataReferenceNavigatorEntry {
  return {
    type: 'DATA_REFERENCE',
    elementPath: elementPath,
    childOrAttribute: childOrAttribute,
    renderedAt: renderedAt,
    surroundingScope: surroundingScope,
  }
}

export function isDataReferenceNavigatorEntry(
  entry: NavigatorEntry,
): entry is DataReferenceNavigatorEntry {
  return entry.type === 'DATA_REFERENCE'
}

export interface SlotNavigatorEntry {
  type: 'SLOT'
  elementPath: ElementPath
  prop: string
}

export function slotNavigatorEntry(elementPath: ElementPath, prop: string): SlotNavigatorEntry {
  return {
    type: 'SLOT',
    elementPath: elementPath,
    prop: prop,
  }
}

export function syntheticNavigatorEntriesEqual(
  first: SyntheticNavigatorEntry,
  second: SyntheticNavigatorEntry,
): boolean {
  return SyntheticNavigatorEntryKeepDeepEquality(first, second).areEqual
}

export interface RenderPropNavigatorEntry {
  type: 'RENDER_PROP'
  elementPath: ElementPath // path of the element containing this render prop
  propName: string
  childPath: ElementPath | null
}

export function renderPropNavigatorEntry(
  elementPath: ElementPath,
  propName: string,
  childPath: ElementPath | null,
): RenderPropNavigatorEntry {
  return {
    type: 'RENDER_PROP',
    elementPath: elementPath,
    propName: propName,
    childPath: childPath,
  }
}

export function renderPropNavigatorEntriesEqual(
  first: RenderPropNavigatorEntry,
  second: RenderPropNavigatorEntry,
): boolean {
  return RenderPropNavigatorEntryKeepDeepEquality(first, second).areEqual
}

export interface RenderPropValueNavigatorEntry {
  type: 'RENDER_PROP_VALUE'
  elementPath: ElementPath // path of the actual element being used inside a render prop
  prop: string
}

export function renderPropValueNavigatorEntry(
  elementPath: ElementPath,
  prop: string,
): RenderPropValueNavigatorEntry {
  return {
    type: 'RENDER_PROP_VALUE',
    elementPath: elementPath,
    prop: prop,
  }
}

export function isRenderPropValueNavigatorEntry(
  v: NavigatorEntry,
): v is RenderPropValueNavigatorEntry {
  return v.type === 'RENDER_PROP_VALUE'
}

export function renderPropValueNavigatorEntriesEqual(
  first: RenderPropValueNavigatorEntry,
  second: RenderPropValueNavigatorEntry,
): boolean {
  return RenderPropValueNavigatorEntryKeepDeepEquality(first, second).areEqual
}

export interface InvalidOverrideNavigatorEntry {
  type: 'INVALID_OVERRIDE'
  elementPath: ElementPath
  message: string
}

export function invalidOverrideNavigatorEntry(
  elementPath: ElementPath,
  message: string,
): InvalidOverrideNavigatorEntry {
  return {
    type: 'INVALID_OVERRIDE',
    elementPath: elementPath,
    message: message,
  }
}

export function isInvalidOverrideNavigatorEntry(
  entry: NavigatorEntry,
): entry is InvalidOverrideNavigatorEntry {
  return entry.type === 'INVALID_OVERRIDE'
}

export function invalidOverrideNavigatorEntriesEqual(
  first: InvalidOverrideNavigatorEntry,
  second: InvalidOverrideNavigatorEntry,
): boolean {
  return InvalidOverrideNavigatorEntryKeepDeepEquality(first, second).areEqual
}

export type NavigatorEntry =
  | RegularNavigatorEntry
  | ConditionalClauseNavigatorEntry
  | SyntheticNavigatorEntry
  | DataReferenceNavigatorEntry
  | InvalidOverrideNavigatorEntry
  | RenderPropNavigatorEntry
  | RenderPropValueNavigatorEntry
  | SlotNavigatorEntry

export function navigatorEntriesEqual(
  first: NavigatorEntry | null,
  second: NavigatorEntry | null,
): boolean {
  if (first == null) {
    return second == null
  } else if (second == null) {
    return false
  } else if (first.type === 'REGULAR' && second.type === 'REGULAR') {
    return regularNavigatorEntriesEqual(first, second)
  } else if (first.type === 'CONDITIONAL_CLAUSE' && second.type === 'CONDITIONAL_CLAUSE') {
    return conditionalClauseNavigatorEntriesEqual(first, second)
  } else if (first.type === 'SYNTHETIC' && second.type === 'SYNTHETIC') {
    return syntheticNavigatorEntriesEqual(first, second)
  } else if (first.type === 'RENDER_PROP' && second.type === 'RENDER_PROP') {
    return renderPropNavigatorEntriesEqual(first, second)
  } else {
    return false
  }
}

export function navigatorRowToKey(row: NavigatorRow): string {
  switch (row.type) {
    case 'regular-row':
      return navigatorEntryToKey(row.entry)
    case 'condensed-row':
      return `condensed-${row.entries.map(navigatorEntryToKey).join('-')}`
    default:
      assertNever(row)
  }
}

export function navigatorEntryToKey(entry: NavigatorEntry): string {
  switch (entry.type) {
    case 'REGULAR':
      return `regular-${EP.toComponentId(entry.elementPath)}`
    case 'CONDITIONAL_CLAUSE':
      return `conditional-clause-${EP.toComponentId(entry.elementPath)}-${entry.clause}`
    case 'SYNTHETIC': {
      const childOrAttributeDetails = isJSExpression(entry.childOrAttribute)
        ? `attribute`
        : `element-${getUtopiaID(entry.childOrAttribute)}`
      return `synthetic-${EP.toComponentId(entry.elementPath)}-${childOrAttributeDetails}`
    }
    case 'DATA_REFERENCE': {
      const childOrAttributeDetails = isJSExpression(entry.childOrAttribute)
        ? `attribute`
        : `element-${getUtopiaID(entry.childOrAttribute)}`
      return `data-reference-${EP.toComponentId(entry.elementPath)}-${childOrAttributeDetails}`
    }
    case 'RENDER_PROP': {
      return `render-prop-${EP.toComponentId(entry.elementPath)}-${entry.propName}`
    }
    case 'RENDER_PROP_VALUE': {
      return `render-prop-value-${EP.toComponentId(entry.elementPath)}-${entry.prop}`
    }
    case 'INVALID_OVERRIDE':
      return `error-${EP.toComponentId(entry.elementPath)}`
    case 'SLOT':
      return `slot_${EP.toComponentId(entry.elementPath)}`
    default:
      assertNever(entry)
  }
}

export function varSafeNavigatorEntryToKey(entry: NavigatorEntry): string {
  switch (entry.type) {
    case 'REGULAR':
      return `regular_${EP.toVarSafeComponentId(entry.elementPath)}`
    case 'CONDITIONAL_CLAUSE':
      return `conditional_clause_${EP.toVarSafeComponentId(entry.elementPath)}_${entry.clause}`
    case 'SYNTHETIC': {
      const childOrAttributeDetails = isJSExpression(entry.childOrAttribute)
        ? `attribute`
        : `element_${getUtopiaID(entry.childOrAttribute)}`
      return `synthetic_${EP.toVarSafeComponentId(entry.elementPath)}_${childOrAttributeDetails}`
    }
    case 'DATA_REFERENCE': {
      const childOrAttributeDetails = isJSExpression(entry.childOrAttribute)
        ? `attribute`
        : `element_${getUtopiaID(entry.childOrAttribute)}`
      return `data_reference_${EP.toVarSafeComponentId(
        entry.elementPath,
      )}_${childOrAttributeDetails}`
    }
    case 'RENDER_PROP':
      return `renderprop_${EP.toVarSafeComponentId(entry.elementPath)}_${entry.propName}`
    case 'RENDER_PROP_VALUE':
      return `renderpropvalue_${EP.toVarSafeComponentId(entry.elementPath)}_${entry.prop}`
    case 'INVALID_OVERRIDE':
      return `error_${EP.toVarSafeComponentId(entry.elementPath)}`
    case 'SLOT':
      return `slot_${EP.toVarSafeComponentId(entry.elementPath)}`
    default:
      assertNever(entry)
  }
}

export function isRegularNavigatorEntry(entry: NavigatorEntry): entry is RegularNavigatorEntry {
  return entry.type === 'REGULAR'
}

export const regularNavigatorEntryOptic: Optic<NavigatorEntry, RegularNavigatorEntry> =
  fromTypeGuard(isRegularNavigatorEntry)

export function isConditionalClauseNavigatorEntry(
  entry: NavigatorEntry,
): entry is ConditionalClauseNavigatorEntry {
  return entry.type === 'CONDITIONAL_CLAUSE'
}

export const conditionalClauseNavigatorEntryOptic: Optic<
  NavigatorEntry,
  ConditionalClauseNavigatorEntry
> = fromTypeGuard(isConditionalClauseNavigatorEntry)

export function isSyntheticNavigatorEntry(entry: NavigatorEntry): entry is SyntheticNavigatorEntry {
  return entry.type === 'SYNTHETIC'
}

export const syntheticNavigatorEntryOptic: Optic<NavigatorEntry, SyntheticNavigatorEntry> =
  fromTypeGuard(isSyntheticNavigatorEntry)

export function isRenderPropNavigatorEntry(
  entry: NavigatorEntry,
): entry is RenderPropNavigatorEntry {
  return entry.type === 'RENDER_PROP'
}

export function isSlotNavigatorEntry(entry: NavigatorEntry): entry is SlotNavigatorEntry {
  return entry.type === 'SLOT'
}

export interface DerivedState {
  navigatorRows: Array<NavigatorRow>
  navigatorTargets: Array<NavigatorEntry>
  visibleNavigatorTargets: Array<NavigatorEntry>
  autoFocusedPaths: Array<ElementPath>
  controls: Array<HigherOrderControl>
  elementWarnings: { [key: string]: ElementWarnings }
  projectContentsChecksums: FileChecksumsWithFile
  branchOriginContentsChecksums: FileChecksumsWithFile | null
  remixData: RemixDerivedData | null
  filePathMappings: FilePathMappings
}

function emptyDerivedState(editor: EditorState): DerivedState {
  return {
    navigatorRows: [],
    navigatorTargets: [],
    visibleNavigatorTargets: [],
    autoFocusedPaths: [],
    controls: [],
    elementWarnings: {},
    projectContentsChecksums: {},
    branchOriginContentsChecksums: {},
    remixData: null,
    filePathMappings: [],
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
    componentDescriptorErrors: ErrorMessages
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
  colorSwatches: Array<ColorSwatch>
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
    colorSwatches: second.colorSwatches,
  }
}

function randomArrayElement(array: string[]): string {
  return array[Math.floor(Math.random() * array.length)]
}

export function createNewProjectName(): string {
  const friendlyWordsPredicate = randomArrayElement(friendlyWords.predicates)
  const friendlyWordsObject = randomArrayElement(friendlyWords.objects)
  return `${friendlyWordsPredicate}-${friendlyWordsObject}`
}

export function createNewPageName(): string {
  return `page-${randomArrayElement(friendlyWords.predicates)}-${randomArrayElement(
    friendlyWords.objects,
  )}`
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
    forkedFromProjectId: null,
    appID: null,
    projectName: createNewProjectName(),
    projectDescription: 'Made with Utopia',
    projectVersion: CURRENT_PROJECT_VERSION,
    isLoaded: false,
    trueUpElementsAfterDomWalkerRuns: [],
    spyMetadata: emptyJsxMetadata,
    domMetadata: emptyJsxMetadata,
    jsxMetadata: emptyJsxMetadata,
    elementPathTree: {},
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
    hoveredViews: [],
    hiddenInstances: [],
    displayNoneInstances: [],
    warnedInstances: [],
    lockedElements: {
      simpleLock: [],
      hierarchyLock: [],
    },
    mode: EditorModes.selectMode(null, false, 'none'),
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
      selectedTab: LeftMenuTab.Navigator,
      visible: true,
    },
    rightMenu: {
      selectedTab: RightMenuTab.Inspector,
      visible: true,
    },
    interfaceDesigner: {
      codePaneVisible: false,
      additionalControls: true,
    },
    canvas: {
      elementsToRerender: 'rerender-all-elements',
      interactionSession: null,
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
        dragToMoveIndicatorFlags: emptyDragToMoveIndicatorFlags,
        parentOutlineHighlight: null,
      },
    },
    floatingInsertMenu: {
      insertMenuMode: 'closed',
    },
    inspector: {
      visible: true,
      classnameFocusCounter: 0,
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
      dropTargetHint: null,
      collapsedViews: [],
      renamingTarget: null,
      highlightedTargets: [],
      hiddenInNavigator: [],
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
    codeEditorErrors: {
      buildErrors: {},
      lintErrors: {},
      componentDescriptorErrors: {},
    },
    thumbnailLastGenerated: 0,
    pasteTargetsToIgnore: [],
    parseOrPrintInFlight: false,
    previousParseOrPrintSkipped: false,
    safeMode: false,
    saveError: false,
    vscodeBridgeReady: false,
    vscodeReady: false,
    focusedElementPath: null,
    config: defaultConfig(),
    vscodeLoadingScreenVisible: true,
    indexedDBFailed: false,
    forceParseFiles: [],
    allElementProps: {},
    currentAllElementProps: {},
    variablesInScope: {},
    currentVariablesInScope: {},
    githubSettings: emptyGithubSettings(),
    imageDragSessionState: notDragging(),
    githubOperations: [],
    branchOriginContents: null,
    githubData: emptyGithubData(),
    refreshingDependencies: false,
    colorSwatches: [],
    internalClipboard: {
      styleClipboard: [],
      elements: [],
    },
    filesModifiedByAnotherUser: [],
    activeFrames: [],
    commentFilterMode: 'all',
    forking: false,
    collaborators: [],
    sharingDialogOpen: false,
  }
}

export type OriginalFrame = FrameAndTarget<LocalRectangle>

export interface OriginalCanvasAndLocalFrame {
  target: ElementPath
  frame?: LocalRectangle
  canvasFrame?: CanvasRectangle
}

function getElementWarningsInner(
  projectContents: ProjectContentTreeRoot,
  rootMetadata: ElementInstanceMetadataMap,
  allElementProps: AllElementProps,
  pathTrees: ElementPathTrees,
): { [key: string]: ElementWarnings } {
  let result: { [key: string]: ElementWarnings } = {}
  Object.values(rootMetadata).forEach((elementMetadata) => {
    // Check to see if this element is collapsed in one dimension.
    const globalFrame = elementMetadata.globalFrame
    const widthOrHeightZero =
      globalFrame != null &&
      isFiniteRectangle(globalFrame) &&
      (globalFrame.width === 0 || globalFrame.height === 0)

    const parentPath = EP.parentPath(elementMetadata.elementPath)

    // Identify if this element looks to be trying to position itself with "pins", but
    // the parent element isn't appropriately configured.
    const isParentFragmentLike = treatElementAsFragmentLike(
      rootMetadata,
      allElementProps,
      pathTrees,
      parentPath,
    )

    const isParentNotConfiguredForPins =
      MetadataUtils.isPositionAbsolute(elementMetadata) &&
      !elementMetadata.specialSizeMeasurements.immediateParentProvidesLayout
    const absoluteWithUnpositionedParent = isParentNotConfiguredForPins && !isParentFragmentLike

    const parentElement = MetadataUtils.findElementByElementPath(rootMetadata, parentPath)

    const groupState = treatElementAsGroupLikeFromMetadata(elementMetadata)
      ? getGroupState(
          elementMetadata.elementPath,
          rootMetadata,
          pathTrees,
          allElementProps,
          projectContents,
        )
      : null
    const invalidGroup = isInvalidGroupState(groupState) ? groupState : null

    const groupChildState =
      parentElement != null && treatElementAsGroupLikeFromMetadata(parentElement)
        ? getGroupChildStateWithGroupMetadata(projectContents, elementMetadata, parentElement)
        : null
    const invalidGroupChild = isInvalidGroupState(groupChildState) ? groupChildState : null

    const warnings: ElementWarnings = {
      widthOrHeightZero: widthOrHeightZero,
      absoluteWithUnpositionedParent: absoluteWithUnpositionedParent,
      dynamicSceneChildWidthHeightPercentage: false,
      invalidGroup: invalidGroup,
      invalidGroupChild: invalidGroupChild,
    }
    result[EP.toString(elementMetadata.elementPath)] = warnings
  })
  return result
}

const getElementWarnings = memoize(getElementWarningsInner, { maxSize: 1 })

type CacheableDerivedState = {
  navigatorRows: Array<NavigatorRow>
  navigatorTargets: Array<NavigatorEntry>
  visibleNavigatorTargets: Array<NavigatorEntry>
  elementWarnings: { [key: string]: ElementWarnings }
  autoFocusedPaths: Array<ElementPath>
}

function deriveCacheableStateInner(
  projectContents: ProjectContentTreeRoot,
  jsxMetadata: ElementInstanceMetadataMap,
  elementPathTree: ElementPathTrees,
  allElementProps: AllElementProps,
  collapsedViews: ElementPath[],
  hiddenInNavigator: ElementPath[],
  propertyControlsInfo: PropertyControlsInfo,
): CacheableDerivedState {
  const { navigatorRows, navigatorTargets, visibleNavigatorTargets } = getNavigatorTargets(
    jsxMetadata,
    elementPathTree,
    collapsedViews,
    hiddenInNavigator,
    propertyControlsInfo,
    projectContents,
  )

  const warnings = getElementWarnings(
    projectContents,
    jsxMetadata,
    allElementProps,
    elementPathTree,
  )

  const autoFocusedPaths = MetadataUtils.getAllPaths(jsxMetadata, elementPathTree).filter(
    (path) => {
      return MetadataUtils.isAutofocusable(
        jsxMetadata,
        elementPathTree,
        path,
        propertyControlsInfo,
        projectContents,
      )
    },
  )

  return {
    navigatorRows: navigatorRows,
    navigatorTargets: navigatorTargets,
    visibleNavigatorTargets: visibleNavigatorTargets,
    elementWarnings: warnings,
    autoFocusedPaths: autoFocusedPaths,
  }
}

const patchedDeriveCacheableState = memoize(deriveCacheableStateInner, { maxSize: 1 })
const unpatchedDeriveCacheableState = memoize(deriveCacheableStateInner, { maxSize: 1 })

export function deriveState(
  editor: EditorState,
  oldDerivedState: DerivedState | null,
  cacheKey: 'patched' | 'unpatched',
  createRemixDerivedDataMemo: RemixDerivedDataFactory,
): DerivedState {
  const derivedState = oldDerivedState == null ? emptyDerivedState(editor) : oldDerivedState

  const deriveCacheableState =
    cacheKey === 'patched' ? patchedDeriveCacheableState : unpatchedDeriveCacheableState

  const {
    navigatorRows,
    navigatorTargets,
    visibleNavigatorTargets,
    elementWarnings: warnings,
    autoFocusedPaths,
  } = deriveCacheableState(
    editor.projectContents,
    editor.jsxMetadata,
    editor.elementPathTree,
    editor.allElementProps,
    editor.navigator.collapsedViews,
    editor.navigator.hiddenInNavigator,
    editor.propertyControlsInfo,
  )

  const remixDerivedData = createRemixDerivedDataMemo(
    editor.projectContents,
    editor.codeResultCache.curriedRequireFn,
    editor.codeResultCache.curriedResolveFn,
  )

  const filePathMappings = getFilePathMappings(editor.projectContents)

  const derived: DerivedState = {
    navigatorRows: navigatorRows,
    navigatorTargets: navigatorTargets,
    visibleNavigatorTargets: visibleNavigatorTargets,
    autoFocusedPaths: autoFocusedPaths,
    controls: derivedState.controls,
    elementWarnings: warnings,
    projectContentsChecksums: getProjectContentsChecksums(
      editor.projectContents,
      oldDerivedState?.projectContentsChecksums ?? {},
    ),
    branchOriginContentsChecksums:
      editor.branchOriginContents == null
        ? null
        : getProjectContentsChecksums(
            editor.branchOriginContents,
            oldDerivedState?.branchOriginContentsChecksums ?? {},
          ),
    remixData: remixDerivedData,
    filePathMappings: filePathMappings,
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
    forkedFromProjectId: persistentModel.forkedFromProjectId,
    appID: persistentModel.appID ?? null,
    projectName: createNewProjectName(),
    projectDescription: persistentModel.projectDescription,
    projectVersion: persistentModel.projectVersion,
    isLoaded: false,
    trueUpElementsAfterDomWalkerRuns: [],
    spyMetadata: emptyJsxMetadata,
    domMetadata: emptyJsxMetadata,
    jsxMetadata: emptyJsxMetadata,
    elementPathTree: {},
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
    hoveredViews: [],
    hiddenInstances: persistentModel.hiddenInstances,
    displayNoneInstances: [],
    warnedInstances: [],
    lockedElements: {
      simpleLock: [],
      hierarchyLock: [],
    },
    mode: EditorModes.selectMode(null, false, 'none'),
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
      selectedTab: LeftMenuTab.Navigator,
      visible: true,
    },
    rightMenu: {
      selectedTab: RightMenuTab.Inspector,
      visible: true,
    },
    interfaceDesigner: {
      codePaneVisible: false,
      additionalControls: true,
    },
    canvas: {
      elementsToRerender: 'rerender-all-elements',
      interactionSession: null,
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
        dragToMoveIndicatorFlags: emptyDragToMoveIndicatorFlags,
        parentOutlineHighlight: null,
      },
    },
    floatingInsertMenu: {
      insertMenuMode: 'closed',
    },
    inspector: {
      visible: true,
      classnameFocusCounter: 0,
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
    thumbnailLastGenerated: 0,
    pasteTargetsToIgnore: [],
    parseOrPrintInFlight: false,
    previousParseOrPrintSkipped: false,
    safeMode: false,
    saveError: false,
    navigator: {
      dropTargetHint: null,
      collapsedViews: [],
      renamingTarget: null,
      minimised: persistentModel.navigator.minimised,
      highlightedTargets: [],
      hiddenInNavigator: [],
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
    vscodeLoadingScreenVisible: true,
    indexedDBFailed: false,
    forceParseFiles: [],
    allElementProps: {},
    currentAllElementProps: {},
    variablesInScope: {},
    currentVariablesInScope: {},
    githubSettings: persistentModel.githubSettings,
    imageDragSessionState: notDragging(),
    githubOperations: [],
    refreshingDependencies: false,
    branchOriginContents: null,
    githubData: emptyGithubData(),
    colorSwatches: persistentModel.colorSwatches,
    internalClipboard: {
      styleClipboard: [],
      elements: [],
    },
    filesModifiedByAnotherUser: [],
    activeFrames: [],
    commentFilterMode: 'all',
    forking: false,
    collaborators: [],
    sharingDialogOpen: false,
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
    colorSwatches: editor.colorSwatches,
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
    projectContents: removeParsedModelsFromProjectContents(projectContents),
    exportsInfo: [],
    codeEditorErrors: {
      buildErrors: {},
      lintErrors: {},
      componentDescriptorErrors: {},
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
    githubSettings: emptyGithubSettings(),
    colorSwatches: [],
  }
}

const defaultDependencies = Utils.mapArrayToDictionary(
  DefaultPackagesList,
  (p) => p.name,
  (p) => p.version,
)

export const defaultIndexHtmlFilePath = 'public/index.html'

export const EmptyPackageJson = {
  name: 'utopia-project',
  version: '0.1.0',
}

export const DefaultPackageJson = {
  name: 'utopia-project',
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

export function getPackageJsonFromProjectContents(
  projectContents: ProjectContentTreeRoot,
): Either<string, any> {
  const packageJsonFile = packageJsonFileFromProjectContents(projectContents)
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
  const packageJsonContents = getPackageJsonFromProjectContents(model.projectContents)
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
    getPackageJsonFromProjectContents(editor.projectContents),
  )
  const filePath =
    isRight(parsedFilePath) && typeof parsedFilePath.value === 'string'
      ? parsedFilePath.value
      : 'public/index.html'
  const indexHtml = getProjectFileByFilePath(editor.projectContents, `/${filePath}`)
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
        RevisionsState.CodeAhead,
        packageJsonFile.versionNumber + 1,
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

export function getAllCodeEditorErrors(
  codeEditorErrors: EditorStateCodeEditorErrors,
  minimumSeverity: ErrorMessageSeverity,
  skipTsErrors: boolean,
): Array<ErrorMessage> {
  const allLintErrors = getAllLintErrors(codeEditorErrors)
  const allBuildErrors = getAllBuildErrors(codeEditorErrors)
  const allComponentDescriptorErrors = getAllComponentDescriptorErrors(codeEditorErrors)
  const errorsAndWarnings = skipTsErrors
    ? [...allLintErrors, ...allComponentDescriptorErrors]
    : [...allBuildErrors, ...allLintErrors, ...allComponentDescriptorErrors]
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

export function getAllBuildErrors(
  codeEditorErrors: EditorStateCodeEditorErrors,
): Array<ErrorMessage> {
  return getAllErrorsFromFiles(codeEditorErrors.buildErrors)
}

export function getAllLintErrors(
  codeEditorErrors: EditorStateCodeEditorErrors,
): Array<ErrorMessage> {
  return getAllErrorsFromFiles(codeEditorErrors.lintErrors)
}

export function getAllComponentDescriptorErrors(
  codeEditorErrors: EditorStateCodeEditorErrors,
): Array<ErrorMessage> {
  return getAllErrorsFromFiles(codeEditorErrors.componentDescriptorErrors)
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

export function reconstructJSXMetadata(editor: EditorState): {
  metadata: ElementInstanceMetadataMap
  elementPathTree: ElementPathTrees
} {
  const uiFile = getOpenUIJSFile(editor)
  if (uiFile == null) {
    return {
      metadata: editor.jsxMetadata,
      elementPathTree: editor.elementPathTree,
    }
  } else {
    return foldParsedTextFile(
      (_) => {
        return {
          metadata: editor.jsxMetadata,
          elementPathTree: editor.elementPathTree,
        }
      },
      (success) => {
        const elementsByUID = getElementsByUIDFromTopLevelElements(success.topLevelElements)
        const { mergedMetadata, elementPathTree } = MetadataUtils.mergeComponentMetadata(
          elementsByUID,
          editor.spyMetadata,
          editor.domMetadata,
        )
        return {
          metadata: ElementInstanceMetadataMapKeepDeepEquality(editor.jsxMetadata, mergedMetadata)
            .value,
          elementPathTree: elementPathTree,
        }
      },
      (_) => ({
        metadata: editor.jsxMetadata,
        elementPathTree: editor.elementPathTree,
      }),
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
  const file = getProjectFileByFilePath(editor.projectContents, fullPath)
  if (file != null && isTextFile(file)) {
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
  throwForErrors: boolean = true,
): EditorState {
  const projectFile = getProjectFileByFilePath(editor.projectContents, filePath)
  if (projectFile != null && isTextFile(projectFile)) {
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
      if (throwForErrors) {
        throw new Error(`File ${filePath} is not currently parsed.`)
      } else {
        return editor
      }
    }
  } else {
    if (throwForErrors) {
      throw new Error(`No text file found at ${filePath}`)
    } else {
      return editor
    }
  }
}

export function defaultModifyParseSuccess(success: ParseSuccess): ParseSuccess {
  return success
}

export function modifyUnderlyingTarget(
  target: ElementPath | null,
  editor: EditorState,
  modifyElement: (
    element: JSXElementChild,
    underlying: ElementPath,
    underlyingFilePath: string,
  ) => JSXElementChild,
  modifyParseSuccess: (
    parseSuccess: ParseSuccess,
    underlying: StaticElementPath | null,
    underlyingFilePath: string,
  ) => ParseSuccess = defaultModifyParseSuccess,
): EditorState {
  const underlyingTarget = normalisePathToUnderlyingTarget(editor.projectContents, target)
  const targetSuccess = normalisePathSuccessOrThrowError(underlyingTarget)

  function innerModifyParseSuccess(oldParseSuccess: ParseSuccess): ParseSuccess {
    // Apply the ParseSuccess level changes.
    const updatedParseSuccess: ParseSuccess = modifyParseSuccess(
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
      function innerModifyElement(element: JSXElementChild): JSXElementChild {
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

export function modifyUnderlyingParseSuccessOnly(
  target: ElementPath | null,
  editor: EditorState,
  modifyParseSuccess: (
    parseSuccess: ParseSuccess,
    underlyingFilePath: string,
  ) => ParseSuccess = defaultModifyParseSuccess,
): EditorState {
  const underlyingTarget = normalisePathToUnderlyingTarget(editor.projectContents, target)
  const targetSuccess = normalisePathSuccessOrThrowError(underlyingTarget)

  function innerModifyParseSuccess(oldParseSuccess: ParseSuccess): ParseSuccess {
    // Apply the ParseSuccess level changes.
    return modifyParseSuccess(oldParseSuccess, targetSuccess.filePath)
  }

  return modifyParseSuccessAtPath(targetSuccess.filePath, editor, innerModifyParseSuccess)
}

export function modifyUnderlyingForOpenFile(
  target: ElementPath | null,
  editor: EditorState,
  modifyElement: (
    element: JSXElementChild,
    underlying: ElementPath,
    underlyingFilePath: string,
  ) => JSXElementChild,
): EditorState {
  return modifyUnderlyingTarget(target, editor, modifyElement)
}

export function modifyUnderlyingTargetElement(
  target: ElementPath,
  editor: EditorState,
  modifyElement: (
    element: JSXElement | JSXConditionalExpression | JSXFragment,
    underlying: ElementPath,
    underlyingFilePath: string,
  ) => JSXElement | JSXConditionalExpression | JSXFragment = (element) => element,
  modifyParseSuccess: (
    parseSuccess: ParseSuccess,
    underlying: StaticElementPath | null,
    underlyingFilePath: string,
  ) => ParseSuccess = defaultModifyParseSuccess,
): EditorState {
  return modifyUnderlyingTarget(
    target,
    editor,
    (element, underlying, underlyingFilePath) => {
      if (isJSXElement(element) || isJSXConditionalExpression(element) || isJSXFragment(element)) {
        return modifyElement(element, underlying, underlyingFilePath)
      }
      return element
    },
    modifyParseSuccess,
  )
}

export function modifyUnderlyingTargetJSXElement(
  target: ElementPath,
  editor: EditorState,
  modifyElement: (
    element: JSXElement,
    underlying: ElementPath,
    underlyingFilePath: string,
  ) => JSXElement,
): EditorState {
  return modifyUnderlyingTarget(
    target,
    editor,
    (element, underlying, underlyingFilePath) => {
      if (isJSXElement(element)) {
        return modifyElement(element, underlying, underlyingFilePath)
      }
      return element
    },
    defaultModifyParseSuccess,
  )
}

export function modifyUnderlyingElementForOpenFile(
  target: ElementPath,
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
  return modifyUnderlyingTargetElement(
    target,
    editor,
    (element, underlying, underlyingFilePath) =>
      isJSXElement(element) ? modifyElement(element, underlying, underlyingFilePath) : element,
    modifyParseSuccess,
  )
}

export function withUnderlyingTarget<T>(
  target: ElementPath | null | undefined,
  projectContents: ProjectContentTreeRoot,
  defaultValue: T,
  withTarget: (
    success: ParseSuccess,
    element: JSXElementChild,
    underlyingTarget: StaticElementPath,
    underlyingFilePath: string,
    underlyingDynamicTarget: ElementPath,
  ) => T,
): T {
  const underlyingTarget = normalisePathToUnderlyingTarget(projectContents, target ?? null)

  if (
    underlyingTarget.type === 'NORMALISE_PATH_SUCCESS' &&
    underlyingTarget.normalisedPath != null &&
    underlyingTarget.normalisedDynamicPath != null
  ) {
    const parsed = underlyingTarget.textFile.fileContents.parsed
    if (isParseSuccess(parsed)) {
      const element = findJSXElementChildAtPath(
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
    element: JSXElementChild,
    underlyingTarget: StaticElementPath,
    underlyingFilePath: string,
  ) => T,
): T {
  return withUnderlyingTarget(target, editor.projectContents, defaultValue, withTarget)
}

export function forUnderlyingTargetFromEditorState(
  target: ElementPath | null,
  editor: EditorState,
  withTarget: (
    success: ParseSuccess,
    element: JSXElementChild,
    underlyingTarget: StaticElementPath,
    underlyingFilePath: string,
  ) => void,
): void {
  withUnderlyingTargetFromEditorState<any>(target, editor, {}, withTarget)
}

export function getJSXElementFromProjectContents(
  target: ElementPath | null,
  projectContents: ProjectContentTreeRoot,
): JSXElement | null {
  return withUnderlyingTarget(target, projectContents, null, (_, element) => {
    if (isJSXElement(element)) {
      return element
    } else {
      return null
    }
  })
}

export function getElementFromProjectContents(
  target: ElementPath | null,
  projectContents: ProjectContentTreeRoot,
): JSXElementChild | null {
  return withUnderlyingTarget(target, projectContents, null, (_, element) => element)
}

export function getCurrentTheme(userConfiguration: ThemeSubstate['userState']): Theme {
  const currentTheme = userConfiguration.themeConfig ?? DefaultTheme
  if (currentTheme === 'system') {
    return getPreferredColorScheme()
  } else {
    return currentTheme
  }
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
