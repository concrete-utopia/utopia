import { produce } from 'immer'
import update from 'immutability-helper'
import localforage from 'localforage'
import { LayoutSystem } from 'utopia-api/core'
import { imagePathURL } from '../../../common/server'
import { PinLayoutHelpers } from '../../../core/layout/layout-helpers'
import {
  maybeSwitchChildrenLayoutProps,
  roundAttributeLayoutValues,
  switchLayoutMetadata,
} from '../../../core/layout/layout-utils'
import {
  findElementAtPath,
  getZIndexOrderedViewsWithoutDirectChildren,
  MetadataUtils,
} from '../../../core/model/element-metadata-utils'
import type { InsertChildAndDetails } from '../../../core/model/element-template-utils'
import {
  generateUidWithExistingComponents,
  getIndexInParent,
  transformJSXComponentAtElementPath,
} from '../../../core/model/element-template-utils'
import {
  applyToAllUIJSFiles,
  applyUtopiaJSXComponentsChanges,
  assetFile,
  directory,
  fileTypeFromFileName,
  getHighlightBoundsFromParseResult,
  getUtopiaJSXComponentsFromSuccess,
  imageFile,
  isDirectory,
  isImageFile,
  isImg,
  revertFile,
  saveFile,
  saveTextFileContents,
  switchToFileType,
  uniqueProjectContentID,
  updateFileContents,
  updateFileIfPossible,
  updateParsedTextFileHighlightBounds,
} from '../../../core/model/project-file-utils'
import { getStoryboardElementPath, PathForSceneDataLabel } from '../../../core/model/scene-utils'
import type { Either } from '../../../core/shared/either'
import {
  eitherToMaybe,
  foldEither,
  forceRight,
  isLeft,
  isRight,
  left,
  mapEither,
  right,
  sequenceEither,
  traverseEither,
} from '../../../core/shared/either'
import * as EP from '../../../core/shared/element-path'
import type {
  Comment,
  DetectedLayoutSystem,
  ElementInstanceMetadataMap,
  JSXAttributes,
  JSExpressionValue,
  JSXElement,
  JSXElementChildren,
  SettableLayoutSystem,
  UtopiaJSXComponent,
  ElementInstanceMetadata,
} from '../../../core/shared/element-template'
import {
  deleteJSXAttribute,
  emptyComments,
  emptyJsxMetadata,
  getJSXAttribute,
  isElementWithUid,
  isImportStatement,
  isJSXAttributeValue,
  isJSXConditionalExpression,
  isJSXElement,
  isJSXFragment,
  modifiableAttributeIsPartOfAttributeValue,
  jsExpressionOtherJavaScript,
  jsxAttributesFromMap,
  jsExpressionValue,
  jsxConditionalExpression,
  JSXConditionalExpression,
  jsxElement,
  JSXElementChild,
  jsxElementName,
  JSXFragment,
  jsxFragment,
  jsxTextBlock,
  singleLineComment,
  walkElements,
  modifiableAttributeIsAttributeValue,
  isUtopiaJSXComponent,
  isNullJSXAttributeValue,
  isJSExpression,
} from '../../../core/shared/element-template'
import type { ValueAtPath } from '../../../core/shared/jsx-attributes'
import {
  getJSXAttributesAtPath,
  jsxSimpleAttributeToValue,
  setJSXValueAtPath,
  setJSXValuesAtPaths,
  unsetJSXValueAtPath,
  unsetJSXValuesAtPaths,
  valueAtPath,
} from '../../../core/shared/jsx-attributes'
import type {
  CanvasPoint,
  CanvasRectangle,
  LocalRectangle,
  Size,
  CanvasVector,
} from '../../../core/shared/math-utils'
import {
  canvasRectangle,
  isInfinityRectangle,
  isFiniteRectangle,
  rectangleIntersection,
  canvasPoint,
  roundTo,
  zeroCanvasPoint,
  zeroRectangle,
  MaybeInfinityCanvasRectangle,
  zeroCanvasRect,
  zeroLocalRect,
  LocalPoint,
  boundingRectangleArray,
  offsetPoint,
  getRectCenter,
  nullIfInfinity,
  isNotNullFiniteRectangle,
  localRectangle,
} from '../../../core/shared/math-utils'
import type {
  PackageStatusMap,
  RequestedNpmDependency,
} from '../../../core/shared/npm-dependency-types'
import { requestedNpmDependency } from '../../../core/shared/npm-dependency-types'
import { arrayToMaybe, forceNotNull, optionalMap } from '../../../core/shared/optional-utils'
import type {
  ElementPath,
  id,
  Imports,
  NodeModules,
  ParsedTextFile,
  ParseSuccess,
  ProjectContents,
  ProjectFile,
  StaticElementPath,
  TextFile,
} from '../../../core/shared/project-file-types'
import {
  codeFile,
  importStatementFromImportDetails,
  isAssetFile,
  isParseSuccess,
  isTextFile,
  RevisionsState,
  StaticElementPathPart,
  textFile,
  textFileContents,
  unparsed,
} from '../../../core/shared/project-file-types'
import * as PP from '../../../core/shared/property-path'
import { assertNever, fastForEach, getProjectLockedKey } from '../../../core/shared/utils'
import { emptyImports, mergeImports } from '../../../core/workers/common/project-file-utils'
import type { UtopiaTsWorkers } from '../../../core/workers/common/worker-types'
import type { IndexPosition } from '../../../utils/utils'
import Utils, { absolute } from '../../../utils/utils'
import type { ProjectContentTreeRoot } from '../../assets'
import {
  addFileToProjectContents,
  contentsToTree,
  getContentsTreeFileFromString,
  removeFromProjectContents,
  treeToContents,
  walkContentsTreeForParseSuccess,
} from '../../assets'
import type { CanvasFrameAndTarget, PinOrFlexFrameChange } from '../../canvas/canvas-types'
import { pinSizeChange } from '../../canvas/canvas-types'
import {
  duplicate,
  getFrameChange,
  moveTemplate,
  SkipFrameChange,
  updateFramesOfScenesAndComponents,
} from '../../canvas/canvas-utils'
import type { ResizeLeftPane, SetFocus } from '../../common/actions'
import { openMenu } from '../../context-menu-side-effect'
import type { CodeResultCache, PropertyControlsInfo } from '../../custom-code/code-file'
import {
  codeCacheToBuildResult,
  generateCodeResultCache,
  normalisePathSuccessOrThrowError,
  normalisePathToUnderlyingTarget,
} from '../../custom-code/code-file'
import { getFilePathToImport } from '../../filebrowser/filepath-utils'
import { getFrameAndMultiplier } from '../../images'
import type {
  AddFolder,
  AddImports,
  AddMissingDimensions,
  AddStoryboardFile,
  AddTailwindConfig,
  AddTextFile,
  AddToast,
  Alignment,
  AlignSelectedViews,
  ClearHighlightedViews,
  ClearImageFileBlob,
  ClearParseOrPrintInFlight,
  ClearTransientProps,
  CloseFloatingInsertMenu,
  ClosePopup,
  CloseTextEditor,
  CopySelectionToClipboard,
  DeleteFile,
  DeleteView,
  DistributeSelectedViews,
  Distribution,
  DuplicateSpecificElements,
  EditorAction,
  EditorDispatch,
  EditorModel,
  FinishCheckpointTimer,
  ForceParseFile,
  HideModal,
  InsertDroppedImage,
  InsertImageIntoUI,
  InsertInsertable,
  InsertJSXElement,
  Load,
  NewProject,
  OpenCodeEditorFile,
  OpenFloatingInsertMenu,
  OpenPopup,
  OpenTextEditor,
  RegenerateThumbnail,
  RemoveFromNodeModulesContents,
  RemoveToast,
  RenameComponent,
  RenameStyleSelector,
  ResetCanvas,
  ResetPins,
  ResizeInterfaceDesignerCodePane,
  RunEscapeHatch,
  SaveAsset,
  SaveCurrentFile,
  SaveDOMReport,
  ScrollToElement,
  SelectAllSiblings,
  SelectComponents,
  SendPreviewModel,
  SetAspectRatioLock,
  SetCanvasFrames,
  SetCodeEditorBuildErrors,
  SetCodeEditorLintErrors,
  SetCodeEditorVisibility,
  SetCurrentTheme,
  SetCursorOverlay,
  SetElementsToRerender,
  SetFilebrowserDropTarget,
  SetFilebrowserRenamingTarget,
  SetFocusedElement,
  SetFollowSelectionEnabled,
  SetForkedFromProjectID,
  SetGithubState,
  SetHighlightedViews,
  SetImageDragSessionState,
  SetLeftMenuExpanded,
  SetLeftMenuTab,
  SetLoginState,
  SetMainUIFile,
  SetNavigatorRenamingTarget,
  SetPackageStatus,
  SetPanelVisibility,
  SetProjectDescription,
  SetProjectID,
  SetProjectName,
  SetProp,
  SetPropTransient,
  SetResizeOptionsTargetOptions,
  SetRightMenuExpanded,
  SetRightMenuTab,
  SetSafeMode,
  SetSaveError,
  SetScrollAnimation,
  SetShortcut,
  SetStoredFontSettings,
  SetZIndex,
  ShowContextMenu,
  ShowModal,
  StartCheckpointTimer,
  SwitchEditorMode,
  ToggleCollapse,
  ToggleHidden,
  ToggleInterfaceDesignerAdditionalControls,
  ToggleInterfaceDesignerCodeEditor,
  TogglePane,
  ToggleProperty,
  ToggleSelectionLock,
  UnsetProperty,
  UnwrapElement,
  UpdateText,
  UpdateCodeResultCache,
  UpdateDuplicationState,
  UpdateEditorMode,
  UpdateFile,
  UpdateFilePath,
  UpdateFormulaBarMode,
  UpdateFrameDimensions,
  UpdateFromWorker,
  UpdateGithubSettings,
  UpdateJSXElementName,
  UpdateKeysPressed,
  UpdateMouseButtonsPressed,
  UpdateNodeModulesContents,
  UpdatePackageJson,
  UpdatePreviewConnected,
  UpdateProjectContents,
  UpdatePropertyControlsInfo,
  UpdateThumbnailGenerated,
  WrapInElement,
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
  UpdateColorSwatches,
  PasteProperties,
  CopyProperties,
  SetConditionalOverriddenCondition,
  SwitchConditionalBranches,
  UpdateConditionalExpression,
  ElementPaste,
  TrueUpGroups,
} from '../action-types'
import { DeleteSelected, isLoggedIn } from '../action-types'
import type { Mode } from '../editor-modes'
import { EditorModes, isLiveMode, isSelectMode } from '../editor-modes'
import * as History from '../history'
import type { StateHistory } from '../history'
import {
  createLoadedPackageStatusMapFromDependencies,
  dependenciesFromPackageJson,
  dependenciesFromPackageJsonContents,
  dependenciesWithEditorRequirements,
  findLatestVersion,
  updateDependenciesInEditorState,
} from '../npm-dependency/npm-dependency'
import { updateRemoteThumbnail } from '../persistence/persistence-backend'
import {
  deleteAssetFile,
  saveAsset as saveAssetToServer,
  saveUserConfiguration,
  updateAssetFileName,
} from '../server'
import type {
  CanvasBase64Blobs,
  DerivedState,
  EditorState,
  PersistentModel,
  RightMenuTab,
  SimpleParseSuccess,
  UIFileBase64Blobs,
  UserConfiguration,
  UserState,
  EditorStoreUnpatched,
  NavigatorEntry,
  AllElementProps,
} from '../store/editor-state'
import {
  areGeneratedElementsTargeted,
  BaseCanvasOffset,
  BaseCanvasOffsetLeftPane,
  editorModelFromPersistentModel,
  getAllBuildErrors,
  getAllLintErrors,
  getCurrentTheme,
  getElementPathsInBounds,
  getHighlightBoundsForFile,
  getJSXComponentsAndImportsForPathFromState,
  getMainUIFromModel,
  getNewSceneName,
  getOpenFilename,
  getOpenTextFileKey,
  getOpenUIJSFileKey,
  FileChecksums,
  insertElementAtPath,
  LeftMenuTab,
  LeftPaneDefaultWidth,
  LeftPaneMinimumWidth,
  mergeStoredEditorStateIntoEditorState,
  modifyOpenJsxElementAtPath,
  modifyParseSuccessAtPath,
  modifyParseSuccessWithSimple,
  modifyUnderlyingElementForOpenFile,
  modifyUnderlyingTargetElement,
  packageJsonFileFromProjectContents,
  persistentModelFromEditorModel,
  removeElementAtPath,
  StoryboardFilePath,
  transformElementAtPath,
  updateMainUIInEditorState,
  vsCodeBridgeIdProjectId,
  withUnderlyingTarget,
  modifyOpenJsxElementOrConditionalAtPath,
  isRegularNavigatorEntry,
  regularNavigatorEntryOptic,
  ConditionalClauseNavigatorEntry,
  reparentTargetFromNavigatorEntry,
  modifyOpenJsxChildAtPath,
  isConditionalClauseNavigatorEntry,
  deriveState,
  DefaultNavigatorWidth,
} from '../store/editor-state'
import { loadStoredState } from '../stored-state'
import { applyMigrations } from './migrations/migrations'

import { defaultConfig } from 'utopia-vscode-common'
import { reorderElement } from '../../../components/canvas/commands/reorder-element-command'
import type { BuiltInDependencies } from '../../../core/es-modules/package-manager/built-in-dependencies-list'
import { fetchNodeModules } from '../../../core/es-modules/package-manager/fetch-packages'
import { resolveModule } from '../../../core/es-modules/package-manager/module-resolution'
import { addStoryboardFileToProject } from '../../../core/model/storyboard-utils'
import { UTOPIA_UID_KEY } from '../../../core/model/utopia-constants'
import { mapDropNulls, reverse, uniqBy } from '../../../core/shared/array-utils'
import type { TreeConflicts } from '../../../core/shared/github/helpers'
import { mergeProjectContents } from '../../../core/shared/github/helpers'
import { emptySet } from '../../../core/shared/set-utils'
import { fixUtopiaElement, getUtopiaID } from '../../../core/shared/uid-utils'
import {
  DefaultPostCSSConfig,
  DefaultTailwindConfig,
  PostCSSPath,
  TailwindConfigPath,
} from '../../../core/tailwind/tailwind-config'
import {
  initVSCodeBridge,
  sendCodeEditorDecorations,
  sendOpenFileMessage,
  sendSelectedElement,
  sendSetFollowSelectionEnabledMessage,
  sendSetVSCodeTheme,
} from '../../../core/vscode/vscode-bridge'
import {
  createClipboardDataFromSelection,
  Clipboard,
  getTargetParentForPaste,
  ReparentTargetForPaste,
} from '../../../utils/clipboard'
import { NavigatorStateKeepDeepEquality } from '../store/store-deep-equality-instances'
import type { MouseButtonsPressed } from '../../../utils/mouse'
import { addButtonPressed, removeButtonPressed } from '../../../utils/mouse'
import { stripLeadingSlash } from '../../../utils/path-utils'
import utils from '../../../utils/utils'
import { pickCanvasStateFromEditorState } from '../../canvas/canvas-strategies/canvas-strategies'
import { getEscapeHatchCommands } from '../../canvas/canvas-strategies/strategies/convert-to-absolute-and-move-strategy'
import {
  absolutePositionForPaste,
  absolutePositionForReparent,
  canCopyElement,
  isAllowedToReparent,
  offsetPositionInPasteBoundingBox,
} from '../../canvas/canvas-strategies/strategies/reparent-helpers/reparent-helpers'
import type { StaticReparentTarget } from '../../canvas/canvas-strategies/strategies/reparent-helpers/reparent-strategy-helpers'
import {
  ReparentAsAbsolute,
  ReparentAsStatic,
  reparentStrategyForPaste,
  reparentStrategyForPaste as reparentStrategyForStaticReparent,
} from '../../canvas/canvas-strategies/strategies/reparent-helpers/reparent-strategy-helpers'
import type { ToReparent } from '../../canvas/canvas-strategies/strategies/reparent-utils'
import {
  elementToReparent,
  getReparentOutcome,
  getReparentOutcomeMultiselect,
  pathToReparent,
} from '../../canvas/canvas-strategies/strategies/reparent-utils'
import { areAllSelectedElementsNonAbsolute } from '../../canvas/canvas-strategies/strategies/shared-move-strategies-helpers'
import { foldAndApplyCommandsSimple } from '../../canvas/commands/commands'
import { setElementsToRerenderCommand } from '../../canvas/commands/set-elements-to-rerender-command'
import type { UiJsxCanvasContextData } from '../../canvas/ui-jsx-canvas'
import { notice } from '../../common/notice'
import { stylePropPathMappingFn } from '../../inspector/common/property-path-hooks'
import type { ShortcutConfiguration } from '../shortcut-definitions'
import { ElementInstanceMetadataMapKeepDeepEquality } from '../store/store-deep-equality-instances'
import {
  addImports,
  addToast,
  clearImageFileBlob,
  deleteView,
  enableInsertModeForJSXElement,
  finishCheckpointTimer,
  insertJSXElement,
  openCodeEditorFile,
  removeToast,
  selectComponents,
  setFocusedElement,
  setPackageStatus,
  setScrollAnimation,
  showToast,
  updateFile,
  updateNodeModulesContents,
  updatePackageJson,
  updateThumbnailGenerated,
} from './action-creators'
import { addToastToState, includeToast, removeToastFromState, uniqToasts } from './toast-helpers'
import { AspectRatioLockedProp } from '../../aspect-ratio'
import {
  refreshDependencies,
  removeModulesFromNodeModules,
} from '../../../core/shared/dependencies'
import {
  getReparentPropertyChanges,
  positionElementToCoordinatesCommands,
} from '../../canvas/canvas-strategies/strategies/reparent-helpers/reparent-property-changes'
import { styleStringInArray } from '../../../utils/common-constants'
import { collapseTextElements } from '../../../components/text-editor/text-handling'
import { LayoutPropsWithoutTLBR, StyleProperties } from '../../inspector/common/css-utils'
import { isFeatureEnabled } from '../../../utils/feature-switches'
import { isUtopiaCommentFlag, makeUtopiaFlagComment } from '../../../core/shared/comment-flags'
import { modify, toArrayOf } from '../../../core/shared/optics/optic-utilities'
import { Optic } from '../../../core/shared/optics/optics'
import { fromField, traverseArray } from '../../../core/shared/optics/optic-creators'
import type { InsertionPath } from '../store/insertion-path'
import {
  commonInsertionPathFromArray,
  getElementPathFromInsertionPath,
  isConditionalClauseInsertionPath,
  isChildInsertionPath,
  childInsertionPath,
  conditionalClauseInsertionPath,
  getInsertionPathWithSlotBehavior,
  getInsertionPathWithWrapWithFragmentBehavior,
} from '../store/insertion-path'
import {
  findMaybeConditionalExpression,
  getClauseOptic,
  getConditionalCaseCorrespondingToBranchPath,
  isEmptyConditionalBranch,
  maybeBranchConditionalCase,
  maybeConditionalExpression,
} from '../../../core/model/conditionals'
import { deleteProperties } from '../../canvas/commands/delete-properties-command'
import { treatElementAsFragmentLike } from '../../canvas/canvas-strategies/strategies/fragment-like-helpers'
import {
  isTextContainingConditional,
  unwrapConditionalClause,
  unwrapTextContainingConditional,
  wrapElementInsertions,
} from './wrap-unwrap-helpers'
import { ConditionalClauseInsertionPath } from '../store/insertion-path'
import { encodeUtopiaDataToHtml } from '../../../utils/clipboard-utils'
import { wildcardPatch } from '../../canvas/commands/wildcard-patch-command'
import { updateSelectedViews } from '../../canvas/commands/update-selected-views-command'
import { front } from '../../../utils/utils'
import { getAllUniqueUids } from '../../../core/model/get-unique-ids'
import type { ElementPathTrees } from '../../../core/shared/element-path-tree'
import { addToReparentedToPaths } from '../../canvas/commands/add-to-reparented-to-paths-command'
import type {
  DeleteFileFromVSCode,
  HideVSCodeLoadingScreen,
  MarkVSCodeBridgeReady,
  SelectFromFileAndPosition,
  SendCodeEditorInitialisation,
  SetIndexedDBFailed,
  UpdateConfigFromVSCode,
  UpdateFromCodeEditor,
} from './actions-from-vscode'
import { pushIntendedBoundsAndUpdateGroups } from '../../canvas/commands/push-intended-bounds-and-update-groups-command'
import { addToTrueUpGroups } from '../../../core/model/groups'
import {
  groupStateFromJSXElement,
  invalidGroupStateToString,
  isInvalidGroupState,
  treatElementAsGroupLike,
} from '../../canvas/canvas-strategies/strategies/group-helpers'
import { createPinChangeCommandsForElementBecomingGroupChild } from '../../canvas/canvas-strategies/strategies/group-conversion-helpers'

export const MIN_CODE_PANE_REOPEN_WIDTH = 100

export function updateSelectedLeftMenuTab(editorState: EditorState, tab: LeftMenuTab): EditorState {
  return {
    ...editorState,
    leftMenu: {
      ...editorState.leftMenu,
      selectedTab: tab,
    },
  }
}

export function updateLeftMenuExpanded(editorState: EditorState, expanded: boolean): EditorState {
  return {
    ...editorState,
    leftMenu: {
      ...editorState.leftMenu,
      expanded: expanded,
    },
  }
}

export function setLeftMenuTabFromFocusedPanel(editorState: EditorState): EditorState {
  switch (editorState.focusedPanel) {
    case 'misccodeeditor':
      return updateSelectedLeftMenuTab(editorState, LeftMenuTab.Project)
    case 'inspector':
    case 'canvas':
    case 'codeEditor':
    default:
      return editorState
  }
}

export function updateSelectedRightMenuTab(
  editorState: EditorState,
  tab: RightMenuTab,
): EditorState {
  return {
    ...editorState,
    rightMenu: {
      ...editorState.rightMenu,
      selectedTab: tab,
    },
  }
}

export function updateRightMenuExpanded(editorState: EditorState, expanded: boolean): EditorState {
  return {
    ...editorState,
    rightMenu: {
      ...editorState.rightMenu,
      expanded: expanded,
    },
  }
}

function applyUpdateToJSXElement(
  element: JSXElement,
  updateFn: (props: JSXAttributes) => Either<any, JSXAttributes>,
): JSXElement {
  const result = updateFn(element.props)
  if (isLeft(result)) {
    return element
  } else {
    return {
      ...element,
      props: result.value,
    }
  }
}

function setPropertyOnTarget(
  editor: EditorModel,
  target: ElementPath,
  updateFn: (props: JSXAttributes) => Either<any, JSXAttributes>,
): EditorModel {
  return modifyOpenJsxElementAtPath(
    target,
    (e: JSXElement) => applyUpdateToJSXElement(e, updateFn),
    editor,
  )
}

function setSpecialSizeMeasurementParentLayoutSystemOnAllChildren(
  scenes: ElementInstanceMetadataMap,
  pathTrees: ElementPathTrees,
  parentPath: ElementPath,
  value: DetectedLayoutSystem,
): ElementInstanceMetadataMap {
  const allChildren = MetadataUtils.getImmediateChildrenOrdered(scenes, pathTrees, parentPath)
  return allChildren.reduce((transformedScenes, child) => {
    return switchLayoutMetadata(transformedScenes, child.elementPath, value, undefined, undefined)
  }, scenes)
}

export function editorMoveMultiSelectedTemplates(
  builtInDependencies: BuiltInDependencies,
  targets: ElementPath[],
  indexPosition: IndexPosition,
  newParent: InsertionPath | null,
  editor: EditorModel,
): {
  editor: EditorModel
  newPaths: Array<ElementPath>
} {
  let updatedTargets: Array<ElementPath> = [...targets]
  let newPaths: Array<ElementPath> = []
  const updatedEditor = targets.reduce((working, target, i) => {
    let templateToMove = updatedTargets[i]

    const outcomeResult = getReparentOutcome(
      editor.jsxMetadata,
      editor.elementPathTree,
      editor.allElementProps,
      builtInDependencies,
      editor.projectContents,
      editor.nodeModules.files,
      editor.canvas.openFile?.filename,
      pathToReparent(target),
      newParent,
      'on-complete', // TODO make sure this is the right pick here
      null,
    )
    if (outcomeResult == null) {
      return working
    } else {
      const { commands: reparentCommands, newPath } = outcomeResult
      const reorderCommand = reorderElement('on-complete', newPath, indexPosition)

      const withCommandsApplied = foldAndApplyCommandsSimple(working, [
        ...reparentCommands,
        reorderCommand,
      ])

      // when moving multiselected elements that are in a hierarchy the editor has the ancestor with a new path
      updatedTargets = updatedTargets.map((path) => {
        const newChildPath = EP.replaceIfAncestor(path, templateToMove, newPath)
        return Utils.defaultIfNull(path, newChildPath)
      })
      newPaths.push(newPath)

      return withCommandsApplied
    }
  }, editor)
  return {
    editor: updatedEditor,
    newPaths: newPaths,
  }
}

export function editorMoveTemplate(
  target: ElementPath,
  originalPath: ElementPath,
  newFrame: CanvasRectangle | typeof SkipFrameChange | null,
  indexPosition: IndexPosition,
  newParentPath: ElementPath | null,
  parentFrame: CanvasRectangle | null,
  editor: EditorModel,
  newParentLayoutSystem: SettableLayoutSystem | null,
  newParentMainAxis: 'horizontal' | 'vertical' | null,
): {
  editor: EditorModel
  newPath: ElementPath | null
} {
  const moveResult = moveTemplate(
    target,
    originalPath,
    newFrame,
    indexPosition,
    newParentPath,
    parentFrame,
    editor,
    editor.jsxMetadata,
    editor.selectedViews,
    editor.highlightedViews,
    newParentLayoutSystem,
    newParentMainAxis,
  )
  return {
    newPath: moveResult.newPath,
    editor: moveResult.updatedEditorState,
  }
}

export function restoreEditorState(
  currentEditor: EditorModel,
  desiredEditor: EditorModel,
): EditorModel {
  // FIXME Ask Team Components to check over these
  return {
    id: currentEditor.id,
    vscodeBridgeId: currentEditor.vscodeBridgeId,
    forkedFromProjectId: currentEditor.forkedFromProjectId,
    appID: currentEditor.appID,
    projectName: currentEditor.projectName,
    projectDescription: currentEditor.projectDescription,
    projectVersion: currentEditor.projectVersion,
    isLoaded: currentEditor.isLoaded,
    trueUpGroupsForElementAfterDomWalkerRuns: [], // <- we reset the group true-up value
    spyMetadata: desiredEditor.spyMetadata,
    domMetadata: desiredEditor.domMetadata,
    jsxMetadata: desiredEditor.jsxMetadata,
    elementPathTree: desiredEditor.elementPathTree,
    projectContents: desiredEditor.projectContents,
    nodeModules: currentEditor.nodeModules,
    codeResultCache: currentEditor.codeResultCache,
    propertyControlsInfo: currentEditor.propertyControlsInfo,
    selectedViews: desiredEditor.selectedViews,
    highlightedViews: currentEditor.highlightedViews,
    hoveredViews: currentEditor.hoveredViews,
    hiddenInstances: desiredEditor.hiddenInstances,
    displayNoneInstances: desiredEditor.displayNoneInstances,
    warnedInstances: desiredEditor.warnedInstances,
    lockedElements: desiredEditor.lockedElements,
    mode: EditorModes.selectMode(),
    focusedPanel: currentEditor.focusedPanel,
    keysPressed: {},
    mouseButtonsPressed: emptySet(),
    openPopupId: null,
    toasts: currentEditor.toasts,
    cursorStack: {
      fixed: null,
      mouseOver: [],
    },
    leftMenu: {
      selectedTab: currentEditor.leftMenu.selectedTab,
      expanded: currentEditor.leftMenu.expanded,
      paneWidth: currentEditor.leftMenu.paneWidth,
    },
    rightMenu: {
      selectedTab: currentEditor.rightMenu.selectedTab,
      expanded: currentEditor.rightMenu.expanded,
    },
    interfaceDesigner: {
      codePaneWidth: currentEditor.interfaceDesigner.codePaneWidth,
      codePaneVisible: currentEditor.interfaceDesigner.codePaneVisible,
      restorableCodePaneWidth: currentEditor.interfaceDesigner.codePaneWidth,
      additionalControls: currentEditor.interfaceDesigner.additionalControls,
    },
    canvas: {
      elementsToRerender: currentEditor.canvas.elementsToRerender,
      visible: currentEditor.canvas.visible,
      interactionSession: null,
      scale: currentEditor.canvas.scale,
      snappingThreshold: currentEditor.canvas.snappingThreshold,
      realCanvasOffset: currentEditor.canvas.realCanvasOffset,
      roundedCanvasOffset: currentEditor.canvas.roundedCanvasOffset,
      textEditor: null,
      selectionControlsVisible: currentEditor.canvas.selectionControlsVisible,
      cursor: null,
      duplicationState: null,
      base64Blobs: {},
      mountCount: currentEditor.canvas.mountCount, // QUESTION should undo-redo forcibly remount the canvas?
      canvasContentInvalidateCount: currentEditor.canvas.canvasContentInvalidateCount + 1,
      domWalkerInvalidateCount: currentEditor.canvas.domWalkerInvalidateCount + 1,
      openFile: currentEditor.canvas.openFile,
      scrollAnimation: currentEditor.canvas.scrollAnimation,
      transientProperties: null,
      resizeOptions: currentEditor.canvas.resizeOptions,
      domWalkerAdditionalElementsToUpdate: currentEditor.canvas.domWalkerAdditionalElementsToUpdate,
      controls: currentEditor.canvas.controls,
    },
    floatingInsertMenu: currentEditor.floatingInsertMenu,
    inspector: {
      visible: currentEditor.inspector.visible,
      classnameFocusCounter: currentEditor.inspector.classnameFocusCounter,
    },
    fileBrowser: {
      minimised: currentEditor.fileBrowser.minimised,
      dropTarget: null,
      renamingTarget: currentEditor.fileBrowser.renamingTarget,
    },
    dependencyList: {
      minimised: currentEditor.dependencyList.minimised,
    },
    genericExternalResources: {
      minimised: currentEditor.genericExternalResources.minimised,
    },
    googleFontsResources: {
      minimised: currentEditor.googleFontsResources.minimised,
    },
    projectSettings: {
      minimised: currentEditor.projectSettings.minimised,
    },
    navigator: {
      minimised: currentEditor.navigator.minimised,
      dropTargetHint: null,
      collapsedViews: desiredEditor.navigator.collapsedViews,
      renamingTarget: null,
      highlightedTargets: [],
      hiddenInNavigator: [],
    },
    topmenu: {
      formulaBarMode: desiredEditor.topmenu.formulaBarMode,
      formulaBarFocusCounter: currentEditor.topmenu.formulaBarFocusCounter,
    },
    preview: {
      visible: currentEditor.preview.visible,
      connected: currentEditor.preview.connected,
    },
    home: {
      visible: currentEditor.home.visible,
    },
    lastUsedFont: currentEditor.lastUsedFont,
    modal: null,
    localProjectList: currentEditor.localProjectList,
    projectList: currentEditor.projectList,
    showcaseProjects: currentEditor.showcaseProjects,
    codeEditingEnabled: desiredEditor.codeEditingEnabled,
    thumbnailLastGenerated: currentEditor.thumbnailLastGenerated,
    pasteTargetsToIgnore: desiredEditor.pasteTargetsToIgnore,
    codeEditorErrors: currentEditor.codeEditorErrors,
    parseOrPrintInFlight: false,
    safeMode: currentEditor.safeMode,
    saveError: currentEditor.saveError,
    vscodeBridgeReady: currentEditor.vscodeBridgeReady,
    vscodeReady: currentEditor.vscodeReady,
    focusedElementPath: desiredEditor.focusedElementPath,
    config: defaultConfig(),
    vscodeLoadingScreenVisible: currentEditor.vscodeLoadingScreenVisible,
    indexedDBFailed: currentEditor.indexedDBFailed,
    forceParseFiles: currentEditor.forceParseFiles,
    allElementProps: desiredEditor.allElementProps,
    _currentAllElementProps_KILLME: desiredEditor._currentAllElementProps_KILLME,
    githubSettings: currentEditor.githubSettings,
    imageDragSessionState: currentEditor.imageDragSessionState,
    githubOperations: currentEditor.githubOperations,
    branchOriginContents: currentEditor.branchOriginContents,
    githubData: currentEditor.githubData,
    refreshingDependencies: currentEditor.refreshingDependencies,
    colorSwatches: currentEditor.colorSwatches,
    internalClipboard: currentEditor.internalClipboard,
  }
}

function restoreEditorStateFromHistory(
  currentEditor: EditorModel,
  history: StateHistory,
): EditorModel {
  const poppedEditor = history.current.editor
  return restoreEditorState(currentEditor, poppedEditor)
}

export function restoreDerivedState(history: StateHistory): DerivedState {
  const poppedDerived = history.current.derived

  return {
    navigatorTargets: poppedDerived.navigatorTargets,
    visibleNavigatorTargets: poppedDerived.visibleNavigatorTargets,
    autoFocusedPaths: poppedDerived.autoFocusedPaths,
    controls: [],
    elementWarnings: poppedDerived.elementWarnings,
    projectContentsChecksums: poppedDerived.projectContentsChecksums,
    branchOriginContentsChecksums: poppedDerived.branchOriginContentsChecksums,
  }
}

function deleteElements(targets: ElementPath[], editor: EditorModel): EditorModel {
  const openUIJSFilePath = getOpenUIJSFileKey(editor)
  if (openUIJSFilePath == null) {
    console.error(`Attempted to delete element(s) with no UI file open.`)
    return editor
  } else {
    const updatedEditor = targets.reduce((working, targetPath) => {
      const underlyingTarget = normalisePathToUnderlyingTarget(
        working.projectContents,
        working.nodeModules.files,
        openUIJSFilePath,
        targetPath,
      )
      const targetSuccess = normalisePathSuccessOrThrowError(underlyingTarget)

      function deleteElementFromParseSuccess(parseSuccess: ParseSuccess): ParseSuccess {
        const utopiaComponents = getUtopiaJSXComponentsFromSuccess(parseSuccess)
        const withTargetRemoved: Array<UtopiaJSXComponent> = removeElementAtPath(
          targetPath,
          utopiaComponents,
        )
        return modifyParseSuccessWithSimple((success: SimpleParseSuccess) => {
          return {
            ...success,
            utopiaComponents: withTargetRemoved,
          }
        }, parseSuccess)
      }
      return modifyParseSuccessAtPath(
        targetSuccess.filePath,
        working,
        deleteElementFromParseSuccess,
      )
    }, editor)
    const withUpdatedSelectedViews = {
      ...updatedEditor,
      selectedViews: EP.filterPaths(updatedEditor.selectedViews, targets),
    }
    const siblings = targets
      .flatMap((target) => {
        return MetadataUtils.getSiblingsOrdered(editor.jsxMetadata, editor.elementPathTree, target)
      })
      .map((entry) => entry.elementPath)
    return addToTrueUpGroups(withUpdatedSelectedViews, ...siblings)
  }
}

function duplicateMany(paths: ElementPath[], editor: EditorModel): EditorModel {
  const targetParent = EP.getCommonParent(paths)
  const duplicateResult = duplicate(paths, targetParent, editor)
  if (duplicateResult == null) {
    return editor
  } else {
    return duplicateResult.updatedEditorState
  }
}

function indexPositionForAdjustment(
  target: StaticElementPath | ElementPath,
  editor: EditorModel,
  index: 'back' | 'front' | 'backward' | 'forward',
): IndexPosition {
  switch (index) {
    case 'back':
      return { type: 'back' }
    case 'front':
      return { type: 'front' }
    case 'backward':
    case 'forward':
      const openUIJSFileKey = getOpenUIJSFileKey(editor)
      if (openUIJSFileKey != null) {
        const current = withUnderlyingTarget(
          target,
          editor.projectContents,
          editor.nodeModules.files,
          openUIJSFileKey,
          0,
          (success) => {
            return getIndexInParent(success.topLevelElements, EP.asStatic(target))
          },
        )
        return {
          type: 'absolute',
          index: index === 'backward' ? Math.max(current - 1, 0) : current + 1,
        }
      } else {
        throw new Error('no open ui JS file found')
      }
  }
}

function setZIndexOnSelected(
  editor: EditorModel,
  index: 'back' | 'front' | 'backward' | 'forward',
): EditorModel {
  const selectedViews = editor.selectedViews

  return selectedViews.reduce((working, selectedView) => {
    const siblings = MetadataUtils.getSiblingsOrdered(
      editor.jsxMetadata,
      editor.elementPathTree,
      selectedView,
    )
    const currentIndex = MetadataUtils.getIndexInParent(
      editor.jsxMetadata,
      editor.elementPathTree,
      selectedView,
    )
    const isFirstSiblingMovedBackwards =
      currentIndex === 0 && (index === 'back' || index === 'backward')

    const isLastSiblingMovedForward =
      currentIndex === siblings.length - 1 && (index === 'front' || index === 'forward')

    const isElementRootOfConditionalBranch =
      getConditionalCaseCorrespondingToBranchPath(selectedView, editor.jsxMetadata) != null

    if (
      isFirstSiblingMovedBackwards ||
      isLastSiblingMovedForward ||
      isElementRootOfConditionalBranch
    ) {
      return working
    }

    const indexPosition = indexPositionForAdjustment(selectedView, working, index)
    return editorMoveTemplate(
      selectedView,
      selectedView,
      SkipFrameChange,
      indexPosition,
      EP.parentPath(selectedView),
      null,
      editor,
      null,
      null,
    ).editor
  }, editor)
}

function setModeState(mode: Mode, editor: EditorModel): EditorModel {
  return update(editor, {
    mode: { $set: mode },
  })
}

function updateNavigatorCollapsedState(
  selectedViews: Array<ElementPath>,
  navigator: EditorModel['navigator'],
): EditorModel['navigator'] {
  const allCollapsedViews = navigator.collapsedViews
  let collapsedWithChildrenSelected: ElementPath[] = []
  let collapsedNoChildrenSelected: ElementPath[] = []
  selectedViews.forEach((selectedView) => {
    allCollapsedViews.forEach((collapsedView) => {
      if (
        EP.isDescendantOfOrEqualTo(selectedView, collapsedView) &&
        !EP.pathsEqual(selectedView, collapsedView)
      ) {
        if (!EP.containsPath(collapsedView, collapsedWithChildrenSelected)) {
          collapsedWithChildrenSelected.push(collapsedView)
        }
      } else {
        if (!EP.containsPath(collapsedView, collapsedNoChildrenSelected)) {
          collapsedNoChildrenSelected.push(collapsedView)
        }
      }
    })
  })
  if (selectedViews.length == 0) {
    collapsedNoChildrenSelected = allCollapsedViews
  }

  return update(navigator, {
    collapsedViews: {
      $set: collapsedNoChildrenSelected.filter(
        (path) => !EP.containsPath(path, collapsedWithChildrenSelected),
      ),
    },
  })
}

interface ReplaceFilePathSuccess {
  type: 'SUCCESS'
  projectContents: ProjectContentTreeRoot
  updatedFiles: Array<{ oldPath: string; newPath: string }>
}

interface ReplaceFilePathFailure {
  type: 'FAILURE'
  errorMessage: string
}

type ReplaceFilePathResult = ReplaceFilePathFailure | ReplaceFilePathSuccess

function replaceFilePath(
  oldPath: string,
  newPath: string,
  projectContentsTree: ProjectContentTreeRoot,
): ReplaceFilePathResult {
  // FIXME: Reimplement this in a way that doesn't require converting to and from `ProjectContents`.
  const projectContents = treeToContents(projectContentsTree)
  // if there is no file in projectContents it's probably a non-empty directory
  const oldFolderRegex = new RegExp('^' + oldPath)
  let error: string | null = null
  let updatedProjectContents: ProjectContents = {
    ...projectContents,
  }
  let updatedFiles: Array<{ oldPath: string; newPath: string }> = []
  Utils.fastForEach(Object.keys(projectContents), (filename) => {
    if (oldFolderRegex.test(filename)) {
      const projectFile = projectContents[filename]
      const newFilePath = filename.replace(oldPath, newPath)
      const fileType = isDirectory(projectFile) ? 'DIRECTORY' : fileTypeFromFileName(newFilePath)
      if (fileType == null) {
        // Can't identify the file type.
        error = `Can't rename ${filename} to ${newFilePath}.`
      } else {
        const updatedProjectFile = switchToFileType(projectFile, fileType)
        if (updatedProjectFile == null) {
          // Appears this file can't validly be changed.
          error = `Can't rename ${filename} to ${newFilePath}.`
        } else {
          // Remove the old file.
          delete updatedProjectContents[filename]
          updatedProjectContents[newFilePath] = updatedProjectFile
          updatedFiles.push({ oldPath: filename, newPath: newFilePath })
        }
      }
    }
  })

  // Correct any imports in files that have changed because of the above file movements.
  Utils.fastForEach(Object.keys(updatedProjectContents), (filename) => {
    const projectFile = updatedProjectContents[filename]
    // Only for successfully parsed text files, with some protection for files that are yet to be parsed.
    if (
      isTextFile(projectFile) &&
      isParseSuccess(projectFile.fileContents.parsed) &&
      projectFile.fileContents.revisionsState !== RevisionsState.CodeAhead
    ) {
      let updatedParseResult: ParseSuccess = projectFile.fileContents.parsed
      fastForEach(updatedFiles, (updatedFile) => {
        fastForEach(Object.keys(updatedParseResult.imports), (importSource) => {
          // Only do this for import sources that look like file paths.
          if (importSource.startsWith('.') || importSource.startsWith('/')) {
            const resolveResult = resolveModule(projectContentsTree, {}, filename, importSource)

            if (
              resolveResult.type === 'RESOLVE_SUCCESS' &&
              resolveResult.success.path === updatedFile.oldPath
            ) {
              // Create new absolute import path and shift the import in this file to represent that.
              const importFromParse = updatedParseResult.imports[importSource]
              let updatedImports: Imports = {
                ...updatedParseResult.imports,
              }
              delete updatedImports[importSource]
              // If an absolute path was used before, use the updated absolute path.
              const newImportPath = importSource.startsWith('/')
                ? updatedFile.newPath
                : getFilePathToImport(updatedFile.newPath, filename)
              updatedImports[newImportPath] = importFromParse

              // Update the parse result to be incorporated later.
              updatedParseResult = {
                ...updatedParseResult,
                imports: updatedImports,
              }
            }
          }
        })

        // Update the top level element import statements.
        const updatedTopLevelElements = updatedParseResult.topLevelElements.map(
          (topLevelElement) => {
            if (isImportStatement(topLevelElement)) {
              const resolveResult = resolveModule(
                projectContentsTree,
                {},
                filename,
                topLevelElement.module,
              )
              if (
                resolveResult.type === 'RESOLVE_SUCCESS' &&
                resolveResult.success.path === updatedFile.oldPath
              ) {
                // If an absolute path was used before, use the updated absolute path.
                const newImportPath = topLevelElement.module.startsWith('/')
                  ? updatedFile.newPath
                  : getFilePathToImport(updatedFile.newPath, filename)
                const importDefinition = forceNotNull(
                  'Import should exist.',
                  updatedParseResult.imports[newImportPath],
                )
                return importStatementFromImportDetails(newImportPath, importDefinition)
              } else {
                return topLevelElement
              }
            } else {
              return topLevelElement
            }
          },
        )

        updatedParseResult = {
          ...updatedParseResult,
          topLevelElements: updatedTopLevelElements,
        }
      })

      updatedProjectContents[filename] = saveTextFileContents(
        projectFile,
        textFileContents(
          projectFile.fileContents.code,
          updatedParseResult,
          RevisionsState.ParsedAhead,
        ),
        projectFile.lastSavedContents == null,
      )
    }
  })
  // Check if we discovered an error.
  if (error == null) {
    return {
      type: 'SUCCESS',
      projectContents: contentsToTree(updatedProjectContents),
      updatedFiles: updatedFiles,
    }
  } else {
    return {
      type: 'FAILURE',
      errorMessage: error,
    }
  }
}

function loadModel(newModel: EditorModel, oldModel: EditorModel): EditorModel {
  return setLeftMenuTabFromFocusedPanel({
    ...newModel,
    isLoaded: true,
    localProjectList: oldModel.projectList,
    projectList: oldModel.projectList,
    showcaseProjects: oldModel.showcaseProjects,
  })
}

function toastOnGeneratedElementsSelected(
  message: string,
  editor: EditorState,
  allowActionRegardless: boolean,
  actionOtherwise: (e: EditorState) => EditorState,
  dispatch: EditorDispatch,
): EditorState {
  return toastOnGeneratedElementsTargeted(
    message,
    editor.selectedViews,
    editor,
    allowActionRegardless,
    actionOtherwise,
    dispatch,
  )
}

function toastOnGeneratedElementsTargeted(
  message: string,
  targets: ElementPath[],
  editor: EditorState,
  allowActionRegardless: boolean,
  actionOtherwise: (e: EditorState) => EditorState,
  dispatch: EditorDispatch,
): EditorState {
  const generatedElementsTargeted = areGeneratedElementsTargeted(targets)
  let result: EditorState = editor
  if (generatedElementsTargeted) {
    const showToastAction = showToast(notice(message))
    result = UPDATE_FNS.ADD_TOAST(showToastAction, result)
  }

  if (!generatedElementsTargeted || allowActionRegardless) {
    result = actionOtherwise(result)
  }

  return result
}

function toastOnUncopyableElementsSelected(
  message: string,
  editor: EditorState,
  allowActionRegardless: boolean,
  actionOtherwise: (e: EditorState) => EditorState,
  dispatch: EditorDispatch,
): EditorState {
  const isReparentable = editor.selectedViews.every((target) => {
    return isAllowedToReparent(editor.projectContents, editor.jsxMetadata, target)
  })
  let result: EditorState = editor
  if (!isReparentable) {
    const showToastAction = showToast(notice(message))
    result = UPDATE_FNS.ADD_TOAST(showToastAction, result)
  }

  if (isReparentable || allowActionRegardless) {
    result = actionOtherwise(result)
  }

  return result
}

let checkpointTimeoutId: number | undefined = undefined
let canvasScrollAnimationTimer: number | undefined = undefined

function updateSelectedComponentsFromEditorPosition(
  derived: DerivedState,
  editor: EditorState,
  dispatch: EditorDispatch,
  filePath: string,
  line: number,
): EditorState {
  if (Object.keys(editor.jsxMetadata).length === 0) {
    // Looks like the canvas has errored out, so leave it alone for now.
    return editor
  } else {
    const highlightBoundsForUids = getHighlightBoundsForFile(editor, filePath)
    const allElementPathsOptic = traverseArray<NavigatorEntry>().compose(fromField('elementPath'))
    const newlySelectedElements = getElementPathsInBounds(
      line,
      highlightBoundsForUids,
      toArrayOf(
        allElementPathsOptic,
        derived.navigatorTargets.filter((t) => !isConditionalClauseNavigatorEntry(t)),
      ),
    )
    return UPDATE_FNS.SELECT_COMPONENTS(
      selectComponents(newlySelectedElements, false),
      editor,
      dispatch,
    )
  }
}

function normalizeGithubData(editor: EditorModel): EditorModel {
  const { githubSettings } = editor
  const hasRepo = githubSettings.targetRepository != null
  const hasBranch = githubSettings.branchName != null
  return {
    ...editor,
    githubSettings: {
      ...githubSettings,
      branchName: hasRepo ? githubSettings.branchName : null,
      branchLoaded: hasRepo && hasBranch && githubSettings.branchLoaded,
      originCommit: hasRepo && hasBranch ? githubSettings.originCommit : null,
      pendingCommit: hasRepo && hasBranch ? githubSettings.pendingCommit : null,
    },
    githubData: {
      ...editor.githubData,
      upstreamChanges: null,
      currentBranchPullRequests: null,
    },
  }
}

function updateCodeEditorVisibility(editor: EditorModel, codePaneVisible: boolean): EditorModel {
  return {
    ...editor,
    interfaceDesigner: {
      ...editor.interfaceDesigner,
      codePaneVisible: codePaneVisible,
      codePaneWidth: editor.interfaceDesigner.codePaneVisible
        ? editor.interfaceDesigner.codePaneWidth
        : Math.max(MIN_CODE_PANE_REOPEN_WIDTH, editor.interfaceDesigner.codePaneWidth),
    },
  }
}

// JS Editor Actions:
export const UPDATE_FNS = {
  NEW: (
    action: NewProject,
    oldEditor: EditorModel,
    workers: UtopiaTsWorkers,
    dispatch: EditorDispatch,
  ): EditorModel => {
    const newPersistentModel = applyMigrations(action.persistentModel)
    const newModel = editorModelFromPersistentModel(newPersistentModel, dispatch)
    return {
      ...loadModel(newModel, oldEditor),
      nodeModules: {
        skipDeepFreeze: true,
        files: action.nodeModules,
        projectFilesBuildResults: {},
        packageStatus: action.packageResult,
      },
      codeResultCache: action.codeResultCache,
    }
  },
  LOAD: (action: Load, oldEditor: EditorModel, dispatch: EditorDispatch): EditorModel => {
    const migratedModel = applyMigrations(action.model)
    const parsedProjectFiles = applyToAllUIJSFiles(
      migratedModel.projectContents,
      (filename: string, file: TextFile) => {
        const lastSavedFileContents = optionalMap((lastSaved) => {
          return textFileContents(lastSaved.code, unparsed, RevisionsState.CodeAhead)
        }, file.lastSavedContents)
        return textFile(
          textFileContents(file.fileContents.code, unparsed, RevisionsState.CodeAhead),
          lastSavedFileContents,
          null,
          file.versionNumber + 1,
        )
      },
    )
    const storyboardFile = getContentsTreeFileFromString(parsedProjectFiles, StoryboardFilePath)
    const openFilePath = storyboardFile != null ? StoryboardFilePath : null
    initVSCodeBridge(parsedProjectFiles, dispatch, openFilePath)

    const parsedModel = {
      ...migratedModel,
      projectContents: parsedProjectFiles,
    }
    const newModel: EditorModel = {
      ...editorModelFromPersistentModel(parsedModel, dispatch),
      projectName: action.title,
      id: action.projectId,
      vscodeBridgeId: vsCodeBridgeIdProjectId(action.projectId), // we assign a first value when loading a project. SET_PROJECT_ID will not change this, saving us from having to reload VSCode
      nodeModules: {
        skipDeepFreeze: true,
        files: action.nodeModules,
        projectFilesBuildResults: {},
        packageStatus: action.packageResult,
      },
      codeResultCache: action.codeResultCache,
      safeMode: action.safeMode,
    }
    const newModelMergedWithStoredState: EditorModel = mergeStoredEditorStateIntoEditorState(
      action.storedState,
      newModel,
    )

    return loadModel(newModelMergedWithStoredState, oldEditor)
  },
  SET_HIGHLIGHTED_VIEWS: (action: SetHighlightedViews, editor: EditorModel): EditorModel => {
    return {
      ...editor,
      highlightedViews: action.targets,
    }
  },
  SET_HOVERED_VIEWS: (action: SetHoveredViews, editor: EditorModel): EditorModel => {
    return {
      ...editor,
      hoveredViews: action.targets,
    }
  },
  CLEAR_HIGHLIGHTED_VIEWS: (action: ClearHighlightedViews, editor: EditorModel): EditorModel => {
    if (editor.highlightedViews.length === 0) {
      return editor
    }
    return {
      ...editor,
      highlightedViews: [],
    }
  },
  CLEAR_HOVERED_VIEWS: (action: ClearHoveredViews, editor: EditorModel): EditorModel => {
    if (editor.hoveredViews.length === 0) {
      return editor
    }
    return {
      ...editor,
      hoveredViews: [],
    }
  },
  UNDO: (editor: EditorModel, stateHistory: StateHistory): EditorModel => {
    if (History.canUndo(stateHistory)) {
      const history = History.undo(editor.id, stateHistory, 'run-side-effects')
      return restoreEditorStateFromHistory(editor, history)
    } else {
      return editor
    }
  },
  REDO: (editor: EditorModel, stateHistory: StateHistory): EditorModel => {
    if (History.canRedo(stateHistory)) {
      const history = History.redo(editor.id, stateHistory, 'run-side-effects')
      return restoreEditorStateFromHistory(editor, history)
    } else {
      return editor
    }
  },
  UNSET_PROPERTY: (
    action: UnsetProperty,
    editor: EditorModel,
    dispatch: EditorDispatch,
  ): EditorModel => {
    let unsetPropFailedMessage: string | null = null
    const updatedEditor = modifyUnderlyingElementForOpenFile(
      action.element,
      editor,
      (element) => {
        const updatedProps = unsetJSXValueAtPath(element.props, action.property)
        return foldEither(
          (failureMessage) => {
            unsetPropFailedMessage = failureMessage
            return element
          },
          (updatedAttributes) => ({
            ...element,
            props: updatedAttributes,
          }),
          updatedProps,
        )
      },
      (parseSuccess) => parseSuccess,
    )
    if (unsetPropFailedMessage != null) {
      const toastAction = showToast(notice(unsetPropFailedMessage, 'ERROR'))
      return UPDATE_FNS.ADD_TOAST(toastAction, editor)
    } else {
      return updatedEditor
    }
  },
  SET_PROP: (action: SetProp, editor: EditorModel): EditorModel => {
    let setPropFailedMessage: string | null = null
    let updatedEditor = modifyUnderlyingElementForOpenFile(
      action.target,
      editor,
      (element) => {
        const updatedProps = setJSXValueAtPath(element.props, action.propertyPath, action.value)
        if (
          isRight(updatedProps) &&
          PP.contains(
            [
              PP.create('style', 'top'),
              PP.create('style', 'bottom'),
              PP.create('style', 'left'),
              PP.create('style', 'right'),
              PP.create('style', 'width'),
              PP.create('style', 'height'),
            ],
            action.propertyPath,
          )
        ) {
          const maybeInvalidGroupState = groupStateFromJSXElement(
            { ...element, props: updatedProps.value },
            action.target,
            editor.jsxMetadata,
            editor.elementPathTree,
            editor.allElementProps,
          )
          if (isInvalidGroupState(maybeInvalidGroupState)) {
            setPropFailedMessage = invalidGroupStateToString(maybeInvalidGroupState)
            return element
          }
        }
        return foldEither(
          (failureMessage) => {
            setPropFailedMessage = failureMessage
            return element
          },
          (updatedAttributes) => ({
            ...element,
            // we round style.left/top/right/bottom/width/height pins for the modified element
            props: roundAttributeLayoutValues(styleStringInArray, updatedAttributes),
          }),
          updatedProps,
        )
      },
      (parseSuccess) => parseSuccess,
    )

    updatedEditor = addToTrueUpGroups(updatedEditor, action.target)

    if (setPropFailedMessage != null) {
      const toastAction = showToast(notice(setPropFailedMessage, 'ERROR'))
      updatedEditor = UPDATE_FNS.ADD_TOAST(toastAction, editor)
    }

    return updatedEditor
  },
  SET_CANVAS_FRAMES: (
    action: SetCanvasFrames,
    editor: EditorModel,
    derived: DerivedState,
  ): EditorModel => {
    return setCanvasFramesInnerNew(editor, action.framesAndTargets, null)
  },
  SET_Z_INDEX: (action: SetZIndex, editor: EditorModel, derived: DerivedState): EditorModel => {
    return editorMoveTemplate(
      action.target,
      action.target,
      SkipFrameChange,
      action.indexPosition,
      EP.parentPath(action.target),
      null,
      editor,
      null,
      null,
    ).editor
  },
  DELETE_SELECTED: (editorForAction: EditorModel, dispatch: EditorDispatch): EditorModel => {
    return toastOnGeneratedElementsSelected(
      'Generated elements can only be deleted in code.',
      editorForAction,
      true,
      (editor) => {
        const staticSelectedElements = editor.selectedViews
          .filter((selectedView) => {
            return !MetadataUtils.isElementGenerated(selectedView)
          })
          .map((path, _, allSelectedPaths) => {
            const siblings = MetadataUtils.getSiblingsOrdered(
              editor.jsxMetadata,
              editor.elementPathTree,
              path,
            )
            const selectedSiblings = allSelectedPaths.filter((p) =>
              siblings.includes(editor.jsxMetadata[EP.toString(p)]),
            )

            const parentPath = EP.parentPath(path)
            const parentIsFragment = MetadataUtils.isFragmentFromMetadata(
              editor.jsxMetadata[EP.toString(parentPath)],
            )
            const parentWillBeEmpty =
              MetadataUtils.getChildrenOrdered(
                editor.jsxMetadata,
                editor.elementPathTree,
                parentPath,
              ).length === selectedSiblings.length
            if (parentIsFragment && parentWillBeEmpty) {
              return parentPath
            }

            return path
          })

        const withElementDeleted = deleteElements(staticSelectedElements, editor)
        const parentsToSelect = uniqBy(
          mapDropNulls((view) => {
            const parentPath = EP.parentPath(view)
            const parent = MetadataUtils.findElementByElementPath(editor.jsxMetadata, parentPath)
            if (
              parent != null &&
              isRight(parent.element) &&
              isJSXConditionalExpression(parent.element.value)
            ) {
              const isTrueBranch = EP.toUid(view) === getUtopiaID(parent.element.value.whenTrue)

              const branchPath = withUnderlyingTarget(
                parentPath,
                withElementDeleted.projectContents,
                withElementDeleted.nodeModules.files,
                withElementDeleted.canvas.openFile?.filename ?? null,
                null,
                (_, element) => {
                  if (isJSXConditionalExpression(element) && element.uid === EP.toUid(parentPath)) {
                    return EP.appendToPath(
                      parentPath,
                      getUtopiaID(isTrueBranch ? element.whenTrue : element.whenFalse),
                    )
                  }
                  return null
                },
              )
              if (branchPath != null) {
                return branchPath
              }
            }
            return EP.isStoryboardPath(parentPath) ? null : parentPath
          }, staticSelectedElements),
          EP.pathsEqual,
        )

        return {
          ...withElementDeleted,
          selectedViews: parentsToSelect,
        }
      },
      dispatch,
    )
  },
  DELETE_VIEW: (action: DeleteView, editor: EditorModel, dispatch: EditorDispatch): EditorModel => {
    return toastOnGeneratedElementsTargeted(
      'Generated elements can only be deleted in code.',
      [action.target],
      editor,
      false,
      (e) => {
        const updatedEditor = deleteElements([action.target], e)
        const parentPath = EP.parentPath(action.target)
        const newSelection = EP.isStoryboardPath(parentPath) ? [] : [parentPath]
        return {
          ...updatedEditor,
          selectedViews: newSelection,
        }
      },
      dispatch,
    )
  },
  DUPLICATE_SELECTED: (editor: EditorModel, dispatch: EditorDispatch): EditorModel => {
    return toastOnGeneratedElementsSelected(
      'Generated elements can only be duplicated in code',
      editor,
      false,
      (e) => {
        return duplicateMany(editor.selectedViews, e)
      },
      dispatch,
    )
  },
  DUPLICATE_SPECIFIC_ELEMENTS: (
    action: DuplicateSpecificElements,
    editor: EditorModel,
    dispatch: EditorDispatch,
  ): EditorModel => {
    return toastOnGeneratedElementsTargeted(
      'Generated elements can only be duplicated in code.',
      action.paths,
      editor,
      false,
      () => {
        return duplicateMany(action.paths, editor)
      },
      dispatch,
    )
  },
  UPDATE_DUPLICATION_STATE: (action: UpdateDuplicationState, editor: EditorModel): EditorModel => {
    return {
      ...editor,
      canvas: {
        ...editor.canvas,
        duplicationState: action.duplicationState,
      },
    }
  },
  MOVE_SELECTED_TO_BACK: (editor: EditorModel): EditorModel => {
    return setZIndexOnSelected(editor, 'back')
  },
  MOVE_SELECTED_TO_FRONT: (editor: EditorModel): EditorModel => {
    return setZIndexOnSelected(editor, 'front')
  },
  MOVE_SELECTED_BACKWARD: (editor: EditorModel): EditorModel => {
    return setZIndexOnSelected(editor, 'backward')
  },
  MOVE_SELECTED_FORWARD: (editor: EditorModel): EditorModel => {
    return setZIndexOnSelected(editor, 'forward')
  },
  SELECT_COMPONENTS: (
    action: SelectComponents,
    editor: EditorModel,
    dispatch: EditorDispatch,
  ): EditorModel => {
    let newlySelectedPaths: Array<ElementPath>
    if (action.addToSelection) {
      newlySelectedPaths = action.target.reduce((working, path) => {
        return EP.addPathIfMissing(path, working)
      }, editor.selectedViews)
    } else {
      newlySelectedPaths = EP.uniqueElementPaths(action.target)
    }

    const updatedEditor: EditorModel = {
      ...editor,
      selectedViews: newlySelectedPaths,
      navigator:
        newlySelectedPaths === editor.selectedViews
          ? editor.navigator
          : updateNavigatorCollapsedState(newlySelectedPaths, editor.navigator),
      pasteTargetsToIgnore: [],
    }

    return updatedEditor
  },
  CLEAR_SELECTION: (editor: EditorModel, derived: DerivedState): EditorModel => {
    if (editor.selectedViews.length === 0) {
      return UPDATE_FNS.SET_FOCUSED_ELEMENT(setFocusedElement(null), editor, derived)
    }

    return {
      ...editor,
      selectedViews: [],
      navigator: updateNavigatorCollapsedState([], editor.navigator),
      pasteTargetsToIgnore: [],
    }
  },
  SELECT_ALL_SIBLINGS: (
    action: SelectAllSiblings,
    editor: EditorModel,
    derived: DerivedState,
  ): EditorModel => {
    const selectedElements = editor.selectedViews
    const uniqueParents = uniqBy(
      Utils.stripNulls(selectedElements.map(EP.parentPath)),
      EP.pathsEqual,
    )
    const additionalTargets = Utils.flatMapArray((uniqueParent) => {
      const children = MetadataUtils.getImmediateChildrenOrdered(
        editor.jsxMetadata,
        editor.elementPathTree,
        uniqueParent,
      )
      return children
        .map((child) => child.elementPath)
        .filter((childPath) => {
          return !EP.containsPath(childPath, selectedElements)
        })
    }, uniqueParents)

    return {
      ...editor,
      selectedViews: [...editor.selectedViews, ...additionalTargets],
      pasteTargetsToIgnore: [],
    }
  },
  UPDATE_EDITOR_MODE: (action: UpdateEditorMode, editor: EditorModel): EditorModel => {
    return setModeState(action.mode, editor)
  },
  SWITCH_EDITOR_MODE: (
    action: SwitchEditorMode,
    editor: EditorModel,
    derived: DerivedState,
  ): EditorModel => {
    // TODO this should probably be merged with UPDATE_EDITOR_MODE
    if (action.unlessMode === editor.mode.type) {
      // FIXME: this is a bit unfortunate as this action should just do what its name suggests, without additional flags.
      // For now there's not much more that we can do since the action here can be (and is) evaluated also for transient states
      // (e.g. a `textEdit` mode after an `insertMode`) created with wildcard patches.
      return editor
    }
    return setModeState(action.mode, editor)
  },
  TOGGLE_CANVAS_IS_LIVE: (editor: EditorModel, derived: DerivedState): EditorModel => {
    // same as UPDATE_EDITOR_MODE, but clears the drag state
    if (isLiveMode(editor.mode)) {
      return setModeState(EditorModes.selectMode(editor.mode.controlId), editor)
    } else {
      return setModeState(
        EditorModes.liveMode(isSelectMode(editor.mode) ? editor.mode.controlId : null),
        editor,
      )
    }
  },
  ADD_TOAST: (action: AddToast, editor: EditorModel): EditorModel => {
    return addToastToState(editor, action.toast)
  },
  UPDATE_GITHUB_OPERATIONS: (action: UpdateGithubOperations, editor: EditorModel): EditorModel => {
    const operations = [...editor.githubOperations]
    switch (action.type) {
      case 'add':
        operations.push(action.operation)
        break
      case 'remove':
        const idx = operations.indexOf(action.operation)
        if (idx >= 0) {
          operations.splice(idx, 1)
        }
        break
      default:
        const _exhaustiveCheck: never = action.type
        throw new Error('Unknown operation type.')
    }
    return {
      ...editor,
      githubOperations: operations,
    }
  },
  SET_REFRESHING_DEPENDENCIES: (
    action: SetRefreshingDependencies,
    editor: EditorModel,
  ): EditorModel => {
    return {
      ...editor,
      refreshingDependencies: action.value,
    }
  },
  REMOVE_TOAST: (action: RemoveToast, editor: EditorModel): EditorModel => {
    return removeToastFromState(editor, action.id)
  },
  TOGGLE_HIDDEN: (action: ToggleHidden, editor: EditorModel): EditorModel => {
    const targets = action.targets.length > 0 ? action.targets : editor.selectedViews
    return targets.reduce((working, target) => {
      if (working.hiddenInstances.some((element) => EP.pathsEqual(element, target))) {
        return update(working, {
          hiddenInstances: {
            $set: working.hiddenInstances.filter((element) => !EP.pathsEqual(element, target)),
          },
        })
      } else {
        return update(working, {
          hiddenInstances: { $set: working.hiddenInstances.concat(target) },
        })
      }
    }, editor)
  },
  RENAME_COMPONENT: (action: RenameComponent, editor: EditorModel): EditorModel => {
    const { name } = action
    const target = action.target
    let propsTransform: (props: JSXAttributes) => Either<string, JSXAttributes>
    if (name == null) {
      propsTransform = (props) => unsetJSXValueAtPath(props, PathForSceneDataLabel)
    } else {
      propsTransform = (props) =>
        setJSXValueAtPath(props, PathForSceneDataLabel, jsExpressionValue(name, emptyComments))
    }
    return modifyOpenJsxElementAtPath(
      target,
      (element) => {
        const updatedElementProps = propsTransform(element.props)
        return foldEither(
          () => element,
          (elementProps) => {
            return {
              ...element,
              props: elementProps,
            }
          },
          updatedElementProps,
        )
      },
      editor,
    )
  },
  INSERT_JSX_ELEMENT: (action: InsertJSXElement, editor: EditorModel): EditorModel => {
    let newSelectedViews: ElementPath[] = []
    const parentPath =
      action.parent ??
      forceNotNull(
        'found no element path for the storyboard root',
        getStoryboardElementPath(editor.projectContents, editor.canvas.openFile?.filename),
      )
    const withNewElement = modifyUnderlyingTargetElement(
      parentPath,
      forceNotNull('Should originate from a designer', editor.canvas.openFile?.filename),
      editor,
      (element) => element,
      (success, _, underlyingFilePath) => {
        const utopiaComponents = getUtopiaJSXComponentsFromSuccess(success)
        const targetParent =
          action.parent == null
            ? // action.parent == null means Canvas, which means storyboard root element
              getStoryboardElementPath(
                editor.projectContents,
                editor.canvas.openFile?.filename ?? null,
              )
            : action.parent

        if (targetParent == null) {
          // This means there is no storyboard element to add it to
          return success
        }

        const withInsertedElement = insertElementAtPath(
          editor.projectContents,
          childInsertionPath(targetParent),
          action.jsxElement,
          utopiaComponents,
          null,
        )

        const uid = getUtopiaID(action.jsxElement)
        const newPath = EP.appendToPath(targetParent, uid)
        newSelectedViews.push(newPath)

        const updatedTopLevelElements = applyUtopiaJSXComponentsChanges(
          success.topLevelElements,
          withInsertedElement.components,
        )

        const updatedImports = mergeImports(
          underlyingFilePath,
          success.imports,
          mergeImports(underlyingFilePath, action.importsToAdd, withInsertedElement.importsToAdd),
        )
        return {
          ...success,
          topLevelElements: updatedTopLevelElements,
          imports: updatedImports,
        }
      },
    )
    return {
      ...withNewElement,
      selectedViews: newSelectedViews,
    }
  },
  WRAP_IN_ELEMENT: (
    action: WrapInElement,
    editorForAction: EditorModel,
    derived: DerivedState,
    dispatch: EditorDispatch,
    builtInDependencies: BuiltInDependencies,
  ): EditorModel => {
    return toastOnGeneratedElementsSelected(
      `Generated elements can't be wrapped into other elements.`,
      editorForAction,
      false,
      (editor) => {
        const orderedActionTargets = getZIndexOrderedViewsWithoutDirectChildren(
          action.targets,
          derived.navigatorTargets,
        ).reverse() // for some reason WRAP_IN_ELEMENT needs a reversed array where the first element is going to end up inserted as the last child

        const parentPath = commonInsertionPathFromArray(
          editorForAction.jsxMetadata,
          orderedActionTargets.map((actionTarget) => {
            return MetadataUtils.getReparentTargetOfTarget(
              editorForAction.jsxMetadata,
              actionTarget,
            )
          }),
          'replace',
        )
        if (parentPath == null) {
          return editor
        }
        // If any of the targets are a root element, we check that the parentPath is its parent
        // If not, we bail and do nothing
        // If it is, we add the new element as the root element of the parent instance
        const anyTargetIsARootElement = orderedActionTargets.some(EP.isRootElementOfInstance)
        const targetThatIsRootElementOfCommonParent = orderedActionTargets.find(
          (elementPath) =>
            EP.isRootElementOfInstance(elementPath) &&
            EP.isParentOf(getElementPathFromInsertionPath(parentPath), elementPath),
        )

        if (anyTargetIsARootElement) {
          const showToastAction = showToast(
            notice(`Root elements can't be wrapped into other elements.`),
          )
          return UPDATE_FNS.ADD_TOAST(showToastAction, editor)
        }

        const detailsOfUpdate = null
        const { updatedEditor, newPath } = wrapElementInsertions(
          editor,
          action.targets,
          parentPath,
          action.whatToWrapWith.element,
          action.whatToWrapWith.importsToAdd,
          anyTargetIsARootElement,
          targetThatIsRootElementOfCommonParent,
        )
        if (newPath == null) {
          return editor
        }

        // TODO maybe update frames and position
        const frameChanges: Array<PinOrFlexFrameChange> = []
        const withWrapperViewAdded = {
          ...setCanvasFramesInnerNew(
            includeToast(detailsOfUpdate, updatedEditor),
            frameChanges,
            null,
          ),
        }

        const indexPosition: IndexPosition = {
          type: 'back',
        }

        const insertionPath = isJSXConditionalExpression(action.whatToWrapWith.element)
          ? conditionalClauseInsertionPath(newPath, 'true-case', 'wrap-with-fragment')
          : childInsertionPath(newPath)

        const withElementsAdded = editorMoveMultiSelectedTemplates(
          builtInDependencies,
          orderedActionTargets,
          indexPosition,
          insertionPath,
          includeToast(detailsOfUpdate, withWrapperViewAdded),
        )

        return {
          ...withElementsAdded.editor,
          selectedViews: Utils.maybeToArray(newPath),
          highlightedViews: [],
          trueUpGroupsForElementAfterDomWalkerRuns: [
            ...withElementsAdded.editor.trueUpGroupsForElementAfterDomWalkerRuns,
            ...withElementsAdded.newPaths,
          ],
        }
      },
      dispatch,
    )
  },
  OPEN_FLOATING_INSERT_MENU: (action: OpenFloatingInsertMenu, editor: EditorModel): EditorModel => {
    if (action.mode.insertMenuMode !== 'closed' && editor.selectedViews.length === 0) {
      const showToastAction = showToast(notice(`There are no elements selected`, 'WARNING'))
      return UPDATE_FNS.ADD_TOAST(showToastAction, editor)
    }
    return {
      ...editor,
      floatingInsertMenu: action.mode,
    }
  },
  CLOSE_FLOATING_INSERT_MENU: (
    action: CloseFloatingInsertMenu,
    editor: EditorModel,
  ): EditorModel => {
    return {
      ...editor,
      floatingInsertMenu: {
        insertMenuMode: 'closed',
      },
    }
  },
  UNWRAP_ELEMENT: (
    action: UnwrapElement,
    editorForAction: EditorModel,
    dispatch: EditorDispatch,
    builtInDependencies: BuiltInDependencies,
  ): EditorModel => {
    return toastOnGeneratedElementsSelected(
      `Cannot unwrap a generated element.`,
      editorForAction,
      false,
      (editor) => {
        const supportsChildren = MetadataUtils.targetSupportsChildren(
          editor.projectContents,
          editor.jsxMetadata,
          editor.nodeModules.files,
          editor.canvas.openFile?.filename,
          action.target,
          editor.elementPathTree,
        )

        const elementIsFragmentLike = treatElementAsFragmentLike(
          editor.jsxMetadata,
          editor.allElementProps,
          editor.elementPathTree,
          action.target,
        )

        if (!(supportsChildren || elementIsFragmentLike)) {
          return editor
        }

        const parentPath = MetadataUtils.getReparentTargetOfTarget(
          editorForAction.jsxMetadata,
          action.target,
        )

        const indexPosition: IndexPosition = indexPositionForAdjustment(
          action.target,
          editor,
          'forward',
        )
        const children = MetadataUtils.getChildrenOrdered(
          editor.jsxMetadata,
          editor.elementPathTree,
          action.target,
        ).reverse() // children are reversed so when they are readded one by one as 'forward' index they keep their original order

        const isGroupChild = treatElementAsGroupLike(
          editor.jsxMetadata,
          EP.parentPath(action.target),
        )

        if (parentPath != null && isConditionalClauseInsertionPath(parentPath)) {
          return unwrapConditionalClause(editor, action.target, parentPath)
        }

        if (elementIsFragmentLike) {
          if (isTextContainingConditional(action.target, editor.jsxMetadata)) {
            return unwrapTextContainingConditional(editor, action.target, dispatch)
          }

          const { editor: withChildrenMoved, newPaths } = editorMoveMultiSelectedTemplates(
            builtInDependencies,
            children.map((child) => child.elementPath),
            indexPosition,
            parentPath,
            editor,
          )
          const withViewDeleted = deleteElements([action.target], withChildrenMoved)

          return {
            ...withViewDeleted,
            selectedViews: newPaths,
            canvas: {
              ...withViewDeleted.canvas,
              domWalkerInvalidateCount: editor.canvas.domWalkerInvalidateCount + 1,
            },
          }
        } else {
          const parentFrame =
            parentPath == null
              ? (Utils.zeroRectangle as CanvasRectangle)
              : MetadataUtils.getFrameOrZeroRectInCanvasCoords(
                  parentPath.intendedParentPath,
                  editor.jsxMetadata,
                )

          let groupTrueUps: ElementPath[] = []
          if (isGroupChild && parentPath != null) {
            groupTrueUps.push(parentPath.intendedParentPath)
          }

          let newSelection: ElementPath[] = []
          const withChildrenMoved = children.reduce((working, child) => {
            const childFrame = MetadataUtils.getFrameOrZeroRectInCanvasCoords(
              child.elementPath,
              editor.jsxMetadata,
            )
            const result = editorMoveTemplate(
              child.elementPath,
              child.elementPath,
              childFrame,
              indexPosition,
              parentPath?.intendedParentPath ?? null,
              parentFrame,
              working,
              null,
              null,
            )
            if (result.newPath != null) {
              newSelection.push(result.newPath)
              if (isGroupChild) {
                groupTrueUps.push(result.newPath)
                return foldAndApplyCommandsSimple(
                  result.editor,
                  createPinChangeCommandsForElementBecomingGroupChild(
                    child,
                    result.newPath,
                    parentFrame,
                    localRectangle(parentFrame),
                  ),
                )
              }
            }
            return result.editor
          }, editor)
          const withViewDeleted = deleteElements([action.target], withChildrenMoved)

          return {
            ...withViewDeleted,
            selectedViews: newSelection,
            canvas: {
              ...withViewDeleted.canvas,
              domWalkerInvalidateCount: editor.canvas.domWalkerInvalidateCount + 1,
              trueUpGroupsForElementAfterDomWalkerRuns: [
                ...withViewDeleted.trueUpGroupsForElementAfterDomWalkerRuns,
                ...groupTrueUps,
              ],
            },
          }
        }
      },
      dispatch,
    )
  },
  SET_PANEL_VISIBILITY: (action: SetPanelVisibility, editor: EditorModel): EditorModel => {
    switch (action.target) {
      case 'leftmenu':
        return {
          ...editor,
          leftMenu: {
            ...editor.leftMenu,
            expanded: action.visible,
          },
        }
      case 'navigator':
        return {
          ...editor,
          navigator: {
            ...editor.navigator,
            minimised: !action.visible,
          },
        }
      case 'filebrowser':
        return {
          ...editor,
          fileBrowser: {
            ...editor.fileBrowser,
            minimised: !action.visible,
          },
        }
      case 'dependencylist':
        return {
          ...editor,
          dependencyList: {
            ...editor.dependencyList,
            minimised: !action.visible,
          },
        }
      case 'genericExternalResources':
        return {
          ...editor,
          genericExternalResources: {
            ...editor.dependencyList,
            minimised: !action.visible,
          },
        }
      case 'googleFontsResources':
        return {
          ...editor,
          googleFontsResources: {
            ...editor.dependencyList,
            minimised: !action.visible,
          },
        }
      case 'inspector':
        return {
          ...editor,
          inspector: {
            ...editor.inspector,
            visible: action.visible,
          },
        }
      case 'rightmenu':
        return {
          ...editor,
          rightMenu: {
            ...editor.rightMenu,
            expanded: action.visible,
          },
        }
      case 'preview':
        return {
          ...editor,
          preview: {
            ...editor.preview,
            visible: action.visible,
          },
        }
      case 'canvas':
        return {
          ...editor,
          canvas: {
            ...editor.canvas,
            visible: action.visible,
          },
        }
      case 'codeEditor':
        return {
          ...editor,
          interfaceDesigner: {
            ...editor.interfaceDesigner,
            codePaneVisible: action.visible,
            codePaneWidth: Math.max(
              MIN_CODE_PANE_REOPEN_WIDTH,
              editor.interfaceDesigner.codePaneWidth,
            ),
          },
        }
      case 'misccodeeditor':
      case 'center':
      case 'insertmenu':
      case 'projectsettings':
      case 'githuboptions':
        return editor
      default:
        const _exhaustiveCheck: never = action.target
        return editor
    }
  },
  TOGGLE_FOCUSED_OMNIBOX_TAB: (editor: EditorModel): EditorModel => {
    return {
      ...editor,
      topmenu: {
        ...editor.topmenu,
        formulaBarMode: editor.topmenu.formulaBarMode === 'css' ? 'content' : 'css',
      },
    }
  },
  TOGGLE_PANE: (action: TogglePane, editor: EditorModel): EditorModel => {
    switch (action.target) {
      case 'leftmenu':
        return {
          ...editor,
          leftMenu: {
            ...editor.leftMenu,
            expanded: !editor.leftMenu.expanded,
          },
        }
      case 'rightmenu':
        return {
          ...editor,
          rightMenu: {
            ...editor.rightMenu,
            expanded: !editor.rightMenu.expanded,
          },
        }
      case 'dependencylist':
        return {
          ...editor,
          dependencyList: {
            ...editor.dependencyList,
            minimised: !editor.dependencyList.minimised,
          },
        }
      case 'genericExternalResources':
        return {
          ...editor,
          genericExternalResources: {
            ...editor.genericExternalResources,
            minimised: !editor.genericExternalResources.minimised,
          },
        }
      case 'googleFontsResources':
        return {
          ...editor,
          googleFontsResources: {
            ...editor.googleFontsResources,
            minimised: !editor.googleFontsResources.minimised,
          },
        }
      case 'filebrowser':
        return {
          ...editor,
          fileBrowser: {
            ...editor.fileBrowser,
            minimised: !editor.fileBrowser.minimised,
          },
        }
      case 'navigator':
        return {
          ...editor,
          navigator: {
            ...editor.navigator,
            minimised: !editor.navigator.minimised,
          },
        }
      case 'inspector':
        return {
          ...editor,
          inspector: {
            ...editor.inspector,
            visible: !editor.inspector.visible,
          },
        }
      case 'preview':
        return {
          ...editor,
          preview: {
            ...editor.preview,
            visible: !editor.preview.visible,
          },
        }
      case 'canvas':
        if (!editor.canvas.visible) {
          return {
            ...editor,
            canvas: {
              ...editor.canvas,
              visible: !editor.canvas.visible,
            },
          }
        }
        return editor
      case 'projectsettings':
        return {
          ...editor,
          projectSettings: {
            ...editor.projectSettings,
            minimised: !editor.projectSettings.minimised,
          },
        }

      case 'codeEditor':
        return updateCodeEditorVisibility(editor, !editor.interfaceDesigner.codePaneVisible)
      case 'misccodeeditor':
      case 'center':
      case 'insertmenu':
      case 'githuboptions':
        return editor
      default:
        const _exhaustiveCheck: never = action.target
        return editor
    }
  },
  TOGGLE_INTERFACEDESIGNER_ADDITIONAL_CONTROLS: (
    action: ToggleInterfaceDesignerAdditionalControls,
    editor: EditorModel,
  ): EditorModel => {
    return {
      ...editor,
      interfaceDesigner: {
        ...editor.interfaceDesigner,
        additionalControls: !editor.interfaceDesigner.additionalControls,
      },
    }
  },

  OPEN_POPUP: (action: OpenPopup, editor: EditorModel): EditorModel => {
    return update(editor, {
      openPopupId: { $set: action.popupId },
    })
  },
  CLOSE_POPUP: (action: ClosePopup, editor: EditorModel): EditorModel => {
    if (editor.openPopupId == null) {
      return editor
    }

    return update(editor, {
      openPopupId: { $set: null },
    })
  },
  PASTE_PROPERTIES: (action: PasteProperties, editor: EditorModel): EditorModel => {
    if (editor.internalClipboard.styleClipboard.length === 0) {
      return editor
    }
    return editor.selectedViews.reduce((working, target) => {
      return setPropertyOnTarget(working, target, (attributes) => {
        const filterForNames = action.type === 'layout' ? LayoutPropsWithoutTLBR : StyleProperties
        const originalPropsToUnset = filterForNames.map((propName) => PP.create('style', propName))
        const withOriginalPropertiesCleared = unsetJSXValuesAtPaths(
          attributes,
          originalPropsToUnset,
        )

        const propsToSet = editor.internalClipboard.styleClipboard.filter(
          (styleClipboardData: ValueAtPath) => {
            const propName = PP.lastPartToString(styleClipboardData.path)
            return filterForNames.includes(propName) ? styleClipboardData : null
          },
        )

        return foldEither(
          () => {
            return right(attributes)
          },
          (withPropertiesCleared) => {
            return setJSXValuesAtPaths(withPropertiesCleared, propsToSet)
          },
          withOriginalPropertiesCleared,
        )
      })
    }, editor)
  },
  COPY_SELECTION_TO_CLIPBOARD: (
    action: CopySelectionToClipboard,
    editor: EditorModel,
    dispatch: EditorDispatch,
    builtInDependencies: BuiltInDependencies,
  ): EditorModel => {
    const canReparent = traverseEither(
      (target) => canCopyElement(editor, target),
      editor.selectedViews,
    )

    if (isLeft(canReparent)) {
      const showToastAction = showToast(notice(canReparent.value))
      return UPDATE_FNS.ADD_TOAST(showToastAction, editor)
    }

    return copySelectionToClipboardMutating(editor, builtInDependencies)
  },
  CUT_SELECTION_TO_CLIPBOARD: (
    editor: EditorModel,
    dispatch: EditorDispatch,
    builtInDependencies: BuiltInDependencies,
  ): EditorModel => {
    const canReparent = traverseEither(
      (target) => canCopyElement(editor, target),
      editor.selectedViews,
    )

    if (isLeft(canReparent)) {
      const showToastAction = showToast(notice(canReparent.value))
      return UPDATE_FNS.ADD_TOAST(showToastAction, editor)
    }

    const editorWithCopyData = copySelectionToClipboardMutating(editor, builtInDependencies)

    return UPDATE_FNS.DELETE_SELECTED(editorWithCopyData, dispatch)
  },
  COPY_PROPERTIES: (action: CopyProperties, editor: EditorModel): EditorModel => {
    if (editor.selectedViews.length === 0) {
      return editor
    } else {
      const target = editor.selectedViews[0]
      const styleProps = editor._currentAllElementProps_KILLME[EP.toString(target)]?.style ?? {}
      const styleClipboardData = Object.keys(styleProps).map((name) =>
        valueAtPath(PP.create('style', name), jsExpressionValue(styleProps[name], emptyComments)),
      )
      return {
        ...editor,
        internalClipboard: {
          styleClipboard: styleClipboardData,
          elements: [],
        },
      }
    }
  },
  OPEN_TEXT_EDITOR: (action: OpenTextEditor, editor: EditorModel): EditorModel => {
    return {
      ...editor,
      canvas: {
        ...editor.canvas,
        textEditor: {
          elementPath: action.target,
          triggerMousePosition: action.mousePosition,
        },
      },
    }
  },
  CLOSE_TEXT_EDITOR: (action: CloseTextEditor, editor: EditorModel): EditorModel => {
    return update(editor, {
      canvas: {
        textEditor: {
          $set: null,
        },
      },
    })
  },
  SET_LEFT_MENU_TAB: (action: SetLeftMenuTab, editor: EditorModel): EditorModel => {
    let result: EditorModel = updateSelectedLeftMenuTab(editor, action.tab)
    // Expand the menu if it's not already visible.
    if (!result.leftMenu.expanded || result.leftMenu.paneWidth <= LeftPaneMinimumWidth) {
      result = {
        ...result,
        leftMenu: {
          ...result.leftMenu,
          paneWidth: LeftPaneDefaultWidth,
        },
      }
    }
    return result
  },
  SET_LEFT_MENU_EXPANDED: (action: SetLeftMenuExpanded, editor: EditorModel): EditorModel => {
    return updateLeftMenuExpanded(editor, action.expanded)
  },
  SET_RIGHT_MENU_TAB: (action: SetRightMenuTab, editor: EditorModel): EditorModel => {
    return updateSelectedRightMenuTab(editor, action.tab)
  },
  SET_RIGHT_MENU_EXPANDED: (action: SetRightMenuExpanded, editor: EditorModel): EditorModel => {
    return updateRightMenuExpanded(editor, action.expanded)
  },

  TOGGLE_COLLAPSE: (action: ToggleCollapse, editor: EditorModel): EditorModel => {
    if (editor.navigator.collapsedViews.some((element) => EP.pathsEqual(element, action.target))) {
      return {
        ...editor,
        navigator: NavigatorStateKeepDeepEquality(editor.navigator, {
          ...editor.navigator,
          collapsedViews: editor.navigator.collapsedViews.filter(
            (element) => !EP.pathsEqual(element, action.target),
          ),
        }).value,
      }
    } else {
      return {
        ...editor,
        navigator: NavigatorStateKeepDeepEquality(editor.navigator, {
          ...editor.navigator,
          collapsedViews: editor.navigator.collapsedViews.concat(action.target),
        }).value,
      }
    }
  },
  UPDATE_KEYS_PRESSED: (action: UpdateKeysPressed, editor: EditorModel): EditorModel => {
    if (Utils.shallowEqual(action.keys, editor.keysPressed)) {
      return editor
    }

    return update(editor, {
      keysPressed: { $set: action.keys },
    })
  },
  UPDATE_MOUSE_BUTTONS_PRESSED: (
    action: UpdateMouseButtonsPressed,
    editor: EditorModel,
  ): EditorModel => {
    let mouseButtonsPressed: MouseButtonsPressed = editor.mouseButtonsPressed
    if (action.added != null) {
      mouseButtonsPressed = addButtonPressed(mouseButtonsPressed, action.added)
    }
    if (action.removed != null) {
      mouseButtonsPressed = removeButtonPressed(mouseButtonsPressed, action.removed)
    }
    return {
      ...editor,
      mouseButtonsPressed: mouseButtonsPressed,
    }
  },
  HIDE_MODAL: (action: HideModal, editor: EditorModel): EditorModel => {
    return update(editor, {
      modal: { $set: null },
    })
  },
  SHOW_MODAL: (action: ShowModal, editor: EditorModel): EditorModel => {
    return update(editor, {
      modal: { $set: action.modal },
    })
  },
  RESIZE_INTERFACEDESIGNER_CODEPANE: (
    action: ResizeInterfaceDesignerCodePane,
    editor: EditorModel,
    dispatch: EditorDispatch,
  ): EditorModel => {
    // resulting pane needs to have a width of 2 so it can be resized-to-open
    const minWidth = 2
    const hideWidth = 20

    const priorWidth = editor.interfaceDesigner.codePaneWidth
    const targetWidth = editor.interfaceDesigner.codePaneWidth + action.deltaCodePaneWidth

    const shouldShowToast = targetWidth < hideWidth && priorWidth > minWidth
    const updatedEditor = shouldShowToast
      ? UPDATE_FNS.ADD_TOAST(showToast(notice('Code editor hidden')), editor)
      : editor

    return {
      ...updatedEditor,
      interfaceDesigner: {
        ...editor.interfaceDesigner,
        codePaneVisible: targetWidth < hideWidth ? false : true,
        codePaneWidth: targetWidth < hideWidth ? minWidth : targetWidth,
      },
    }
  },
  RESIZE_LEFTPANE: (action: ResizeLeftPane, editor: EditorModel): EditorModel => {
    const priorWidth = editor.leftMenu.paneWidth
    const targetWidth = priorWidth + action.deltaPaneWidth
    return {
      ...editor,
      leftMenu: {
        ...editor.leftMenu,
        paneWidth: Math.max(LeftPaneMinimumWidth, targetWidth),
      },
    }
  },
  TOGGLE_INTERFACEDESIGNER_CODEEDITOR: (
    action: ToggleInterfaceDesignerCodeEditor,
    editor: EditorModel,
    dispatch: EditorDispatch,
  ): EditorModel => {
    // resulting pane needs to have a width of 2 so it can be resized-to-open
    const minWidth = 2
    const codeEditorVisibleAfter = !editor.interfaceDesigner.codePaneVisible

    const updatedEditor = codeEditorVisibleAfter
      ? editor
      : UPDATE_FNS.ADD_TOAST(showToast(notice('Code editor hidden')), editor)

    return {
      ...updatedEditor,
      interfaceDesigner: {
        ...editor.interfaceDesigner,
        codePaneVisible: codeEditorVisibleAfter,
        codePaneWidth: codeEditorVisibleAfter
          ? editor.interfaceDesigner.restorableCodePaneWidth
          : minWidth,
        restorableCodePaneWidth: codeEditorVisibleAfter
          ? editor.interfaceDesigner.restorableCodePaneWidth
          : editor.interfaceDesigner.codePaneWidth,
      },
    }
  },
  RESET_PINS: (action: ResetPins, editor: EditorModel, dispatch: EditorDispatch): EditorModel => {
    const target = action.target
    const frame = MetadataUtils.getFrame(target, editor.jsxMetadata)

    if (frame == null || isInfinityRectangle(frame)) {
      return editor
    }
    const commands = [
      deleteProperties('always', target, [
        PP.create('style', 'left'),
        PP.create('style', 'right'),
        PP.create('style', 'top'),
        PP.create('style', 'bottom'),
      ]),
    ]
    return foldAndApplyCommandsSimple(editor, commands)
  },
  SET_CURSOR_OVERLAY: (action: SetCursorOverlay, editor: EditorModel): EditorModel => {
    if (editor.canvas.cursor === action.cursor) {
      return editor
    }
    return {
      ...editor,
      canvas: {
        ...editor.canvas,
        cursor: action.cursor,
      },
    }
  },
  UPDATE_FRAME_DIMENSIONS: (
    action: UpdateFrameDimensions,
    editor: EditorModel,
    derived: DerivedState,
  ): EditorModel => {
    const initialFrame = MetadataUtils.getFrame(action.element, editor.jsxMetadata)

    if (initialFrame == null || isInfinityRectangle(initialFrame)) {
      return editor
    }

    let frame = {
      x: initialFrame.x,
      y: initialFrame.y,
      width: action.width,
      height: action.height,
    } as LocalRectangle

    const parentPath = EP.parentPath(action.element)
    let offset = { x: 0, y: 0 } as CanvasPoint
    if (parentPath != null) {
      const parentFrame = MetadataUtils.getFrameInCanvasCoords(parentPath, editor.jsxMetadata)
      if (parentFrame != null && isFiniteRectangle(parentFrame)) {
        offset = { x: parentFrame.x, y: parentFrame.y } as CanvasPoint
      }
    }
    const canvasFrame = Utils.getCanvasRectangleWithCanvasOffset(offset, frame)

    const isParentFlex = MetadataUtils.isParentYogaLayoutedContainerAndElementParticipatesInLayout(
      action.element,
      editor.jsxMetadata,
    )
    const frameChanges: Array<PinOrFlexFrameChange> = [
      getFrameChange(action.element, canvasFrame, isParentFlex),
    ]
    return setCanvasFramesInnerNew(editor, frameChanges, null)
  },
  SET_NAVIGATOR_RENAMING_TARGET: (
    action: SetNavigatorRenamingTarget,
    editor: EditorModel,
  ): EditorModel => {
    return {
      ...editor,
      navigator: {
        ...editor.navigator,
        renamingTarget: action.target,
      },
    }
  },
  SET_STORED_FONT_SETTINGS: (action: SetStoredFontSettings, editor: EditorModel): EditorModel => {
    return {
      ...editor,
      lastUsedFont: action.fontSettings,
    }
  },
  SAVE_CURRENT_FILE: (action: SaveCurrentFile, editor: EditorModel): EditorModel => {
    const openFilePath = getOpenTextFileKey(editor)
    if (openFilePath != null) {
      return {
        ...editor,
        projectContents: saveFileInProjectContents(editor.projectContents, openFilePath),
      }
    }
    return editor
  },
  SAVE_ASSET: (
    action: SaveAsset,
    editor: EditorModel,
    derived: DerivedState,
    dispatch: EditorDispatch,
    userState: UserState,
  ): EditorModel => {
    const replaceImage = action.imageDetails?.afterSave.type === 'SAVE_IMAGE_REPLACE'
    const assetFilename = replaceImage
      ? action.fileName
      : uniqueProjectContentID(action.fileName, editor.projectContents)
    const notNullProjectID = Utils.forceNotNull('No project ID', editor.id)

    const width = Utils.pathOr(undefined, ['imageDetails', 'imageSize', 'width'], action)
    const height = Utils.pathOr(undefined, ['imageDetails', 'imageSize', 'height'], action)

    const imageURL = imagePathURL(assetFilename)
    const imageAttribute = jsExpressionValue(imageURL, emptyComments)

    const newUID = generateUidWithExistingComponents(editor.projectContents)
    const openUIJSFile = getOpenUIJSFileKey(editor)

    let actionsToRunAfterSave: Array<EditorAction> = []
    // Bit weird, but when replacing an image, we need to change the URLs only once the image has been saved.
    if (action.imageDetails != null) {
      if (replaceImage) {
        const imageWithoutHashURL = imagePathURL(assetFilename)
        const propertyPath = PP.create('src')
        walkContentsTreeForParseSuccess(editor.projectContents, (filePath, success) => {
          walkElements(getUtopiaJSXComponentsFromSuccess(success), (element, elementPath) => {
            if (isJSXElement(element)) {
              const srcAttribute = getJSXAttribute(element.props, 'src')
              if (srcAttribute != null && isJSXAttributeValue(srcAttribute)) {
                const srcValue: JSExpressionValue<any> = srcAttribute
                if (
                  typeof srcValue.value === 'string' &&
                  srcValue.value.startsWith(imageWithoutHashURL)
                ) {
                  // Balazs: I think this code was already dormant / broken, keeping it as a comment for reference
                  // actionsToRunAfterSave.push(
                  //   setPropWithElementPath_UNSAFE(elementPath, propertyPath, imageAttribute),
                  // )
                }
              }
            }
          })
        })
      } else {
        if (action.imageDetails.afterSave.type !== 'SAVE_IMAGE_DO_NOTHING') {
          actionsToRunAfterSave.push(
            clearImageFileBlob(Utils.forceNotNull('Need an open UI file.', openUIJSFile), newUID),
          )
        }
      }
    }

    let projectFile: ProjectFile
    if (action.imageDetails == null) {
      // Assume stock ASSET_FILE case when there's no image details.
      projectFile = assetFile(undefined)
    } else {
      // Assume IMAGE_FILE otherwise.
      projectFile = imageFile(action.fileType, undefined, width, height, action.hash)
    }
    actionsToRunAfterSave.push(updateFile(assetFilename, projectFile, true))

    // Side effects.
    let editorWithToast = editor
    if (isLoggedIn(userState.loginState) && editor.id != null) {
      saveAssetToServer(notNullProjectID, action.fileType, action.base64, assetFilename)
        .then((checksum) => {
          dispatch(
            [
              ...actionsToRunAfterSave,
              showToast(notice(`Succesfully uploaded ${assetFilename}`, 'INFO')),
            ],
            'everyone',
          )
        })
        .catch(() => {
          dispatch([showToast(notice(`Failed to upload ${assetFilename}`, 'ERROR'))])
        })
    } else {
      editorWithToast = UPDATE_FNS.ADD_TOAST(
        showToast(notice(`Please log in to upload assets`, 'ERROR', true)),
        editor,
      )
    }

    const updatedProjectContents = addFileToProjectContents(
      editor.projectContents,
      assetFilename,
      projectFile,
    )

    let updatedBlobs: CanvasBase64Blobs = editor.canvas.base64Blobs

    if (openUIJSFile != null) {
      const existingFileBlobs = Utils.defaultIfNull<UIFileBase64Blobs>(
        {},
        editor.canvas.base64Blobs[action.fileName],
      )
      const updatedFileBlobs: UIFileBase64Blobs = {
        ...existingFileBlobs,
        [newUID]: {
          base64: action.base64,
        },
      }

      updatedBlobs = {
        ...editor.canvas.base64Blobs,
        [openUIJSFile]: updatedFileBlobs,
      }
    }

    if (action.imageDetails == null) {
      return editor
    } else {
      switch (action.imageDetails.afterSave.type) {
        case 'SAVE_IMAGE_SWITCH_MODE': {
          // TODO make a default image and put it in defaults
          const imageElement = jsxElement(
            jsxElementName('img', []),
            newUID,
            jsxAttributesFromMap({
              alt: jsExpressionValue('', emptyComments),
              src: imageAttribute,
              style: jsExpressionValue({ width: width, height: height }, emptyComments),
              'data-uid': jsExpressionValue(newUID, emptyComments),
              [AspectRatioLockedProp]: jsExpressionValue(true, emptyComments),
            }),
            [],
          )
          const size = width != null && height != null ? { width: width, height: height } : null
          const switchMode = enableInsertModeForJSXElement(imageElement, newUID, {}, size)
          const editorInsertEnabled = UPDATE_FNS.SWITCH_EDITOR_MODE(
            switchMode,
            editorWithToast,
            derived,
          )
          return {
            ...editorInsertEnabled,
            projectContents: updatedProjectContents,
            canvas: {
              ...editorInsertEnabled.canvas,
              base64Blobs: updatedBlobs,
            },
          }
        }
        case 'SAVE_IMAGE_INSERT_WITH': {
          const parent =
            action.imageDetails.afterSave.parentPath == null
              ? null
              : MetadataUtils.resolveReparentTargetParentToPath(
                  editor.jsxMetadata,
                  action.imageDetails.afterSave.parentPath,
                )
          const relativeFrame = MetadataUtils.getFrameRelativeTo(
            parent,
            editor.jsxMetadata,
            action.imageDetails.afterSave.frame,
          )

          const imageElement = jsxElement(
            jsxElementName('img', []),
            newUID,
            jsxAttributesFromMap({
              alt: jsExpressionValue('', emptyComments),
              src: imageAttribute,
              style: MetadataUtils.isFlexLayoutedContainer(
                MetadataUtils.findElementByElementPath(editor.jsxMetadata, parent),
              )
                ? jsExpressionValue(
                    {
                      width: relativeFrame.width,
                      height: relativeFrame.height,
                    },
                    emptyComments,
                  )
                : jsExpressionValue(
                    {
                      position: 'absolute',
                      left: relativeFrame.x,
                      top: relativeFrame.y,
                      width: relativeFrame.width,
                      height: relativeFrame.height,
                    },
                    emptyComments,
                  ),
              'data-uid': jsExpressionValue(newUID, emptyComments),
              [AspectRatioLockedProp]: jsExpressionValue(true, emptyComments),
            }),
            [],
          )

          const insertJSXElementAction = insertJSXElement(imageElement, parent, {})

          const withComponentCreated = UPDATE_FNS.INSERT_JSX_ELEMENT(insertJSXElementAction, {
            ...editorWithToast,
            projectContents: updatedProjectContents,
          })
          return {
            ...withComponentCreated,
            projectContents: withComponentCreated.projectContents,
            canvas: {
              ...withComponentCreated.canvas,
              base64Blobs: updatedBlobs,
            },
          }
        }
        case 'SAVE_IMAGE_REPLACE':
          return editorWithToast
        case 'SAVE_IMAGE_DO_NOTHING':
          return editorWithToast
      }
    }
  },
  INSERT_IMAGE_INTO_UI: (
    action: InsertImageIntoUI,
    editor: EditorModel,
    derived: DerivedState,
  ): EditorModel => {
    const possiblyAnImage = getContentsTreeFileFromString(editor.projectContents, action.imagePath)
    if (possiblyAnImage != null && isImageFile(possiblyAnImage)) {
      const newUID = generateUidWithExistingComponents(editor.projectContents)
      const imageURL = imagePathURL(action.imagePath)
      const imageSrcAttribute = jsExpressionValue(imageURL, emptyComments)
      const width = Utils.optionalMap((w) => w / 2, possiblyAnImage.width)
      const height = Utils.optionalMap((h) => h / 2, possiblyAnImage.height)
      const imageElement = jsxElement(
        jsxElementName('img', []),
        newUID,
        jsxAttributesFromMap({
          alt: jsExpressionValue('', emptyComments),
          src: imageSrcAttribute,
          style: jsExpressionValue(
            {
              width: width,
              height: height,
            },
            emptyComments,
          ),
          'data-uid': jsExpressionValue(newUID, emptyComments),
          'data-label': jsExpressionValue('Image', emptyComments),
          [AspectRatioLockedProp]: jsExpressionValue(true, emptyComments),
        }),
        [],
      )
      const size = width != null && height != null ? { width: width, height: height } : null
      const switchMode = enableInsertModeForJSXElement(imageElement, newUID, {}, size)
      return UPDATE_FNS.SWITCH_EDITOR_MODE(switchMode, editor, derived)
    } else {
      return editor
    }
  },
  SET_PROJECT_ID: (
    action: SetProjectID,
    editor: EditorModel,
    dispatch: EditorDispatch,
  ): EditorModel => {
    let vscodeBridgeId = editor.vscodeBridgeId
    if (vscodeBridgeId.type === 'VSCODE_BRIDGE_ID_DEFAULT') {
      vscodeBridgeId = vsCodeBridgeIdProjectId(action.id)
      // Side effect.
      initVSCodeBridge(editor.projectContents, dispatch, editor.canvas.openFile?.filename ?? null)
    }
    return {
      ...editor,
      id: action.id,
      vscodeBridgeId: vscodeBridgeId,
    }
  },
  UPDATE_CODE_RESULT_CACHE: (action: UpdateCodeResultCache, editor: EditorModel): EditorModel => {
    return {
      ...editor,
      codeResultCache: {
        skipDeepFreeze: true,
        cache: {
          ...editor.codeResultCache.cache,
          ...action.codeResultCache.cache,
        },
        exportsInfo: action.codeResultCache.exportsInfo,
        error: action.codeResultCache.error,
        curriedRequireFn: action.codeResultCache.curriedRequireFn,
        curriedResolveFn: action.codeResultCache.curriedResolveFn,
        projectModules: action.codeResultCache.projectModules,
        evaluationCache: action.codeResultCache.evaluationCache,
      },
    }
  },
  SET_CODE_EDITOR_VISIBILITY: (
    action: SetCodeEditorVisibility,
    editor: EditorModel,
  ): EditorModel => {
    return {
      ...editor,
      codeEditingEnabled: action.value,
    }
  },
  OPEN_CODE_EDITOR: (editor: EditorModel): EditorModel => {
    return updateCodeEditorVisibility(editor, true)
  },
  SET_PROJECT_NAME: (action: SetProjectName, editor: EditorModel): EditorModel => {
    return {
      ...editor,
      projectName: action.name,
    }
  },

  SET_PROJECT_DESCRIPTION: (action: SetProjectDescription, editor: EditorModel): EditorModel => {
    return {
      ...editor,
      projectDescription: action.description,
    }
  },

  REGENERATE_THUMBNAIL: (
    action: RegenerateThumbnail,
    editor: EditorModel,
    dispatch: EditorDispatch,
  ): EditorModel => {
    if (editor.id != null) {
      void updateRemoteThumbnail(editor.id, true).then(() => {
        dispatch([updateThumbnailGenerated(new Date().getTime())], 'everyone')
      })
    }
    return editor
  },

  UPDATE_THUMBNAIL_GENERATED: (
    action: UpdateThumbnailGenerated,
    editor: EditorModel,
  ): EditorModel => {
    return {
      ...editor,
      thumbnailLastGenerated: action.timestamp,
    }
  },

  UPDATE_PREVIEW_CONNECTED: (action: UpdatePreviewConnected, editor: EditorModel): EditorModel => {
    return produce(editor, (editorState) => {
      editorState.preview.connected = action.connected
    })
  },
  ALIGN_SELECTED_VIEWS: (
    action: AlignSelectedViews,
    editor: EditorModel,
    derived: DerivedState,
  ): EditorModel => {
    return alignOrDistributeSelectedViews(editor, derived, action.alignment)
  },
  DISTRIBUTE_SELECTED_VIEWS: (
    action: DistributeSelectedViews,
    editor: EditorModel,
    derived: DerivedState,
  ): EditorModel => {
    return alignOrDistributeSelectedViews(editor, derived, action.distribution)
  },
  SHOW_CONTEXT_MENU: (action: ShowContextMenu, editor: EditorModel): EditorModel => {
    // side effect!
    openMenu(action.menuName, action.event)
    return editor
  },
  SEND_PREVIEW_MODEL: (action: SendPreviewModel, editor: EditorModel): EditorModel => {
    return editor
  },
  UPDATE_FILE_PATH: (
    action: UpdateFilePath,
    editor: EditorModel,
    userState: UserState,
  ): EditorModel => {
    const replaceFilePathResults = replaceFilePath(
      action.oldPath,
      action.newPath,
      editor.projectContents,
    )
    if (replaceFilePathResults.type === 'FAILURE') {
      const toastAction = showToast(notice(replaceFilePathResults.errorMessage, 'ERROR', true))
      return UPDATE_FNS.ADD_TOAST(toastAction, editor)
    } else {
      let currentDesignerFile = editor.canvas.openFile
      const { projectContents, updatedFiles } = replaceFilePathResults
      const mainUIFile = getMainUIFromModel(editor)
      let updateUIFile: (e: EditorModel) => EditorModel = (e) => e
      Utils.fastForEach(updatedFiles, (updatedFile) => {
        const { oldPath, newPath } = updatedFile
        // If the main UI file is what we have renamed, update that later.
        if (oldPath === mainUIFile) {
          updateUIFile = (e: EditorModel) => {
            return updateMainUIInEditorState(e, newPath)
          }
        }
        // update currently open file
        if (currentDesignerFile != null && currentDesignerFile.filename === oldPath) {
          currentDesignerFile = {
            filename: newPath,
          }
        }
        const oldContent = getContentsTreeFileFromString(editor.projectContents, oldPath)
        if (oldContent != null && (isImageFile(oldContent) || isAssetFile(oldContent))) {
          // Update assets.
          if (isLoggedIn(userState.loginState) && editor.id != null) {
            void updateAssetFileName(editor.id, stripLeadingSlash(oldPath), newPath)
          }
        }
      })

      return updateUIFile({
        ...editor,
        projectContents: projectContents,
        codeEditorErrors: {
          buildErrors: {},
          lintErrors: {},
        },
        canvas: {
          ...editor.canvas,
          openFile: currentDesignerFile,
        },
      })
    }
  },
  SET_FOCUS: (action: SetFocus, editor: EditorModel): EditorModel => {
    if (editor.focusedPanel === action.focusedPanel) {
      return editor
    } else {
      return setLeftMenuTabFromFocusedPanel({
        ...editor,
        focusedPanel: action.focusedPanel,
      })
    }
  },
  OPEN_CODE_EDITOR_FILE: (action: OpenCodeEditorFile, editor: EditorModel): EditorModel => {
    // Side effect.
    sendOpenFileMessage(action.filename)
    if (action.forceShowCodeEditor) {
      return {
        ...editor,
        interfaceDesigner: {
          ...editor.interfaceDesigner,
          codePaneVisible: true,
          codePaneWidth: 500,
        },
      }
    } else {
      return editor
    }
  },
  UPDATE_FILE: (
    action: UpdateFile,
    editor: EditorModel,
    dispatch: EditorDispatch,
    builtInDependencies: BuiltInDependencies,
  ): EditorModel => {
    if (
      !action.addIfNotInFiles &&
      getContentsTreeFileFromString(editor.projectContents, action.filePath) == null
    ) {
      return editor
    }

    const { file } = action

    const existing = getContentsTreeFileFromString(editor.projectContents, action.filePath)
    const updatedFile = updateFileIfPossible(file, existing)

    if (updatedFile === 'cant-update') {
      return editor
    }

    const updatedProjectContents = addFileToProjectContents(
      editor.projectContents,
      action.filePath,
      updatedFile,
    )

    let updatedNodeModulesFiles = editor.nodeModules.files
    let packageLoadingStatus: PackageStatusMap = {}

    // Ensure dependencies are updated if the `package.json` file has been changed.
    if (action.filePath === '/package.json' && isTextFile(updatedFile)) {
      const packageJson = packageJsonFileFromProjectContents(editor.projectContents)
      const currentDeps =
        packageJson != null && isTextFile(packageJson)
          ? dependenciesFromPackageJsonContents(packageJson.fileContents.code)
          : null
      void refreshDependencies(
        dispatch,
        updatedFile.fileContents.code,
        currentDeps,
        builtInDependencies,
        editor.nodeModules.files,
      )
    }

    return {
      ...editor,
      projectContents: updatedProjectContents,
      canvas: {
        ...editor.canvas,
        canvasContentInvalidateCount:
          editor.canvas.canvasContentInvalidateCount + (isTextFile(updatedFile) ? 0 : 1),
        domWalkerInvalidateCount:
          editor.canvas.domWalkerInvalidateCount + (isTextFile(updatedFile) ? 0 : 1),
      },
      nodeModules: {
        ...editor.nodeModules,
        files: updatedNodeModulesFiles,
        packageStatus: {
          ...editor.nodeModules.packageStatus,
          ...packageLoadingStatus,
        },
      },
    }
  },
  UPDATE_PROJECT_CONTENTS: (action: UpdateProjectContents, editor: EditorModel): EditorModel => {
    return {
      ...editor,
      projectContents: action.contents,
    }
  },
  UPDATE_BRANCH_CONTENTS: (action: UpdateBranchContents, editor: EditorModel): EditorModel => {
    return {
      ...editor,
      branchOriginContents: action.contents,
    }
  },
  UPDATE_GITHUB_SETTINGS: (action: UpdateGithubSettings, editor: EditorModel): EditorModel => {
    return normalizeGithubData({
      ...editor,
      githubSettings: {
        ...editor.githubSettings,
        ...action.settings,
      },
    })
  },
  UPDATE_GITHUB_DATA: (action: UpdateGithubData, editor: EditorModel): EditorModel => {
    return {
      ...editor,
      githubData: {
        ...editor.githubData,
        lastUpdatedAt: Date.now(),
        ...action.data,
      },
    }
  },
  REMOVE_FILE_CONFLICT: (action: RemoveFileConflict, editor: EditorModel): EditorModel => {
    let updatedConflicts: TreeConflicts = { ...editor.githubData.treeConflicts }
    delete updatedConflicts[action.path]
    const treeConflictsRemain = Object.keys(updatedConflicts).length > 0
    const newOriginCommit = treeConflictsRemain
      ? editor.githubSettings.originCommit
      : editor.githubSettings.pendingCommit
    const newPendingCommit = treeConflictsRemain ? editor.githubSettings.pendingCommit : null
    return {
      ...editor,
      githubSettings: {
        ...editor.githubSettings,
        originCommit: newOriginCommit,
        pendingCommit: newPendingCommit,
      },
      githubData: {
        ...editor.githubData,
        treeConflicts: updatedConflicts,
      },
    }
  },
  UPDATE_FROM_WORKER: (action: UpdateFromWorker, editor: EditorModel): EditorModel => {
    let workingProjectContents: ProjectContentTreeRoot = editor.projectContents
    let anyParsedUpdates: boolean = false

    // This prevents partial updates to the model which can then cause UIDs to clash between files.
    // Where updates to files A and B resulted in new UIDs in each but as the update to one of those
    // files ends up stale only the model in one of them gets updated which clashes with the UIDs in
    // the old version of the other.
    for (const fileUpdate of action.updates) {
      const existing = getContentsTreeFileFromString(editor.projectContents, fileUpdate.filePath)
      if (existing != null && isTextFile(existing)) {
        anyParsedUpdates = true
        const updateIsStale = fileUpdate.versionNumber < existing.versionNumber
        if (updateIsStale && action.updates.length > 1) {
          return editor
        }
      }
    }

    for (const fileUpdate of action.updates) {
      const existing = getContentsTreeFileFromString(editor.projectContents, fileUpdate.filePath)
      if (existing != null && isTextFile(existing)) {
        anyParsedUpdates = true
        let updatedFile: TextFile
        let updatedContents: ParsedTextFile
        let code: string
        const updateIsStale = fileUpdate.versionNumber < existing.versionNumber
        switch (fileUpdate.type) {
          case 'WORKER_PARSED_UPDATE': {
            code = existing.fileContents.code
            const highlightBounds = getHighlightBoundsFromParseResult(fileUpdate.parsed)
            updatedContents = updateParsedTextFileHighlightBounds(
              fileUpdate.parsed,
              highlightBounds,
            )
            break
          }
          case 'WORKER_CODE_AND_PARSED_UPDATE':
            code = fileUpdate.code
            const highlightBounds = getHighlightBoundsFromParseResult(fileUpdate.parsed)
            // Because this will print and reparse, we need to be careful of changes to the parsed
            // model that have happened since we requested this update
            updatedContents = updateIsStale
              ? existing.fileContents.parsed
              : updateParsedTextFileHighlightBounds(fileUpdate.parsed, highlightBounds)
            break
          default:
            const _exhaustiveCheck: never = fileUpdate
            throw new Error(`Invalid file update: ${fileUpdate}`)
        }

        if (updateIsStale) {
          // if the received file is older than the existing, we still allow it to update the other side,
          // but we don't bump the revision state.
          updatedFile = textFile(
            textFileContents(code, updatedContents, existing.fileContents.revisionsState),
            existing.lastSavedContents,
            isParseSuccess(updatedContents) ? updatedContents : existing.lastParseSuccess,
            existing.versionNumber,
          )
        } else {
          updatedFile = textFile(
            textFileContents(code, updatedContents, RevisionsState.BothMatch),
            existing.lastSavedContents,
            isParseSuccess(updatedContents) ? updatedContents : existing.lastParseSuccess,
            existing.versionNumber,
          )
        }

        workingProjectContents = addFileToProjectContents(
          workingProjectContents,
          fileUpdate.filePath,
          updatedFile,
        )
      } else {
        // The worker shouldn't be recreating deleted files or reformated files
        console.error(`Worker thread is trying to update an invalid file ${fileUpdate.filePath}`)
        return editor
      }
    }
    if (anyParsedUpdates) {
      // Clear any cached paths since UIDs will have been regenerated and property paths may no longer exist
      // FIXME take a similar approach as ElementPath cache culling to the PropertyPath cache culling. Or don't even clear it.
      PP.clearPropertyPathCache()
    }
    return {
      ...editor,
      projectContents: workingProjectContents,
      canvas: {
        ...editor.canvas,
        canvasContentInvalidateCount: anyParsedUpdates
          ? editor.canvas.canvasContentInvalidateCount + 1
          : editor.canvas.canvasContentInvalidateCount,
        domWalkerInvalidateCount: anyParsedUpdates
          ? editor.canvas.domWalkerInvalidateCount + 1
          : editor.canvas.domWalkerInvalidateCount,
      },
      parseOrPrintInFlight: false, // only ever clear it here
    }
  },
  UPDATE_FROM_CODE_EDITOR: (
    action: UpdateFromCodeEditor,
    editor: EditorModel,
    dispatch: EditorDispatch,
    builtInDependencies: BuiltInDependencies,
  ): EditorModel => {
    const existing = getContentsTreeFileFromString(editor.projectContents, action.filePath)

    const manualSave = action.unsavedContent == null
    const code = action.unsavedContent ?? action.savedContent

    let updatedFile: ProjectFile
    if (existing == null || !isTextFile(existing)) {
      const contents = textFileContents(code, unparsed, RevisionsState.CodeAhead)
      const lastSavedContents = manualSave
        ? null
        : textFileContents(action.savedContent, unparsed, RevisionsState.CodeAhead)

      updatedFile = textFile(contents, lastSavedContents, null, 0)
    } else {
      updatedFile = updateFileContents(code, existing, manualSave)
    }

    const updateAction = updateFile(action.filePath, updatedFile, true)
    return UPDATE_FNS.UPDATE_FILE(updateAction, editor, dispatch, builtInDependencies)
  },
  CLEAR_PARSE_OR_PRINT_IN_FLIGHT: (
    action: ClearParseOrPrintInFlight,
    editor: EditorModel,
  ): EditorModel => {
    return {
      ...editor,
      parseOrPrintInFlight: false,
    }
  },
  CLEAR_IMAGE_FILE_BLOB: (action: ClearImageFileBlob, editor: EditorModel): EditorModel => {
    if (action.uiFilePath in editor.canvas.base64Blobs) {
      const uiFileBlobs: UIFileBase64Blobs = editor.canvas.base64Blobs[action.uiFilePath]
      if (action.elementID in uiFileBlobs) {
        let updatedUIFileBlobs: UIFileBase64Blobs = { ...uiFileBlobs }
        delete updatedUIFileBlobs[action.elementID]
        const updateBase64Blobs = {
          ...editor.canvas.base64Blobs,
          [action.uiFilePath]: updatedUIFileBlobs,
        }
        return {
          ...editor,
          canvas: {
            ...editor.canvas,
            base64Blobs: updateBase64Blobs,
          },
        }
      }
    }
    return editor
  },
  ADD_FOLDER: (action: AddFolder, editor: EditorModel): EditorModel => {
    const pathPrefix = action.parentPath == '' ? '' : action.parentPath + '/'
    const newFolderKey = uniqueProjectContentID(
      pathPrefix + action.fileName,
      editor.projectContents,
    )
    return {
      ...editor,
      projectContents: addFileToProjectContents(editor.projectContents, newFolderKey, directory()),
    }
  },
  ADD_TEXT_FILE: (action: AddTextFile, editor: EditorModel): EditorModel => {
    const pathPrefix = action.parentPath == '' ? '' : action.parentPath + '/'
    const newFileKey = uniqueProjectContentID(pathPrefix + action.fileName, editor.projectContents)
    const newTextFile = codeFile('', null)

    const updatedProjectContents = addFileToProjectContents(
      editor.projectContents,
      newFileKey,
      newTextFile,
    )

    // Update the model.
    const updatedEditor: EditorModel = {
      ...editor,
      projectContents: updatedProjectContents,
    }
    return UPDATE_FNS.OPEN_CODE_EDITOR_FILE(openCodeEditorFile(newFileKey, false), updatedEditor)
  },
  DELETE_FILE: (
    action: DeleteFile | DeleteFileFromVSCode,
    editor: EditorModel,
    derived: DerivedState,
    userState: UserState,
  ): EditorModel => {
    const file = getContentsTreeFileFromString(editor.projectContents, action.filename)

    // Don't delete package.json, otherwise it will bring about the end of days.
    if (file == null || action.filename === 'package.json') {
      return editor
    }

    const updatedProjectContents = removeFromProjectContents(
      editor.projectContents,
      action.filename,
    )
    const selectedFile = getOpenFilename(editor)
    const updatedCanvas = selectedFile === action.filename ? null : editor.canvas.openFile

    switch (file.type) {
      case 'DIRECTORY': {
        // this deletes directory contents too
        const updatedEditor = {
          ...editor,
          projectContents: updatedProjectContents,
        }
        const oldFolderRegex = new RegExp('^' + action.filename)
        const filesToDelete: Array<string> = Object.keys(updatedEditor.projectContents).filter(
          (key) => oldFolderRegex.test(key),
        )
        return filesToDelete.reduce((working, filename) => {
          return UPDATE_FNS.DELETE_FILE(
            { action: 'DELETE_FILE', filename: filename },
            working,
            derived,
            userState,
          )
        }, updatedEditor)
      }
      case 'TEXT_FILE': {
        return {
          ...editor,
          projectContents: updatedProjectContents,
        }
      }
      case 'ASSET_FILE':
      case 'IMAGE_FILE': {
        if (isLoggedIn(userState.loginState) && editor.id != null) {
          // Side effect
          void deleteAssetFile(editor.id, action.filename)
        }

        return {
          ...editor,
          projectContents: updatedProjectContents,
          canvas: {
            ...editor.canvas,
            openFile: updatedCanvas,
          },
        }
      }
      default:
        return editor
    }
  },
  SET_MAIN_UI_FILE_OLDWORLD: (action: SetMainUIFile, editor: EditorModel): EditorModel => {
    return updateMainUIInEditorState(editor, action.uiFile)
  },
  SET_CODE_EDITOR_BUILD_ERRORS: (
    action: SetCodeEditorBuildErrors,
    editor: EditorModel,
  ): EditorModel => {
    const allBuildErrorsInState = getAllBuildErrors(editor.codeEditorErrors)
    const allBuildErrorsInAction = Utils.flatMapArray(
      (filename) => action.buildErrors[filename],
      Object.keys(action.buildErrors),
    )
    if (allBuildErrorsInState.length === 0 && allBuildErrorsInAction.length === 0) {
      return editor
    } else {
      const updatedCodeEditorErrors = Object.keys(action.buildErrors).reduce((acc, filename) => {
        return {
          ...acc,
          buildErrors: {
            ...acc.buildErrors,
            [filename]: action.buildErrors[filename],
          },
        }
      }, editor.codeEditorErrors)
      return {
        ...editor,
        codeEditorErrors: updatedCodeEditorErrors,
        jsxMetadata: emptyJsxMetadata,
        domMetadata: emptyJsxMetadata,
        spyMetadata: emptyJsxMetadata,
      }
    }
  },
  SET_CODE_EDITOR_LINT_ERRORS: (
    action: SetCodeEditorLintErrors,
    editor: EditorModel,
  ): EditorModel => {
    const allLintErrorsInState = getAllLintErrors(editor.codeEditorErrors)
    const allLintErrorsInAction = Utils.flatMapArray(
      (filename) => action.lintErrors[filename],
      Object.keys(action.lintErrors),
    )
    if (allLintErrorsInState.length === 0 && allLintErrorsInAction.length === 0) {
      return editor
    } else {
      const updatedCodeEditorErrors = Object.keys(action.lintErrors).reduce((acc, filename) => {
        return {
          ...acc,
          lintErrors: {
            ...acc.lintErrors,
            [filename]: action.lintErrors[filename],
          },
        }
      }, editor.codeEditorErrors)
      return {
        ...editor,
        codeEditorErrors: updatedCodeEditorErrors,
      }
    }
  },
  SAVE_DOM_REPORT: (
    action: SaveDOMReport,
    editor: EditorModel,
    spyCollector: UiJsxCanvasContextData,
  ): EditorModel => {
    // Note: If this DOM report only includes values for a single canvas
    // it will wipe out any spy data that any other canvas may have produced.

    // Calculate the spy metadata given what has been collected.
    const spyResult = spyCollector.current.spyValues.metadata

    const finalDomMetadata = ElementInstanceMetadataMapKeepDeepEquality(
      editor.domMetadata,
      action.elementMetadata,
    ).value
    const finalSpyMetadata = ElementInstanceMetadataMapKeepDeepEquality(
      editor.spyMetadata,
      spyResult,
    ).value

    const stayedTheSame =
      editor.domMetadata === finalDomMetadata && editor.spyMetadata === finalSpyMetadata

    if (stayedTheSame) {
      return editor
    } else {
      return {
        ...editor,
        // TODO move the reconstructMetadata call here, and remove _currentAllElementProps_KILLME
        domMetadata: finalDomMetadata,
        spyMetadata: finalSpyMetadata,
        _currentAllElementProps_KILLME: {
          ...spyCollector.current.spyValues.allElementProps,
        },
      }
    }
  },
  TRUE_UP_GROUPS: (editor: EditorModel): EditorModel => {
    const canvasFrameAndTargets: Array<CanvasFrameAndTarget> = mapDropNulls((element) => {
      const globalFrame = MetadataUtils.findElementByElementPath(
        editor.jsxMetadata,
        element,
      )?.globalFrame
      if (globalFrame == null || isInfinityRectangle(globalFrame)) {
        return null
      }
      return {
        frame: globalFrame,
        target: element,
      }
    }, editor.trueUpGroupsForElementAfterDomWalkerRuns)
    const editorWithGroupsTruedUp = foldAndApplyCommandsSimple(editor, [
      pushIntendedBoundsAndUpdateGroups(canvasFrameAndTargets, 'live-metadata'),
    ])
    return { ...editorWithGroupsTruedUp, trueUpGroupsForElementAfterDomWalkerRuns: [] }
  },
  // NB: this can only update attribute values and part of attribute value,
  // If you want other types of JSXAttributes, that needs to be added
  RENAME_PROP_KEY: (action: RenameStyleSelector, editor: EditorModel): EditorModel => {
    return setPropertyOnTarget(editor, action.target, (props) => {
      const originalPropertyPath = PP.createFromArray(action.cssTargetPath.path)
      const newPropertyPath = PP.createFromArray(action.value)
      const originalValue = getJSXAttributesAtPath(props, originalPropertyPath).attribute
      const attributesWithUnsetKey = unsetJSXValueAtPath(props, originalPropertyPath)
      if (
        modifiableAttributeIsAttributeValue(originalValue) ||
        modifiableAttributeIsPartOfAttributeValue(originalValue)
      ) {
        if (isRight(attributesWithUnsetKey)) {
          const setResult = setJSXValueAtPath(
            attributesWithUnsetKey.value,
            newPropertyPath,
            jsExpressionValue(originalValue.value, emptyComments),
          )
          return setResult
        } else {
          return attributesWithUnsetKey
        }
      } else {
        return left(
          `Original value was not a JSXAttributeValue or PartofJSXAttributeValue, was ${originalValue.type}`,
        )
      }
    })
  },
  SET_FILEBROWSER_RENAMING_TARGET: (
    action: SetFilebrowserRenamingTarget,
    editor: EditorModel,
  ): EditorModel => {
    return {
      ...editor,
      fileBrowser: {
        ...editor.fileBrowser,
        renamingTarget: action.filename,
      },
    }
  },
  TOGGLE_PROPERTY: (action: ToggleProperty, editor: EditorModel): EditorModel => {
    return modifyOpenJsxElementAtPath(action.target, action.togglePropValue, editor)
  },
  UPDATE_JSX_ELEMENT_NAME: (action: UpdateJSXElementName, editor: EditorModel): EditorModel => {
    const updatedEditor = UPDATE_FNS.ADD_IMPORTS(
      addImports(action.importsToAdd, action.target),
      editor,
    )

    return modifyOpenJsxElementOrConditionalAtPath(
      action.target,
      (element) => {
        switch (element.type) {
          case 'JSX_CONDITIONAL_EXPRESSION':
            return element
          case 'JSX_ELEMENT':
            if (action.elementName.type === 'JSX_FRAGMENT') {
              return jsxFragment(element.uid, element.children, true)
            } else {
              return {
                ...element,
                name: action.elementName.name,
              }
            }
          case 'JSX_FRAGMENT':
            if (action.elementName.type === 'JSX_FRAGMENT') {
              return element
            }
            return jsxElement(
              action.elementName.name,
              element.uid,
              jsxAttributesFromMap({
                'data-uid': jsExpressionValue(element.uid, emptyComments),
              }),
              element.children,
            )
          default:
            assertNever(element)
        }
      },
      updatedEditor,
    )
  },
  SET_CONDITIONAL_OVERRIDDEN_CONDITION: (
    action: SetConditionalOverriddenCondition,
    editor: EditorModel,
  ): EditorModel => {
    return modifyOpenJsxElementOrConditionalAtPath(
      action.target,
      (element) => {
        if (!isJSXConditionalExpression(element)) {
          return element
        }

        function isNotConditionalFlag(c: Comment): boolean {
          return !isUtopiaCommentFlag(c, 'conditional')
        }

        const leadingComments = [...element.comments.leadingComments.filter(isNotConditionalFlag)]
        if (action.condition != null) {
          leadingComments.push(
            makeUtopiaFlagComment({ type: 'conditional', value: action.condition }),
          )
        }

        return {
          ...element,
          comments: {
            leadingComments: leadingComments,
            trailingComments: element.comments.trailingComments.filter(isNotConditionalFlag),
            questionTokenComments: element.comments.questionTokenComments,
          },
        }
      },
      editor,
    )
  },
  UPDATE_CONDITIONAL_EXPRESSION: (
    action: UpdateConditionalExpression,
    editor: EditorModel,
  ): EditorModel => {
    return modifyOpenJsxElementOrConditionalAtPath(
      action.target,
      (element) => {
        if (!isJSXConditionalExpression(element)) {
          return element
        }

        // Use the values from the previous version in an attempt to stop a brief error screen
        // from flashing between committing this change and re-parsing the file
        const oldDefinedElsewhere =
          element.condition.type === 'ATTRIBUTE_OTHER_JAVASCRIPT'
            ? element.condition.definedElsewhere
            : []
        const oldElementsWithin =
          element.condition.type === 'ATTRIBUTE_OTHER_JAVASCRIPT'
            ? element.condition.elementsWithin
            : {}

        return {
          ...element,
          condition: jsExpressionOtherJavaScript(
            action.expression,
            action.expression,
            oldDefinedElsewhere,
            null,
            oldElementsWithin,
          ),
          originalConditionString: action.expression,
        }
      },
      editor,
    )
  },
  ADD_IMPORTS: (action: AddImports, editor: EditorModel): EditorModel => {
    return modifyUnderlyingTargetElement(
      action.target,
      forceNotNull('Missing open file', editor.canvas.openFile?.filename),
      editor,
      (element) => element,
      (success, _, underlyingFilePath) => {
        return {
          ...success,
          imports: mergeImports(underlyingFilePath, success.imports, action.importsToAdd),
        }
      },
    )
  },
  SET_ASPECT_RATIO_LOCK: (action: SetAspectRatioLock, editor: EditorModel): EditorModel => {
    return modifyOpenJsxElementAtPath(
      action.target,
      (element) => {
        const path = PP.create(AspectRatioLockedProp)
        const updatedProps = action.locked
          ? eitherToMaybe(
              setJSXValueAtPath(element.props, path, jsExpressionValue(true, emptyComments)),
            )
          : deleteJSXAttribute(element.props, AspectRatioLockedProp)
        return {
          ...element,
          props: updatedProps ?? element.props,
        }
      },
      editor,
    )
  },
  SET_SAFE_MODE: (action: SetSafeMode, editor: EditorModel): EditorModel => {
    return {
      ...editor,
      safeMode: action.value,
    }
  },
  SET_SAVE_ERROR: (action: SetSaveError, editor: EditorModel): EditorModel => {
    return {
      ...editor,
      saveError: action.value,
    }
  },
  INSERT_DROPPED_IMAGE: (action: InsertDroppedImage, editor: EditorModel): EditorModel => {
    const projectContent = action.image
    const parent = arrayToMaybe(editor.highlightedViews)
    const newUID = generateUidWithExistingComponents(editor.projectContents)
    const imageAttribute = jsExpressionValue(imagePathURL(action.path), emptyComments)
    const size: Size = {
      width: projectContent.width ?? 100,
      height: projectContent.height ?? 100,
    }
    const { frame } = getFrameAndMultiplier(action.position, action.path, size, null)
    let parentShiftX: number = 0
    let parentShiftY: number = 0
    if (parent != null) {
      const frameOfParent = MetadataUtils.getFrameInCanvasCoords(parent, editor.jsxMetadata)
      if (frameOfParent != null && isFiniteRectangle(frameOfParent)) {
        parentShiftX = -frameOfParent.x
        parentShiftY = -frameOfParent.y
      }
    }
    const imageElement = jsxElement(
      jsxElementName('img', []),
      newUID,
      jsxAttributesFromMap({
        alt: jsExpressionValue('', emptyComments),
        src: imageAttribute,
        style: jsExpressionValue(
          {
            position: 'absolute',
            left: parentShiftX + frame.x,
            top: parentShiftY + frame.y,
            width: frame.width,
            height: frame.height,
          },
          emptyComments,
        ),
        'data-uid': jsExpressionValue(newUID, emptyComments),
        [AspectRatioLockedProp]: jsExpressionValue(true, emptyComments),
      }),
      [],
    )

    const insertJSXElementAction = insertJSXElement(imageElement, parent, {})
    return UPDATE_FNS.INSERT_JSX_ELEMENT(insertJSXElementAction, editor)
  },
  REMOVE_FROM_NODE_MODULES_CONTENTS: (
    action: RemoveFromNodeModulesContents,
    editor: EditorState,
    dispatch: EditorDispatch,
    builtInDependencies: BuiltInDependencies,
  ): EditorState => {
    const updatedNodeModulesFiles = removeModulesFromNodeModules(
      action.modulesToRemove,
      editor.nodeModules.files,
    )

    return {
      ...editor,
      nodeModules: {
        ...editor.nodeModules,
        files: updatedNodeModulesFiles,
      },
      codeResultCache: generateCodeResultCache(
        editor.projectContents,
        codeCacheToBuildResult(editor.codeResultCache.cache),
        editor.codeResultCache.exportsInfo,
        updatedNodeModulesFiles,
        dispatch,
        editor.codeResultCache.evaluationCache,
        builtInDependencies,
      ),
    }
  },
  UPDATE_NODE_MODULES_CONTENTS: (
    action: UpdateNodeModulesContents,
    editor: EditorState,
    dispatch: EditorDispatch,
    builtInDependencies: BuiltInDependencies,
  ): EditorState => {
    const updatedNodeModulesFiles = { ...editor.nodeModules.files, ...action.contentsToAdd }

    return {
      ...editor,
      nodeModules: {
        ...editor.nodeModules,
        files: updatedNodeModulesFiles,
      },
      codeResultCache: generateCodeResultCache(
        editor.projectContents,
        codeCacheToBuildResult(editor.codeResultCache.cache),
        editor.codeResultCache.exportsInfo,
        updatedNodeModulesFiles,
        dispatch,
        editor.codeResultCache.evaluationCache,
        builtInDependencies,
      ),
    }
  },
  UPDATE_PACKAGE_JSON: (action: UpdatePackageJson, editor: EditorState): EditorState => {
    const dependencies = action.dependencies.reduce(
      (acc: Array<RequestedNpmDependency>, curr: RequestedNpmDependency) => {
        return {
          ...acc,
          [curr.name]: curr.version,
        }
      },
      [] as Array<RequestedNpmDependency>,
    )
    return updateDependenciesInEditorState(editor, dependencies)
  },
  START_CHECKPOINT_TIMER: (
    action: StartCheckpointTimer,
    editor: EditorState,
    dispatch: EditorDispatch,
  ): EditorState => {
    // Side effects.
    clearTimeout(checkpointTimeoutId)
    checkpointTimeoutId = window.setTimeout(() => {
      dispatch([finishCheckpointTimer()], 'everyone')
    }, 1000)
    // No need to actually change the editor state.
    return editor
  },
  FINISH_CHECKPOINT_TIMER: (action: FinishCheckpointTimer, editor: EditorState): EditorState => {
    // Side effects.
    checkpointTimeoutId = undefined
    // No need to actually change the editor state.
    return editor
  },
  ADD_MISSING_DIMENSIONS: (action: AddMissingDimensions, editor: EditorState): EditorState => {
    const ArbitrarySize = 10
    const frameWithExtendedDimensions = canvasRectangle({
      x: action.existingSize.x,
      y: action.existingSize.y,
      width: action.existingSize.width === 0 ? ArbitrarySize : action.existingSize.width,
      height: action.existingSize.height === 0 ? ArbitrarySize : action.existingSize.height,
    })
    const frameAndTarget: PinOrFlexFrameChange = pinSizeChange(
      action.target,
      frameWithExtendedDimensions,
      null,
    )
    return setCanvasFramesInnerNew(editor, [frameAndTarget], null)
  },
  SET_PACKAGE_STATUS: (action: SetPackageStatus, editor: EditorState): EditorState => {
    const packageName = action.packageName
    return produce(editor, (draft) => {
      draft.nodeModules.packageStatus[packageName] = { status: action.status }
    })
  },
  SET_SHORTCUT: (action: SetShortcut, userState: UserState): UserState => {
    let updatedShortcutConfig: ShortcutConfiguration = {}
    if (userState.shortcutConfig != null) {
      updatedShortcutConfig = {
        ...userState.shortcutConfig,
      }
    }
    updatedShortcutConfig[action.shortcutName] = [action.newKey]
    const updatedUserConfiguration: UserConfiguration = {
      shortcutConfig: updatedShortcutConfig,
      themeConfig: userState.themeConfig,
    }
    // Side effect.
    void saveUserConfiguration(updatedUserConfiguration)
    return {
      ...updatedUserConfiguration,
      loginState: userState.loginState,
      githubState: userState.githubState,
    }
  },
  UPDATE_PROPERTY_CONTROLS_INFO: (
    action: UpdatePropertyControlsInfo,
    editor: EditorState,
  ): EditorState => {
    let updatedPropertyControlsInfo: PropertyControlsInfo = {
      ...editor.propertyControlsInfo,
      ...action.propertyControlsInfo,
    }
    for (const moduleNameOrPathToDelete of action.moduleNamesOrPathsToDelete) {
      delete updatedPropertyControlsInfo[moduleNameOrPathToDelete]
    }
    return {
      ...editor,
      propertyControlsInfo: updatedPropertyControlsInfo,
    }
  },
  ADD_STORYBOARD_FILE: (_action: AddStoryboardFile, editor: EditorModel): EditorModel => {
    const updatedEditor = addStoryboardFileToProject(editor)
    if (updatedEditor == null) {
      return editor
    } else {
      const openTab = openCodeEditorFile(StoryboardFilePath, true)
      return UPDATE_FNS.OPEN_CODE_EDITOR_FILE(openTab, updatedEditor)
    }
  },
  UPDATE_TEXT: (action: UpdateText, editorStore: EditorStoreUnpatched): EditorStoreUnpatched => {
    const { textProp } = action
    // This flag is useful when editing conditional expressions:
    // if the edited element is a js expression AND the content is still between curly brackets after editing,
    // just save it as an expression, otherwise save it as text content
    const isActionTextExpression =
      (textProp === 'itself' || textProp === 'whenTrue' || textProp === 'whenFalse') &&
      action.text.length > 1 &&
      action.text[0] === '{' &&
      action.text[action.text.length - 1] === '}'

    const withUpdatedText = ((): EditorState => {
      if (textProp === 'child') {
        return modifyOpenJsxElementOrConditionalAtPath(
          action.target,
          (element) => {
            if (action.text.trim() === '') {
              return {
                ...element,
                children: [],
              }
            } else {
              const result = {
                ...element,
                children: [jsxTextBlock(action.text)],
              }
              return result
            }
          },
          editorStore.unpatchedEditor,
        )
      } else if (textProp === 'itself') {
        return modifyOpenJsxChildAtPath(
          action.target,
          (element) => {
            if (isJSExpression(element) && isActionTextExpression) {
              return {
                ...element,
                javascript: action.text.slice(1, -1),
              }
            }
            const comments = 'comments' in element ? element.comments : emptyComments
            if (action.text.trim() === '') {
              return jsExpressionValue(null, comments, element.uid)
            } else {
              return jsExpressionValue(action.text, comments, element.uid)
            }
          },
          editorStore.unpatchedEditor,
        )
      } else if (textProp === 'fullConditional') {
        return modifyOpenJsxChildAtPath(
          action.target,
          () => {
            // the whole expression will be reparsed again so we can just save it as a text block
            return jsxTextBlock(action.text)
          },
          editorStore.unpatchedEditor,
        )
      } else if (textProp === 'whenFalse' || textProp === 'whenTrue') {
        return modifyOpenJsxElementOrConditionalAtPath(
          action.target,
          (element) => {
            if (isJSXConditionalExpression(element)) {
              const textElement = element[textProp]
              if (isJSExpression(textElement) && isActionTextExpression) {
                return {
                  ...element,
                  [textProp]: {
                    ...textElement,
                    javascript: action.text.slice(1, -1),
                  },
                }
              }
              return {
                ...element,
                [textProp]: jsExpressionValue(action.text, emptyComments, textElement.uid),
              }
            }
            return element
          },
          editorStore.unpatchedEditor,
        )
      } else {
        assertNever(textProp)
      }
    })()
    const withGroupTrueUpQueued: EditorState = addToTrueUpGroups(withUpdatedText, action.target)

    const withCollapsedElements = collapseTextElements(action.target, withGroupTrueUpQueued)

    if (withGroupTrueUpQueued === withCollapsedElements) {
      return {
        ...editorStore,
        unpatchedEditor: withGroupTrueUpQueued,
      }
    } else {
      return {
        ...editorStore,
        unpatchedEditor: withCollapsedElements,
        history: History.add(
          editorStore.history,
          withGroupTrueUpQueued,
          editorStore.unpatchedDerived,
          [],
        ),
      }
    }
  },
  MARK_VSCODE_BRIDGE_READY: (action: MarkVSCodeBridgeReady, editor: EditorModel): EditorModel => {
    return {
      ...editor,
      vscodeBridgeReady: action.ready,
    }
  },
  SELECT_FROM_FILE_AND_POSITION: (
    action: SelectFromFileAndPosition,
    editor: EditorModel,
    derived: DerivedState,
    dispatch: EditorDispatch,
  ): EditorModel => {
    return updateSelectedComponentsFromEditorPosition(
      derived,
      editor,
      dispatch,
      action.filePath,
      action.line,
    )
  },
  SEND_CODE_EDITOR_INITIALISATION: (
    action: SendCodeEditorInitialisation,
    editor: EditorModel,
    userState: UserState,
  ): EditorModel => {
    // Side effects.
    sendCodeEditorDecorations(editor)
    sendSelectedElement(editor)
    sendSetVSCodeTheme(getCurrentTheme(userState))
    return {
      ...editor,
      vscodeReady: true,
    }
  },
  SET_FOCUSED_ELEMENT: (
    action: SetFocusedElement,
    editor: EditorModel,
    derived: DerivedState,
  ): EditorModel => {
    let shouldApplyChange: boolean = false
    if (action.focusedElementPath == null) {
      shouldApplyChange = editor.focusedElementPath != null
    } else if (
      MetadataUtils.isManuallyFocusableComponent(
        action.focusedElementPath,
        editor.jsxMetadata,
        derived.autoFocusedPaths,
      )
    ) {
      shouldApplyChange = true
    }
    if (EP.pathsEqual(editor.focusedElementPath, action.focusedElementPath)) {
      shouldApplyChange = false
    }

    if (shouldApplyChange) {
      return {
        ...editor,
        focusedElementPath: action.focusedElementPath,
        canvas: {
          ...editor.canvas,
          domWalkerInvalidateCount: editor.canvas.domWalkerInvalidateCount + 1,
        },
      }
    } else {
      return editor
    }
  },
  SCROLL_TO_ELEMENT: (
    action: ScrollToElement,
    editor: EditorModel,
    dispatch: EditorDispatch,
  ): EditorModel => {
    const targetElementCoords = MetadataUtils.getFrameInCanvasCoords(
      action.target,
      editor.jsxMetadata,
    )
    if (targetElementCoords != null && isFiniteRectangle(targetElementCoords)) {
      const isNavigatorOnTop = !editor.navigator.minimised
      const containerRootDiv = document.getElementById('canvas-root')
      const navigatorOffset = isNavigatorOnTop ? DefaultNavigatorWidth : 0
      const scale = 1 / editor.canvas.scale

      // This returns the offset used as the fallback for the other behaviours when the container bounds are not defined.
      // It will effectively scroll to the element by positioning it at the origin (TL) of the
      // canvas, based on the BaseCanvasOffset value(s).
      function canvasOffsetToOrigin(frame: CanvasRectangle): CanvasVector {
        const baseCanvasOffset = isNavigatorOnTop ? BaseCanvasOffsetLeftPane : BaseCanvasOffset
        const target = canvasPoint({
          x: baseCanvasOffset.x * scale,
          y: baseCanvasOffset.y * scale,
        })
        return Utils.pointDifference(frame, target)
      }

      function canvasOffsetToCenter(
        frame: CanvasRectangle,
        bounds: CanvasRectangle | null,
      ): CanvasVector {
        if (bounds == null) {
          return canvasOffsetToOrigin(frame) // fallback default
        }
        const canvasCenter = getRectCenter(
          canvasRectangle({
            x: bounds.x,
            y: bounds.y,
            width: bounds.width * scale,
            height: bounds.height * scale,
          }),
        )
        const topLeftTarget = canvasPoint({
          x: canvasCenter.x - frame.width / 2 - bounds.x + (navigatorOffset / 2) * scale,
          y: canvasCenter.y - frame.height / 2 - bounds.y,
        })
        return Utils.pointDifference(frame, topLeftTarget)
      }

      function canvasOffsetKeepScrollPositionIfVisible(
        frame: CanvasRectangle,
        bounds: CanvasRectangle | null,
      ): CanvasVector | null {
        if (bounds == null) {
          return canvasOffsetToOrigin(frame) // fallback default
        }
        const containerRectangle = {
          x: navigatorOffset - editor.canvas.realCanvasOffset.x,
          y: -editor.canvas.realCanvasOffset.y,
          width: bounds.width,
          height: bounds.height,
        } as CanvasRectangle
        const isVisible = rectangleIntersection(containerRectangle, frame) != null
        return isVisible
          ? null // when the element is on screen no scrolling is needed
          : canvasOffsetToOrigin(frame) // fallback default
      }

      function getNewCanvasOffset(frame: CanvasRectangle): CanvasVector | null {
        const containerDivBoundingRect = canvasRectangle(
          containerRootDiv?.getBoundingClientRect() ?? null,
        )
        switch (action.behaviour) {
          case 'keep-scroll-position-if-visible':
            return canvasOffsetKeepScrollPositionIfVisible(frame, containerDivBoundingRect)
          case 'to-center':
            return canvasOffsetToCenter(frame, containerDivBoundingRect)
          default:
            assertNever(action.behaviour)
        }
      }

      const newCanvasOffset = getNewCanvasOffset(targetElementCoords)
      return newCanvasOffset == null
        ? editor
        : UPDATE_FNS.SET_SCROLL_ANIMATION(
            setScrollAnimation(true),
            {
              ...editor,
              canvas: {
                ...editor.canvas,
                realCanvasOffset: newCanvasOffset,
                roundedCanvasOffset: utils.roundPointTo(newCanvasOffset, 0),
              },
            },
            dispatch,
          )
    } else {
      return {
        ...editor,
      }
    }
  },
  SET_SCROLL_ANIMATION: (
    action: SetScrollAnimation,
    editor: EditorModel,
    dispatch: EditorDispatch,
  ): EditorModel => {
    if (action.value) {
      if (canvasScrollAnimationTimer != null) {
        clearTimeout(canvasScrollAnimationTimer)
      }
      canvasScrollAnimationTimer = window.setTimeout(() => {
        clearTimeout(canvasScrollAnimationTimer)
        canvasScrollAnimationTimer = undefined
        dispatch([setScrollAnimation(false)], 'everyone')
      }, 500)
    }
    return {
      ...editor,
      canvas: {
        ...editor.canvas,
        scrollAnimation: action.value,
      },
    }
  },
  SET_FOLLOW_SELECTION_ENABLED: (
    action: SetFollowSelectionEnabled,
    editor: EditorModel,
  ): EditorModel => {
    // Side effects
    sendSetFollowSelectionEnabledMessage(action.value)
    return {
      ...editor,
      config: {
        ...editor.config,
        followSelection: {
          ...editor.config.followSelection,
          enabled: action.value,
        },
      },
    }
  },
  UPDATE_CONFIG_FROM_VSCODE: (action: UpdateConfigFromVSCode, editor: EditorModel): EditorModel => {
    return {
      ...editor,
      config: action.config,
    }
  },
  SET_LOGIN_STATE: (action: SetLoginState, userState: UserState): UserState => {
    return {
      ...userState,
      loginState: action.loginState,
    }
  },
  SET_GITHUB_STATE: (action: SetGithubState, userState: UserState): UserState => {
    return {
      ...userState,
      githubState: action.githubState,
    }
  },
  SET_USER_CONFIGURATION: (action: SetUserConfiguration, userState: UserState): UserState => {
    // Side effect - update the theme setting in VS Code
    sendSetVSCodeTheme(getCurrentTheme(action.userConfiguration))

    return {
      ...userState,
      ...action.userConfiguration,
    }
  },
  RESET_CANVAS: (action: ResetCanvas, editor: EditorModel): EditorModel => {
    return {
      ...editor,
      canvas: {
        ...editor.canvas,
        mountCount: editor.canvas.mountCount + 1,
        domWalkerInvalidateCount: editor.canvas.domWalkerInvalidateCount + 1,
      },
    }
  },
  SET_FILEBROWSER_DROPTARGET: (
    action: SetFilebrowserDropTarget,
    editor: EditorModel,
  ): EditorModel => {
    return {
      ...editor,
      fileBrowser: {
        ...editor.fileBrowser,
        dropTarget: action.target,
      },
    }
  },
  SET_FORKED_FROM_PROJECT_ID: (
    action: SetForkedFromProjectID,
    editor: EditorModel,
  ): EditorModel => {
    return {
      ...editor,
      forkedFromProjectId: action.id,
    }
  },
  SET_CURRENT_THEME: (action: SetCurrentTheme, userState: UserState): UserState => {
    const updatedUserConfiguration: UserConfiguration = {
      shortcutConfig: userState.shortcutConfig,
      themeConfig: action.theme,
    }

    // Side effect - update the setting in VS Code
    sendSetVSCodeTheme(getCurrentTheme(updatedUserConfiguration))

    // Side effect - store the setting on the server
    void saveUserConfiguration(updatedUserConfiguration)

    return { ...userState, ...updatedUserConfiguration }
  },
  FOCUS_CLASS_NAME_INPUT: (editor: EditorModel): EditorModel => {
    return {
      ...editor,
      inspector: {
        ...editor.inspector,
        classnameFocusCounter: editor.inspector.classnameFocusCounter + 1,
      },
    }
  },
  FOCUS_FORMULA_BAR: (editor: EditorModel): EditorModel => {
    return {
      ...editor,
      topmenu: {
        ...editor.topmenu,
        formulaBarFocusCounter: editor.topmenu.formulaBarFocusCounter + 1,
      },
    }
  },
  UPDATE_FORMULA_BAR_MODE: (action: UpdateFormulaBarMode, editor: EditorModel): EditorModel => {
    return {
      ...editor,
      topmenu: {
        ...editor.topmenu,
        formulaBarMode: action.value,
      },
    }
  },
  INSERT_INSERTABLE: (action: InsertInsertable, editor: EditorModel): EditorModel => {
    const openFilename = editor.canvas.openFile?.filename
    if (openFilename == null) {
      return editor
    } else {
      let newSelectedViews: ElementPath[] = []
      let detailsOfUpdate: string | null = null
      let withInsertedElement: InsertChildAndDetails | null = null

      if (action.insertionPath == null) {
        return includeToast('Selected element does not support children', editor)
      }

      const { insertionPath } = action

      function addNewSelectedView(newUID: string) {
        const newPath = EP.appendToPath(insertionPath.intendedParentPath, newUID)
        newSelectedViews.push(newPath)
      }

      const withNewElement = modifyUnderlyingTargetElement(
        insertionPath.intendedParentPath,
        openFilename,
        editor,
        (element) => element,
        (success, _, underlyingFilePath) => {
          const utopiaComponents = getUtopiaJSXComponentsFromSuccess(success)
          const newUID = generateUidWithExistingComponents(editor.projectContents)

          if (action.toInsert.element.type === 'JSX_ELEMENT') {
            const propsWithUid = forceRight(
              setJSXValueAtPath(
                action.toInsert.element.props,
                PP.create(UTOPIA_UID_KEY),
                jsExpressionValue(newUID, emptyComments),
              ),
              `Could not set data-uid on props of insertable element ${action.toInsert.element.name}`,
            )
            // Potentially add in some default position and sizing.
            let props = propsWithUid
            if (action.styleProps === 'add-size') {
              const sizesToSet: Array<ValueAtPath> = [
                { path: PP.create('style', 'width'), value: jsExpressionValue(100, emptyComments) },
                {
                  path: PP.create('style', 'height'),
                  value: jsExpressionValue(100, emptyComments),
                },
              ]
              const withSizeUpdates = setJSXValuesAtPaths(props, sizesToSet)
              if (isRight(withSizeUpdates)) {
                props = withSizeUpdates.value
              } else {
                console.error('Unable to set sizes on element.')
                return success
              }
            }

            const insertedElementName = action.toInsert.element.name
            let withMaybeUpdatedParent = utopiaComponents
            let insertedElementChildren: JSXElementChildren = []

            insertedElementChildren.push(...action.toInsert.element.children)
            const element = jsxElement(insertedElementName, newUID, props, insertedElementChildren)

            withInsertedElement = insertElementAtPath(
              editor.projectContents,
              insertionPath,
              element,
              withMaybeUpdatedParent,
              action.indexPosition,
            )
            detailsOfUpdate = withInsertedElement.insertionDetails

            addNewSelectedView(newUID)
          } else if (action.toInsert.element.type === 'JSX_CONDITIONAL_EXPRESSION') {
            const element = jsxConditionalExpression(
              newUID,
              action.toInsert.element.condition,
              action.toInsert.element.originalConditionString,
              action.toInsert.element.whenTrue,
              action.toInsert.element.whenFalse,
              action.toInsert.element.comments,
            )

            withInsertedElement = insertElementAtPath(
              editor.projectContents,
              insertionPath,
              element,
              utopiaComponents,
              action.indexPosition,
            )
            detailsOfUpdate = withInsertedElement.insertionDetails

            const newPath = EP.appendToPath(insertionPath.intendedParentPath, newUID)
            newSelectedViews.push(newPath)
          } else if (action.toInsert.element.type === 'JSX_FRAGMENT') {
            const element = jsxFragment(
              newUID,
              action.toInsert.element.children,
              action.toInsert.element.longForm,
            )

            withInsertedElement = insertElementAtPath(
              editor.projectContents,
              insertionPath,
              element,
              utopiaComponents,
              action.indexPosition,
            )
            detailsOfUpdate = withInsertedElement.insertionDetails

            addNewSelectedView(newUID)
          } else {
            assertNever(action.toInsert.element)
          }

          const updatedTopLevelElements = applyUtopiaJSXComponentsChanges(
            success.topLevelElements,
            withInsertedElement.components,
          )

          const updatedImports = mergeImports(
            underlyingFilePath,
            success.imports,
            mergeImports(
              underlyingFilePath,
              withInsertedElement.importsToAdd,
              action.toInsert.importsToAdd,
            ),
          )
          return {
            ...success,
            topLevelElements: updatedTopLevelElements,
            imports: updatedImports,
          }
        },
      )
      const updatedEditorState: EditorModel = {
        ...withNewElement,
        selectedViews: newSelectedViews,
      }

      // Add the toast for the update details if necessary.
      return includeToast(detailsOfUpdate, updatedEditorState)
    }
  },
  SET_PROP_TRANSIENT: (action: SetPropTransient, editor: EditorModel): EditorModel => {
    const currentTransientProps =
      editor.canvas.transientProperties != null
        ? editor.canvas.transientProperties[EP.toString(action.target)]?.attributesToUpdate
        : null
    return {
      ...editor,
      canvas: {
        ...editor.canvas,
        transientProperties: {
          ...editor.canvas.transientProperties,
          [EP.toString(action.target)]: {
            elementPath: action.target,
            attributesToUpdate: {
              ...(currentTransientProps ?? {}),
              [PP.toString(action.propertyPath)]: action.value,
            },
          },
        },
      },
    }
  },
  CLEAR_TRANSIENT_PROPS: (action: ClearTransientProps, editor: EditorModel): EditorModel => {
    return {
      ...editor,
      canvas: {
        ...editor.canvas,
        transientProperties: null,
      },
    }
  },
  ADD_TAILWIND_CONFIG: (
    action: AddTailwindConfig,
    editor: EditorModel,
    dispatch: EditorDispatch,
    builtInDependencies: BuiltInDependencies,
  ): EditorModel => {
    const packageJsonFile = getContentsTreeFileFromString(editor.projectContents, '/package.json')
    const currentNpmDeps = dependenciesFromPackageJson(packageJsonFile, 'regular-only')

    void findLatestVersion('tailwindcss').then((tailwindResult) => {
      if (tailwindResult.type === 'VERSION_LOOKUP_SUCCESS') {
        const tailwindVersion = tailwindResult.version
        void findLatestVersion('postcss').then((postcssResult) => {
          if (postcssResult.type === 'VERSION_LOOKUP_SUCCESS') {
            const postcssVersion = postcssResult.version
            const updatedNpmDeps = [
              ...currentNpmDeps,
              requestedNpmDependency('tailwindcss', tailwindVersion.version),
              requestedNpmDependency('postcss', postcssVersion.version),
            ]
            void fetchNodeModules(updatedNpmDeps, builtInDependencies).then(
              (fetchNodeModulesResult) => {
                const loadedPackagesStatus = createLoadedPackageStatusMapFromDependencies(
                  updatedNpmDeps,
                  fetchNodeModulesResult.dependenciesWithError,
                  fetchNodeModulesResult.dependenciesNotFound,
                )
                const packageErrorActions = Object.keys(loadedPackagesStatus).map(
                  (dependencyName) =>
                    setPackageStatus(dependencyName, loadedPackagesStatus[dependencyName].status),
                )
                dispatch([
                  ...packageErrorActions,
                  updateNodeModulesContents(fetchNodeModulesResult.nodeModules),
                ])
              },
            )

            const packageJsonUpdateAction = updatePackageJson(updatedNpmDeps)

            dispatch([packageJsonUpdateAction], 'everyone')
          }
        })
      }
    })

    let updatedProjectContents = editor.projectContents
    if (getContentsTreeFileFromString(editor.projectContents, TailwindConfigPath) == null) {
      updatedProjectContents = addFileToProjectContents(
        editor.projectContents,
        TailwindConfigPath,
        DefaultTailwindConfig(),
      )
    }

    if (getContentsTreeFileFromString(editor.projectContents, PostCSSPath) == null) {
      updatedProjectContents = addFileToProjectContents(
        updatedProjectContents,
        PostCSSPath,
        DefaultPostCSSConfig(),
      )
    }

    return {
      ...editor,
      projectContents: updatedProjectContents,
      nodeModules: {
        ...editor.nodeModules,
        packageStatus: {
          ...editor.nodeModules.packageStatus,
          ...{
            postcss: { status: 'loading' },
            tailwindcss: { status: 'loading' },
          },
        },
      },
    }
  },
  DECREMENT_RESIZE_OPTIONS_SELECTED_INDEX: (editor: EditorModel): EditorModel => {
    const resizeOptions = editor.canvas.resizeOptions
    const decrementedIndex = resizeOptions.propertyTargetSelectedIndex - 1
    const newIndex =
      decrementedIndex < 0 ? resizeOptions.propertyTargetOptions.length - 1 : decrementedIndex
    return {
      ...editor,
      canvas: {
        ...editor.canvas,
        resizeOptions: {
          ...resizeOptions,
          propertyTargetSelectedIndex: newIndex,
        },
      },
    }
  },
  INCREMENT_RESIZE_OPTIONS_SELECTED_INDEX: (editor: EditorModel): EditorModel => {
    const resizeOptions = editor.canvas.resizeOptions
    const newIndex =
      (resizeOptions.propertyTargetSelectedIndex + 1) % resizeOptions.propertyTargetOptions.length
    return {
      ...editor,
      canvas: {
        ...editor.canvas,
        resizeOptions: {
          ...resizeOptions,
          propertyTargetSelectedIndex: newIndex,
        },
      },
    }
  },
  SET_RESIZE_OPTIONS_TARGET_OPTIONS: (
    action: SetResizeOptionsTargetOptions,
    editor: EditorModel,
  ): EditorModel => {
    return {
      ...editor,
      canvas: {
        ...editor.canvas,
        resizeOptions: {
          propertyTargetOptions: action.propertyTargetOptions,
          propertyTargetSelectedIndex: action.index ?? 0,
        },
      },
    }
  },
  HIDE_VSCODE_LOADING_SCREEN: (
    action: HideVSCodeLoadingScreen,
    editor: EditorModel,
  ): EditorModel => {
    return { ...editor, vscodeLoadingScreenVisible: false }
  },
  SET_INDEXED_DB_FAILED: (action: SetIndexedDBFailed, editor: EditorModel): EditorModel => {
    return { ...editor, indexedDBFailed: action.indexedDBFailed }
  },
  FORCE_PARSE_FILE: (action: ForceParseFile, editor: EditorModel): EditorModel => {
    return {
      ...editor,
      forceParseFiles: editor.forceParseFiles.concat(action.filePath),
    }
  },
  RUN_ESCAPE_HATCH: (
    action: RunEscapeHatch,
    editor: EditorModel,
    builtInDependencies: BuiltInDependencies,
  ): EditorModel => {
    const canvasState = pickCanvasStateFromEditorState(editor, builtInDependencies)
    if (areAllSelectedElementsNonAbsolute(action.targets, editor.jsxMetadata)) {
      const commands = getEscapeHatchCommands(
        action.targets,
        editor.jsxMetadata,
        canvasState,
        null,
      ).commands
      return foldAndApplyCommandsSimple(editor, commands)
    } else {
      return editor
    }
  },
  SET_ELEMENTS_TO_RERENDER: (action: SetElementsToRerender, editor: EditorModel): EditorModel => {
    return foldAndApplyCommandsSimple(editor, [setElementsToRerenderCommand(action.value)])
  },
  TOGGLE_SELECTION_LOCK: (action: ToggleSelectionLock, editor: EditorModel): EditorModel => {
    const targets = action.targets
    return targets.reduce((working, target) => {
      switch (action.newValue) {
        case 'locked':
          return update(working, {
            lockedElements: {
              simpleLock: { $set: working.lockedElements.simpleLock.concat(target) },
              hierarchyLock: {
                $set: working.lockedElements.hierarchyLock.filter(
                  (element) => !EP.pathsEqual(element, target),
                ),
              },
            },
          })
        case 'locked-hierarchy':
          return update(working, {
            lockedElements: {
              simpleLock: {
                $set: working.lockedElements.simpleLock.filter(
                  (element) => !EP.pathsEqual(element, target),
                ),
              },
              hierarchyLock: { $set: working.lockedElements.hierarchyLock.concat(target) },
            },
          })
        case 'selectable':
        default:
          return update(working, {
            lockedElements: {
              simpleLock: {
                $set: working.lockedElements.simpleLock.filter(
                  (element) => !EP.pathsEqual(element, target),
                ),
              },
              hierarchyLock: {
                $set: working.lockedElements.hierarchyLock.filter(
                  (element) => !EP.pathsEqual(element, target),
                ),
              },
            },
          })
      }
    }, editor)
  },
  UPDATE_AGAINST_GITHUB: (action: UpdateAgainstGithub, editor: EditorModel): EditorModel => {
    const githubSettings = editor.githubSettings
    if (
      githubSettings.targetRepository != null &&
      githubSettings.branchName != null &&
      githubSettings.originCommit != null
    ) {
      const mergeResults = mergeProjectContents(
        editor.projectContents,
        action.specificCommitContent,
        action.branchLatestContent,
      )
      // If there are conflicts, then don't update the origin commit so we can try again.
      const treeConflictsPresent = Object.keys(mergeResults.treeConflicts).length > 0
      const newOriginCommit = treeConflictsPresent
        ? githubSettings.originCommit
        : action.latestCommit
      const newPendingCommit = treeConflictsPresent ? action.latestCommit : null
      return {
        ...editor,
        projectContents: mergeResults.value,
        githubSettings: {
          ...githubSettings,
          originCommit: newOriginCommit,
          pendingCommit: newPendingCommit,
        },
        githubData: {
          ...editor.githubData,
          treeConflicts: mergeResults.treeConflicts,
        },
      }
    } else {
      return editor
    }
  },
  SET_FILE_BROWSER_DRAG_STATE: (
    action: SetImageDragSessionState,
    editor: EditorModel,
  ): EditorModel => {
    return {
      ...editor,
      imageDragSessionState: action.imageDragSessionState,
    }
  },
  APPLY_COMMANDS: (action: ApplyCommandsAction, editor: EditorModel): EditorModel => {
    return foldAndApplyCommandsSimple(editor, action.commands)
  },
  UPDATE_COLOR_SWATCHES: (action: UpdateColorSwatches, editor: EditorModel): EditorModel => {
    return {
      ...editor,
      colorSwatches: action.colorSwatches,
    }
  },
  SWITCH_CONDITIONAL_BRANCHES: (
    action: SwitchConditionalBranches,
    editor: EditorModel,
  ): EditorModel => {
    const openFile = editor.canvas.openFile?.filename
    if (openFile == null) {
      return editor
    }

    const updatedEditor = modifyUnderlyingTargetElement(
      action.target,
      openFile,
      editor,
      (element) => {
        if (!isJSXConditionalExpression(element)) {
          return element
        }
        return jsxConditionalExpression(
          element.uid,
          element.condition,
          element.originalConditionString,
          element.whenFalse,
          element.whenTrue,
          element.comments,
        )
      },
      (parseSuccess) => parseSuccess,
    )

    return updatedEditor
  },
}

function copySelectionToClipboardMutating(
  editor: EditorState,
  builtInDependencies: BuiltInDependencies,
): EditorState {
  const copyData = createClipboardDataFromSelection(editor, builtInDependencies)
  if (copyData != null) {
    // side effect 😟
    Clipboard.setClipboardData({
      plainText: copyData.plaintext,
      html: encodeUtopiaDataToHtml(copyData.data),
    })
  }

  return {
    ...editor,
    pasteTargetsToIgnore: editor.selectedViews,
    internalClipboard: {
      styleClipboard: [],
      elements: copyData?.data ?? [],
    },
  }
}

/** DO NOT USE outside of actions.ts, only exported for testing purposes */
export function alignOrDistributeSelectedViews(
  editor: EditorModel,
  derived: DerivedState,
  alignmentOrDistribution: Alignment | Distribution,
): EditorModel {
  const selectedViews = editor.selectedViews

  if (selectedViews.length > 0) {
    // this array of canvasFrames excludes the non-layoutables. it means in a multiselect, they will not be considered
    const canvasFrames: Array<{
      target: ElementPath
      frame: CanvasRectangle
    }> = Utils.stripNulls(
      selectedViews.map((target) => {
        const instanceGlobalFrame = MetadataUtils.getFrameInCanvasCoords(target, editor.jsxMetadata)
        if (instanceGlobalFrame == null || isInfinityRectangle(instanceGlobalFrame)) {
          return null
        } else {
          return {
            target: target,
            frame: instanceGlobalFrame,
          }
        }
      }),
    )

    if (canvasFrames.length > 0) {
      const parentPath = EP.parentPath(selectedViews[0])
      const sourceIsParent = selectedViews.length === 1 && parentPath != null
      let source: CanvasRectangle
      if (sourceIsParent) {
        const parentFrame = MetadataUtils.getFrameInCanvasCoords(parentPath, editor.jsxMetadata)

        // if the parent frame is null, that means we probably ran into some error state,
        // as it means the child's globalFrame should also be null, so we shouldn't be in this branch
        const maybeSource = Utils.forceNotNull(
          `found no parent global frame for ${EP.toComponentId(parentPath!)}`,
          parentFrame,
        )

        // If the parent frame is infinite, fall back to using the selected element's frame
        source = isInfinityRectangle(maybeSource) ? canvasFrames[0].frame : maybeSource
      } else {
        source = Utils.boundingRectangleArray(Utils.pluck(canvasFrames, 'frame'))! // I know this can't be null because we checked the canvasFrames array is non-empty
      }
      const updatedCanvasFrames = alignOrDistributeCanvasRects(
        editor.jsxMetadata,
        canvasFrames,
        source,
        alignmentOrDistribution,
        sourceIsParent,
      )
      return setCanvasFramesInnerNew(editor, updatedCanvasFrames, null)
    }
  }
  return editor
}

function alignOrDistributeCanvasRects(
  componentMetadata: ElementInstanceMetadataMap,
  targets: CanvasFrameAndTarget[],
  source: CanvasRectangle,
  alignmentOrDistribution: Alignment | Distribution,
  sourceIsParent: boolean,
): Array<PinOrFlexFrameChange> {
  let results: Array<PinOrFlexFrameChange> = []

  function addChange(target: ElementPath, frame: CanvasRectangle | null): void {
    if (frame != null) {
      const isParentFlex =
        MetadataUtils.isParentYogaLayoutedContainerAndElementParticipatesInLayout(
          target,
          componentMetadata,
        )
      results.push(getFrameChange(target, frame, isParentFlex))
    }
  }

  function addTransformedChanges(transform: (frame: CanvasRectangle) => CanvasRectangle): void {
    Utils.fastForEach(targets, (target) => {
      if (target.frame != null) {
        addChange(target.target, transform(target.frame))
      }
    })
  }

  switch (alignmentOrDistribution) {
    case 'left':
      addTransformedChanges((frame) => Utils.setRectLeftX(frame, source.x))
      break
    case 'hcenter':
      const centerX = Utils.getRectCenter(source).x
      addTransformedChanges((frame) => Utils.setRectCenterX(frame, centerX))
      break
    case 'right':
      addTransformedChanges((frame) => Utils.setRectRightX(frame, source.x + source.width))
      break
    case 'top':
      addTransformedChanges((frame) => Utils.setRectTopY(frame, source.y))
      break
    case 'vcenter':
      const centerY = Utils.getRectCenter(source).y
      addTransformedChanges((frame) => Utils.setRectCenterY(frame, centerY))
      break
    case 'bottom':
      const bottom = source.y + source.height
      addTransformedChanges((frame) => Utils.setRectBottomY(frame, bottom))
      break
    case 'horizontal': {
      let totalWidth: number = 0
      let toOperateOn: Array<{ target: ElementPath; frame: CanvasRectangle }> = []
      Utils.fastForEach(targets, (target) => {
        if (target.frame != null) {
          totalWidth += target.frame.width
          toOperateOn.push({ target: target.target, frame: target.frame })
        }
      })
      const totalHorizontalSpace = source.width - totalWidth
      const numberOfSpaces = sourceIsParent ? toOperateOn.length + 1 : toOperateOn.length - 1
      const horizontalSpacing = totalHorizontalSpace / numberOfSpaces

      toOperateOn = toOperateOn.sort((l, r) => {
        return l.frame.x - r.frame.x
      })

      let previous: CanvasRectangle | null = null
      Utils.fastForEach(toOperateOn, (target) => {
        const referencePoint = previous == null ? source.x : previous.x + previous.width
        const shouldSpace = previous != null || sourceIsParent
        const newX = referencePoint + (shouldSpace ? horizontalSpacing : 0)
        const newRectangle = Utils.setRectLeftX(target.frame, newX)
        previous = newRectangle
        addChange(target.target, newRectangle)
      })
      break
    }
    case 'vertical': {
      let totalHeight: number = 0
      let toOperateOn: Array<{ target: ElementPath; frame: CanvasRectangle }> = []
      Utils.fastForEach(targets, (target) => {
        if (target.frame != null) {
          totalHeight += target.frame.height
          toOperateOn.push({ target: target.target, frame: target.frame })
        }
      })
      const totalVerticalSpace = source.height - totalHeight
      const numberOfSpaces = sourceIsParent ? toOperateOn.length + 1 : toOperateOn.length - 1
      const verticalSpacing = totalVerticalSpace / numberOfSpaces

      toOperateOn = toOperateOn.sort((l, r) => {
        return l.frame.y - r.frame.y
      })

      let previous: CanvasRectangle | null = null
      Utils.fastForEach(toOperateOn, (target) => {
        const referencePoint = previous == null ? source.y : previous.y + previous.height
        const shouldSpace = previous != null || sourceIsParent
        const newY = referencePoint + (shouldSpace ? verticalSpacing : 0)
        const newRectangle = Utils.setRectTopY(target.frame, newY)
        previous = newRectangle
        addChange(target.target, newRectangle)
      })
      break
    }
    default:
      const _exhaustiveCheck: never = alignmentOrDistribution
      throw new Error('Something went really wrong.')
  }

  return results
}

function setCanvasFramesInnerNew(
  editor: EditorModel,
  framesAndTargets: Array<PinOrFlexFrameChange>,
  optionalParentFrame: CanvasRectangle | null,
): EditorModel {
  return updateFramesOfScenesAndComponents(editor, framesAndTargets, optionalParentFrame)
}

export async function load(
  dispatch: EditorDispatch,
  model: PersistentModel,
  title: string,
  projectId: string,
  builtInDependencies: BuiltInDependencies,
  retryFetchNodeModules: boolean = true,
): Promise<void> {
  // this action is now async!
  const migratedModel = applyMigrations(model)
  const npmDependencies = dependenciesWithEditorRequirements(migratedModel.projectContents)
  const fetchNodeModulesResult = await fetchNodeModules(
    npmDependencies,
    builtInDependencies,
    retryFetchNodeModules,
  )

  const nodeModules: NodeModules = fetchNodeModulesResult.nodeModules
  const packageResult: PackageStatusMap = createLoadedPackageStatusMapFromDependencies(
    npmDependencies,
    fetchNodeModulesResult.dependenciesWithError,
    fetchNodeModulesResult.dependenciesNotFound,
  )

  const codeResultCache: CodeResultCache = generateCodeResultCache(
    // TODO is this sufficient here?
    migratedModel.projectContents,
    {},
    migratedModel.exportsInfo,
    nodeModules,
    dispatch,
    {},
    builtInDependencies,
  )

  const storedState = await loadStoredState(projectId)

  const safeMode = await localforage.getItem<boolean>(getProjectLockedKey(projectId))

  dispatch(
    [
      {
        action: 'LOAD',
        model: migratedModel,
        nodeModules: nodeModules,
        packageResult: packageResult,
        codeResultCache: codeResultCache,
        title: title,
        projectId: projectId,
        storedState: storedState,
        safeMode: safeMode,
      },
    ],
    'everyone',
  )
}

export function isSendPreviewModel(action: any): action is SendPreviewModel {
  return action != null && (action as SendPreviewModel).action === 'SEND_PREVIEW_MODEL'
}

function saveFileInProjectContents(
  projectContents: ProjectContentTreeRoot,
  filePath: string,
): ProjectContentTreeRoot {
  const file = getContentsTreeFileFromString(projectContents, filePath)
  if (file == null) {
    return projectContents
  } else {
    return addFileToProjectContents(projectContents, filePath, saveFile(file))
  }
}
