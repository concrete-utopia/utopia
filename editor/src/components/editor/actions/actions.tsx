import { produce } from 'immer'
import update from 'immutability-helper'
import localforage from 'localforage'
import { imagePathURL } from '../../../common/server'
import { roundAttributeLayoutValues } from '../../../core/layout/layout-utils'
import {
  findElementAtPath,
  findJSXElementAtPath,
  getZIndexOrderedViewsWithoutDirectChildren,
  MetadataUtils,
} from '../../../core/model/element-metadata-utils'
import type { InsertChildAndDetails } from '../../../core/model/element-template-utils'
import {
  elementPathForNonChildInsertions,
  elementPathFromInsertionPath,
  findJSXElementChildAtPath,
  generateUidWithExistingComponents,
  getIndexInParent,
  insertJSXElementChildren,
  renameJsxElementChild,
  renameJsxElementChildWithoutId,
  transformJSXComponentAtPath,
} from '../../../core/model/element-template-utils'
import {
  applyToAllUIJSFiles,
  applyUtopiaJSXComponentsChanges,
  fileExists,
  fileTypeFromFileName,
  getFilePathMappings,
  getUtopiaJSXComponentsFromSuccess,
  saveFile,
  saveTextFileContents,
  switchToFileType,
  uniqueProjectContentID,
  updateFileContents,
} from '../../../core/model/project-file-utils'
import { getStoryboardElementPath, PathForSceneDataLabel } from '../../../core/model/scene-utils'
import type { Either } from '../../../core/shared/either'
import {
  defaultEither,
  eitherToMaybe,
  foldEither,
  forceRight,
  isLeft,
  isRight,
  left,
  right,
  traverseEither,
} from '../../../core/shared/either'
import * as EP from '../../../core/shared/element-path'
import type {
  Comment,
  ElementInstanceMetadataMap,
  JSXAttributes,
  JSExpressionValue,
  JSXElement,
  JSXElementChildren,
  JSXElementChild,
  JSXConditionalExpression,
  JSXFragment,
  JSExpression,
} from '../../../core/shared/element-template'
import {
  deleteJSXAttribute,
  emptyComments,
  emptyJsxMetadata,
  getJSXAttribute,
  isImportStatement,
  isJSXAttributeValue,
  isJSXConditionalExpression,
  isJSXElement,
  modifiableAttributeIsPartOfAttributeValue,
  jsExpressionOtherJavaScript,
  jsxAttributesFromMap,
  jsExpressionValue,
  jsxConditionalExpression,
  jsxElement,
  jsxElementName,
  jsxFragment,
  jsxTextBlock,
  walkElements,
  modifiableAttributeIsAttributeValue,
  isJSExpression,
  isJSXMapExpression,
  getDefinedElsewhereFromElementChild,
  isJSXFragment,
  jsxConditionalExpressionConditionOptic,
  isJSExpressionOtherJavaScript,
  setJSXAttributesAttribute,
} from '../../../core/shared/element-template'
import type { ValueAtPath } from '../../../core/shared/jsx-attributes'
import {
  setJSXValuesAtPaths,
  unsetJSXValueAtPath,
  unsetJSXValuesAtPaths,
  valueAtPath,
} from '../../../core/shared/jsx-attributes'
import { getJSXAttributesAtPath, setJSXValueAtPath } from '../../../core/shared/jsx-attribute-utils'
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
  getRectCenter,
  localRectangle,
  zeroRectIfNullOrInfinity,
  roundPointToNearestWhole,
  boundingRectangleArray,
  zeroRectangle,
} from '../../../core/shared/math-utils'
import type {
  PackageStatusMap,
  RequestedNpmDependency,
} from '../../../core/shared/npm-dependency-types'
import { requestedNpmDependency } from '../../../core/shared/npm-dependency-types'
import { arrayToMaybe, forceNotNull, optionalMap } from '../../../core/shared/optional-utils'
import type {
  ElementPath,
  Imports,
  NodeModules,
  ParseSuccess,
  ProjectContents,
  ProjectFile,
  PropertyPath,
  StaticElementPath,
  TextFile,
} from '../../../core/shared/project-file-types'
import {
  assetFile,
  directory,
  imageFile,
  isImageFile,
  isDirectory,
  parseSuccess,
} from '../../../core/shared/project-file-types'
import {
  codeFile,
  importStatementFromImportDetails,
  isAssetFile,
  isParseSuccess,
  isTextFile,
  RevisionsState,
  textFile,
  textFileContents,
  unparsed,
} from '../../../core/shared/project-file-types'
import * as PP from '../../../core/shared/property-path'
import { assertNever, fastForEach, getProjectLockedKey, identity } from '../../../core/shared/utils'
import { emptyImports, mergeImports } from '../../../core/workers/common/project-file-utils'
import type { UtopiaTsWorkers } from '../../../core/workers/common/worker-types'
import type { IndexPosition } from '../../../utils/utils'
import Utils, { absolute } from '../../../utils/utils'
import type { ProjectContentTreeRoot } from '../../assets'
import {
  isProjectContentFile,
  packageJsonFileFromProjectContents,
  zipContentsTree,
} from '../../assets'
import {
  addFileToProjectContents,
  contentsToTree,
  getProjectFileByFilePath,
  removeFromProjectContents,
  treeToContents,
  walkContentsTreeForParseSuccess,
} from '../../assets'
import type { CanvasFrameAndTarget, PinOrFlexFrameChange } from '../../canvas/canvas-types'
import { pinSizeChange } from '../../canvas/canvas-types'
import {
  canvasPanelOffsets,
  duplicate,
  getFrameChange,
  updateFramesOfScenesAndComponents,
} from '../../canvas/canvas-utils'
import type { SetFocus } from '../../common/actions'
import { openMenu } from '../../context-menu-side-effect'
import type { CodeResultCache } from '../../custom-code/code-file'
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
  AddTailwindConfig,
  AddTextFile,
  AddToast,
  Alignment,
  AlignSelectedViews,
  ClearHighlightedViews,
  ClearImageFileBlob,
  ClearParseOrPrintInFlight,
  ClearTransientProps,
  ClosePopup,
  CloseTextEditor,
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
  OpenPopup,
  OpenTextEditor,
  RemoveFromNodeModulesContents,
  RemoveToast,
  RenameComponent,
  RenameStyleSelector,
  ResetCanvas,
  ResetPins,
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
  TogglePane,
  ToggleProperty,
  ToggleSelectionLock,
  UnsetProperty,
  UnwrapElements,
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
  SetMapCountOverride,
  ScrollToPosition,
  UpdateTopLevelElementsFromCollaborationUpdate,
  DeleteFileFromCollaboration,
  UpdateExportsDetailFromCollaborationUpdate,
  UpdateImportsFromCollaborationUpdate,
  UpdateCodeFromCollaborationUpdate,
  SetCommentFilterMode,
  SetForking,
  InsertAttributeOtherJavascriptIntoElement,
  SetCollaborators,
  ExtractPropertyControlsFromDescriptorFiles,
  SetCodeEditorComponentDescriptorErrors,
  SetSharingDialogOpen,
  AddNewPage,
  UpdateRemixRoute,
  AddNewFeaturedRoute,
  RemoveFeaturedRoute,
  AddCollapsedViews,
  ReplaceMappedElement,
  ReplaceElementInScope,
  ReplaceJSXElement,
  ToggleDataCanCondense,
} from '../action-types'
import { isLoggedIn } from '../action-types'
import type { Mode } from '../editor-modes'
import { isCommentMode, isFollowMode, isTextEditMode } from '../editor-modes'
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
import {
  deleteAssetFile,
  saveAsset as saveAssetToServer,
  saveUserConfiguration,
  updateAssetFileName,
  updateGithubRepository,
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
  TrueUpTarget,
  TrueUpHuggingElement,
  CollaborativeEditingSupport,
  ProjectGithubSettings,
} from '../store/editor-state'
import {
  trueUpChildrenOfGroupChanged,
  trueUpHuggingElement,
  trueUpGroupElementChanged,
  getPackageJsonFromProjectContents,
  modifyUnderlyingTargetJSXElement,
  getAllComponentDescriptorErrors,
  updatePackageJsonInEditorState,
  modifyUnderlyingTarget,
  modifyUnderlyingParseSuccessOnly,
} from '../store/editor-state'
import {
  BaseCanvasOffset,
  BaseCanvasOffsetLeftPane,
  editorModelFromPersistentModel,
  getAllBuildErrors,
  getAllLintErrors,
  getCurrentTheme,
  getElementPathsInBounds,
  getHighlightBoundsForFile,
  getMainUIFromModel,
  getOpenFilename,
  getOpenTextFileKey,
  getOpenUIJSFileKey,
  LeftMenuTab,
  mergeStoredEditorStateIntoEditorState,
  modifyOpenJsxElementAtPath,
  modifyParseSuccessAtPath,
  modifyParseSuccessWithSimple,
  modifyUnderlyingElementForOpenFile,
  modifyUnderlyingTargetElement,
  removeElementAtPath,
  StoryboardFilePath,
  updateMainUIInEditorState,
  withUnderlyingTarget,
  modifyOpenJsxElementOrConditionalAtPath,
  modifyOpenJsxChildAtPath,
  isConditionalClauseNavigatorEntry,
} from '../store/editor-state'
import { loadStoredState } from '../stored-state'
import { applyMigrations } from './migrations/migrations'

import { boundsInFile, defaultConfig } from 'utopia-vscode-common'
import { reorderElement } from '../../../components/canvas/commands/reorder-element-command'
import type { BuiltInDependencies } from '../../../core/es-modules/package-manager/built-in-dependencies-list'
import { fetchNodeModules } from '../../../core/es-modules/package-manager/fetch-packages'
import { resolveModule } from '../../../core/es-modules/package-manager/module-resolution'
import { addStoryboardFileToProject } from '../../../core/model/storyboard-utils'
import { UTOPIA_UID_KEY } from '../../../core/model/utopia-constants'
import { mapDropNulls, uniqBy } from '../../../core/shared/array-utils'
import type { TreeConflicts } from '../../../core/shared/github/helpers'
import { mergeProjectContents } from '../../../core/shared/github/helpers'
import { emptySet } from '../../../core/shared/set-utils'
import {
  fixUtopiaElement,
  generateConsistentUID,
  getUtopiaID,
  setUtopiaID,
} from '../../../core/shared/uid-utils'
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
import { createClipboardDataFromSelection, Clipboard } from '../../../utils/clipboard'
import {
  ExportDetailKeepDeepEquality,
  ImportDetailsKeepDeepEquality,
  NavigatorStateKeepDeepEquality,
  ParseSuccessKeepDeepEquality,
  TopLevelElementKeepDeepEquality,
} from '../store/store-deep-equality-instances'
import type { MouseButtonsPressed } from '../../../utils/mouse'
import { addButtonPressed, removeButtonPressed } from '../../../utils/mouse'
import { stripLeadingSlash } from '../../../utils/path-utils'
import { pickCanvasStateFromEditorState } from '../../canvas/canvas-strategies/canvas-strategies'
import { getEscapeHatchCommands } from '../../canvas/canvas-strategies/strategies/convert-to-absolute-and-move-strategy'
import { canCopyElement } from '../../canvas/canvas-strategies/strategies/reparent-helpers/reparent-helpers'
import {
  getReparentOutcome,
  pathToReparent,
} from '../../canvas/canvas-strategies/strategies/reparent-utils'
import {
  areAllSelectedElementsNonAbsolute,
  flattenSelection,
} from '../../canvas/canvas-strategies/strategies/shared-move-strategies-helpers'
import type { CanvasCommand } from '../../canvas/commands/commands'
import { foldAndApplyCommandsSimple } from '../../canvas/commands/commands'
import { setElementsToRerenderCommand } from '../../canvas/commands/set-elements-to-rerender-command'
import type { UiJsxCanvasContextData } from '../../canvas/ui-jsx-canvas'
import { notice } from '../../common/notice'
import type { ShortcutConfiguration } from '../shortcut-definitions'
import { ElementInstanceMetadataMapKeepDeepEquality } from '../store/store-deep-equality-instances'
import {
  addImports,
  addToast,
  clearImageFileBlob,
  enableInsertModeForJSXElement,
  finishCheckpointTimer,
  insertAsChildTarget,
  insertJSXElement,
  openCodeEditorFile,
  replaceMappedElement,
  scrollToPosition,
  selectComponents,
  setCodeEditorBuildErrors,
  setCodeEditorComponentDescriptorErrors,
  setCodeEditorLintErrors,
  setFocusedElement,
  setPackageStatus,
  setScrollAnimation,
  showToast,
  updateFile,
  updateFromWorker,
  updateNodeModulesContents,
  updatePackageJson,
  workerCodeAndParsedUpdate,
} from './action-creators'
import { addToastToState, includeToast, removeToastFromState } from './toast-helpers'
import { AspectRatioLockedProp } from '../../aspect-ratio'
import {
  refreshDependencies,
  removeModulesFromNodeModules,
} from '../../../core/shared/dependencies'
import { styleStringInArray } from '../../../utils/common-constants'
import { collapseTextElements } from '../../../components/text-editor/text-handling'
import { LayoutPropertyList, StyleProperties } from '../../inspector/common/css-utils'
import { isUtopiaCommentFlag, makeUtopiaFlagComment } from '../../../core/shared/comment-flags'
import { modify, toArrayOf } from '../../../core/shared/optics/optic-utilities'
import { fromField, traverseArray } from '../../../core/shared/optics/optic-creators'
import type { ConditionalClauseInsertBehavior, InsertionPath } from '../store/insertion-path'
import {
  commonInsertionPathFromArray,
  getElementPathFromInsertionPath,
  isConditionalClauseInsertionPath,
  childInsertionPath,
  conditionalClauseInsertionPath,
  replaceWithSingleElement,
  replaceWithElementsWrappedInFragmentBehaviour,
} from '../store/insertion-path'
import { getConditionalCaseCorrespondingToBranchPath } from '../../../core/model/conditionals'
import { deleteProperties } from '../../canvas/commands/delete-properties-command'
import { treatElementAsFragmentLike } from '../../canvas/canvas-strategies/strategies/fragment-like-helpers'
import {
  fixParentContainingBlockSettings,
  isTextContainingConditional,
  unwrapConditionalClause,
  unwrapTextContainingConditional,
  wrapElementInsertions,
} from './wrap-unwrap-helpers'
import { encodeUtopiaDataToHtml } from '../../../utils/clipboard-utils'
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
import {
  addToTrueUpElements,
  getCommandsForPushIntendedBounds,
} from '../../../core/model/true-up-targets'
import {
  groupStateFromJSXElement,
  invalidGroupStateToString,
  isEmptyGroup,
  isMaybeGroupForWrapping,
  isInvalidGroupState,
  treatElementAsGroupLike,
} from '../../canvas/canvas-strategies/strategies/group-helpers'
import {
  createPinChangeCommandsForElementInsertedIntoGroup,
  createPinChangeCommandsForElementBecomingGroupChild,
  elementCanBeAGroupChild,
} from '../../canvas/canvas-strategies/strategies/group-conversion-helpers'
import { addElements } from '../../canvas/commands/add-elements-command'
import { deleteElement } from '../../canvas/commands/delete-element-command'
import { queueTrueUpElement } from '../../canvas/commands/queue-true-up-command'
import {
  getFilesToUpdate,
  processWorkerUpdates,
} from '../../../core/shared/parser-projectcontents-utils'
import { getAllUniqueUids } from '../../../core/model/get-unique-ids'
import { getLayoutProperty } from '../../../core/layout/getLayoutProperty'
import { resultForFirstApplicableStrategy } from '../../inspector/inspector-strategies/inspector-strategy'
import { reparentToUnwrapAsAbsoluteStrategy } from '../one-shot-unwrap-strategies/reparent-to-unwrap-as-absolute-strategy'
import { convertToAbsoluteAndReparentToUnwrapStrategy } from '../one-shot-unwrap-strategies/convert-to-absolute-and-reparent-to-unwrap'
import { addHookForProjectChanges } from '../store/collaborative-editing'
import { arrayDeepEquality, objectDeepEquality } from '../../../utils/deep-equality'
import type { ProjectServerState } from '../store/project-server-state'
import { updateFileIfPossible } from './can-update-file'
import {
  getParseFileResult,
  getPrintAndReparseCodeResult,
} from '../../../core/workers/parser-printer/parser-printer-worker'
import { isSteganographyEnabled } from '../../../core/shared/stegano-text'
import type { TextFileContentsWithPath } from '../../../core/property-controls/property-controls-local'
import {
  updatePropertyControlsOnDescriptorFileDelete,
  isComponentDescriptorFile,
  createModuleEvaluator,
  maybeUpdatePropertyControls,
} from '../../../core/property-controls/property-controls-local'
import {
  addNewFeaturedRouteToPackageJson,
  addOrReplaceFeaturedRouteToPackageJson,
  isInsideRemixFolder,
  remixFilenameMatchPrefix,
  renameRemixFile,
  removeFeaturedRouteFromPackageJson,
} from '../../canvas/remix/remix-utils'
import type { FixUIDsState } from '../../../core/workers/parser-printer/uid-fix'
import { fixTopLevelElementsUIDs } from '../../../core/workers/parser-printer/uid-fix'
import { nextSelectedTab } from '../../navigator/left-pane/left-pane-utils'
import { getRemixRootDir } from '../store/remix-derived-data'
import { isReplaceKeepChildrenAndStyleTarget } from '../../navigator/navigator-item/component-picker-context-menu'
import {
  canCondenseJSXElementChild,
  dataCanCondenseProp,
  isDataCanCondenseProp,
} from '../../../utils/can-condense'
import { clearExecutionScopeCache } from '../../../components/canvas/ui-jsx-canvas-renderer/ui-jsx-canvas-execution-scope'

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
      visible: expanded,
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
      visible: expanded,
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
  if (newParent == null) {
    return {
      editor: editor,
      newPaths: [],
    }
  }

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

export function replaceInsideMap(
  targets: ElementPath[],
  intendedParentPath: StaticElementPath,
  insertBehavior: ConditionalClauseInsertBehavior,
  editor: EditorModel,
): {
  editor: EditorModel
  newPaths: Array<ElementPath>
} {
  const elements: Array<JSXElementChild> = mapDropNulls((path) => {
    const instance = MetadataUtils.findElementByElementPath(editor.jsxMetadata, path)
    if (instance == null || isLeft(instance.element)) {
      return null
    }

    return instance.element.value
  }, targets)

  let newPaths: Array<ElementPath> = targets.map((target) =>
    elementPathForNonChildInsertions(insertBehavior, intendedParentPath, EP.toUid(target)),
  )

  // TODO: handle multiple elements - currently we're taking the first one
  const elementToReplace = elements.find((element) => isJSXElement(element))

  if (elementToReplace != null && isJSXElement(elementToReplace)) {
    const editorAfterReplace = UPDATE_FNS.REPLACE_MAPPED_ELEMENT(
      replaceMappedElement(elementToReplace, intendedParentPath, emptyImports()),
      editor,
    )
    const updatedEditor = foldAndApplyCommandsSimple(editorAfterReplace, [
      ...targets.map((path) => deleteElement('always', path)),
    ])
    return {
      editor: updatedEditor,
      newPaths: newPaths,
    }
  }

  // if we couldn't find the JSXElement to replace, we just return the editor as is
  return {
    editor: editor,
    newPaths: newPaths,
  }
}

export function insertIntoWrapper(
  targets: ElementPath[],
  newParent: InsertionPath,
  editor: EditorModel,
): {
  editor: EditorModel
  newPaths: Array<ElementPath>
} {
  const elements: Array<JSXElementChild> = mapDropNulls((path) => {
    const instance = MetadataUtils.findElementByElementPath(editor.jsxMetadata, path)
    if (instance == null || isLeft(instance.element)) {
      return null
    }

    return instance.element.value
  }, targets)

  let newPaths: Array<ElementPath> = targets.map((target) =>
    elementPathFromInsertionPath(newParent, EP.toUid(target)),
  )

  const updatedEditor = foldAndApplyCommandsSimple(editor, [
    ...targets.map((path) => deleteElement('always', path)),
    addElements('always', newParent, elements),
  ])

  return {
    editor: updatedEditor,
    newPaths: newPaths,
  }
}

export function reparentElementToUnwrap(
  target: ElementPath,
  insertionPath: InsertionPath,
  indexPosition: IndexPosition,
  editor: EditorModel,
  builtInDependencies: BuiltInDependencies,
): { editor: EditorModel; newPath: ElementPath | null } {
  const result = resultForFirstApplicableStrategy([
    reparentToUnwrapAsAbsoluteStrategy(
      pathToReparent(target),
      editor.jsxMetadata,
      editor.elementPathTree,
      editor.allElementProps,
      insertionPath,
      indexPosition,
      builtInDependencies,
      editor.projectContents,
      editor.nodeModules.files,
    ),
    convertToAbsoluteAndReparentToUnwrapStrategy(
      pathToReparent(target),
      editor.jsxMetadata,
      editor.elementPathTree,
      editor.allElementProps,
      insertionPath,
      indexPosition,
      builtInDependencies,
      editor.projectContents,
      editor.nodeModules.files,
    ),
  ])

  if (result == null) {
    return { editor: editor, newPath: null }
  }

  const updatedEditor = foldAndApplyCommandsSimple(editor, result.commands)

  return {
    editor: updatedEditor,
    newPath: result.data.newPath,
  }
}

export function restoreEditorState(
  currentEditor: EditorModel,
  desiredEditor: EditorModel,
): EditorModel {
  // FIXME Ask Team Components to check over these
  return {
    id: currentEditor.id,
    forkedFromProjectId: currentEditor.forkedFromProjectId,
    appID: currentEditor.appID,
    projectName: currentEditor.projectName,
    projectDescription: currentEditor.projectDescription,
    projectVersion: currentEditor.projectVersion,
    isLoaded: currentEditor.isLoaded,
    trueUpElementsAfterDomWalkerRuns: [], // <- we reset the elements true-up value
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
    mode: EditorModes.selectMode(null, false, 'none'),
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
      visible: currentEditor.leftMenu.visible,
    },
    rightMenu: {
      selectedTab: currentEditor.rightMenu.selectedTab,
      visible: currentEditor.rightMenu.visible,
    },
    interfaceDesigner: {
      codePaneVisible: currentEditor.interfaceDesigner.codePaneVisible,
      additionalControls: currentEditor.interfaceDesigner.additionalControls,
    },
    canvas: {
      elementsToRerender: currentEditor.canvas.elementsToRerender,
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
    thumbnailLastGenerated: currentEditor.thumbnailLastGenerated,
    pasteTargetsToIgnore: desiredEditor.pasteTargetsToIgnore,
    codeEditorErrors: currentEditor.codeEditorErrors,
    parseOrPrintInFlight: false,
    previousParseOrPrintSkipped: desiredEditor.previousParseOrPrintSkipped,
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
    currentAllElementProps: desiredEditor.currentAllElementProps,
    variablesInScope: desiredEditor.variablesInScope,
    currentVariablesInScope: desiredEditor.currentVariablesInScope,
    githubSettings: currentEditor.githubSettings,
    imageDragSessionState: currentEditor.imageDragSessionState,
    githubOperations: currentEditor.githubOperations,
    branchOriginContents: currentEditor.branchOriginContents,
    githubData: currentEditor.githubData,
    refreshingDependencies: currentEditor.refreshingDependencies,
    colorSwatches: currentEditor.colorSwatches,
    internalClipboard: currentEditor.internalClipboard,
    filesModifiedByAnotherUser: currentEditor.filesModifiedByAnotherUser,
    activeFrames: currentEditor.activeFrames,
    commentFilterMode: currentEditor.commentFilterMode,
    forking: currentEditor.forking,
    collaborators: currentEditor.collaborators,
    sharingDialogOpen: currentEditor.sharingDialogOpen,
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
    navigatorRows: poppedDerived.navigatorRows,
    navigatorTargets: poppedDerived.navigatorTargets,
    visibleNavigatorTargets: poppedDerived.visibleNavigatorTargets,
    autoFocusedPaths: poppedDerived.autoFocusedPaths,
    controls: [],
    elementWarnings: poppedDerived.elementWarnings,
    projectContentsChecksums: poppedDerived.projectContentsChecksums,
    branchOriginContentsChecksums: poppedDerived.branchOriginContentsChecksums,
    remixData: poppedDerived.remixData,
    filePathMappings: poppedDerived.filePathMappings,
  }
}

function deleteElements(
  targets: ElementPath[],
  editor: EditorModel,
  options: {
    trueUpHuggingElements: boolean
  },
): EditorModel {
  const openUIJSFilePath = getOpenUIJSFileKey(editor)
  if (openUIJSFilePath == null) {
    console.error(`Attempted to delete element(s) with no UI file open.`)
    return editor
  } else {
    const updatedEditor = targets.reduce((working, targetPath) => {
      const underlyingTarget = normalisePathToUnderlyingTarget(working.projectContents, targetPath)

      if (underlyingTarget.type === 'NORMALISE_PATH_ELEMENT_NOT_FOUND') {
        return working // The element has likely already been deleted
      }

      const targetSuccess = normalisePathSuccessOrThrowError(underlyingTarget)

      function deleteElementFromParseSuccess(success: ParseSuccess): ParseSuccess {
        const utopiaComponents = getUtopiaJSXComponentsFromSuccess(success)
        const withTargetRemoved = removeElementAtPath(targetPath, utopiaComponents, success.imports)
        return modifyParseSuccessWithSimple((simpleSuccess: SimpleParseSuccess) => {
          return {
            ...simpleSuccess,
            utopiaComponents: withTargetRemoved.components,
            imports: withTargetRemoved.imports,
          }
        }, success)
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

    const trueUpGroupElementsChanged = siblings.map(trueUpGroupElementChanged)

    const trueUps: Array<TrueUpTarget> = [...trueUpGroupElementsChanged]

    if (options.trueUpHuggingElements) {
      const trueUpHuggingElements = mapDropNulls((path): TrueUpHuggingElement | null => {
        if (EP.isStoryboardPath(path) || shouldCascadeDelete(editor, path)) {
          return null
        }

        const metadata = MetadataUtils.findElementByElementPath(editor.jsxMetadata, path)
        if (metadata == null || isLeft(metadata.element)) {
          return null
        }
        const frame = metadata.localFrame
        if (frame == null || !isFiniteRectangle(frame)) {
          return null
        }

        const jsxProps = isJSXElement(metadata.element.value)
          ? right(metadata.element.value.props)
          : null

        const childrenFrame =
          boundingRectangleArray(
            mapDropNulls((child) => {
              const childFrame = child.globalFrame
              if (childFrame == null || !isFiniteRectangle(childFrame)) {
                return null
              }
              return childFrame
            }, MetadataUtils.getChildrenUnordered(editor.jsxMetadata, path)),
          ) ?? canvasRectangle(zeroRectangle)

        const hasHorizontalPosition =
          jsxProps != null &&
          (getLayoutProperty('left', jsxProps, styleStringInArray).value != null ||
            getLayoutProperty('right', jsxProps, styleStringInArray).value != null)
        const hasVerticalPosition =
          jsxProps != null &&
          (getLayoutProperty('top', jsxProps, styleStringInArray).value != null ||
            getLayoutProperty('bottom', jsxProps, styleStringInArray).value != null)

        function combineFrames(main: CanvasRectangle, backup: CanvasRectangle): CanvasRectangle {
          return canvasRectangle({
            x: hasHorizontalPosition ? main.x : backup.x,
            y: hasVerticalPosition ? main.y : backup.y,
            width: main.width !== 0 ? main.width : backup.width,
            height: main.height !== 0 ? main.height : backup.height,
          })
        }
        return trueUpHuggingElement(path, combineFrames(canvasRectangle(frame), childrenFrame))
      }, uniqBy(targets.map(EP.parentPath), EP.pathsEqual))
      trueUps.push(...trueUpHuggingElements)
    }

    return addToTrueUpElements(withUpdatedSelectedViews, ...trueUps)
  }
}

function shouldCascadeDelete(editor: EditorState, path: ElementPath): boolean {
  return (
    // it's a group
    treatElementAsGroupLike(editor.jsxMetadata, path) ||
    // it's a framgent
    treatElementAsFragmentLike(
      editor.jsxMetadata,
      editor.allElementProps,
      editor.elementPathTree,
      path,
      'sizeless-div-not-considered-fragment-like',
    )
    // TODO it's hug?
  )
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
        const current = withUnderlyingTarget(target, editor.projectContents, 0, (success) => {
          return getIndexInParent(success.topLevelElements, EP.asStatic(target))
        })
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

    const reorderElementCommand = reorderElement('always', selectedView, indexPosition)

    return foldAndApplyCommandsSimple(working, [reorderElementCommand])
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
  renamedOptionalPrefix: boolean
}

interface ReplaceFilePathFailure {
  type: 'FAILURE'
  errorMessage: string
}

type ReplaceFilePathResult = ReplaceFilePathFailure | ReplaceFilePathSuccess

export function replaceFilePath(
  oldPath: string,
  newPath: string,
  projectContentsTree: ProjectContentTreeRoot,
): ReplaceFilePathResult {
  // FIXME: Reimplement this in a way that doesn't require converting to and from `ProjectContents`.
  const projectContents = treeToContents(projectContentsTree)
  // if there is no file in projectContents it's probably a non-empty directory
  let error: string | null = null
  let updatedProjectContents: ProjectContents = {
    ...projectContents,
  }
  let updatedFiles: Array<{ oldPath: string; newPath: string }> = []

  const remixRootDir = getRemixRootDir(projectContentsTree)

  let renamedOptionalPrefix = false
  Utils.fastForEach(Object.keys(projectContents), (filename) => {
    if (
      filename === oldPath ||
      filename.startsWith(oldPath + '/') ||
      remixFilenameMatchPrefix(remixRootDir, filename, oldPath)
    ) {
      // TODO make sure the prefix search only happens when it makes sense so
      const projectFile = projectContents[filename]

      const maybeNewFilePathForRemix = isInsideRemixFolder(remixRootDir, filename)
        ? renameRemixFile({
            remixRootDir: remixRootDir,
            filename: filename,
            oldPath: oldPath,
            newPath: newPath,
          })
        : null
      if (maybeNewFilePathForRemix?.renamedOptionalPrefix) {
        renamedOptionalPrefix = true
      }

      const newFilePath = maybeNewFilePathForRemix?.filename ?? filename.replace(oldPath, newPath)

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

      // Only mark these as parsed ahead if they have meaningfully changed,
      // or if the filename has been changed for this file.
      const oldFilename =
        updatedFiles.find((updatedFile) => updatedFile.newPath === filename) ?? filename
      if (
        oldFilename !== filename ||
        !ParseSuccessKeepDeepEquality(projectFile.fileContents.parsed, updatedParseResult).areEqual
      ) {
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
    }
  })
  // Check if we discovered an error.
  if (error == null) {
    return {
      type: 'SUCCESS',
      projectContents: contentsToTree(updatedProjectContents),
      updatedFiles: updatedFiles,
      renamedOptionalPrefix: renamedOptionalPrefix,
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
  }

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

  if (newlySelectedElements.length === 0) {
    return editor
  }

  return UPDATE_FNS.SELECT_COMPONENTS(
    selectComponents(newlySelectedElements, false),
    editor,
    dispatch,
  )
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
    },
  }
}

function createStoryboardFileIfRemixProject(
  projectContents: ProjectContentTreeRoot,
): ProjectContentTreeRoot | null {
  const packageJsonContents = defaultEither(
    null,
    getPackageJsonFromProjectContents(projectContents),
  )
  if (packageJsonContents == null) {
    return null
  }
  const remixNotIncluded = packageJsonContents['dependencies']?.['@remix-run/react'] == null
  if (remixNotIncluded) {
    return null
  }

  const updatedProjectContents = addFileToProjectContents(
    projectContents,
    StoryboardFilePath,
    codeFile(DefaultStoryboardWithRemix, null, 1),
  )
  return updatedProjectContents
}

function createStoryboardFileIfMainComponentPresent(
  projectContents: ProjectContentTreeRoot,
): ProjectContentTreeRoot | null {
  return addStoryboardFileToProject(projectContents)
}

function createStoryboardFileWithPlaceholderContents(
  projectContents: ProjectContentTreeRoot,
  createPlaceholder: 'create-placeholder' | 'skip-creating-placeholder',
): ProjectContentTreeRoot {
  if (createPlaceholder === 'skip-creating-placeholder') {
    return projectContents
  }
  const updatedProjectContents = addFileToProjectContents(
    projectContents,
    StoryboardFilePath,
    codeFile(DefaultStoryboardContents, null, 1),
  )
  return updatedProjectContents
}

export function createStoryboardFileIfNecessary(
  projectContents: ProjectContentTreeRoot,
  createPlaceholder: 'create-placeholder' | 'skip-creating-placeholder',
): ProjectContentTreeRoot {
  const storyboardFile = getProjectFileByFilePath(projectContents, StoryboardFilePath)
  if (storyboardFile != null) {
    return projectContents
  }

  return (
    createStoryboardFileIfRemixProject(projectContents) ??
    createStoryboardFileIfMainComponentPresent(projectContents) ??
    createStoryboardFileWithPlaceholderContents(projectContents, createPlaceholder)
  )
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
  LOAD: (
    action: Load,
    oldEditor: EditorModel,
    dispatch: EditorDispatch,
    collaborativeEditingSupport: CollaborativeEditingSupport,
  ): EditorModel => {
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

    const parsedModel = {
      ...migratedModel,
      projectContents: parsedProjectFiles,
    }

    let newModel: EditorModel = {
      ...editorModelFromPersistentModel(parsedModel, dispatch),
      projectName: action.title,
      id: action.projectId,
      nodeModules: {
        skipDeepFreeze: true,
        files: action.nodeModules,
        projectFilesBuildResults: {},
        packageStatus: action.packageResult,
      },
      codeResultCache: action.codeResultCache,
      safeMode: action.safeMode,
    }

    const newModelMergedWithStoredStateAndStoryboardFile: EditorModel =
      mergeStoredEditorStateIntoEditorState(action.storedState, newModel)

    initVSCodeBridge(
      newModelMergedWithStoredStateAndStoryboardFile.projectContents,
      dispatch,
      StoryboardFilePath,
    )
    if (collaborativeEditingSupport.session != null) {
      addHookForProjectChanges(collaborativeEditingSupport.session, dispatch)
    }

    return loadModel(newModelMergedWithStoredStateAndStoryboardFile, oldEditor)
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
  UNSET_PROPERTY: (action: UnsetProperty, editor: EditorModel): EditorModel => {
    // TODO also queue group true up, just like for SET_PROP
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
      (success) => success,
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
    let newSelectedViews: Array<ElementPath> = editor.selectedViews
    let updatedEditor = modifyUnderlyingTargetElement(
      action.target,
      editor,
      (element) => {
        if (!isJSXElement(element)) {
          return element
        }
        const updatedProps = setJSXValueAtPath(element.props, action.propertyPath, action.value)
        // when this is a render prop we should select it
        if (isJSXElement(action.value)) {
          newSelectedViews = [EP.appendToPath(action.target, action.value.uid)]
        }
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
            editor.projectContents,
          )
          if (
            isInvalidGroupState(maybeInvalidGroupState) &&
            /**
             * we want to exempt 'child-has-missing-pins' from this list, because SET_PROP maybe what the user is doing to _fix_ the situation highlighted by 'child-has-missing-pins'
             * if 'child-has-missing-pins' prevents us from SET_PROP, that means we prevent ourselves from re-adding those missing props!
             */
            maybeInvalidGroupState !== 'child-has-missing-pins'
          ) {
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
      (success, _, underlyingFilePath) => {
        const updatedImports = mergeImports(
          underlyingFilePath,
          getFilePathMappings(editor.projectContents),
          success.imports,
          action.importsToAdd,
        ).imports
        return { ...success, imports: updatedImports }
      },
    )

    updatedEditor = addToTrueUpElements(updatedEditor, trueUpGroupElementChanged(action.target))

    if (setPropFailedMessage != null) {
      const toastAction = showToast(notice(setPropFailedMessage, 'ERROR'))
      updatedEditor = UPDATE_FNS.ADD_TOAST(toastAction, editor)
    }

    return {
      ...updatedEditor,
      selectedViews: newSelectedViews,
    }
  },
  SET_CANVAS_FRAMES: (action: SetCanvasFrames, editor: EditorModel): EditorModel => {
    return setCanvasFramesInnerNew(editor, action.framesAndTargets, null)
  },
  SET_Z_INDEX: (action: SetZIndex, editor: EditorModel): EditorModel => {
    return foldAndApplyCommandsSimple(editor, [
      reorderElement('always', action.target, action.indexPosition),
    ])
  },
  DELETE_SELECTED: (editor: EditorModel, dispatch: EditorDispatch): EditorModel => {
    // This function returns whether the given path will have the following deletion behavior:
    //  1. when deleting one of its children, the next sibling will be selected
    //  2. when deleting the last chilren, it is removed as well so as not to remain empty
    function behavesLikeGroupOrFragmentForDeletion(
      metadata: ElementInstanceMetadataMap,
      path: ElementPath,
    ): boolean {
      return (
        MetadataUtils.isFragmentFromMetadata(metadata[EP.toString(path)]) ||
        treatElementAsGroupLike(metadata, path)
      )
    }

    // find all parents of the current path which can be bulk-deleted
    function deletableParents(
      metadata: ElementInstanceMetadataMap,
      path: ElementPath,
      selected: ElementPath[],
    ): ElementPath[] {
      let result: Array<ElementPath> = []
      let parent: ElementPath = EP.parentPath(path)
      while (!EP.isStoryboardPath(parent)) {
        const children = MetadataUtils.getChildrenUnordered(metadata, parent)
        const count = 1 + children.filter((c) => EP.containsPath(c.elementPath, selected)).length
        if (!behavesLikeGroupOrFragmentForDeletion(metadata, parent) || children.length > count) {
          break
        }
        result.push(parent)
        parent = EP.parentPath(parent)
      }
      return result
    }

    let bubbledUpDeletions: Array<ElementPath> = []

    const staticSelectedElements = editor.selectedViews.map((path, _, allSelectedPaths) => {
      const siblings = MetadataUtils.getSiblingsOrdered(
        editor.jsxMetadata,
        editor.elementPathTree,
        path,
      )
      const selectedSiblings = allSelectedPaths.filter((p) =>
        siblings.some((sibling) => EP.pathsEqual(sibling.elementPath, p)),
      )

      const parentPath = EP.parentPath(path)

      const mustDeleteEmptyParent = behavesLikeGroupOrFragmentForDeletion(
        editor.jsxMetadata,
        parentPath,
      )

      const parentWillBeEmpty =
        MetadataUtils.getChildrenOrdered(editor.jsxMetadata, editor.elementPathTree, parentPath)
          .length === selectedSiblings.length

      if (mustDeleteEmptyParent && parentWillBeEmpty) {
        const bubbledUp = [
          parentPath,
          ...deletableParents(editor.jsxMetadata, parentPath, allSelectedPaths),
        ]
        bubbledUpDeletions.push(...bubbledUp)
        return EP.getCommonParent(bubbledUpDeletions, true) ?? parentPath
      }

      return path
    })

    const withElementDeleted = deleteElements(staticSelectedElements, editor, {
      trueUpHuggingElements: true,
    })

    const newSelectedViews = uniqBy(
      mapDropNulls((view) => {
        const parentPath = EP.parentPath(view)
        if (behavesLikeGroupOrFragmentForDeletion(editor.jsxMetadata, parentPath)) {
          // there may be bubbled up deletions, so find out which is the actual parent
          // where the bubbles stopped
          const parentsBubbledUp = [
            view,
            ...deletableParents(editor.jsxMetadata, parentPath, staticSelectedElements),
          ].map(EP.parentPath)
          const actualParent = EP.getCommonParent(parentsBubbledUp, true) ?? parentPath

          if (
            EP.pathsEqual(actualParent, parentPath) ||
            behavesLikeGroupOrFragmentForDeletion(editor.jsxMetadata, actualParent)
          ) {
            const ignorePaths = [...staticSelectedElements, ...parentsBubbledUp] // ignore these paths when looking for a sibling
            const target = MetadataUtils.getChildrenOrdered(
              editor.jsxMetadata,
              editor.elementPathTree,
              actualParent,
            ).find((element) => !EP.containsPath(element.elementPath, ignorePaths))
            if (target != null) {
              return target.elementPath
            }
          }
        }

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
    ).filter((path) => {
      // remove descendants of already-deleted elements during multiselect
      return !EP.containsPath(path, bubbledUpDeletions)
    })

    return {
      ...withElementDeleted,
      selectedViews: newSelectedViews,
    }
  },
  DELETE_VIEW: (action: DeleteView, editor: EditorModel): EditorModel => {
    const updatedEditor = deleteElements([action.target], editor, { trueUpHuggingElements: false })
    const parentPath = EP.parentPath(action.target)
    const newSelection = EP.isStoryboardPath(parentPath) ? [] : [parentPath]
    return {
      ...updatedEditor,
      selectedViews: newSelection,
    }
  },
  DUPLICATE_SELECTED: (editor: EditorModel): EditorModel => {
    return duplicateMany(editor.selectedViews, editor)
  },
  DUPLICATE_SPECIFIC_ELEMENTS: (
    action: DuplicateSpecificElements,
    editor: EditorModel,
    dispatch: EditorDispatch,
  ): EditorModel => {
    return duplicateMany(action.paths, editor)
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
      leftMenu: {
        visible: editor.leftMenu.visible,
        selectedTab: nextSelectedTab(editor.leftMenu.selectedTab, newlySelectedPaths),
      },
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

    const newlySelectedPaths: Array<ElementPath> = []

    return {
      ...editor,
      leftMenu: {
        visible: editor.leftMenu.visible,
        selectedTab: nextSelectedTab(editor.leftMenu.selectedTab, newlySelectedPaths),
      },
      selectedViews: [],
      navigator: updateNavigatorCollapsedState([], editor.navigator),
      pasteTargetsToIgnore: newlySelectedPaths,
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

    const nextSelectedViews = [...editor.selectedViews, ...additionalTargets]

    return {
      ...editor,
      leftMenu: {
        visible: editor.leftMenu.visible,
        selectedTab: nextSelectedTab(editor.leftMenu.selectedTab, nextSelectedViews),
      },
      selectedViews: nextSelectedViews,
      pasteTargetsToIgnore: [],
    }
  },
  UPDATE_EDITOR_MODE: (action: UpdateEditorMode, editor: EditorModel): EditorModel => {
    return setModeState(action.mode, editor)
  },
  SWITCH_EDITOR_MODE: (
    action: SwitchEditorMode,
    editor: EditorModel,
    userState: UserState,
  ): EditorModel => {
    // TODO this should probably be merged with UPDATE_EDITOR_MODE
    if (action.unlessMode === editor.mode.type) {
      // FIXME: this is a bit unfortunate as this action should just do what its name suggests, without additional flags.
      // For now there's not much more that we can do since the action here can be (and is) evaluated also for transient states
      // (e.g. a `textEdit` mode after an `insertMode`) created with wildcard patches.
      return editor
    }
    if (isTextEditMode(action.mode)) {
      if (
        !MetadataUtils.targetTextEditable(
          editor.jsxMetadata,
          editor.elementPathTree,
          action.mode.editedText,
        )
      ) {
        // If the target of text edit mode isn't editable, then ignore the requested change.
        console.error(`Invalid target for text edit mode: ${EP.toString(action.mode.editedText)}`)
        return editor
      }
    }
    if (isCommentMode(action.mode) && !isLoggedIn(userState.loginState)) {
      return editor
    }
    return setModeState(action.mode, editor)
  },
  TOGGLE_CANVAS_IS_LIVE: (editor: EditorModel, derived: DerivedState): EditorModel => {
    // same as UPDATE_EDITOR_MODE, but clears the drag state
    if (isLiveMode(editor.mode)) {
      return setModeState(EditorModes.selectMode(editor.mode.controlId, false, 'none'), editor)
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
  SET_FORKING: (action: SetForking, editor: EditorModel): EditorModel => {
    return {
      ...editor,
      forking: action.forking,
    }
  },
  SET_COLLABORATORS: (action: SetCollaborators, editor: EditorModel): EditorModel => {
    return {
      ...editor,
      collaborators: action.collaborators,
    }
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
  TOGGLE_DATA_CAN_CONDENSE: (action: ToggleDataCanCondense, editor: EditorModel): EditorModel => {
    let working = { ...editor }
    for (const path of action.targets) {
      working = modifyOpenJsxElementAtPath(
        path,
        (element) => {
          const canCondense = canCondenseJSXElementChild(element)
          // remove any data-can-condense props
          const props = element.props.filter((prop) => !isDataCanCondenseProp(prop))
          // if it needs to switch to true, append the new prop
          if (!canCondense) {
            props.push(dataCanCondenseProp(true))
          }
          return {
            ...element,
            props: props,
          }
        },
        working,
      )
    }
    return working
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
      action.target == null
        ? // action.target == null means Canvas, which means storyboard root element
          forceNotNull(
            'found no element path for the storyboard root',
            getStoryboardElementPath(editor.projectContents, editor.canvas.openFile?.filename),
          )
        : action.target

    const withNewElement = modifyUnderlyingTargetElement(
      parentPath,
      editor,
      (element) => element,
      (success, _, underlyingFilePath) => {
        const components = getUtopiaJSXComponentsFromSuccess(success)

        const updatedImports = mergeImports(
          underlyingFilePath,
          getFilePathMappings(editor.projectContents),
          success.imports,
          action.importsToAdd,
        )

        const { imports, duplicateNameMapping } = updatedImports

        const fixedElement = renameJsxElementChild(action.jsxElement, duplicateNameMapping)

        const withInsertedElement = insertJSXElementChildren(
          childInsertionPath(parentPath),
          [fixedElement],
          components,
          action.indexPosition,
        )

        const uid = getUtopiaID(fixedElement)
        const newPath = EP.appendToPath(parentPath, uid)
        newSelectedViews.push(newPath)

        const updatedTopLevelElements = applyUtopiaJSXComponentsChanges(
          success.topLevelElements,
          withInsertedElement.components,
        )

        return {
          ...success,
          topLevelElements: updatedTopLevelElements,
          imports: imports,
        }
      },
    )

    return {
      ...withNewElement,
      leftMenu: { visible: editor.leftMenu.visible, selectedTab: LeftMenuTab.Navigator },
      selectedViews: newSelectedViews,
    }
  },
  REPLACE_JSX_ELEMENT: (action: ReplaceJSXElement, editor: EditorModel): EditorModel => {
    const withNewElement = modifyUnderlyingParseSuccessOnly(
      action.target,
      editor,
      (success, underlyingFilePath) => {
        const startingComponents = getUtopiaJSXComponentsFromSuccess(success)

        const originalElement = findJSXElementChildAtPath(
          startingComponents,
          EP.dynamicPathToStaticPath(action.target),
        )

        if (originalElement == null) {
          return success
        }

        const { imports, duplicateNameMapping } = mergeImports(
          underlyingFilePath,
          getFilePathMappings(editor.projectContents),
          success.imports,
          action.importsToAdd,
        )

        const fixedElement = (() => {
          const elemenWithOriginalUid = setUtopiaID(
            action.jsxElement,
            getUtopiaID(originalElement),
          ) as JSXElement

          const renamedJsxElement = renameJsxElementChild(
            elemenWithOriginalUid,
            duplicateNameMapping,
          )
          if (
            !isReplaceKeepChildrenAndStyleTarget(action.behaviour) ||
            originalElement.type !== 'JSX_ELEMENT'
          ) {
            return renamedJsxElement
          }

          // apply the style of original element on the new element
          const renamedJsxElementWithOriginalStyle = applyUpdateToJSXElement(
            renamedJsxElement,
            (props) => {
              const styleProps = getJSXAttribute(originalElement.props, 'style')
              if (styleProps == null) {
                return right(deleteJSXAttribute(props, 'style'))
              } else {
                return right(setJSXAttributesAttribute(props, 'style', styleProps))
              }
            },
          )

          if (originalElement.children.length > 0) {
            // apply the children of original element on the new element
            return {
              ...renamedJsxElementWithOriginalStyle,
              children: originalElement.children,
            }
          }
          return renamedJsxElementWithOriginalStyle
        })()

        const updatedComponents = transformJSXComponentAtPath(
          startingComponents,
          EP.dynamicPathToStaticPath(action.target),
          () => fixedElement,
        )

        const updatedTopLevelElements = applyUtopiaJSXComponentsChanges(
          success.topLevelElements,
          updatedComponents,
        )

        return {
          ...success,
          topLevelElements: updatedTopLevelElements,
          imports: imports,
        }
      },
    )

    return {
      ...withNewElement,
      leftMenu: { visible: editor.leftMenu.visible, selectedTab: LeftMenuTab.Navigator },
    }
  },
  REPLACE_MAPPED_ELEMENT: (action: ReplaceMappedElement, editor: EditorModel): EditorModel => {
    let newSelectedViews: ElementPath[] = []
    const parentPath =
      action.target == null
        ? // action.target == null means Canvas, which means storyboard root element
          forceNotNull(
            'found no element path for the storyboard root',
            getStoryboardElementPath(editor.projectContents, editor.canvas.openFile?.filename),
          )
        : EP.isIndexedElement(action.target)
        ? EP.parentPath(action.target)
        : action.target

    const withNewElement = modifyUnderlyingTarget(
      parentPath,
      editor,
      (element) => element,
      (success, _, underlyingFilePath): ParseSuccess => {
        const startingComponents = getUtopiaJSXComponentsFromSuccess(success)
        const updatedImports = mergeImports(
          underlyingFilePath,
          getFilePathMappings(editor.projectContents),
          success.imports,
          action.importsToAdd,
        )

        const renamedJsxElement = renameJsxElementChild(
          action.jsxElement,
          updatedImports.duplicateNameMapping,
        )

        const withInsertedElement = transformJSXComponentAtPath(
          startingComponents,
          EP.dynamicPathToStaticPath(parentPath),
          (parentElement) => {
            if (!isJSXMapExpression(parentElement)) {
              return parentElement
            }
            const mapFunction = parentElement.mapFunction
            if (!isJSExpressionOtherJavaScript(mapFunction)) {
              return parentElement
            }

            const uidToUse = Object.keys(mapFunction.elementsWithin)[0] ?? renamedJsxElement.uid
            return {
              ...parentElement,
              mapFunction: {
                ...mapFunction,
                elementsWithin: { [uidToUse]: { ...renamedJsxElement, uid: uidToUse } },
              },
            }
          },
        )

        const updatedTopLevelElements = applyUtopiaJSXComponentsChanges(
          success.topLevelElements,
          withInsertedElement,
        )

        return {
          ...success,
          topLevelElements: updatedTopLevelElements,
          imports: updatedImports.imports,
        }
      },
    )
    return {
      ...withNewElement,
      leftMenu: { visible: editor.leftMenu.visible, selectedTab: LeftMenuTab.Navigator },
      selectedViews: newSelectedViews,
    }
  },
  REPLACE_ELEMENT_IN_SCOPE: (action: ReplaceElementInScope, editor: EditorModel): EditorModel => {
    const replaceChildWithUid = (
      element: JSXElementChild,
      uid: string,
      replaceWith: JSXElementChild,
    ): JSXElementChild => {
      if (element.type !== 'JSX_ELEMENT' && element.type !== 'JSX_FRAGMENT') {
        return element
      }

      return {
        ...element,
        children: element.children.map((c) => (c.uid !== uid ? c : replaceWith)),
      }
    }

    const updateMapExpression = (
      element: JSXElementChild,
      valueToMap: JSExpression,
    ): JSXElementChild => {
      if (element.type !== 'JSX_MAP_EXPRESSION') {
        return element
      }
      return {
        ...element,
        valueToMap: valueToMap,
      }
    }

    const replacePropertyValue = (
      element: JSXElementChild,
      propertyPath: PropertyPath,
      replaceWith: JSExpression,
    ): JSXElementChild => {
      if (element.type !== 'JSX_ELEMENT') {
        return element
      }
      return {
        ...element,
        props: defaultEither(
          element.props,
          setJSXValueAtPath(element.props, propertyPath, replaceWith),
        ),
      }
    }

    return modifyUnderlyingTarget(action.target, editor, (element) => {
      const replacementPath = action.replacementPath
      switch (replacementPath.type) {
        case 'replace-child-with-uid':
          return replaceChildWithUid(element, replacementPath.uid, replacementPath.replaceWith)
        case 'replace-property-value':
          return replacePropertyValue(
            element,
            replacementPath.propertyPath,
            replacementPath.replaceWith,
          )
        case 'update-map-expression':
          return updateMapExpression(element, replacementPath.valueToMap)
        default:
          assertNever(replacementPath)
      }
    })
  },
  INSERT_ATTRIBUTE_OTHER_JAVASCRIPT_INTO_ELEMENT: (
    action: InsertAttributeOtherJavascriptIntoElement,
    editor: EditorModel,
  ): EditorModel => {
    const withNewElement = modifyUnderlyingTargetJSXElement(action.parent, editor, (element) => {
      return {
        ...element,
        children: [action.expression],
      }
    })
    return {
      ...withNewElement,
      leftMenu: { visible: editor.leftMenu.visible, selectedTab: LeftMenuTab.Navigator },
    }
  },
  WRAP_IN_ELEMENT: (
    action: WrapInElement,
    editor: EditorModel,
    derived: DerivedState,
  ): EditorModel => {
    const orderedActionTargets = getZIndexOrderedViewsWithoutDirectChildren(
      action.targets,
      derived.navigatorTargets,
    )

    const parentPath = commonInsertionPathFromArray(
      editor.jsxMetadata,
      orderedActionTargets.map((actionTarget) => {
        return MetadataUtils.getReparentTargetOfTarget(editor.jsxMetadata, actionTarget)
      }),
      replaceWithSingleElement(),
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

    const anyTargetIsAnEmptyGroup = orderedActionTargets.some((path) =>
      isEmptyGroup(editor.jsxMetadata, path),
    )
    if (anyTargetIsAnEmptyGroup) {
      return UPDATE_FNS.ADD_TOAST(
        showToast(notice('Empty Groups cannot be wrapped', 'ERROR')),
        editor,
      )
    }

    if (
      isMaybeGroupForWrapping(action.whatToWrapWith.element, action.whatToWrapWith.importsToAdd) &&
      orderedActionTargets.some((path) => {
        return !elementCanBeAGroupChild(
          MetadataUtils.getJsxElementChildFromMetadata(editor.jsxMetadata, path),
          path,
          editor.jsxMetadata,
        )
      })
    ) {
      return UPDATE_FNS.ADD_TOAST(
        showToast(notice('Not all targets can be wrapped into a Group', 'ERROR')),
        editor,
      )
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
    const withFixedParents = fixParentContainingBlockSettings(updatedEditor, newPath)

    // TODO maybe update frames and position
    const frameChanges: Array<PinOrFlexFrameChange> = []
    const withWrapperViewAdded = {
      ...setCanvasFramesInnerNew(
        includeToast(detailsOfUpdate, withFixedParents),
        frameChanges,
        null,
      ),
    }

    const wrapperUID = generateUidWithExistingComponents(editor.projectContents)
    const intendedParentPath = EP.dynamicPathToStaticPath(newPath)
    const insertionBehavior =
      action.targets.length === 1
        ? replaceWithSingleElement()
        : replaceWithElementsWrappedInFragmentBehaviour(wrapperUID)

    let insertionResult: {
      editor: EditorModel
      newPaths: Array<ElementPath>
    }

    if (isJSXMapExpression(action.whatToWrapWith.element)) {
      // in maps we do not insert directly, but replace contents
      insertionResult = replaceInsideMap(
        orderedActionTargets,
        intendedParentPath,
        insertionBehavior,
        includeToast(detailsOfUpdate, withWrapperViewAdded),
      )
    } else if (isJSXConditionalExpression(action.whatToWrapWith.element)) {
      // for conditionals we're inserting into the true-case according to behavior
      insertionResult = insertIntoWrapper(
        orderedActionTargets,
        conditionalClauseInsertionPath(intendedParentPath, 'true-case', insertionBehavior),
        includeToast(detailsOfUpdate, withWrapperViewAdded),
      )
    } else {
      // otherwise we fall back to standard child insertion
      insertionResult = insertIntoWrapper(
        orderedActionTargets,
        childInsertionPath(intendedParentPath),
        includeToast(detailsOfUpdate, withWrapperViewAdded),
      )
    }

    const editorWithElementsInserted = insertionResult.editor
    const newPaths = insertionResult.newPaths

    return {
      ...editorWithElementsInserted,
      selectedViews: [intendedParentPath],
      leftMenu: { visible: editor.leftMenu.visible, selectedTab: LeftMenuTab.Navigator },
      highlightedViews: [],
      trueUpElementsAfterDomWalkerRuns: [
        ...editorWithElementsInserted.trueUpElementsAfterDomWalkerRuns,
        ...newPaths.map(trueUpGroupElementChanged),
      ],
    }
  },
  UNWRAP_ELEMENTS: (
    action: UnwrapElements,
    editor: EditorModel,
    builtInDependencies: BuiltInDependencies,
  ): EditorModel => {
    let groupTrueUps: ElementPath[] = []
    let viewsToDelete: ElementPath[] = []
    let newSelection: ElementPath[] = []

    // order paths by depth
    const orderedPaths = EP.getOrderedPathsByDepth(action.targets)

    // make sure to trim descendant paths, so that unwrapping on a subtree only works on the first ancestor
    const flattenedPaths = flattenSelection(orderedPaths)

    const withViewsUnwrapped: EditorState = flattenedPaths.reduce((workingEditor, target) => {
      const supportsChildren = MetadataUtils.targetSupportsChildren(
        workingEditor.projectContents,
        workingEditor.jsxMetadata,
        target,
        workingEditor.elementPathTree,
        workingEditor.propertyControlsInfo,
      )

      const elementIsFragmentLike = treatElementAsFragmentLike(
        workingEditor.jsxMetadata,
        workingEditor.allElementProps,
        workingEditor.elementPathTree,
        target,
      )

      if (!(supportsChildren || elementIsFragmentLike)) {
        return workingEditor
      }

      viewsToDelete.push(target)

      const parentPath = MetadataUtils.getReparentTargetOfTarget(editor.jsxMetadata, target)

      const indexPosition: IndexPosition = indexPositionForAdjustment(
        target,
        workingEditor,
        'forward',
      )
      const children = MetadataUtils.getChildrenOrdered(
        workingEditor.jsxMetadata,
        workingEditor.elementPathTree,
        target,
      ).reverse() // children are reversed so when they are readded one by one as 'forward' index they keep their original order
      const isGroupChild = treatElementAsGroupLike(workingEditor.jsxMetadata, EP.parentPath(target))

      if (parentPath != null && isConditionalClauseInsertionPath(parentPath)) {
        return unwrapConditionalClause(workingEditor, target, parentPath)
      }
      if (elementIsFragmentLike) {
        if (isTextContainingConditional(target, workingEditor.jsxMetadata)) {
          return unwrapTextContainingConditional(workingEditor, target)
        }

        const { editor: withChildrenMoved, newPaths } = editorMoveMultiSelectedTemplates(
          builtInDependencies,
          children.map((child) => child.elementPath),
          indexPosition,
          parentPath,
          workingEditor,
        )

        return {
          ...withChildrenMoved,
          selectedViews: newPaths,
          canvas: {
            ...withChildrenMoved.canvas,
            domWalkerInvalidateCount: workingEditor.canvas.domWalkerInvalidateCount + 1,
          },
        }
      } else {
        const parentFrame =
          parentPath == null
            ? (Utils.zeroRectangle as CanvasRectangle)
            : MetadataUtils.getFrameOrZeroRectInCanvasCoords(
                parentPath.intendedParentPath,
                workingEditor.jsxMetadata,
              )

        const withChildrenMoved = children.reduce((working, child) => {
          if (parentPath == null) {
            return working
          }
          const result = reparentElementToUnwrap(
            child.elementPath,
            parentPath,
            indexPosition,
            working,
            builtInDependencies,
          )
          if (result.newPath != null) {
            const newPath = result.newPath
            newSelection.push(newPath)
            if (isGroupChild) {
              groupTrueUps.push(newPath)
              return foldAndApplyCommandsSimple(
                result.editor,
                createPinChangeCommandsForElementBecomingGroupChild(
                  workingEditor.jsxMetadata,
                  child,
                  newPath,
                  parentFrame,
                  localRectangle(parentFrame),
                ),
              )
            }
            return result.editor
          }
          return working
        }, workingEditor)

        return {
          ...withChildrenMoved,
          canvas: {
            ...withChildrenMoved.canvas,
            domWalkerInvalidateCount: workingEditor.canvas.domWalkerInvalidateCount + 1,
          },
        }
      }
    }, editor)

    function adjustPathAfterWrap(paths: ElementPath[], path: ElementPath) {
      return paths
        .filter((other) => EP.isDescendantOf(path, other))
        .reduce((current, ancestor) => {
          return EP.replaceIfAncestor(current, ancestor, EP.parentPath(ancestor)) ?? current
        }, path)
    }
    const adjustedViewsToDelete = viewsToDelete.map((path) => {
      // make sure the paths to delete reflect the updated paths as per the unwrapping if there are nested
      // selected views under a common ancestor
      return adjustPathAfterWrap(viewsToDelete, path)
    })
    const adjustedGroupTrueUps = groupTrueUps.map((path) => {
      return adjustPathAfterWrap(groupTrueUps, path)
    })

    const withViewsDeleted = deleteElements(adjustedViewsToDelete, withViewsUnwrapped, {
      trueUpHuggingElements: false,
    })
    return {
      ...withViewsDeleted,
      selectedViews: newSelection,
      leftMenu: {
        visible: withViewsDeleted.leftMenu.visible,
        selectedTab: LeftMenuTab.Navigator,
      },
      trueUpElementsAfterDomWalkerRuns: [
        ...withViewsDeleted.trueUpElementsAfterDomWalkerRuns,
        ...adjustedGroupTrueUps.map(trueUpGroupElementChanged),
      ],
    }
  },
  SET_PANEL_VISIBILITY: (action: SetPanelVisibility, editor: EditorModel): EditorModel => {
    switch (action.target) {
      case 'leftmenu':
        return {
          ...editor,
          leftMenu: {
            ...editor.leftMenu,
            visible: action.visible,
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
            visible: action.visible,
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
      case 'codeEditor':
        return {
          ...editor,
          interfaceDesigner: {
            ...editor.interfaceDesigner,
            codePaneVisible: action.visible,
          },
        }
      case 'misccodeeditor':
      case 'canvas':
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
            visible: !editor.leftMenu.visible,
          },
        }
      case 'rightmenu':
        return {
          ...editor,
          rightMenu: {
            ...editor.rightMenu,
            visible: !editor.rightMenu.visible,
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
      case 'canvas':
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
        const filterForNames = action.type === 'layout' ? LayoutPropertyList : StyleProperties
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
    editor: EditorModel,
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

    const isEmptyGroupOnStoryboard = editor.selectedViews.some(
      (path) => EP.isStoryboardChild(path) && isEmptyGroup(editor.jsxMetadata, path),
    )
    if (isEmptyGroupOnStoryboard) {
      return UPDATE_FNS.ADD_TOAST(
        showToast(notice('Empty Groups on the storyboard cannot be cut', 'ERROR')),
        editor,
      )
    }

    const editorWithCopyData = copySelectionToClipboardMutating(editor, builtInDependencies)

    return UPDATE_FNS.DELETE_SELECTED(editorWithCopyData, dispatch)
  },
  COPY_PROPERTIES: (action: CopyProperties, editor: EditorModel): EditorModel => {
    if (editor.selectedViews.length === 0) {
      return editor
    } else {
      const target = editor.selectedViews[0]
      const styleProps = editor.currentAllElementProps[EP.toString(target)]?.style ?? {}
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
    // Show the menu if it's not already visible.
    if (!result.leftMenu.visible) {
      result = {
        ...result,
        leftMenu: {
          ...result.leftMenu,
          visible: true,
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
  ADD_COLLAPSED_VIEWS: (action: AddCollapsedViews, editor: EditorModel): EditorModel => {
    return {
      ...editor,
      navigator: {
        ...editor.navigator,
        collapsedViews: uniqBy(
          [...editor.navigator.collapsedViews, ...action.collapsedViews],
          EP.pathsEqual,
        ),
      },
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
  RESET_PINS: (action: ResetPins, editor: EditorModel): EditorModel => {
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
  UPDATE_FRAME_DIMENSIONS: (action: UpdateFrameDimensions, editor: EditorModel): EditorModel => {
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
    const withFrameUpdated = setCanvasFramesInnerNew(editor, frameChanges, null)
    return {
      ...withFrameUpdated,
      trueUpElementsAfterDomWalkerRuns: [
        ...withFrameUpdated.trueUpElementsAfterDomWalkerRuns,
        trueUpGroupElementChanged(action.element),
      ],
    }
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
      projectFile = assetFile(undefined, action.gitBlobSha)
    } else {
      // Assume IMAGE_FILE otherwise.
      projectFile = imageFile(
        action.fileType,
        undefined,
        width,
        height,
        action.hash,
        action.gitBlobSha,
      )
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
            userState,
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
    userState: UserState,
  ): EditorModel => {
    const possiblyAnImage = getProjectFileByFilePath(editor.projectContents, action.imagePath)
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
      return UPDATE_FNS.SWITCH_EDITOR_MODE(switchMode, editor, userState)
    } else {
      return editor
    }
  },
  SET_PROJECT_ID: (action: SetProjectID, editor: EditorModel): EditorModel => {
    return {
      ...editor,
      id: action.id,
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
      interfaceDesigner: {
        ...editor.interfaceDesigner,
        codePaneVisible: action.value,
      },
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

  UPDATE_PREVIEW_CONNECTED: (action: UpdatePreviewConnected, editor: EditorModel): EditorModel => {
    return produce(editor, (editorState) => {
      editorState.preview.connected = action.connected
    })
  },
  ALIGN_SELECTED_VIEWS: (action: AlignSelectedViews, editor: EditorModel): EditorModel => {
    return alignOrDistributeSelectedViews(editor, action.alignment)
  },
  DISTRIBUTE_SELECTED_VIEWS: (
    action: DistributeSelectedViews,
    editor: EditorModel,
  ): EditorModel => {
    return alignOrDistributeSelectedViews(editor, action.distribution)
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
    return updateFilePath(editor, userState, {
      oldPath: action.oldPath,
      newPath: action.newPath,
    })
  },
  UPDATE_REMIX_ROUTE: (
    action: UpdateRemixRoute,
    editor: EditorModel,
    userState: UserState,
  ): EditorModel => {
    const withUpdatedFilePath = updateFilePath(editor, userState, {
      oldPath: action.oldPath,
      newPath: action.newPath,
    })

    const withUpdatedFeaturedRoute = updatePackageJsonInEditorState(
      withUpdatedFilePath,
      addOrReplaceFeaturedRouteToPackageJson(action.oldRoute, action.newRoute),
    )

    return withUpdatedFeaturedRoute
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
    sendOpenFileMessage(action.filename, action.bounds)
    if (action.forceShowCodeEditor) {
      return {
        ...editor,
        interfaceDesigner: {
          ...editor.interfaceDesigner,
          codePaneVisible: true,
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
      getProjectFileByFilePath(editor.projectContents, action.filePath) == null
    ) {
      return editor
    }

    const { file } = action

    const existing = getProjectFileByFilePath(editor.projectContents, action.filePath)
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
    const newGithubSettings: ProjectGithubSettings = {
      ...editor.githubSettings,
      ...action.settings,
    }
    if (editor.id != null) {
      void updateGithubRepository(
        editor.id,
        newGithubSettings.targetRepository == null
          ? null
          : {
              owner: newGithubSettings.targetRepository.owner,
              repository: newGithubSettings.targetRepository.repository,
              branch: newGithubSettings.branchName,
            },
      )
    }
    return normalizeGithubData({
      ...editor,
      githubSettings: newGithubSettings,
    })
  },
  UPDATE_GITHUB_DATA: (action: UpdateGithubData, editor: EditorModel): EditorModel => {
    // merge the existing and the new public repos
    const combinedPublicRepos = [
      ...editor.githubData.publicRepositories,
      ...(action.data.publicRepositories ?? []),
    ]

    // sort public repos by descending updatedAt
    const sortedCombinedPublicRepos = combinedPublicRepos.sort((a, b) => {
      if (a.updatedAt == null) {
        return 1
      } else if (b.updatedAt == null) {
        return -1
      } else {
        return b.updatedAt.localeCompare(a.updatedAt)
      }
    })

    // remove duplicate entries
    const uniquePublicRepos = uniqBy(sortedCombinedPublicRepos, (a, b) => a.fullName === b.fullName)

    return {
      ...editor,
      githubData: {
        ...editor.githubData,
        ...action.data,
        lastUpdatedAt: Date.now(),
        publicRepositories: uniquePublicRepos,
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
  UPDATE_FROM_WORKER: (
    action: UpdateFromWorker,
    editor: EditorModel,
    userState: UserState,
  ): EditorModel => {
    let workingProjectContents: ProjectContentTreeRoot = editor.projectContents
    let anyParsedUpdates: boolean = false

    // This prevents partial updates to the model which can then cause UIDs to clash between files.
    // Where updates to files A and B resulted in new UIDs in each but as the update to one of those
    // files ends up stale only the model in one of them gets updated which clashes with the UIDs in
    // the old version of the other.
    for (const fileUpdate of action.updates) {
      const existing = getProjectFileByFilePath(editor.projectContents, fileUpdate.filePath)
      if (existing != null && isTextFile(existing)) {
        anyParsedUpdates = true
        const updateIsStale = fileUpdate.versionNumber < existing.versionNumber
        if (updateIsStale && action.updates.length > 1) {
          return {
            ...editor,
            parseOrPrintInFlight: false,
            previousParseOrPrintSkipped: true,
          }
        }
      }
    }

    // Apply the updates to the projectContents.
    const updateResult = processWorkerUpdates(workingProjectContents, action.updates)
    anyParsedUpdates = anyParsedUpdates || updateResult.anyParsedUpdates
    workingProjectContents = updateResult.projectContents

    if (anyParsedUpdates) {
      // Clear any cached paths since UIDs will have been regenerated and property paths may no longer exist
      // FIXME take a similar approach as ElementPath cache culling to the PropertyPath cache culling. Or don't even clear it.
      PP.clearPropertyPathCache()
    }
    return {
      ...editor,
      projectContents: createStoryboardFileIfNecessary(
        workingProjectContents,
        // If we are in the process of cloning a Github repository, do not create placeholder Storyboard
        userState.githubState.gitRepoToLoad != null
          ? 'skip-creating-placeholder'
          : 'create-placeholder',
      ),
      canvas: {
        ...editor.canvas,
        canvasContentInvalidateCount: anyParsedUpdates
          ? editor.canvas.canvasContentInvalidateCount + 1
          : editor.canvas.canvasContentInvalidateCount,
        domWalkerInvalidateCount: anyParsedUpdates
          ? editor.canvas.domWalkerInvalidateCount + 1
          : editor.canvas.domWalkerInvalidateCount,
      },
      parseOrPrintInFlight: false,
      previousParseOrPrintSkipped: false,
    }
  },
  UPDATE_FROM_CODE_EDITOR: (
    action: UpdateFromCodeEditor,
    editor: EditorModel,
    dispatch: EditorDispatch,
    builtInDependencies: BuiltInDependencies,
    serverState: ProjectServerState,
  ): EditorModel => {
    // Prevents updates from VS Code when the user is not the owner of the project.
    // Also fixes an issue with the collaboration where VS Code is firing one of these updates
    // immediately after it loads which then causes the collaboration update to fail because it's
    // viewed as being "stale".
    if (serverState.isMyProject === 'yes') {
      const existing = getProjectFileByFilePath(editor.projectContents, action.filePath)

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
    } else {
      return editor
    }
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
    const withAddedFile = addTextFile(
      editor,
      action.parentPath,
      action.fileName,
      codeFile('', null),
    )
    return UPDATE_FNS.OPEN_CODE_EDITOR_FILE(
      openCodeEditorFile(withAddedFile.newFileKey, false),
      withAddedFile.editorState,
    )
  },
  ADD_NEW_PAGE: (action: AddNewPage, editor: EditorModel): EditorModel => {
    const newFileName = `${action.newPageName}.jsx` // TODO maybe reuse the original extension?

    const templateFile = getProjectFileByFilePath(editor.projectContents, action.template.path)
    if (templateFile == null || !isTextFile(templateFile)) {
      // nothing to do
      return editor
    }

    // 1. add the new page to the featured routes
    const withPackageJson = updatePackageJsonInEditorState(
      editor,
      addNewFeaturedRouteToPackageJson(action.newPageName),
    )

    // 2. Parse the file upfront.
    const existingUIDs = new Set(getAllUniqueUids(editor.projectContents).uniqueIDs)
    const parsedResult = getParseFileResult(
      newFileName,
      getFilePathMappings(editor.projectContents),
      templateFile.fileContents.code,
      null,
      1,
      existingUIDs,
      isSteganographyEnabled(),
    )

    // 3. write the new text file
    const withTextFile = addTextFile(
      withPackageJson,
      action.parentPath,
      newFileName,
      textFile(
        textFileContents(
          templateFile.fileContents.code,
          parsedResult.parseResult,
          RevisionsState.CodeAhead,
        ),
        null,
        null,
        1,
      ),
    )

    // 4. open the text file
    return UPDATE_FNS.OPEN_CODE_EDITOR_FILE(
      openCodeEditorFile(withTextFile.newFileKey, false),
      withTextFile.editorState,
    )
  },
  ADD_NEW_FEATURED_ROUTE: (action: AddNewFeaturedRoute, editor: EditorModel): EditorModel => {
    return updatePackageJsonInEditorState(
      editor,
      addNewFeaturedRouteToPackageJson(action.featuredRoute),
    )
  },
  REMOVE_FEATURED_ROUTE: (action: RemoveFeaturedRoute, editor: EditorModel): EditorModel => {
    return updatePackageJsonInEditorState(
      editor,
      removeFeaturedRouteFromPackageJson(action.routeToRemove),
    )
  },
  DELETE_FILE: (
    action: DeleteFile | DeleteFileFromVSCode | DeleteFileFromCollaboration,
    editor: EditorModel,
    derived: DerivedState,
    userState: UserState,
  ): EditorModel => {
    const file = getProjectFileByFilePath(editor.projectContents, action.filename)

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
        const nextEditor = {
          ...editor,
          projectContents: updatedProjectContents,
        }
        if (isComponentDescriptorFile(action.filename)) {
          const withUpdatedPropertyControls = {
            ...nextEditor,
            propertyControlsInfo: updatePropertyControlsOnDescriptorFileDelete(
              editor.propertyControlsInfo,
              action.filename,
            ),
          }
          return removeErrorMessagesForFile(withUpdatedPropertyControls, action.filename)
        }
        return nextEditor
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
  SET_CODE_EDITOR_COMPONENT_DESCRIPTOR_ERRORS: (
    action: SetCodeEditorComponentDescriptorErrors,
    editor: EditorModel,
  ): EditorModel => {
    const allComponentDescriptorErrorsInState = getAllComponentDescriptorErrors(
      editor.codeEditorErrors,
    )
    const allComponentDescriptorErrorsInAction = Utils.flatMapArray(
      (filename) => action.componentDescriptorErrors[filename],
      Object.keys(action.componentDescriptorErrors),
    )
    if (
      allComponentDescriptorErrorsInState.length === 0 &&
      allComponentDescriptorErrorsInAction.length === 0
    ) {
      return editor
    } else {
      const updatedCodeEditorErrors = Object.keys(action.componentDescriptorErrors).reduce(
        (acc, filename) => {
          return {
            ...acc,
            componentDescriptorErrors: {
              ...acc.componentDescriptorErrors,
              [filename]: action.componentDescriptorErrors[filename],
            },
          }
        },
        editor.codeEditorErrors,
      )
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
        // TODO move the reconstructMetadata call here, and remove currentAllElementProps
        domMetadata: finalDomMetadata,
        spyMetadata: finalSpyMetadata,
        currentAllElementProps: {
          ...spyCollector.current.spyValues.allElementProps,
        },
        currentVariablesInScope: { ...spyCollector.current.spyValues.variablesInScope },
      }
    }
  },
  TRUE_UP_ELEMENTS: (editor: EditorModel): EditorModel => {
    const commandsToRun = getCommandsForPushIntendedBounds(
      editor.jsxMetadata,
      editor.elementPathTree,
      editor.trueUpElementsAfterDomWalkerRuns,
      'live-metadata',
    )
    const editorAfterTrueUps = foldAndApplyCommandsSimple(editor, commandsToRun)
    return {
      ...editorAfterTrueUps,
      trueUpElementsAfterDomWalkerRuns: [],
    }
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
  SET_MAP_COUNT_OVERRIDE: (action: SetMapCountOverride, editor: EditorModel): EditorModel => {
    return modifyOpenJsxChildAtPath(
      action.target,
      (element) => {
        if (!isJSXMapExpression(element)) {
          return element
        }

        function isNotMapCountFlag(c: Comment): boolean {
          return !isUtopiaCommentFlag(c, 'map-count')
        }

        const leadingComments = [...element.comments.leadingComments.filter(isNotMapCountFlag)]
        if (action.value != null) {
          leadingComments.push(makeUtopiaFlagComment({ type: 'map-count', value: action.value }))
        }

        return {
          ...element,
          comments: {
            leadingComments: leadingComments,
            trailingComments: element.comments.trailingComments.filter(isNotMapCountFlag),
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
            [],
            action.expression,
            action.expression,
            action.expression,
            oldDefinedElsewhere,
            null,
            oldElementsWithin,
            emptyComments,
          ),
          originalConditionString: action.expression,
        }
      },
      editor,
    )
  },
  ADD_IMPORTS: (action: AddImports, editor: EditorModel): EditorModel => {
    let duplicateNames = new Map<string, string>()
    return modifyUnderlyingTargetElement(
      action.target,
      editor,
      (element) => renameJsxElementChild(element, duplicateNames),
      (success, _, underlyingFilePath) => {
        const { imports, duplicateNameMapping } = mergeImports(
          underlyingFilePath,
          getFilePathMappings(editor.projectContents),
          success.imports,
          action.importsToAdd,
        )
        duplicateNames = duplicateNameMapping
        return {
          ...success,
          imports: imports,
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
    return {
      ...editor,
      propertyControlsInfo: action.propertyControlsInfo,
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
          (element): JSXElement | JSXFragment | JSXConditionalExpression => {
            if (isJSXElement(element) || isJSXFragment(element)) {
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
            } else {
              throw new Error('Not an element with children.')
            }
          },
          editorStore.unpatchedEditor,
        )
      } else if (textProp === 'itself') {
        return modifyOpenJsxChildAtPath(
          action.target,
          (element): JSXElementChild => {
            const comments = 'comments' in element ? element.comments : emptyComments
            if (isJSExpression(element) && isActionTextExpression) {
              const code = action.text.slice(1, -1)
              return jsExpressionOtherJavaScript(
                [],
                code,
                code,
                code,
                getDefinedElsewhereFromElementChild([], element),
                null,
                {},
                comments,
                element.uid,
              )
            }
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
          (): JSXElementChild => {
            // the whole expression will be reparsed again so we can just save it as a text block
            return jsxTextBlock(action.text)
          },
          editorStore.unpatchedEditor,
        )
      } else if (textProp === 'whenFalse' || textProp === 'whenTrue') {
        return modifyOpenJsxElementOrConditionalAtPath(
          action.target,
          (element): JSXElement | JSXFragment | JSXConditionalExpression => {
            if (isJSXConditionalExpression(element)) {
              return modify(
                jsxConditionalExpressionConditionOptic(textProp),
                (textElement): JSXElementChild => {
                  if (isJSExpression(textElement) && isActionTextExpression) {
                    const comments =
                      'comments' in textElement ? textElement.comments : emptyComments
                    const code = action.text.slice(1, -1)
                    return jsExpressionOtherJavaScript(
                      [],
                      code,
                      code,
                      code,
                      getDefinedElsewhereFromElementChild([], textElement),
                      null,
                      {},
                      comments,
                      textElement.uid,
                    )
                  } else {
                    return jsExpressionValue(action.text, emptyComments, textElement.uid)
                  }
                },
                element,
              )
            }
            return element
          },
          editorStore.unpatchedEditor,
        )
      } else {
        assertNever(textProp)
      }
    })()
    const withGroupTrueUpQueued: EditorState = addToTrueUpElements(
      withUpdatedText,
      trueUpGroupElementChanged(action.target),
    )

    const withCollapsedElements = collapseTextElements(action.target, withGroupTrueUpQueued)

    let withFileChanges: EditorStoreUnpatched
    if (withGroupTrueUpQueued === withCollapsedElements) {
      withFileChanges = {
        ...editorStore,
        unpatchedEditor: withGroupTrueUpQueued,
      }
    } else {
      withFileChanges = {
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

    // Find the text files that changed as a result of the update.
    let changedTextFilenames: Array<string> = []
    zipContentsTree(
      editorStore.unpatchedEditor.projectContents,
      withFileChanges.unpatchedEditor.projectContents,
      (fullPath, oldContents, newContents) => {
        if (
          isProjectContentFile(oldContents) &&
          isProjectContentFile(newContents) &&
          isTextFile(oldContents.content) &&
          isTextFile(newContents.content)
        ) {
          const oldTextFile = oldContents.content
          const newTextFile = newContents.content
          if (oldTextFile !== newTextFile) {
            changedTextFilenames.push(fullPath)
          }
        }

        return true
      },
    )

    // Accumulate the details of the data we need to update those files.
    const filesToUpdateResult = getFilesToUpdate(
      withFileChanges.unpatchedEditor.projectContents,
      changedTextFilenames,
    )

    const filePathMappings = getFilePathMappings(withFileChanges.unpatchedEditor.projectContents)
    // For those files that changed, do a print-parse against each file.
    const workerUpdates = filesToUpdateResult.filesToUpdate.flatMap((fileToUpdate) => {
      if (fileToUpdate.type === 'printandreparsefile') {
        const printParsedContent = getPrintAndReparseCodeResult(
          fileToUpdate.filename,
          filePathMappings,
          fileToUpdate.parseSuccess,
          fileToUpdate.stripUIDs,
          fileToUpdate.versionNumber,
          filesToUpdateResult.existingUIDs,
          isSteganographyEnabled(),
        )
        const updateAction = workerCodeAndParsedUpdate(
          printParsedContent.filename,
          printParsedContent.printResult,
          printParsedContent.parseResult,
          printParsedContent.versionNumber,
        )
        return [updateAction]
      } else {
        return []
      }
    })

    const updatedEditorState = UPDATE_FNS.UPDATE_FROM_WORKER(
      updateFromWorker(workerUpdates),
      withFileChanges.unpatchedEditor,
      withFileChanges.userState,
    )
    return {
      ...withFileChanges,
      unpatchedEditor: updatedEditorState,
    }
  },
  TRUNCATE_HISTORY: (editorStore: EditorStoreUnpatched): EditorStoreUnpatched => {
    return {
      ...editorStore,
      history: History.init(editorStore.unpatchedEditor, editorStore.unpatchedDerived),
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
        derived.filePathMappings,
        editor.propertyControlsInfo,
        editor.projectContents,
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
  SCROLL_TO_POSITION: (
    action: ScrollToPosition,
    editor: EditorModel,
    dispatch: EditorDispatch,
  ): EditorModel => {
    const isLeftMenuOpen = editor.leftMenu.visible
    const containerRootDiv = document.getElementById('canvas-root')
    const panelOffsets = canvasPanelOffsets()
    const scale = 1 / editor.canvas.scale

    // This returns the offset used as the fallback for the other behaviours when the container bounds are not defined.
    // It will effectively scroll to the element by positioning it at the origin (TL) of the
    // canvas, based on the BaseCanvasOffset value(s).
    function canvasOffsetToOrigin(frame: CanvasRectangle): CanvasVector {
      const baseCanvasOffset = isLeftMenuOpen ? BaseCanvasOffsetLeftPane : BaseCanvasOffset
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
        x:
          canvasCenter.x -
          frame.width / 2 -
          bounds.x +
          (panelOffsets.left / 2) * scale -
          (panelOffsets.right / 2) * scale,
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
        x: panelOffsets.left - editor.canvas.realCanvasOffset.x,
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

    const newCanvasOffset = getNewCanvasOffset(action.target)
    return newCanvasOffset == null
      ? editor
      : UPDATE_FNS.SET_SCROLL_ANIMATION(
          setScrollAnimation(true),
          {
            ...editor,
            canvas: {
              ...editor.canvas,
              realCanvasOffset: newCanvasOffset,
              roundedCanvasOffset: roundPointToNearestWhole(newCanvasOffset),
            },
          },
          dispatch,
        )
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
    if (
      targetElementCoords != null &&
      isFiniteRectangle(targetElementCoords) &&
      !isFollowMode(editor.mode)
    ) {
      return UPDATE_FNS.SCROLL_TO_POSITION(
        scrollToPosition(targetElementCoords, action.behaviour),
        editor,
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
      githubState: {
        ...userState.githubState,
        ...action.githubState,
      },
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

      const existingUids = new Set(getAllUniqueUids(editor.projectContents).uniqueIDs)

      const newUID = generateConsistentUID('new', existingUids)

      const newPath = EP.appendToPath(action.insertionPath.intendedParentPath, newUID)

      let element = action.toInsert.element()

      const withNewElement = modifyUnderlyingTargetElement(
        insertionPath.intendedParentPath,
        editor,
        identity,
        (success, _, underlyingFilePath) => {
          const utopiaComponents = getUtopiaJSXComponentsFromSuccess(success)

          const updatedImports = mergeImports(
            underlyingFilePath,
            getFilePathMappings(editor.projectContents),
            success.imports,
            action.toInsert.importsToAdd,
          )

          const { imports, duplicateNameMapping } = updatedImports
          element = renameJsxElementChildWithoutId(element, duplicateNameMapping)

          if (element.type === 'JSX_ELEMENT') {
            const propsWithUid = forceRight(
              setJSXValueAtPath(
                element.props,
                PP.create(UTOPIA_UID_KEY),
                jsExpressionValue(newUID, emptyComments),
              ),
              `Could not set data-uid on props of insertable element ${element.name}`,
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

            const insertedElementName = element.name
            let withMaybeUpdatedParent = utopiaComponents
            let insertedElementChildren: JSXElementChildren = []

            insertedElementChildren.push(...element.children)
            const fixedElement = fixUtopiaElement(
              jsxElement(insertedElementName, newUID, props, insertedElementChildren),
              existingUids,
            ).value

            withInsertedElement = insertJSXElementChildren(
              insertionPath,
              [fixedElement],
              withMaybeUpdatedParent,
              action.indexPosition,
            )
            detailsOfUpdate = withInsertedElement.insertionDetails

            addNewSelectedView(newUID)
          } else if (element.type === 'JSX_CONDITIONAL_EXPRESSION') {
            const fixedElement = fixUtopiaElement(
              jsxConditionalExpression(
                newUID,
                element.condition,
                element.originalConditionString,
                element.whenTrue,
                element.whenFalse,
                element.comments,
              ),
              existingUids,
            ).value

            withInsertedElement = insertJSXElementChildren(
              insertionPath,
              [fixedElement],
              utopiaComponents,
              action.indexPosition,
            )
            detailsOfUpdate = withInsertedElement.insertionDetails
            newSelectedViews.push(newPath)
          } else if (element.type === 'JSX_FRAGMENT') {
            const fixedElement = jsxFragment(newUID, element.children, element.longForm)

            withInsertedElement = insertJSXElementChildren(
              insertionPath,
              [fixedElement],
              utopiaComponents,
              action.indexPosition,
            )
            detailsOfUpdate = withInsertedElement.insertionDetails

            addNewSelectedView(newUID)
          } else if (element.type === 'JSX_MAP_EXPRESSION') {
            const fixedElement = fixUtopiaElement({ ...element, uid: newUID }, existingUids).value

            withInsertedElement = insertJSXElementChildren(
              insertionPath,
              [fixedElement],
              utopiaComponents,
              action.indexPosition,
            )
            detailsOfUpdate = withInsertedElement.insertionDetails

            addNewSelectedView(newUID)
          } else {
            assertNever(element)
          }

          const updatedTopLevelElements = applyUtopiaJSXComponentsChanges(
            success.topLevelElements,
            withInsertedElement.components,
          )

          return {
            ...success,
            topLevelElements: updatedTopLevelElements,
            imports: imports,
          }
        },
      )

      let groupCommands: CanvasCommand[] = []
      if (treatElementAsGroupLike(editor.jsxMetadata, action.insertionPath.intendedParentPath)) {
        const group = MetadataUtils.findElementByElementPath(
          editor.jsxMetadata,
          action.insertionPath.intendedParentPath,
        )
        if (group != null) {
          switch (element.type) {
            case 'JSX_ELEMENT':
              groupCommands.push(
                ...createPinChangeCommandsForElementInsertedIntoGroup(
                  newPath,
                  right(element.props),
                  zeroRectIfNullOrInfinity(group.globalFrame),
                  zeroRectIfNullOrInfinity(group.localFrame),
                ),
              )
              break
            case 'JSX_CONDITIONAL_EXPRESSION':
              if (element.whenTrue != null || element.whenFalse != null) {
                // FIXME: This is a mid-step, as the conditional being inserted currently
                // has nulls in both clauses, resulting in a zero-sized element.
                groupCommands.push(
                  queueTrueUpElement([
                    trueUpChildrenOfGroupChanged(action.insertionPath.intendedParentPath),
                  ]),
                )
              }
              break
            case 'JSX_FRAGMENT':
              if (element.children.length > 0) {
                // this needs updating when we support inserting fragments with children
                throw new Error('unhandled fragment insert into group')
              }
              break
            default:
              assertNever(action.toInsert.element as never)
          }
        }
      }

      const updatedEditorState: EditorModel = foldAndApplyCommandsSimple(
        {
          ...withNewElement,
          selectedViews: newSelectedViews,
          leftMenu: { visible: editor.leftMenu.visible, selectedTab: LeftMenuTab.Navigator },
          trueUpElementsAfterDomWalkerRuns: [
            ...editor.trueUpElementsAfterDomWalkerRuns,
            trueUpGroupElementChanged(newPath),
          ],
        },
        groupCommands,
      )

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
    const packageJsonFile = getProjectFileByFilePath(editor.projectContents, '/package.json')
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
    if (getProjectFileByFilePath(editor.projectContents, TailwindConfigPath) == null) {
      updatedProjectContents = addFileToProjectContents(
        editor.projectContents,
        TailwindConfigPath,
        DefaultTailwindConfig(),
      )
    }

    if (getProjectFileByFilePath(editor.projectContents, PostCSSPath) == null) {
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
        action.setHuggingParentToFixed,
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
      (success) => success,
    )

    return updatedEditor
  },
  UPDATE_TOP_LEVEL_ELEMENTS_FROM_COLLABORATION_UPDATE: (
    action: UpdateTopLevelElementsFromCollaborationUpdate,
    editor: EditorModel,
  ): EditorModel => {
    let updatedEditor: EditorModel = editor
    if (fileExists(editor.projectContents, action.fullPath)) {
      updatedEditor = modifyParseSuccessAtPath(
        action.fullPath,
        editor,
        (parsed) => {
          const newTopLevelElementsDeepEquals = arrayDeepEquality(TopLevelElementKeepDeepEquality)(
            parsed.topLevelElements,
            action.topLevelElements,
          )

          if (newTopLevelElementsDeepEquals.areEqual) {
            return parsed
          } else {
            const alreadyExistingUIDs = getAllUniqueUids(
              removeFromProjectContents(editor.projectContents, action.fullPath),
            ).uniqueIDs
            const fixUIDsState: FixUIDsState = {
              mutableAllNewUIDs: new Set(alreadyExistingUIDs),
              uidsExpectedToBeSeen: new Set(),
              mappings: [],
              uidUpdateMethod: 'copy-uids-fix-duplicates',
            }
            const fixedUpTopLevelElements = fixTopLevelElementsUIDs(
              parsed.topLevelElements,
              newTopLevelElementsDeepEquals.value,
              fixUIDsState,
            )

            return {
              ...parsed,
              topLevelElements: fixedUpTopLevelElements,
            }
          }
        },
        false,
      )
    } else {
      const newParseSuccess = parseSuccess({}, action.topLevelElements, {}, null, null, [], {})
      const newFile = textFile(
        textFileContents('', newParseSuccess, RevisionsState.ParsedAhead),
        null,
        null,
        1,
      )
      const updatedProjectContents = addFileToProjectContents(
        editor.projectContents,
        action.fullPath,
        newFile,
      )
      updatedEditor = {
        ...editor,
        projectContents: updatedProjectContents,
      }
    }

    return {
      ...updatedEditor,
      filesModifiedByAnotherUser: updatedEditor.filesModifiedByAnotherUser.concat(action.fullPath),
    }
  },
  UPDATE_EXPORTS_DETAIL_FROM_COLLABORATION_UPDATE: (
    action: UpdateExportsDetailFromCollaborationUpdate,
    editor: EditorModel,
  ): EditorModel => {
    let updatedEditor: EditorModel = editor
    if (fileExists(editor.projectContents, action.fullPath)) {
      updatedEditor = modifyParseSuccessAtPath(
        action.fullPath,
        editor,
        (parsed) => {
          const newExportsDetailsDeepEquals = arrayDeepEquality(ExportDetailKeepDeepEquality)(
            parsed.exportsDetail,
            action.exportsDetail,
          )

          if (newExportsDetailsDeepEquals.areEqual) {
            return parsed
          } else {
            return {
              ...parsed,
              exportsDetail: newExportsDetailsDeepEquals.value,
            }
          }
        },
        false,
      )
    } else {
      const newParseSuccess = parseSuccess({}, [], {}, null, null, action.exportsDetail, {})
      const newFile = textFile(
        textFileContents('', newParseSuccess, RevisionsState.ParsedAhead),
        null,
        null,
        1,
      )
      const updatedProjectContents = addFileToProjectContents(
        editor.projectContents,
        action.fullPath,
        newFile,
      )
      updatedEditor = {
        ...editor,
        projectContents: updatedProjectContents,
      }
    }

    return {
      ...updatedEditor,
      filesModifiedByAnotherUser: updatedEditor.filesModifiedByAnotherUser.concat(action.fullPath),
    }
  },
  UPDATE_IMPORTS_FROM_COLLABORATION_UPDATE: (
    action: UpdateImportsFromCollaborationUpdate,
    editor: EditorModel,
  ): EditorModel => {
    let updatedEditor: EditorModel = editor
    if (fileExists(editor.projectContents, action.fullPath)) {
      updatedEditor = modifyParseSuccessAtPath(
        action.fullPath,
        editor,
        (parsed) => {
          const newImportsDeepEquals = objectDeepEquality(ImportDetailsKeepDeepEquality)(
            parsed.imports,
            action.imports,
          )

          if (newImportsDeepEquals.areEqual) {
            return parsed
          } else {
            return {
              ...parsed,
              imports: newImportsDeepEquals.value,
            }
          }
        },
        false,
      )
    } else {
      const newParseSuccess = parseSuccess(action.imports, [], {}, null, null, [], {})
      const newFile = textFile(
        textFileContents('', newParseSuccess, RevisionsState.ParsedAhead),
        null,
        null,
        1,
      )
      const updatedProjectContents = addFileToProjectContents(
        editor.projectContents,
        action.fullPath,
        newFile,
      )
      updatedEditor = {
        ...editor,
        projectContents: updatedProjectContents,
      }
    }

    return {
      ...updatedEditor,
      filesModifiedByAnotherUser: updatedEditor.filesModifiedByAnotherUser.concat(action.fullPath),
    }
  },
  UPDATE_CODE_FROM_COLLABORATION_UPDATE: (
    action: UpdateCodeFromCollaborationUpdate,
    editor: EditorModel,
    dispatch: EditorDispatch,
    builtInDependencies: BuiltInDependencies,
  ): EditorModel => {
    const existing = getProjectFileByFilePath(editor.projectContents, action.fullPath)

    let updatedFile: ProjectFile
    if (existing == null || !isTextFile(existing)) {
      const contents = textFileContents(action.code, unparsed, RevisionsState.CodeAhead)
      updatedFile = textFile(contents, null, null, 0)
    } else {
      updatedFile = updateFileContents(action.code, existing, false)
    }

    const updateAction = updateFile(action.fullPath, updatedFile, true)
    return UPDATE_FNS.UPDATE_FILE(updateAction, editor, dispatch, builtInDependencies)
  },
  SET_SHOW_RESOLVED_THREADS: (action: SetCommentFilterMode, editor: EditorModel): EditorModel => {
    return { ...editor, commentFilterMode: action.commentFilterMode }
  },
  EXTRACT_PROPERTY_CONTROLS_FROM_DESCRIPTOR_FILES: (
    action: ExtractPropertyControlsFromDescriptorFiles,
    state: EditorState,
    workers: UtopiaTsWorkers,
    dispatch: EditorDispatch,
  ): EditorModel => {
    const evaluator = createModuleEvaluator(state)

    const filesToUpdate: TextFileContentsWithPath[] = []
    for (const filePath of action.paths) {
      const file = getProjectFileByFilePath(state.projectContents, filePath)
      if (file != null && file.type === 'TEXT_FILE') {
        filesToUpdate.push({ path: filePath, file: file.fileContents })
      }
    }

    void maybeUpdatePropertyControls(
      state.propertyControlsInfo,
      filesToUpdate,
      workers,
      dispatch,
      evaluator,
    )
    return state
  },
  SET_SHARING_DIALOG_OPEN: (action: SetSharingDialogOpen, editor: EditorModel): EditorModel => {
    return {
      ...editor,
      sharingDialogOpen: action.open,
    }
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
  alignmentOrDistribution: Alignment | Distribution,
): EditorModel {
  const selectedViews = editor.selectedViews

  let groupTrueUps: Array<TrueUpTarget> = [
    ...editor.trueUpElementsAfterDomWalkerRuns,
    ...selectedViews
      .filter((path) => treatElementAsGroupLike(editor.jsxMetadata, EP.parentPath(path)))
      .map(trueUpGroupElementChanged),
  ]

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
      return {
        ...setCanvasFramesInnerNew(editor, updatedCanvasFrames, null),
        trueUpElementsAfterDomWalkerRuns: groupTrueUps,
      }
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
  const file = getProjectFileByFilePath(projectContents, filePath)
  if (file == null) {
    return projectContents
  } else {
    return addFileToProjectContents(projectContents, filePath, saveFile(file))
  }
}

const DefaultStoryboardWithRemix = `import * as React from 'react'
import { Storyboard, RemixScene } from 'utopia-api'

export var storyboard = (
  <Storyboard>
    <RemixScene
      style={{
        position: 'absolute',
        width: 644,
        height: 750,
        left: 200,
        top: 30,
        overflow: 'hidden',
      }}
      data-label='Mood Board'
    />
  </Storyboard>
)
`

const DefaultStoryboardContents = `import * as React from 'react'
import { Scene, Storyboard } from 'utopia-api'

export var storyboard = (
  <Storyboard>
    <Scene
      style={{
        width: 603,
        height: 794,
        position: 'absolute',
        left: 212,
        top: 128,
        display: 'flex',
        flexDirection: 'column',
        padding: '253px 101px',
        alignItems: 'center',
        justifyContent: 'center',
      }}
    >
      <span
        style={{
          wordBreak: 'break-word',
          fontSize: '25px',
          width: 257,
          height: 130,
        }}
      >
        Open the insert menu or press the + button in the
        toolbar to insert components
      </span>
    </Scene>
  </Storyboard>
  )
`

function addTextFile(
  editor: EditorState,
  parentPath: string,
  fileName: string,
  newTextFile: TextFile,
): { editorState: EditorState; newFileKey: string } {
  const pathPrefix = parentPath == '' ? '' : parentPath + '/'
  const newFileKey = uniqueProjectContentID(pathPrefix + fileName, editor.projectContents)

  const updatedProjectContents = addFileToProjectContents(
    editor.projectContents,
    newFileKey,
    newTextFile,
  )

  // Update the model.
  return {
    editorState: {
      ...editor,
      projectContents: updatedProjectContents,
    },
    newFileKey: newFileKey,
  }
}

function updateFilePath(
  editor: EditorModel,
  userState: UserState,
  params: {
    oldPath: string
    newPath: string
  },
): EditorState {
  const replaceFilePathResults = replaceFilePath(
    params.oldPath,
    params.newPath,
    editor.projectContents,
  )
  if (replaceFilePathResults.type === 'FAILURE') {
    const toastAction = showToast(notice(replaceFilePathResults.errorMessage, 'ERROR', true))
    return UPDATE_FNS.ADD_TOAST(toastAction, editor)
  } else {
    let currentDesignerFile = editor.canvas.openFile
    const { projectContents, updatedFiles, renamedOptionalPrefix } = replaceFilePathResults
    const mainUIFile = getMainUIFromModel(editor)
    let updateUIFile: (e: EditorModel) => EditorModel = (e) => e
    let updatePropertyControls: (e: EditorModel) => EditorModel = (e) => e
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
      const oldContent = getProjectFileByFilePath(editor.projectContents, oldPath)
      if (oldContent != null && (isImageFile(oldContent) || isAssetFile(oldContent))) {
        // Update assets.
        if (isLoggedIn(userState.loginState) && editor.id != null) {
          void updateAssetFileName(editor.id, stripLeadingSlash(oldPath), newPath)
        }
      }
      // when we rename a component descriptor file, we need to remove it from property controls
      // if the new name is a component descriptor filename too, the controls will be readded
      // if the new name is not a component descriptor filename, then we really have to remove the property controls
      if (isComponentDescriptorFile(oldPath) && oldPath !== newPath) {
        updatePropertyControls = (e: EditorModel) => ({
          ...e,
          propertyControlsInfo: updatePropertyControlsOnDescriptorFileDelete(
            e.propertyControlsInfo,
            oldPath,
          ),
        })
      }
    })

    const withUpdatedPropertyControls = updatePropertyControls(
      updateUIFile({
        ...editor,
        projectContents: projectContents,
        codeEditorErrors: {
          buildErrors: {},
          lintErrors: {},
          componentDescriptorErrors: {},
        },
        canvas: {
          ...editor.canvas,
          openFile: currentDesignerFile,
        },
      }),
    )

    return renamedOptionalPrefix
      ? UPDATE_FNS.ADD_TOAST(
          addToast(notice('Renamed Remix routes with optional prefixes.', 'NOTICE')),
          withUpdatedPropertyControls,
        )
      : withUpdatedPropertyControls
  }
}

function removeErrorMessagesForFile(editor: EditorState, filename: string): EditorState {
  const noErrors = { [filename]: [] }

  return UPDATE_FNS.SET_CODE_EDITOR_BUILD_ERRORS(
    setCodeEditorBuildErrors(noErrors),
    UPDATE_FNS.SET_CODE_EDITOR_LINT_ERRORS(
      setCodeEditorLintErrors(noErrors),
      UPDATE_FNS.SET_CODE_EDITOR_COMPONENT_DESCRIPTOR_ERRORS(
        setCodeEditorComponentDescriptorErrors(noErrors),
        editor,
      ),
    ),
  )
}
