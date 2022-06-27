import { produce } from 'immer'
import update from 'immutability-helper'
import React from 'react'
import localforage from 'localforage'
import { CursorPosition } from 'src/components/code-editor/code-editor-utils'
import { FramePoint, LayoutSystem } from 'utopia-api/core'
import {
  SampleFileBuildResult,
  SampleFileBundledExportsInfo,
} from '../../../bundled-dependencies/codeBundle'
import { imagePathURL, imagePathWithoutHashURL } from '../../../common/server'
import { LoginState } from '../../../common/user'
import {
  FlexLayoutHelpers,
  LayoutHelpers,
  PinLayoutHelpers,
} from '../../../core/layout/layout-helpers'
import {
  maybeSwitchChildrenLayoutProps,
  maybeSwitchLayoutProps,
  roundAttributeLayoutValues,
  switchLayoutMetadata,
} from '../../../core/layout/layout-utils'
import {
  findElementAtPath,
  MetadataUtils,
  findJSXElementAtPath,
} from '../../../core/model/element-metadata-utils'
import {
  DetectedLayoutSystem,
  ElementInstanceMetadata,
  getJSXElementNameAsString,
  isJSXAttributeFunctionCall,
  isJSXAttributeValue,
  isJSXElement,
  isPartOfJSXAttributeValue,
  JSXAttribute,
  jsxAttributeFunctionCall,
  JSXAttributeFunctionCall,
  jsxAttributeOtherJavaScript,
  JSXAttributes,
  jsxAttributeValue,
  JSXAttributeValue,
  JSXElement,
  jsxElement,
  JSXElementName,
  jsxElementName,
  UtopiaJSXComponent,
  isJSXAttributeOtherJavaScript,
  SettableLayoutSystem,
  walkElements,
  ElementInstanceMetadataMap,
  jsxTextBlock,
  isJSXTextBlock,
  getJSXAttribute,
  jsxAttributesFromMap,
  deleteJSXAttribute,
  setJSXAttributesAttribute,
  emptyJsxMetadata,
  isImportStatement,
  isIntrinsicHTMLElement,
  JSXElementChildren,
  emptyComments,
} from '../../../core/shared/element-template'
import {
  generateUidWithExistingComponents,
  getUtopiaID,
  setUtopiaID,
  transformJSXComponentAtElementPath,
  insertJSXElementChild,
  findJSXElementChildAtPath,
  getZIndexOfElement,
  elementOnlyHasSingleTextChild,
  transformJSXComponentAtPath,
  guaranteeUniqueUids,
  getAllUniqueUids,
} from '../../../core/model/element-template-utils'
import {
  getJSXAttributeAtPath,
  jsxSimpleAttributeToValue,
  setJSXValueAtPath,
  unsetJSXValueAtPath,
  getModifiableJSXAttributeAtPath,
  unsetJSXValuesAtPaths,
  setJSXValuesAtPaths,
  ValueAtPath,
} from '../../../core/shared/jsx-attributes'
import {
  canUpdateFile,
  directory,
  fileTypeFromFileName,
  getHighlightBoundsFromParseResult,
  getUtopiaJSXComponentsFromSuccess,
  imageFile,
  isDirectory,
  isImageFile,
  isOlderThan,
  revertFile,
  saveFile,
  sceneMetadata,
  switchToFileType,
  uniqueProjectContentID,
  updateParsedTextFileHighlightBounds,
  assetFile,
  applyToAllUIJSFiles,
  updateFileContents,
  applyUtopiaJSXComponentsChanges,
  saveTextFileContents,
  isImg,
} from '../../../core/model/project-file-utils'
import {
  Either,
  foldEither,
  isLeft,
  isRight,
  left,
  right,
  eitherToMaybe,
  mapEither,
  defaultEither,
  forceRight,
  traverseEither,
} from '../../../core/shared/either'
import {
  RequireFn,
  TypeDefinitions,
  PackageStatus,
  PackageStatusMap,
  RequestedNpmDependency,
  requestedNpmDependency,
} from '../../../core/shared/npm-dependency-types'
import {
  isParseSuccess,
  isTextFile,
  ParsedTextFile,
  ProjectContents,
  ProjectFile,
  PropertyPath,
  RevisionsState,
  StaticElementPathPart,
  ElementPath,
  TextFile,
  isAssetFile,
  NodeModules,
  StaticElementPath,
  textFileContents,
  textFile,
  codeFile,
  unparsed,
  ParseSuccess,
  importAlias,
  Imports,
  importStatementFromImportDetails,
  importDetails,
} from '../../../core/shared/project-file-types'
import {
  addImport,
  codeNeedsParsing,
  codeNeedsPrinting,
  emptyImports,
  mergeImports,
} from '../../../core/workers/common/project-file-utils'
import { OutgoingWorkerMessage, UtopiaTsWorkers } from '../../../core/workers/common/worker-types'
import { defaultProject } from '../../../sample-projects/sample-project-utils'
import { KeysPressed, Key } from '../../../utils/keyboard'
import Utils, { IndexPosition } from '../../../utils/utils'
import {
  CanvasPoint,
  CanvasRectangle,
  LocalRectangle,
  Size,
  WindowPoint,
  canvasRectangle,
  Rectangle,
  rectangleIntersection,
} from '../../../core/shared/math-utils'
import {
  addFileToProjectContents,
  contentsToTree,
  ensureDirectoriesExist,
  getContentsTreeFileFromString,
  ProjectContentTreeRoot,
  removeFromProjectContents,
  treeToContents,
  walkContentsTree,
  walkContentsTreeForParseSuccess,
} from '../../assets'
import CanvasActions from '../../canvas/canvas-actions'
import {
  CanvasFrameAndTarget,
  CSSCursor,
  PinOrFlexFrameChange,
  pinSizeChange,
} from '../../canvas/canvas-types'
import {
  canvasFrameToNormalisedFrame,
  clearDragState,
  duplicate,
  editorMultiselectReparentNoStyleChange,
  getFrameChange,
  moveTemplate,
  produceCanvasTransientState,
  SkipFrameChange,
  updateFramesOfScenesAndComponents,
} from '../../canvas/canvas-utils'
import { EditorPane, EditorPanel, ResizeLeftPane, SetFocus } from '../../common/actions'
import { openMenu } from '../../context-menu-side-effect'
import {
  CodeResultCache,
  generateCodeResultCache,
  codeCacheToBuildResult,
  PropertyControlsInfo,
  normalisePathSuccessOrThrowError,
  normalisePathToUnderlyingTarget,
} from '../../custom-code/code-file'
import { ElementContextMenuInstance } from '../../element-context-menu'
import { getFilePathToImport } from '../../filebrowser/filepath-utils'
import { FontSettings } from '../../inspector/common/css-utils'
import { CSSTarget } from '../../inspector/sections/header-section/target-selector'
import * as PP from '../../../core/shared/property-path'
import * as EP from '../../../core/shared/element-path'
import {
  AddTextFile,
  AddFolder,
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
  DeleteSelected,
  DeleteView,
  DistributeSelectedViews,
  Distribution,
  DuplicateSelected,
  DuplicateSpecificElements,
  EditorAction,
  EditorDispatch,
  EditorModel,
  HideModal,
  InsertImageIntoUI,
  InsertJSXElement,
  InsertScene,
  isLoggedIn,
  Load,
  MoveSelectedBackward,
  MoveSelectedForward,
  MoveSelectedToBack,
  MoveSelectedToFront,
  NavigatorReorder,
  NewProject,
  OpenCodeEditorFile,
  OpenPopup,
  OpenTextEditor,
  PasteJSXElements,
  Redo,
  RedrawOldCanvasControls,
  RegenerateThumbnail,
  RenameComponent,
  RenameStyleSelector,
  ResetPins,
  ResizeInterfaceDesignerCodePane,
  SaveCurrentFile,
  SaveDOMReport,
  SaveAsset,
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
  SetHighlightedView,
  SetLeftMenuExpanded,
  SetLeftMenuTab,
  SetRightMenuExpanded,
  SetRightMenuTab,
  SetMainUIFile,
  SetNavigatorRenamingTarget,
  SetPanelVisibility,
  SetProjectID,
  SetProjectName,
  SetProjectDescription,
  SetProp,
  SetPropWithElementPath,
  SetStoredFontSettings,
  AddToast,
  SetZIndex,
  ShowContextMenu,
  ShowModal,
  SwitchEditorMode,
  SwitchLayoutSystem,
  ToggleCollapse,
  ToggleHidden,
  ToggleInterfaceDesignerCodeEditor,
  ToggleInterfaceDesignerAdditionalControls,
  TogglePane,
  ToggleProperty,
  ToggleCanvasIsLive,
  TransientActions,
  Undo,
  UnsetProperty,
  UnwrapGroupOrView,
  UpdateCodeResultCache,
  UpdateDuplicationState,
  UpdateEditorMode,
  UpdateFile,
  UpdateFilePath,
  UpdateFrameDimensions,
  UpdateFromWorker,
  UpdateJSXElementName,
  UpdateKeysPressed,
  UpdatePreviewConnected,
  UpdateThumbnailGenerated,
  WrapInView,
  SetSafeMode,
  SaveImageDetails,
  SetSaveError,
  RemoveToast,
  InsertDroppedImage,
  UpdateNodeModulesContents,
  UpdatePackageJson,
  StartCheckpointTimer,
  FinishCheckpointTimer,
  AddMissingDimensions,
  SetPackageStatus,
  SetShortcut,
  UpdatePropertyControlsInfo,
  AddStoryboardFile,
  SendLinterRequestMessage,
  UpdateChildText,
  UpdateFromCodeEditor,
  MarkVSCodeBridgeReady,
  SelectFromFileAndPosition,
  SendCodeEditorInitialisation,
  SetFocusedElement,
  AddImports,
  ScrollToElement,
  SetScrollAnimation,
  SetFollowSelectionEnabled,
  UpdateConfigFromVSCode,
  SetLoginState,
  ResetCanvas,
  SetFilebrowserDropTarget,
  SetForkedFromProjectID,
  SetCurrentTheme,
  FocusFormulaBar,
  UpdateFormulaBarMode,
  OpenFloatingInsertMenu,
  CloseFloatingInsertMenu,
  InsertInsertable,
  SetPropTransient,
  ClearTransientProps,
  AddTailwindConfig,
  FocusClassNameInput,
  WrapInElement,
  SetInspectorLayoutSectionHovered,
  IncrementResizeOptionsSelectedIndex,
  SetResizeOptionsTargetOptions,
  HideVSCodeLoadingScreen,
  SetIndexedDBFailed,
  ForceParseFile,
  RemoveFromNodeModulesContents,
  RunEscapeHatch,
  SetElementsToRerender,
} from '../action-types'
import { defaultTransparentViewElement, defaultSceneElement } from '../defaults'
import {
  EditorModes,
  elementInsertionSubject,
  Mode,
  SceneInsertionSubject,
  isSelectMode,
  isLiveMode,
} from '../editor-modes'
import * as History from '../history'
import { StateHistory } from '../history'
import {
  dependenciesWithEditorRequirements,
  dependenciesFromPackageJsonContents,
  updateDependenciesInEditorState,
  updateDependenciesInPackageJson,
  createLoadedPackageStatusMapFromDependencies,
  dependenciesFromPackageJson,
  findLatestVersion,
} from '../npm-dependency/npm-dependency'
import { updateRemoteThumbnail } from '../persistence/persistence-backend'
import {
  deleteAssetFile,
  saveAsset as saveAssetToServer,
  updateAssetFileName,
  saveUserConfiguration,
} from '../server'
import {
  areGeneratedElementsTargeted,
  CanvasBase64Blobs,
  DerivedState,
  DuplicationState,
  editorModelFromPersistentModel,
  EditorState,
  ErrorMessages,
  getAllBuildErrors,
  getAllLintErrors,
  getFileForName,
  getMainUIFromModel,
  getOpenFilename,
  getOpenTextFileKey,
  getOpenUIJSFileKey,
  insertElementAtPath,
  mergeStoredEditorStateIntoEditorState,
  ModalDialog,
  modifyOpenJsxElementAtPath,
  modifyOpenJSXElements,
  modifyOpenJSXElementsAndMetadata,
  modifyOpenParseSuccess,
  modifyParseSuccessWithSimple,
  OriginalFrame,
  ParseSuccessAndEditorChanges,
  PersistentModel,
  removeElementAtPath,
  SimpleParseSuccess,
  UIFileBase64Blobs,
  updateMainUIInEditorState,
  addNewScene,
  addSceneToJSXComponents,
  UserState,
  UserConfiguration,
  getElementPathsInBounds,
  StoryboardFilePath,
  modifyUnderlyingTarget,
  BaseCanvasOffsetLeftPane,
  BaseCanvasOffset,
  getJSXComponentsAndImportsForPathFromState,
  withUnderlyingTargetFromEditorState,
  modifyUnderlyingForOpenFile,
  forUnderlyingTargetFromEditorState,
  getHighlightBoundsForFile,
  modifyParseSuccessAtPath,
  withUnderlyingTarget,
  LeftPaneDefaultWidth,
  LeftPaneMinimumWidth,
  LeftMenuTab,
  RightMenuTab,
  persistentModelFromEditorModel,
  getPackageJsonFromEditorState,
  transformElementAtPath,
  getNewSceneName,
  packageJsonFileFromProjectContents,
  vsCodeBridgeIdProjectId,
} from '../store/editor-state'
import { loadStoredState } from '../stored-state'
import { applyMigrations } from './migrations/migrations'
import { fastForEach, getProjectLockedKey } from '../../../core/shared/utils'
import { PathForSceneDataLabel, getStoryboardElementPath } from '../../../core/model/scene-utils'
import { getFrameAndMultiplier } from '../../images'
import { arrayToMaybe, forceNotNull, optionalMap } from '../../../core/shared/optional-utils'

import { notice, Notice } from '../../common/notice'
import { objectFilter, objectMap } from '../../../core/shared/object-utils'
import { getDependencyTypeDefinitions } from '../../../core/es-modules/package-manager/package-manager'
import { fetchNodeModules } from '../../../core/es-modules/package-manager/fetch-packages'
import { getPropertyControlsForTargetFromEditor } from '../../../core/property-controls/property-controls-utils'
import { UiJsxCanvasContextData } from '../../canvas/ui-jsx-canvas'
import { ShortcutConfiguration } from '../shortcut-definitions'
import { objectKeyParser, parseString } from '../../../utils/value-parser-utils'
import { addStoryboardFileToProject } from '../../../core/model/storyboard-utils'
import { arrayDeepEquality } from '../../../utils/deep-equality'
import {
  ElementInstanceMetadataKeepDeepEquality,
  ElementInstanceMetadataMapKeepDeepEquality,
} from '../store/store-deep-equality-instances'
import {
  showToast,
  removeToast,
  setPropWithElementPath_UNSAFE,
  clearImageFileBlob,
  updateFile,
  enableInsertModeForJSXElement,
  insertJSXElement,
  updateThumbnailGenerated,
  openCodeEditorFile,
  setPackageStatus,
  updateNodeModulesContents,
  finishCheckpointTimer,
  selectComponents,
  markVSCodeBridgeReady,
  addImports,
  setScrollAnimation,
  updatePackageJson,
  removeFromNodeModulesContents,
} from './action-creators'
import { getAllTargetsAtPoint } from '../../canvas/dom-lookup'
import {
  initVSCodeBridge,
  sendCodeEditorDecorations,
  sendOpenFileMessage,
  sendSelectedElement,
  sendSetFollowSelectionEnabledMessage,
} from '../../../core/vscode/vscode-bridge'
import utils from '../../../utils/utils'
import { defaultConfig } from 'utopia-vscode-common'
import { getTargetParentForPaste } from '../../../utils/clipboard'
import { emptySet } from '../../../core/shared/set-utils'
import { absolutePathFromRelativePath, stripLeadingSlash } from '../../../utils/path-utils'
import { resolveModule } from '../../../core/es-modules/package-manager/module-resolution'
import { mapDropNulls, reverse, uniqBy } from '../../../core/shared/array-utils'
import { UTOPIA_UID_KEY } from '../../../core/model/utopia-constants'
import {
  DefaultPostCSSConfig,
  DefaultTailwindConfig,
  PostCSSPath,
  TailwindConfigPath,
} from '../../../core/tailwind/tailwind-config'
import { uniqToasts } from './toast-helpers'
import { NavigatorStateKeepDeepEquality } from '../../../utils/deep-equality-instances'
import type { BuiltInDependencies } from '../../../core/es-modules/package-manager/built-in-dependencies-list'
import { stylePropPathMappingFn } from '../../inspector/common/property-path-hooks'
import { getEscapeHatchCommands } from '../../../components/canvas/canvas-strategies/escape-hatch-strategy'
import { pickCanvasStateFromEditorState } from '../../canvas/canvas-strategies/canvas-strategies'
import { foldAndApplyCommandsSimple, runCanvasCommand } from '../../canvas/commands/commands'
import { setElementsToRerenderCommand } from '../../canvas/commands/set-elements-to-rerender-command'

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
      return updateSelectedLeftMenuTab(editorState, LeftMenuTab.Contents)
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

function setPropertyOnTargetAtElementPath(
  editor: EditorModel,
  target: StaticElementPathPart,
  updateFn: (props: JSXAttributes) => Either<any, JSXAttributes>,
): EditorModel {
  return modifyOpenJSXElements((components) => {
    return transformJSXComponentAtElementPath(components, target, (e: JSXElement) =>
      applyUpdateToJSXElement(e, updateFn),
    )
  }, editor)
}

function setSpecialSizeMeasurementParentLayoutSystemOnAllChildren(
  scenes: ElementInstanceMetadataMap,
  parentPath: ElementPath,
  value: DetectedLayoutSystem,
): ElementInstanceMetadataMap {
  const allChildren = MetadataUtils.getImmediateChildren(scenes, parentPath)
  return allChildren.reduce((transformedScenes, child) => {
    return switchLayoutMetadata(transformedScenes, child.elementPath, value, undefined, undefined)
  }, scenes)
}

function switchAndUpdateFrames(
  editor: EditorModel,
  target: ElementPath,
  layoutSystem: SettableLayoutSystem,
  propertyTarget: ReadonlyArray<string>,
): EditorModel {
  const targetMetadata = Utils.forceNotNull(
    `Could not find metadata for ${JSON.stringify(target)}`,
    MetadataUtils.findElementByElementPath(editor.jsxMetadata, target),
  )
  if (targetMetadata.globalFrame == null) {
    // The target is a non-layoutable
    return editor
  }

  const styleDisplayPath = stylePropPathMappingFn('display', propertyTarget)

  let withUpdatedLayoutSystem: EditorModel = editor
  switch (layoutSystem) {
    case 'flex':
      withUpdatedLayoutSystem = setPropertyOnTarget(
        withUpdatedLayoutSystem,
        target,
        (attributes) => {
          return setJSXValueAtPath(
            attributes,
            styleDisplayPath,
            jsxAttributeValue('flex', emptyComments),
          )
        },
      )
      break
    case 'flow':
    case 'grid':
      const propsToRemove = [
        stylePropPathMappingFn('left', propertyTarget),
        stylePropPathMappingFn('top', propertyTarget),
        stylePropPathMappingFn('right', propertyTarget),
        stylePropPathMappingFn('bottom', propertyTarget),
        stylePropPathMappingFn('position', propertyTarget),
      ]
      withUpdatedLayoutSystem = setPropertyOnTarget(
        withUpdatedLayoutSystem,
        target,
        (attributes) => {
          return unsetJSXValuesAtPaths(attributes, propsToRemove)
        },
      )
      break
    case LayoutSystem.PinSystem:
    case LayoutSystem.Group:
    default:
      withUpdatedLayoutSystem = setPropertyOnTarget(
        withUpdatedLayoutSystem,
        target,
        (attributes) => {
          return unsetJSXValueAtPath(attributes, styleDisplayPath)
        },
      )
      withUpdatedLayoutSystem = setPropertyOnTarget(
        withUpdatedLayoutSystem,
        target,
        (attributes) => {
          return setJSXValueAtPath(
            attributes,
            stylePropPathMappingFn('position', propertyTarget),
            jsxAttributeValue('absolute', emptyComments),
          )
        },
      )
  }

  // This "fixes" an issue where inside `setCanvasFramesInnerNew` looks at the layout type in the
  // metadata which causes a problem as it's effectively out of date after the above call.
  switch (layoutSystem) {
    case 'flex':
      withUpdatedLayoutSystem = {
        ...withUpdatedLayoutSystem,
        allElementProps: MetadataUtils.setPropertyDirectlyIntoMetadata(
          withUpdatedLayoutSystem.allElementProps,
          target,
          styleDisplayPath, // TODO LAYOUT investigate if we should use also update the DOM walker specialSizeMeasurements
          'flex',
        ),
      }
      withUpdatedLayoutSystem = {
        ...withUpdatedLayoutSystem,
        allElementProps: MetadataUtils.setPropertyDirectlyIntoMetadata(
          withUpdatedLayoutSystem.allElementProps,
          target,
          stylePropPathMappingFn('position', propertyTarget), // TODO LAYOUT investigate if we should use also update the DOM walker specialSizeMeasurements
          'relative',
        ),
      }
      break
    case LayoutSystem.PinSystem:
      withUpdatedLayoutSystem = {
        ...withUpdatedLayoutSystem,
        allElementProps: MetadataUtils.setPropertyDirectlyIntoMetadata(
          withUpdatedLayoutSystem.allElementProps,
          target,
          stylePropPathMappingFn('position', propertyTarget), // TODO LAYOUT investigate if we should use also update the DOM walker specialSizeMeasurements
          'absolute',
        ),
      }
      break
    case LayoutSystem.Group:
    default:
      withUpdatedLayoutSystem = {
        ...withUpdatedLayoutSystem,
        allElementProps: MetadataUtils.unsetPropertyDirectlyIntoMetadata(
          withUpdatedLayoutSystem.allElementProps,
          target,
          styleDisplayPath,
        ),
      }
      withUpdatedLayoutSystem = {
        ...withUpdatedLayoutSystem,
        allElementProps: MetadataUtils.setPropertyDirectlyIntoMetadata(
          withUpdatedLayoutSystem.allElementProps,
          target,
          styleDisplayPath, // TODO LAYOUT investigate if we should use also update the DOM walker specialSizeMeasurements
          layoutSystem,
        ),
      }
  }

  function layoutSystemToSet(): DetectedLayoutSystem {
    switch (layoutSystem) {
      case 'flex':
        return 'flex'
      case LayoutSystem.PinSystem:
        return 'flow'
      case LayoutSystem.Group:
      default:
        return 'flow'
    }
  }

  withUpdatedLayoutSystem = {
    ...withUpdatedLayoutSystem,
    jsxMetadata: setSpecialSizeMeasurementParentLayoutSystemOnAllChildren(
      withUpdatedLayoutSystem.jsxMetadata,
      target,
      layoutSystemToSet(),
    ),
  }
  withUpdatedLayoutSystem = {
    ...withUpdatedLayoutSystem,
    jsxMetadata: switchLayoutMetadata(
      withUpdatedLayoutSystem.jsxMetadata,
      target,
      undefined,
      layoutSystemToSet(),
      undefined,
    ),
  }

  let withChildrenUpdated = modifyOpenJSXElementsAndMetadata(
    (components, metadata) => {
      return maybeSwitchChildrenLayoutProps(
        target,
        editor.jsxMetadata,
        metadata,
        components,
        propertyTarget,
        editor.allElementProps,
      )
    },
    target,
    withUpdatedLayoutSystem,
  )

  let framesAndTargets: Array<PinOrFlexFrameChange> = []
  if (layoutSystem !== 'flow') {
    const isParentFlex = MetadataUtils.isParentYogaLayoutedContainerAndElementParticipatesInLayout(
      target,
      withChildrenUpdated.jsxMetadata,
    )
    framesAndTargets.push(getFrameChange(target, targetMetadata.globalFrame, isParentFlex))
  }

  const children = MetadataUtils.getChildrenPaths(editor.jsxMetadata, target)
  Utils.fastForEach(children, (childPath) => {
    const child = MetadataUtils.findElementByElementPath(editor.jsxMetadata, childPath)
    if (child?.globalFrame != null) {
      // if the globalFrame is null, this child is a non-layoutable so just skip it
      const isParentOfChildFlex =
        MetadataUtils.isParentYogaLayoutedContainerAndElementParticipatesInLayout(
          child.elementPath,
          withChildrenUpdated.jsxMetadata,
        )
      framesAndTargets.push(
        getFrameChange(child.elementPath, child.globalFrame, isParentOfChildFlex),
      )
    }
  })
  return setCanvasFramesInnerNew(withChildrenUpdated, framesAndTargets, null)
}

export function editorMoveMultiSelectedTemplates(
  targets: ElementPath[],
  indexPosition: IndexPosition,
  newParentPath: ElementPath | null,
  parentFrame: CanvasRectangle | null,
  editor: EditorModel,
  newParentLayoutType: SettableLayoutSystem | null,
  newParentMainAxis: 'horizontal' | 'vertical' | null,
): {
  editor: EditorModel
  newPaths: Array<ElementPath>
} {
  let updatedTargets: Array<ElementPath> = [...targets]
  let newPaths: Array<ElementPath> = []
  const updatedEditor = targets.reduce((working, target, i) => {
    const frame = MetadataUtils.getFrameInCanvasCoords(target, editor.jsxMetadata)

    let templateToMove = updatedTargets[i]
    const { editor: updatedTemplates, newPath } = editorMoveTemplate(
      templateToMove,
      target,
      frame,
      indexPosition,
      newParentPath,
      parentFrame,
      working,
      newParentLayoutType,
      newParentMainAxis,
    )
    if (newPath != null) {
      // when moving multiselected elements that are in a hierarchy the editor has the ancestor with a new path
      updatedTargets = updatedTargets.map((path) => {
        const newChildPath = EP.replaceIfAncestor(path, templateToMove, newPath)
        return Utils.defaultIfNull(path, newChildPath)
      })
      newPaths.push(newPath)
    }

    return updatedTemplates
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

function restoreEditorState(currentEditor: EditorModel, history: StateHistory): EditorModel {
  // FIXME Ask Team Components to check over these
  const poppedEditor = history.current.editor
  return {
    id: currentEditor.id,
    vscodeBridgeId: currentEditor.vscodeBridgeId,
    forkedFromProjectId: currentEditor.forkedFromProjectId,
    appID: currentEditor.appID,
    projectName: currentEditor.projectName,
    projectDescription: currentEditor.projectDescription,
    projectVersion: currentEditor.projectVersion,
    isLoaded: currentEditor.isLoaded,
    spyMetadata: poppedEditor.spyMetadata,
    domMetadata: poppedEditor.domMetadata,
    jsxMetadata: poppedEditor.jsxMetadata,
    projectContents: poppedEditor.projectContents,
    nodeModules: currentEditor.nodeModules,
    codeResultCache: currentEditor.codeResultCache,
    propertyControlsInfo: currentEditor.propertyControlsInfo,
    selectedViews: currentEditor.selectedViews,
    highlightedViews: currentEditor.highlightedViews,
    hiddenInstances: poppedEditor.hiddenInstances,
    warnedInstances: poppedEditor.warnedInstances,
    mode: EditorModes.selectMode(),
    focusedPanel: currentEditor.focusedPanel,
    keysPressed: {},
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
      dragState: null,
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
      layoutSectionHovered: currentEditor.inspector.layoutSectionHovered,
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
      dropTargetHint: {
        target: null,
        type: null,
      },
      collapsedViews: poppedEditor.navigator.collapsedViews,
      renamingTarget: null,
    },
    topmenu: {
      formulaBarMode: poppedEditor.topmenu.formulaBarMode,
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
    codeEditingEnabled: poppedEditor.codeEditingEnabled,
    thumbnailLastGenerated: currentEditor.thumbnailLastGenerated,
    pasteTargetsToIgnore: poppedEditor.pasteTargetsToIgnore,
    codeEditorErrors: currentEditor.codeEditorErrors,
    parseOrPrintInFlight: false,
    safeMode: currentEditor.safeMode,
    saveError: currentEditor.saveError,
    vscodeBridgeReady: currentEditor.vscodeBridgeReady,
    vscodeReady: currentEditor.vscodeReady,
    focusedElementPath: currentEditor.focusedElementPath,
    config: defaultConfig(),
    theme: currentEditor.theme,
    vscodeLoadingScreenVisible: currentEditor.vscodeLoadingScreenVisible,
    indexedDBFailed: currentEditor.indexedDBFailed,
    forceParseFiles: currentEditor.forceParseFiles,
    allElementProps: poppedEditor.allElementProps,
  }
}

export function restoreDerivedState(history: StateHistory): DerivedState {
  const poppedDerived = history.current.derived

  return {
    navigatorTargets: poppedDerived.navigatorTargets,
    visibleNavigatorTargets: poppedDerived.visibleNavigatorTargets,
    controls: [],
    transientState: produceCanvasTransientState(
      poppedDerived.transientState.selectedViews,
      history.current.editor,
      true,
    ),
    elementWarnings: poppedDerived.elementWarnings,
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
        const element = findElementAtPath(targetPath, utopiaComponents)
        if (element == null) {
          return parseSuccess
        } else {
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
      }
      return modifyParseSuccessAtPath(
        targetSuccess.filePath,
        working,
        deleteElementFromParseSuccess,
      )
    }, editor)
    return {
      ...updatedEditor,
      selectedViews: EP.filterPaths(updatedEditor.selectedViews, targets),
    }
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
            return getZIndexOfElement(success.topLevelElements, EP.asStatic(target))
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
  return selectedViews.reduce(
    (working, selectedView) => {
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
    },
    {
      ...editor,
      selectedViews: [],
    } as EditorModel,
  )
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

function getZIndexOrderedViewsWithoutDirectChildren(
  targets: Array<ElementPath>,
  derived: DerivedState,
): Array<ElementPath> {
  let targetsAndZIndex: Array<{ target: ElementPath; index: number }> = []
  Utils.fastForEach(targets, (target) => {
    const index = derived.navigatorTargets.findIndex((tp) => EP.pathsEqual(tp, target))
    targetsAndZIndex.push({ target: target, index: index })
  })
  targetsAndZIndex.sort((a, b) => b.index - a.index)
  const orderedTargets = Utils.pluck(targetsAndZIndex, 'target')

  // keep direct children from reparenting
  let filteredTargets: Array<ElementPath> = []
  Utils.fastForEach(orderedTargets, (target) => {
    if (!orderedTargets.some((tp) => EP.pathsEqual(EP.parentPath(target), tp))) {
      filteredTargets.push(target)
    }
  })
  return filteredTargets
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
  const generatedElementsTargeted = areGeneratedElementsTargeted(targets, editor)
  let result: EditorState = editor
  if (generatedElementsTargeted) {
    const showToastAction = showToast(notice(message))
    result = UPDATE_FNS.ADD_TOAST(showToastAction, result, dispatch)
  }

  if (!generatedElementsTargeted || allowActionRegardless) {
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
    const allElementPaths = derived.navigatorTargets
    const highlightBoundsForUids = getHighlightBoundsForFile(editor, filePath)
    const newlySelectedElements = getElementPathsInBounds(
      line,
      highlightBoundsForUids,
      allElementPaths,
    )
    return UPDATE_FNS.SELECT_COMPONENTS(
      selectComponents(newlySelectedElements, false),
      editor,
      dispatch,
    )
  }
}

function removeModulesFromNodeModules(
  modulesToRemove: Array<string>,
  nodeModules: NodeModules,
): NodeModules {
  const filePathsToRemove = modulesToRemove.map((m) => `/node_modules/${m}/`)

  return objectFilter(
    (_module, modulePath) =>
      !filePathsToRemove.some((pathToRemove) => (modulePath as string).startsWith(pathToRemove)),
    nodeModules,
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
          Date.now(),
        )
      },
    )
    const storyboardFile = getContentsTreeFileFromString(parsedProjectFiles, StoryboardFilePath)
    const openFilePath = storyboardFile != null ? StoryboardFilePath : null
    initVSCodeBridge(action.projectId, parsedProjectFiles, dispatch, openFilePath)

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
  SET_HIGHLIGHTED_VIEW: (action: SetHighlightedView, editor: EditorModel): EditorModel => {
    if (
      editor.highlightedViews.length > 0 &&
      EP.containsPath(action.target, editor.highlightedViews)
    ) {
      return editor
    } else {
      return {
        ...editor,
        highlightedViews: [action.target],
      }
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
  UNDO: (editor: EditorModel, stateHistory: StateHistory): EditorModel => {
    if (History.canUndo(stateHistory)) {
      const history = History.undo(stateHistory)
      return restoreEditorState(editor, history)
    } else {
      return editor
    }
  },
  REDO: (editor: EditorModel, stateHistory: StateHistory): EditorModel => {
    if (History.canRedo(stateHistory)) {
      const history = History.redo(stateHistory)
      return restoreEditorState(editor, history)
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
    const updatedEditor = modifyUnderlyingForOpenFile(
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
      return UPDATE_FNS.ADD_TOAST(toastAction, editor, dispatch)
    } else {
      return updatedEditor
    }
  },
  SET_CANVAS_FRAMES: (
    action: SetCanvasFrames,
    editor: EditorModel,
    derived: DerivedState,
  ): EditorModel => {
    return setCanvasFramesInnerNew(editor, action.framesAndTargets, null)
  },
  NAVIGATOR_REORDER: (
    action: NavigatorReorder,
    editor: EditorModel,
    derived: DerivedState,
  ): EditorModel => {
    const dragSources = action.dragSources
    const dropTarget = action.dropTarget
    const targetPath = dropTarget.target
    const index = MetadataUtils.getViewZIndexFromMetadata(editor.jsxMetadata, targetPath)
    let indexPosition: IndexPosition
    let newParentPath: ElementPath | null
    switch (dropTarget.type) {
      case 'MOVE_ROW_BEFORE':
        indexPosition = {
          type: 'before',
          index: index,
        }
        newParentPath = EP.parentPath(targetPath)
        break
      case 'MOVE_ROW_AFTER':
        indexPosition = {
          type: 'after',
          index: index,
        }
        newParentPath = EP.parentPath(targetPath)
        break
      case 'REPARENT_ROW':
        indexPosition = {
          type: 'front',
        }
        newParentPath = targetPath
        break
      case 'REPARENT_TO_INDEX':
        indexPosition = {
          type: 'before',
          index: dropTarget.index,
        }
        newParentPath = targetPath
        break
      default:
        const _exhaustiveCheck: never = dropTarget
        throw new Error('Something went really wrong.')
    }

    const newParentSize =
      newParentPath == null
        ? null
        : MetadataUtils.getFrameInCanvasCoords(newParentPath, editor.jsxMetadata)
    const { editor: withMovedTemplate, newPaths } = editorMoveMultiSelectedTemplates(
      reverse(getZIndexOrderedViewsWithoutDirectChildren(dragSources, derived)),
      indexPosition,
      newParentPath,
      newParentSize,
      editor,
      null,
      null,
    )

    return {
      ...withMovedTemplate,
      selectedViews: newPaths,
      highlightedViews: [],
      canvas: {
        ...withMovedTemplate.canvas,
        domWalkerInvalidateCount: withMovedTemplate.canvas.domWalkerInvalidateCount + 1,
      },
    }
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
  DELETE_SELECTED: (
    action: DeleteSelected,
    editorForAction: EditorModel,
    derived: DerivedState,
    dispatch: EditorDispatch,
  ): EditorModel => {
    return toastOnGeneratedElementsSelected(
      'Generated elements can only be deleted in code.',
      editorForAction,
      true,
      (editor) => {
        const staticSelectedElements = editor.selectedViews.filter((selectedView) => {
          const { components } = getJSXComponentsAndImportsForPathFromState(
            selectedView,
            editorForAction,
            derived,
          )
          return MetadataUtils.isStaticElement(components, selectedView)
        })
        const withElementDeleted = deleteElements(staticSelectedElements, editor)
        const parentsToSelect = uniqBy(
          mapDropNulls((view) => {
            return EP.parentPath(view)
          }, editor.selectedViews),
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
        const newSelection = EP.parentPath(action.target)
        return {
          ...updatedEditor,
          selectedViews: [newSelection],
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
      newlySelectedPaths = action.target
    }
    const newHighlightedViews = editor.highlightedViews.filter(
      (path) => !EP.containsPath(path, newlySelectedPaths),
    )

    const filteredNewlySelectedPaths = newlySelectedPaths
    const updatedEditor: EditorModel = {
      ...editor,
      highlightedViews: newHighlightedViews,
      selectedViews: filteredNewlySelectedPaths,
      navigator:
        filteredNewlySelectedPaths === editor.selectedViews
          ? editor.navigator
          : updateNavigatorCollapsedState(filteredNewlySelectedPaths, editor.navigator),
      pasteTargetsToIgnore: [],
    }
    if (filteredNewlySelectedPaths === newlySelectedPaths) {
      return updatedEditor
    } else {
      const showToastAction = showToast(notice(`Only one scene can be selected`, 'WARNING'))
      return UPDATE_FNS.ADD_TOAST(showToastAction, updatedEditor, dispatch)
    }
  },
  CLEAR_SELECTION: (editor: EditorModel): EditorModel => {
    if (editor.selectedViews.length === 0) {
      return editor
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
      const children = MetadataUtils.getImmediateChildren(editor.jsxMetadata, uniqueParent)
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
    // same as UPDATE_EDITOR_MODE, but clears the drag state
    return clearDragState(setModeState(action.mode, editor), derived, false)
  },
  TOGGLE_CANVAS_IS_LIVE: (editor: EditorModel, derived: DerivedState): EditorModel => {
    // same as UPDATE_EDITOR_MODE, but clears the drag state
    if (isLiveMode(editor.mode)) {
      return clearDragState(
        setModeState(EditorModes.selectMode(editor.mode.controlId), editor),
        derived,
        false,
      )
    } else {
      return clearDragState(
        setModeState(
          EditorModes.liveMode(isSelectMode(editor.mode) ? editor.mode.controlId : null),
          editor,
        ),
        derived,
        false,
      )
    }
  },
  ADD_TOAST: (action: AddToast, editor: EditorModel, dispatch: EditorDispatch): EditorModel => {
    const withOldToastRemoved = UPDATE_FNS.REMOVE_TOAST(removeToast(action.toast.id), editor)
    return {
      ...withOldToastRemoved,
      toasts: uniqToasts([...withOldToastRemoved.toasts, action.toast]),
    }
  },
  REMOVE_TOAST: (action: RemoveToast, editor: EditorModel): EditorModel => {
    return {
      ...editor,
      toasts: editor.toasts.filter((toast) => toast.id !== action.id),
    }
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
        setJSXValueAtPath(props, PathForSceneDataLabel, jsxAttributeValue(name, emptyComments))
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
  INSERT_SCENE: (action: InsertScene, editor: EditorModel): EditorModel => {
    const sceneUID = generateUidWithExistingComponents(editor.projectContents)
    const newSceneLabel = getNewSceneName(editor)
    const newScene: JSXElement = defaultSceneElement(
      sceneUID,
      canvasFrameToNormalisedFrame(action.frame),
      newSceneLabel,
      [],
    )
    const storyBoardPath = getStoryboardElementPath(
      editor.projectContents,
      editor.canvas.openFile?.filename ?? null,
    )
    const newSelection =
      storyBoardPath != null ? [EP.elementPath([[EP.toUid(storyBoardPath), sceneUID]])] : []
    return {
      ...addNewScene(editor, newScene),
      selectedViews: newSelection,
    }
  },
  INSERT_JSX_ELEMENT: (action: InsertJSXElement, editor: EditorModel): EditorModel => {
    let newSelectedViews: ElementPath[] = []
    const withNewElement = modifyUnderlyingTarget(
      action.parent,
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
          editor.canvas.openFile?.filename ?? null,
          targetParent,
          action.jsxElement,
          utopiaComponents,
          null,
        )

        const uid = getUtopiaID(action.jsxElement)
        const newPath = EP.appendToPath(targetParent, uid)
        newSelectedViews.push(newPath)

        const updatedTopLevelElements = applyUtopiaJSXComponentsChanges(
          success.topLevelElements,
          withInsertedElement,
        )

        const updatedImports = mergeImports(
          underlyingFilePath,
          success.imports,
          action.importsToAdd,
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
  WRAP_IN_VIEW: (
    action: WrapInView,
    editorForAction: EditorModel,
    derived: DerivedState,
    dispatch: EditorDispatch,
  ): EditorModel => {
    // FIXME This and WRAP_IN_ELEMENT are very similar, the only difference being that this attempts to maintain
    // the positioning. The two handlers should probably be combined, or perhaps have core shared logic extracted
    return toastOnGeneratedElementsSelected(
      `Generated elements can't be wrapped into other elements.`,
      editorForAction,
      false,
      (editor) => {
        const uiFileKey = getOpenUIJSFileKey(editor)
        if (uiFileKey == null) {
          return editor
        }

        const newUID =
          action.whatToWrapWith === 'default-empty-div'
            ? generateUidWithExistingComponents(editor.projectContents)
            : action.whatToWrapWith.element.uid

        const orderedActionTargets = getZIndexOrderedViewsWithoutDirectChildren(
          action.targets,
          derived,
        )
        const parentPath = EP.getCommonParent(orderedActionTargets)
        const indexInParent = optionalMap(
          (firstPathMatchingCommonParent) =>
            MetadataUtils.getViewZIndexFromMetadata(
              editor.jsxMetadata,
              firstPathMatchingCommonParent,
            ),
          orderedActionTargets.find((target) => EP.pathsEqual(EP.parentPath(target), parentPath)),
        )

        if (parentPath === null) {
          return editor
        } else {
          const anyTargetIsARootElement = orderedActionTargets.some(EP.isRootElementOfInstance)
          const targetThatIsRootElementOfCommonParent = orderedActionTargets.find(
            (elementPath) =>
              EP.isRootElementOfInstance(elementPath) && EP.isParentOf(parentPath, elementPath),
          )

          if (anyTargetIsARootElement && targetThatIsRootElementOfCommonParent == null) {
            return editor
          }

          const canvasFrames = action.targets.map((target) => {
            return MetadataUtils.getFrameInCanvasCoords(target, editor.jsxMetadata)
          })

          const boundingBox = Utils.boundingRectangleArray(canvasFrames)

          if (boundingBox == null) {
            // TODO Should this wrap in a zero sized rectangle so the user can then manually resize that?
            // we are trying to wrap something that is non-layoutable, just give up early
            return editor
          }

          let viewPath: ElementPath | null = null

          const underlyingTarget = normalisePathToUnderlyingTarget(
            editor.projectContents,
            editor.nodeModules.files,
            uiFileKey,
            targetThatIsRootElementOfCommonParent ?? parentPath,
          )

          const parent = MetadataUtils.findElementByElementPath(editor.jsxMetadata, parentPath)
          const isParentFlex =
            parent != null ? MetadataUtils.isFlexLayoutedContainer(parent) : false

          function setPositionAttribute(
            elementToWrapWith: JSXElement,
            position: 'absolute' | 'relative',
          ): JSXElement {
            return {
              ...elementToWrapWith,
              props: forceRight(
                setJSXValueAtPath(
                  elementToWrapWith.props,
                  PP.create(['style', 'position']), // todo make it optional
                  jsxAttributeValue(position, emptyComments),
                ),
              ),
            }
          }

          const targetSuccess = normalisePathSuccessOrThrowError(underlyingTarget)

          const withWrapperViewAddedNoFrame = modifyParseSuccessAtPath(
            targetSuccess.filePath,
            editor,
            (parseSuccess) => {
              const elementToInsert: JSXElement =
                action.whatToWrapWith === 'default-empty-div'
                  ? defaultTransparentViewElement(newUID)
                  : action.whatToWrapWith.element

              const elementToInsertWithPositionAttribute = isParentFlex
                ? setPositionAttribute(elementToInsert, 'relative')
                : setPositionAttribute(elementToInsert, 'absolute')

              const utopiaJSXComponents = getUtopiaJSXComponentsFromSuccess(parseSuccess)
              let withTargetAdded: Array<UtopiaJSXComponent>

              if (targetThatIsRootElementOfCommonParent == null) {
                withTargetAdded = insertElementAtPath(
                  editor.projectContents,
                  editor.canvas.openFile?.filename ?? null,
                  parentPath,
                  elementToInsertWithPositionAttribute,
                  utopiaJSXComponents,
                  optionalMap(
                    (index) => ({
                      type: 'before',
                      index: index,
                    }),
                    indexInParent,
                  ),
                )
              } else {
                const staticTarget = EP.dynamicPathToStaticPath(
                  targetThatIsRootElementOfCommonParent,
                )
                withTargetAdded = transformJSXComponentAtPath(
                  utopiaJSXComponents,
                  staticTarget,
                  (oldRoot) =>
                    jsxElement(elementToInsert.name, elementToInsert.uid, elementToInsert.props, [
                      ...elementToInsert.children,
                      oldRoot,
                    ]),
                )
              }

              viewPath = anyTargetIsARootElement
                ? EP.appendNewElementPath(parentPath, newUID)
                : EP.appendToPath(parentPath, newUID)

              const importsToAdd: Imports =
                action.whatToWrapWith === 'default-empty-div'
                  ? emptyImports()
                  : action.whatToWrapWith.importsToAdd

              return modifyParseSuccessWithSimple((success: SimpleParseSuccess) => {
                return {
                  ...success,
                  utopiaComponents: withTargetAdded,
                  imports: mergeImports(targetSuccess.filePath, success.imports, importsToAdd),
                }
              }, parseSuccess)
            },
          )

          if (viewPath == null) {
            return editor
          }

          const frameChanges: Array<PinOrFlexFrameChange> = isParentFlex
            ? [] // if we are wrapping something in a Flex parent, try not adding frames here
            : [getFrameChange(viewPath, boundingBox, isParentFlex)]
          const withWrapperViewAdded = {
            ...setCanvasFramesInnerNew(withWrapperViewAddedNoFrame, frameChanges, null),
          }

          // If this is a group parent, realign to the origin
          // to prevent shifting the child frames back towards the origin.
          const parentBounds =
            action.layoutSystem === LayoutSystem.Group && boundingBox != null
              ? Utils.shiftToOrigin(boundingBox)
              : boundingBox
          // reparent targets to the view
          const indexPosition: IndexPosition = {
            type: 'back',
          }

          const withElementsAdded = editorMoveMultiSelectedTemplates(
            orderedActionTargets,
            indexPosition,
            viewPath,
            parentBounds,
            withWrapperViewAdded,
            action.layoutSystem,
            action.newParentMainAxis,
          ).editor

          return {
            ...withElementsAdded,
            selectedViews: Utils.maybeToArray(viewPath),
            highlightedViews: [],
          }
        }
      },
      dispatch,
    )
  },
  WRAP_IN_ELEMENT: (
    action: WrapInElement,
    editorForAction: EditorModel,
    derived: DerivedState,
    dispatch: EditorDispatch,
  ): EditorModel => {
    return toastOnGeneratedElementsSelected(
      `Generated elements can't be wrapped into other elements.`,
      editorForAction,
      false,
      (editor) => {
        const uiFileKey = getOpenUIJSFileKey(editor)
        if (uiFileKey == null) {
          return editor
        }

        const newUID = action.whatToWrapWith.element.uid

        const orderedActionTargets = getZIndexOrderedViewsWithoutDirectChildren(
          action.targets,
          derived,
        )
        const parentPath = EP.getCommonParent(orderedActionTargets)
        const indexInParent = optionalMap(
          (firstPathMatchingCommonParent) =>
            MetadataUtils.getViewZIndexFromMetadata(
              editor.jsxMetadata,
              firstPathMatchingCommonParent,
            ),
          orderedActionTargets.find((target) => EP.pathsEqual(EP.parentPath(target), parentPath)),
        )

        if (parentPath === null) {
          return editor
        } else {
          // If any of the targets are a root element, we check that the parentPath is its parent
          // If not, we bail and do nothing
          // If it is, we add the new element as the root element of the parent instance
          const anyTargetIsARootElement = orderedActionTargets.some(EP.isRootElementOfInstance)
          const targetThatIsRootElementOfCommonParent = orderedActionTargets.find(
            (elementPath) =>
              EP.isRootElementOfInstance(elementPath) && EP.isParentOf(parentPath, elementPath),
          )

          if (anyTargetIsARootElement && targetThatIsRootElementOfCommonParent == null) {
            return editor
          }

          let viewPath: ElementPath | null = null

          const underlyingTarget = normalisePathToUnderlyingTarget(
            editor.projectContents,
            editor.nodeModules.files,
            uiFileKey,
            targetThatIsRootElementOfCommonParent != null
              ? targetThatIsRootElementOfCommonParent
              : parentPath,
          )

          const targetSuccess = normalisePathSuccessOrThrowError(underlyingTarget)

          const withWrapperViewAddedNoFrame = modifyParseSuccessAtPath(
            targetSuccess.filePath,
            editor,
            (parseSuccess) => {
              const elementToInsert: JSXElement = action.whatToWrapWith.element

              const utopiaJSXComponents = getUtopiaJSXComponentsFromSuccess(parseSuccess)
              let withTargetAdded: Array<UtopiaJSXComponent>

              if (targetThatIsRootElementOfCommonParent == null) {
                withTargetAdded = insertElementAtPath(
                  editor.projectContents,
                  editor.canvas.openFile?.filename ?? null,
                  parentPath,
                  elementToInsert,
                  utopiaJSXComponents,
                  optionalMap(
                    (index) => ({
                      type: 'before',
                      index: index,
                    }),
                    indexInParent,
                  ),
                )
              } else {
                const staticTarget = EP.dynamicPathToStaticPath(
                  targetThatIsRootElementOfCommonParent,
                )
                withTargetAdded = transformJSXComponentAtPath(
                  utopiaJSXComponents,
                  staticTarget,
                  (oldRoot) =>
                    jsxElement(elementToInsert.name, elementToInsert.uid, elementToInsert.props, [
                      ...elementToInsert.children,
                      oldRoot,
                    ]),
                )
              }

              viewPath = anyTargetIsARootElement
                ? EP.appendNewElementPath(parentPath, newUID)
                : EP.appendToPath(parentPath, newUID)

              const importsToAdd: Imports = action.whatToWrapWith.importsToAdd

              return modifyParseSuccessWithSimple((success: SimpleParseSuccess) => {
                return {
                  ...success,
                  utopiaComponents: withTargetAdded,
                  imports: mergeImports(targetSuccess.filePath, success.imports, importsToAdd),
                }
              }, parseSuccess)
            },
          )

          if (viewPath == null) {
            return editor
          }

          // reparent targets to the view
          const indexPosition: IndexPosition = {
            type: 'back',
          }

          const withElementsAdded = editorMultiselectReparentNoStyleChange(
            orderedActionTargets,
            indexPosition,
            viewPath,
            withWrapperViewAddedNoFrame,
          )

          return {
            ...withElementsAdded,
            selectedViews: Utils.maybeToArray(viewPath),
            highlightedViews: [],
          }
        }
      },
      dispatch,
    )
  },
  OPEN_FLOATING_INSERT_MENU: (action: OpenFloatingInsertMenu, editor: EditorModel): EditorModel => {
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
  UNWRAP_GROUP_OR_VIEW: (
    action: UnwrapGroupOrView,
    editorForAction: EditorModel,
    derived: DerivedState,
    dispatch: EditorDispatch,
  ): EditorModel => {
    return toastOnGeneratedElementsSelected(
      `Cannot unwrap a generated element.`,
      editorForAction,
      false,
      (editor) => {
        if (action.onlyForGroups) {
          // TOOD groups
          // bail early, we shouldn't delete a non-group view
          return editor
        }

        const element = MetadataUtils.findElementByElementPath(editor.jsxMetadata, action.target)
        const children = MetadataUtils.getChildren(editor.jsxMetadata, action.target)
        if (children.length === 0 || !MetadataUtils.isViewAgainstImports(element)) {
          return editor
        }

        const parentPath = EP.parentPath(action.target)
        const parentFrame =
          parentPath == null
            ? (Utils.zeroRectangle as CanvasRectangle)
            : MetadataUtils.getFrameInCanvasCoords(parentPath, editor.jsxMetadata)
        const indexPosition: IndexPosition = indexPositionForAdjustment(
          action.target,
          editor,
          'forward',
        )
        let newSelection: ElementPath[] = []
        const withChildrenMoved = children.reduce((working, child) => {
          const childFrame = MetadataUtils.getFrameInCanvasCoords(
            child.elementPath,
            editor.jsxMetadata,
          )
          const result = editorMoveTemplate(
            child.elementPath,
            child.elementPath,
            childFrame,
            indexPosition,
            parentPath,
            parentFrame,
            working,
            null,
            null,
          )
          if (result.newPath != null) {
            newSelection.push(result.newPath)
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
          },
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
          },
        }
      case 'misccodeeditor':
      case 'center':
      case 'insertmenu':
      case 'projectsettings':
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
        return {
          ...editor,
          interfaceDesigner: {
            ...editor.interfaceDesigner,
            codePaneVisible: !editor.interfaceDesigner.codePaneVisible,
          },
        }
      case 'misccodeeditor':
      case 'center':
      case 'insertmenu':
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
  PASTE_JSX_ELEMENTS: (
    action: PasteJSXElements,
    editor: EditorModel,
    dispatch: EditorDispatch,
  ): EditorModel => {
    const targetParent = getTargetParentForPaste(
      editor.selectedViews,
      editor.jsxMetadata,
      editor.pasteTargetsToIgnore,
    )

    let insertionAllowed: boolean = true
    if (targetParent != null) {
      const parentOriginType = withUnderlyingTargetFromEditorState(
        targetParent,
        editor,
        'unknown-element',
        (targetParentSuccess) => {
          return MetadataUtils.getElementOriginType(
            getUtopiaJSXComponentsFromSuccess(targetParentSuccess),
            targetParent,
          )
        },
      )
      switch (parentOriginType) {
        case 'unknown-element':
          insertionAllowed = false
          break
        default:
          insertionAllowed = true
      }
    }
    if (insertionAllowed) {
      const elements = guaranteeUniqueUids(
        action.elements,
        getAllUniqueUids(editor.projectContents),
      )
      return elements.reduce((workingEditorState, currentValue, index) => {
        let toastsAdded: Array<Notice> = []
        const modifyResult = modifyUnderlyingForOpenFile(
          targetParent,
          workingEditorState,
          (elem) => elem,
          (underlyingSuccess) => {
            const originalComponents = getUtopiaJSXComponentsFromSuccess(underlyingSuccess)
            const newUID = generateUidWithExistingComponents(workingEditorState.projectContents)
            const elementToAdd = setUtopiaID(currentValue, newUID)
            const originalPath = action.originalElementPaths[index]
            let updatedComponents: Array<UtopiaJSXComponent>
            const components = insertElementAtPath(
              workingEditorState.projectContents,
              workingEditorState.canvas.openFile?.filename ?? null,
              targetParent,
              elementToAdd,
              originalComponents,
              null,
            )
            if (targetParent == null) {
              updatedComponents = components
            } else {
              const newPath = EP.appendToPath(targetParent, newUID)
              const maybeSwitchResult = maybeSwitchLayoutProps(
                newPath,
                originalPath,
                targetParent,
                action.targetOriginalContextMetadata,
                workingEditorState.jsxMetadata,
                components,
                null,
                null,
                null,
                ['style'],
                workingEditorState.allElementProps,
              )
              updatedComponents = maybeSwitchResult.components
              toastsAdded.push(...maybeSwitchResult.toast)
            }

            return {
              ...underlyingSuccess,
              topLevelElements: applyUtopiaJSXComponentsChanges(
                underlyingSuccess.topLevelElements,
                updatedComponents,
              ),
            }
          },
        )
        return {
          ...modifyResult,
          toasts: uniqToasts([...modifyResult.toasts, ...toastsAdded]),
        }
      }, editor)
    } else {
      const showToastAction = showToast(
        notice(`Unable to paste into a generated element.`, 'WARNING'),
      )
      return UPDATE_FNS.ADD_TOAST(showToastAction, editor, dispatch)
    }
  },
  COPY_SELECTION_TO_CLIPBOARD: (
    action: CopySelectionToClipboard,
    editorForAction: EditorModel,
    dispatch: EditorDispatch,
  ): EditorModel => {
    return toastOnGeneratedElementsSelected(
      'Cannot copy generated elements.',
      editorForAction,
      false,
      (editor) => {
        return {
          ...editor,
          pasteTargetsToIgnore: editor.selectedViews,
        }
      },
      dispatch,
    )
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
      ? UPDATE_FNS.ADD_TOAST(
          showToast(notice('Code editor hidden. Use the menu or resize to get it back.')),
          editor,
          dispatch,
        )
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
      : UPDATE_FNS.ADD_TOAST(
          showToast(notice('Code editor hidden. Use the menu or resize to get it back.')),
          editor,
          dispatch,
        )

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

    if (frame == null) {
      return editor
    }

    const newLayout = {
      left: frame.x,
      top: frame.y,
      width: frame.width,
      height: frame.height,
    }

    let errorMessage: string | null = null

    const updatedEditor = modifyOpenJsxElementAtPath(
      target,
      (element: JSXElement) => {
        const updatedAttributes = PinLayoutHelpers.setLayoutPropsToPinsWithFrame(
          element.props,
          newLayout,
          ['style'],
        )

        if (isLeft(updatedAttributes)) {
          errorMessage = `Failed to reset pins: ${updatedAttributes.value}`
          return element
        } else {
          return {
            ...element,
            props: updatedAttributes.value,
          }
        }
      },
      editor,
    )

    /* eslint-disable no-unreachable */
    // this is faulty, as PinLayoutHelpers.setLayoutPropsToPinsWithFrame also erases all of props.layout, including properties that are not connected to the pins
    throw new Error('WARNING RESET_PINS is not correctly implemented, please contact Balazs')

    if (errorMessage != null) {
      console.error(errorMessage)
      const toastAction = showToast(notice(errorMessage!, 'WARNING'))
      return UPDATE_FNS.ADD_TOAST(toastAction, updatedEditor, dispatch)
    } else {
      return updatedEditor
    }
    /* eslint-enable no-unreachable */
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

    if (initialFrame == null) {
      return editor
    }

    let frame = {
      x: initialFrame.x,
      y: initialFrame.y,
      width: action.width,
      height: action.height,
    } as LocalRectangle

    const element = MetadataUtils.findElementByElementPath(editor.jsxMetadata, action.element)
    const elementProps = editor.allElementProps[EP.toString(action.element)] ?? {}
    if (
      element != null &&
      MetadataUtils.isTextAgainstImports(element) &&
      elementProps.textSizing == 'auto'
    ) {
      const alignment = elementProps.style.textAlign
      if (alignment === 'center') {
        frame = Utils.setRectCenterX(frame, initialFrame.x + initialFrame.width / 2)
      } else if (alignment === 'right') {
        frame = Utils.setRectRightX(frame, initialFrame.x + initialFrame.width)
      }
    }

    const parentPath = EP.parentPath(action.element)
    let offset = { x: 0, y: 0 } as CanvasPoint
    if (parentPath != null) {
      const parentFrame = MetadataUtils.getFrameInCanvasCoords(parentPath, editor.jsxMetadata)
      if (parentFrame != null) {
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
    const imageAttribute = jsxAttributeValue(imageURL, emptyComments)

    const newUID = generateUidWithExistingComponents(editor.projectContents)
    const openUIJSFile = getOpenUIJSFileKey(editor)

    let actionsToRunAfterSave: Array<EditorAction> = []
    // Bit weird, but when replacing an image, we need to change the URLs only once the image has been saved.
    if (action.imageDetails != null) {
      if (replaceImage) {
        const imageWithoutHashURL = imagePathURL(assetFilename)
        const propertyPath = PP.create(['src'])
        walkContentsTreeForParseSuccess(editor.projectContents, (filePath, success) => {
          walkElements(getUtopiaJSXComponentsFromSuccess(success), (element, elementPath) => {
            if (isJSXElement(element)) {
              const srcAttribute = getJSXAttribute(element.props, 'src')
              if (srcAttribute != null && isJSXAttributeValue(srcAttribute)) {
                const srcValue: JSXAttributeValue<any> = srcAttribute
                if (
                  typeof srcValue.value === 'string' &&
                  srcValue.value.startsWith(imageWithoutHashURL)
                ) {
                  actionsToRunAfterSave.push(
                    setPropWithElementPath_UNSAFE(elementPath, propertyPath, imageAttribute),
                  )
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
        .then(() => {
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
        dispatch,
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
              alt: jsxAttributeValue('', emptyComments),
              src: imageAttribute,
              style: jsxAttributeValue({ width: width, height: height }, emptyComments),
              'data-uid': jsxAttributeValue(newUID, emptyComments),
              'data-aspect-ratio-locked': jsxAttributeValue(true, emptyComments),
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
          const parent = action.imageDetails.afterSave.parentPath
          const relativeFrame = MetadataUtils.getFrameRelativeTo(
            parent,
            editor.jsxMetadata,
            action.imageDetails.afterSave.frame,
          )

          const imageElement = jsxElement(
            jsxElementName('img', []),
            newUID,
            jsxAttributesFromMap({
              alt: jsxAttributeValue('', emptyComments),
              src: imageAttribute,
              style: jsxAttributeValue(
                {
                  left: relativeFrame.x,
                  top: relativeFrame.y,
                  width: relativeFrame.width,
                  height: relativeFrame.height,
                },
                emptyComments,
              ),
              'data-uid': jsxAttributeValue(newUID, emptyComments),
              'data-aspect-ratio-locked': jsxAttributeValue(true, emptyComments),
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
        case 'SAVE_IMAGE_REPLACE': {
          const toastAction = showToast(
            notice(
              'Assets replaced. You may need to reload the editor to see changes.',
              'WARNING',
              true,
            ),
          )
          return UPDATE_FNS.ADD_TOAST(toastAction, editorWithToast, dispatch)
        }
        case 'SAVE_IMAGE_DO_NOTHING':
          return editor
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
      const imageSrcAttribute = jsxAttributeValue(imageURL, emptyComments)
      const width = Utils.optionalMap((w) => w / 2, possiblyAnImage.width)
      const height = Utils.optionalMap((h) => h / 2, possiblyAnImage.height)
      const imageElement = jsxElement(
        jsxElementName('img', []),
        newUID,
        jsxAttributesFromMap({
          alt: jsxAttributeValue('', emptyComments),
          src: imageSrcAttribute,
          style: jsxAttributeValue(
            {
              width: width,
              height: height,
            },
            emptyComments,
          ),
          'data-uid': jsxAttributeValue(newUID, emptyComments),
          'data-label': jsxAttributeValue('Image', emptyComments),
          'data-aspect-ratio-locked': jsxAttributeValue(true, emptyComments),
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
      initVSCodeBridge(
        action.id,
        editor.projectContents,
        dispatch,
        editor.canvas.openFile?.filename ?? null,
      )
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
      updateRemoteThumbnail(editor.id, true).then(() => {
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
    dispatch: EditorDispatch,
  ): EditorModel => {
    const replaceFilePathResults = replaceFilePath(
      action.oldPath,
      action.newPath,
      editor.projectContents,
    )
    if (replaceFilePathResults.type === 'FAILURE') {
      const toastAction = showToast(notice(replaceFilePathResults.errorMessage, 'ERROR', true))
      return UPDATE_FNS.ADD_TOAST(toastAction, editor, dispatch)
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
            updateAssetFileName(editor.id, stripLeadingSlash(oldPath), newPath)
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

    if (isTextFile(file)) {
      const existing = getContentsTreeFileFromString(editor.projectContents, action.filePath)
      const canUpdate = canUpdateFile(file, existing)
      if (!canUpdate) {
        return editor
      }
    }

    const updatedProjectContents = addFileToProjectContents(
      editor.projectContents,
      action.filePath,
      file,
    )

    let updatedNodeModulesFiles = editor.nodeModules.files
    let packageLoadingStatus: PackageStatusMap = {}

    // Ensure dependencies are updated if the `package.json` file has been changed.
    if (action.filePath === '/package.json' && isTextFile(file)) {
      const deps = dependenciesFromPackageJsonContents(file.fileContents.code)
      if (deps != null) {
        packageLoadingStatus = deps.reduce((packageStatus: PackageStatusMap, dep) => {
          packageStatus[dep.name] = { status: 'loading' }
          return packageStatus
        }, {})
        let newDeps: RequestedNpmDependency[] = []
        let updatedDeps: RequestedNpmDependency[] = []
        let removedDeps: RequestedNpmDependency[] = []
        const currentDepsFile = packageJsonFileFromProjectContents(editor.projectContents)
        if (isTextFile(currentDepsFile)) {
          const currentDeps = dependenciesFromPackageJsonContents(currentDepsFile.fileContents.code)
          let foundMatchingDeps: RequestedNpmDependency[] = []

          fastForEach(deps, (dep) => {
            const matchingCurrentDep = currentDeps.find(
              (currentDep) => dep.name === currentDep.name,
            )

            // Find the new or updated dependencies
            if (matchingCurrentDep == null) {
              // A new dependency has been added
              newDeps.push(dep)
            } else {
              foundMatchingDeps.push(matchingCurrentDep)

              if (matchingCurrentDep.version !== dep.version) {
                // An updated dependency
                updatedDeps.push(dep)
              }
            }

            // Find the deleted dependencies
            removedDeps = currentDeps.filter(
              (currentDep) => !foundMatchingDeps.includes(currentDep),
            )
          })
        } else {
          newDeps = deps
        }

        const modulesToRemove = updatedDeps.concat(removedDeps).map((d) => d.name)

        updatedNodeModulesFiles = removeModulesFromNodeModules(
          modulesToRemove,
          editor.nodeModules.files,
        )

        const depsToFetch = newDeps.concat(updatedDeps)

        fetchNodeModules(depsToFetch, builtInDependencies).then((fetchNodeModulesResult) => {
          const loadedPackagesStatus = createLoadedPackageStatusMapFromDependencies(
            deps,
            fetchNodeModulesResult.dependenciesWithError,
            fetchNodeModulesResult.dependenciesNotFound,
          )
          const packageErrorActions = Object.keys(loadedPackagesStatus).map((dependencyName) =>
            setPackageStatus(dependencyName, loadedPackagesStatus[dependencyName].status),
          )
          dispatch([
            ...packageErrorActions,
            updateNodeModulesContents(fetchNodeModulesResult.nodeModules),
          ])
        })
      }
    }

    return {
      ...editor,
      projectContents: updatedProjectContents,
      canvas: {
        ...editor.canvas,
        canvasContentInvalidateCount:
          editor.canvas.canvasContentInvalidateCount + (isTextFile(file) ? 0 : 1),
        domWalkerInvalidateCount:
          editor.canvas.domWalkerInvalidateCount + (isTextFile(file) ? 0 : 1),
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
  UPDATE_FROM_WORKER: (action: UpdateFromWorker, editor: EditorModel): EditorModel => {
    if (editor.parseOrPrintInFlight) {
      let workingProjectContents: ProjectContentTreeRoot = editor.projectContents
      let anyParsedUpdates: boolean = false

      for (const fileUpdate of action.updates) {
        const existing = getContentsTreeFileFromString(editor.projectContents, fileUpdate.filePath)
        if (existing != null && isTextFile(existing)) {
          let updatedFile: TextFile
          let updatedContents: ParsedTextFile
          let code: string
          switch (fileUpdate.type) {
            case 'WORKER_CODE_UPDATE': {
              // we use the new highlightBounds coming from the action
              code = fileUpdate.code
              updatedContents = updateParsedTextFileHighlightBounds(
                existing.fileContents.parsed,
                fileUpdate.highlightBounds,
              )
              break
            }
            case 'WORKER_PARSED_UPDATE': {
              anyParsedUpdates = true

              // we use the new highlightBounds coming from the action
              code = existing.fileContents.code
              const highlightBounds = getHighlightBoundsFromParseResult(fileUpdate.parsed)
              updatedContents = updateParsedTextFileHighlightBounds(
                fileUpdate.parsed,
                highlightBounds,
              )
              break
            }
            default:
              const _exhaustiveCheck: never = fileUpdate
              throw new Error(`Invalid file update: ${fileUpdate}`)
          }

          if (fileUpdate.lastRevisedTime < existing.lastRevisedTime) {
            // if the received file is older than the existing, we still allow it to update the other side,
            // but we don't bump the revision state or the lastRevisedTime.
            updatedFile = textFile(
              textFileContents(code, updatedContents, existing.fileContents.revisionsState),
              existing.lastSavedContents,
              isParseSuccess(updatedContents) ? updatedContents : existing.lastParseSuccess,
              existing.lastRevisedTime,
            )
          } else {
            updatedFile = textFile(
              textFileContents(code, updatedContents, RevisionsState.BothMatch),
              existing.lastSavedContents,
              isParseSuccess(updatedContents) ? updatedContents : existing.lastParseSuccess,
              Date.now(),
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
    } else {
      // We've received this update after an editor undo action, so we should discard it
      return editor
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

      updatedFile = textFile(contents, lastSavedContents, null, Date.now())
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
    action: DeleteFile,
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
          deleteAssetFile(editor.id, action.filename)
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
    const allBuildErrorsInState = getAllBuildErrors(editor)
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
    const allLintErrorsInState = getAllLintErrors(editor)
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
        domMetadata: finalDomMetadata,
        spyMetadata: finalSpyMetadata,
        allElementProps: {
          ...spyCollector.current.spyValues.allElementProps,
        },
      }
    }
  },
  SET_PROP: (action: SetProp, editor: EditorModel): EditorModel => {
    return setPropertyOnTarget(editor, action.target, (props) => {
      return mapEither(
        (attrs) => roundAttributeLayoutValues(['style'], attrs),
        setJSXValueAtPath(props, action.propertyPath, action.value),
      )
    })
  },
  SET_PROP_WITH_ELEMENT_PATH: (
    action: SetPropWithElementPath,
    editor: EditorModel,
  ): EditorModel => {
    return setPropertyOnTargetAtElementPath(editor, action.target, (props) => {
      return setJSXValueAtPath(props, action.propertyPath, action.value)
    })
  },
  // NB: this can only update attribute values and part of attribute value,
  // If you want other types of JSXAttributes, that needs to be added
  RENAME_PROP_KEY: (action: RenameStyleSelector, editor: EditorModel): EditorModel => {
    return setPropertyOnTarget(editor, action.target, (props) => {
      const originalPropertyPath = PP.create(action.cssTargetPath.path)
      const newPropertyPath = PP.create(action.value)
      const originalValue = getJSXAttributeAtPath(props, originalPropertyPath).attribute
      const attributesWithUnsetKey = unsetJSXValueAtPath(props, originalPropertyPath)
      if (isJSXAttributeValue(originalValue) || isPartOfJSXAttributeValue(originalValue)) {
        if (isRight(attributesWithUnsetKey)) {
          const setResult = setJSXValueAtPath(
            attributesWithUnsetKey.value,
            newPropertyPath,
            jsxAttributeValue(originalValue.value, emptyComments),
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
  SWITCH_LAYOUT_SYSTEM: (action: SwitchLayoutSystem, editor: EditorModel): EditorModel => {
    return editor.selectedViews.reduce((working, target) => {
      return switchAndUpdateFrames(working, target, action.layoutSystem, action.propertyTarget)
    }, editor)
  },
  UPDATE_JSX_ELEMENT_NAME: (action: UpdateJSXElementName, editor: EditorModel): EditorModel => {
    const updatedEditor = UPDATE_FNS.ADD_IMPORTS(
      addImports(action.importsToAdd, action.target),
      editor,
    )

    return modifyOpenJsxElementAtPath(
      action.target,
      (element) => {
        return {
          ...element,
          name: action.elementName,
        }
      },
      updatedEditor,
    )
  },
  ADD_IMPORTS: (action: AddImports, editor: EditorModel): EditorModel => {
    return modifyUnderlyingTarget(
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
        const locked = jsxAttributeValue(action.locked, emptyComments)
        const updatedProps = eitherToMaybe(
          setJSXValueAtPath(element.props, PP.create(['data-aspect-ratio-locked']), locked),
        )
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
    const projectContent = getContentsTreeFileFromString(editor.projectContents, action.imagePath)
    const parent = arrayToMaybe(editor.highlightedViews)
    if (projectContent != null && isImageFile(projectContent)) {
      const newUID = generateUidWithExistingComponents(editor.projectContents)
      const imageAttribute = jsxAttributeValue(imagePathURL(action.imagePath), emptyComments)
      const size: Size = {
        width: projectContent.width ?? 100,
        height: projectContent.height ?? 100,
      }
      const { frame } = getFrameAndMultiplier(action.position, action.imagePath, size, null)
      let parentShiftX: number = 0
      let parentShiftY: number = 0
      if (parent != null) {
        const frameOfParent = MetadataUtils.getFrameInCanvasCoords(parent, editor.jsxMetadata)
        if (frameOfParent != null) {
          parentShiftX = -frameOfParent.x
          parentShiftY = -frameOfParent.y
        }
      }
      const imageElement = jsxElement(
        jsxElementName('img', []),
        newUID,
        jsxAttributesFromMap({
          alt: jsxAttributeValue('', emptyComments),
          src: imageAttribute,
          style: jsxAttributeValue(
            {
              left: parentShiftX + frame.x,
              top: parentShiftY + frame.y,
              width: frame.width,
              height: frame.height,
            },
            emptyComments,
          ),
          'data-uid': jsxAttributeValue(newUID, emptyComments),
          'data-aspect-ratio-locked': jsxAttributeValue(true, emptyComments),
        }),
        [],
      )

      const insertJSXElementAction = insertJSXElement(imageElement, parent, {})
      return UPDATE_FNS.INSERT_JSX_ELEMENT(insertJSXElementAction, editor)
    } else {
      throw new Error(`Could not be found or is not a file: ${action.imagePath}`)
    }
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
    }
    // Side effect.
    saveUserConfiguration(updatedUserConfiguration)
    return {
      ...updatedUserConfiguration,
      loginState: userState.loginState,
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
  UPDATE_CHILD_TEXT: (action: UpdateChildText, editor: EditorModel): EditorModel => {
    return modifyOpenJsxElementAtPath(
      action.target,
      (element) => {
        if (action.text.trim() === '') {
          return {
            ...element,
            children: [],
          }
        } else {
          return {
            ...element,
            children: [jsxTextBlock(action.text)],
          }
        }
      },
      editor,
    )
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
  ): EditorModel => {
    // Side effects.
    sendCodeEditorDecorations(editor)
    sendSelectedElement(editor)
    return {
      ...editor,
      vscodeReady: true,
    }
  },
  SET_FOCUSED_ELEMENT: (action: SetFocusedElement, editor: EditorModel): EditorModel => {
    let shouldApplyChange: boolean = false
    if (action.focusedElementPath == null) {
      shouldApplyChange = true
    } else if (MetadataUtils.isFocusableComponent(action.focusedElementPath, editor.jsxMetadata)) {
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
    if (targetElementCoords != null) {
      const isNavigatorOnTop = !editor.navigator.minimised
      const containerRootDiv = document.getElementById('canvas-root')
      if (action.keepScrollPositionIfVisible && containerRootDiv != null) {
        const containerDivBoundingRect = containerRootDiv.getBoundingClientRect()
        const navigatorOffset = isNavigatorOnTop ? LeftPaneDefaultWidth : 0
        const containerRectangle = {
          x: navigatorOffset - editor.canvas.realCanvasOffset.x,
          y: -editor.canvas.realCanvasOffset.y,
          width: containerDivBoundingRect.width,
          height: containerDivBoundingRect.height,
        } as CanvasRectangle
        const isVisible = rectangleIntersection(containerRectangle, targetElementCoords) != null
        // when the element is on screen no scrolling is needed
        if (isVisible) {
          return editor
        }
      }
      const baseCanvasOffset = isNavigatorOnTop ? BaseCanvasOffsetLeftPane : BaseCanvasOffset
      const newCanvasOffset = Utils.pointDifference(targetElementCoords, baseCanvasOffset)

      return UPDATE_FNS.SET_SCROLL_ANIMATION(
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
  SET_CURRENT_THEME: (action: SetCurrentTheme, editor: EditorModel): EditorModel => {
    return {
      ...editor,
      theme: action.theme,
    }
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
      const withNewElement = modifyUnderlyingTarget(
        action.targetParent,
        openFilename,
        editor,
        (element) => element,
        (success, _, underlyingFilePath) => {
          const utopiaComponents = getUtopiaJSXComponentsFromSuccess(success)
          const newUID = generateUidWithExistingComponents(editor.projectContents)

          const propsWithUid = forceRight(
            setJSXValueAtPath(
              action.toInsert.element.props,
              PP.create([UTOPIA_UID_KEY]),
              jsxAttributeValue(newUID, emptyComments),
            ),
            `Could not set data-uid on props of insertable element ${action.toInsert.element.name}`,
          )
          // Potentially add in some default position and sizing.
          let props = propsWithUid
          if (action.styleProps === 'add-size') {
            const sizesToSet: Array<ValueAtPath> = [
              { path: PP.create(['style', 'width']), value: jsxAttributeValue(100, emptyComments) },
              {
                path: PP.create(['style', 'height']),
                value: jsxAttributeValue(100, emptyComments),
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

          if (action.wrapContent === 'wrap-content' && !isImg(insertedElementName)) {
            withMaybeUpdatedParent = transformElementAtPath(
              utopiaComponents,
              action.targetParent,
              (parentElement) => {
                insertedElementChildren.push(...parentElement.children)
                return jsxElement(parentElement.name, parentElement.uid, parentElement.props, [])
              },
            )
          }

          insertedElementChildren.push(...action.toInsert.element.children)
          const element = jsxElement(insertedElementName, newUID, props, insertedElementChildren)

          const withInsertedElement = insertElementAtPath(
            editor.projectContents,
            openFilename,
            action.targetParent,
            element,
            withMaybeUpdatedParent,
            action.indexPosition,
          )

          const newPath = EP.appendToPath(action.targetParent, newUID)
          newSelectedViews.push(newPath)

          const updatedTopLevelElements = applyUtopiaJSXComponentsChanges(
            success.topLevelElements,
            withInsertedElement,
          )

          const updatedImports = mergeImports(
            underlyingFilePath,
            success.imports,
            action.toInsert.importsToAdd,
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

    findLatestVersion('tailwindcss').then((tailwindResult) => {
      if (tailwindResult.type === 'VERSION_LOOKUP_SUCCESS') {
        const tailwindVersion = tailwindResult.version
        findLatestVersion('postcss').then((postcssResult) => {
          if (postcssResult.type === 'VERSION_LOOKUP_SUCCESS') {
            const postcssVersion = postcssResult.version
            const updatedNpmDeps = [
              ...currentNpmDeps,
              requestedNpmDependency('tailwindcss', tailwindVersion.version),
              requestedNpmDependency('postcss', postcssVersion.version),
            ]
            fetchNodeModules(updatedNpmDeps, builtInDependencies).then((fetchNodeModulesResult) => {
              const loadedPackagesStatus = createLoadedPackageStatusMapFromDependencies(
                updatedNpmDeps,
                fetchNodeModulesResult.dependenciesWithError,
                fetchNodeModulesResult.dependenciesNotFound,
              )
              const packageErrorActions = Object.keys(loadedPackagesStatus).map((dependencyName) =>
                setPackageStatus(dependencyName, loadedPackagesStatus[dependencyName].status),
              )
              dispatch([
                ...packageErrorActions,
                updateNodeModulesContents(fetchNodeModulesResult.nodeModules),
              ])
            })

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
  SET_INSPECTOR_LAYOUT_SECTION_HOVERED: (
    action: SetInspectorLayoutSectionHovered,
    editor: EditorModel,
  ): EditorModel => {
    return {
      ...editor,
      inspector: {
        ...editor.inspector,
        layoutSectionHovered: action.hovered,
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
  RUN_ESCAPE_HATCH: (action: RunEscapeHatch, editor: EditorModel): EditorModel => {
    const canvasState = pickCanvasStateFromEditorState(editor)
    const commands = getEscapeHatchCommands(action.targets, editor.jsxMetadata, canvasState, null)
    return foldAndApplyCommandsSimple(editor, commands)
  },
  SET_ELEMENTS_TO_RERENDER: (action: SetElementsToRerender, editor: EditorModel): EditorModel => {
    return foldAndApplyCommandsSimple(editor, [setElementsToRerenderCommand(action.value)])
  },
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
        if (instanceGlobalFrame == null) {
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
        source = Utils.forceNotNull(
          `found no parent global frame for ${EP.toComponentId(parentPath!)}`,
          parentFrame,
        )
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

function revertFileInProjectContents(
  projectContents: ProjectContentTreeRoot,
  filePath: string,
): ProjectContentTreeRoot {
  const file = getContentsTreeFileFromString(projectContents, filePath)
  if (file == null) {
    return projectContents
  } else {
    const updatedProjectContents = addFileToProjectContents(
      projectContents,
      filePath,
      revertFile(file),
    )
    return updatedProjectContents
  }
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
