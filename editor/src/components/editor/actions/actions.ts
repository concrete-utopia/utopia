import { produce } from 'immer'
import * as update from 'immutability-helper'
import * as React from 'react'
import * as localforage from 'localforage'
import { CursorPosition } from 'src/components/code-editor/code-editor-utils'
import {
  FlexAlignment,
  FramePoint,
  LayoutSystem,
  NormalisedFrame,
  PropertyControls,
} from 'utopia-api'
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
import { createLayoutPropertyPath } from '../../../core/layout/layout-helpers-new'
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
} from '../../../core/shared/element-template'
import {
  generateUidWithExistingComponents,
  getUtopiaID,
  setUtopiaID,
  transformJSXComponentAtElementPath,
  insertJSXElementChild,
  findJSXElementChildAtPath,
  getZIndexOfElement,
} from '../../../core/model/element-template-utils'
import {
  getJSXAttributeAtPath,
  jsxSimpleAttributeToValue,
  setJSXValueAtPath,
  unsetJSXValueAtPath,
  getModifiableJSXAttributeAtPath,
  unsetJSXValuesAtPaths,
  setJSXValuesAtPaths,
} from '../../../core/shared/jsx-attributes'
import { getDefaultUIJsFile } from '../../../core/model/new-project-files'
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
} from '../../../core/shared/either'
import type {
  RequireFn,
  TypeDefinitions,
  PackageStatus,
  PackageStatusMap,
  RequestedNpmDependency,
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
} from '../../../core/shared/project-file-types'
import {
  addImport,
  codeNeedsParsing,
  codeNeedsPrinting,
  mergeImports,
} from '../../../core/workers/common/project-file-utils'
import { OutgoingWorkerMessage, isJsFile, BuildType } from '../../../core/workers/ts/ts-worker'
import { UtopiaTsWorkers } from '../../../core/workers/common/worker-types'
import { defaultProject, sampleProjectForId } from '../../../sample-projects/sample-project-utils'
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
  getFrameChange,
  moveTemplate,
  produceCanvasTransientState,
  SkipFrameChange,
  updateFramesOfScenesAndComponents,
  cullSpyCollector,
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
  SetCanvasAnimationsEnabled,
  SetCanvasFrames,
  SetCodeEditorBuildErrors,
  SetCodeEditorLintErrors,
  SetCodeEditorVisibility,
  SetCursorOverlay,
  SetFilebrowserRenamingTarget,
  SetHighlightedView,
  SetHighlightsEnabled,
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
  ResetPropToDefault,
  UpdateNodeModulesContents,
  UpdatePackageJson,
  StartCheckpointTimer,
  FinishCheckpointTimer,
  AddMissingDimensions,
  SetPackageStatus,
  SetShortcut,
  UpdatePropertyControlsInfo,
  PropertyControlsIFrameReady,
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
} from '../npm-dependency/npm-dependency'
import { updateRemoteThumbnail } from '../persistence'
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
  getOpenUIJSFile,
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
  getNumberOfScenes,
  addSceneToJSXComponents,
  UserState,
  UserConfiguration,
  getHighlightBoundsForUids,
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
} from '../store/editor-state'
import { loadStoredState } from '../stored-state'
import { applyMigrations } from './migrations/migrations'
import { fastForEach, getProjectLockedKey } from '../../../core/shared/utils'
import { PathForSceneDataLabel, getStoryboardElementPath } from '../../../core/model/scene-utils'
import { getFrameAndMultiplier } from '../../images'
import { arrayToMaybe, forceNotNull, optionalMap } from '../../../core/shared/optional-utils'

import { notice, Notice } from '../../common/notice'
import { objectMap } from '../../../core/shared/object-utils'
import { getDependencyTypeDefinitions } from '../../../core/es-modules/package-manager/package-manager'
import { fetchNodeModules } from '../../../core/es-modules/package-manager/fetch-packages'
import {
  getPropertyControlsForTarget,
  getPropertyControlsForTargetFromEditor,
  setPropertyControlsIFrameReady,
} from '../../../core/property-controls/property-controls-utils'
import { UiJsxCanvasContextData } from '../../canvas/ui-jsx-canvas'
import { lintAndParse } from '../../../core/workers/parser-printer/parser-printer'
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
} from './action-creators'
import { emptyComments } from '../../../core/workers/parser-printer/parser-printer-comments'
import { getAllTargetsAtPoint } from '../../canvas/dom-lookup'
import {
  initVSCodeBridge,
  sendCodeEditorDecorations,
  sendOpenFileMessage,
  sendSelectedElement,
  sendSetFollowSelectionEnabledMessage,
} from '../../../core/vscode/vscode-bridge'
import Meta from 'antd/lib/card/Meta'
import utils from '../../../utils/utils'
import { defaultConfig } from 'utopia-vscode-common'
import { getTargetParentForPaste } from '../../../utils/clipboard'
import { absolutePathFromRelativePath, stripLeadingSlash } from '../../../utils/path-utils'
import { resolveModule } from '../../../core/es-modules/package-manager/module-resolution'
import { reverse, uniqBy } from '../../../core/shared/array-utils'

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
): EditorModel {
  const targetMetadata = Utils.forceNotNull(
    `Could not find metadata for ${JSON.stringify(target)}`,
    MetadataUtils.findElementByElementPath(editor.jsxMetadata, target),
  )
  if (targetMetadata.globalFrame == null) {
    // The target is a non-layoutable
    return editor
  }

  const layoutSystemPath = createLayoutPropertyPath('LayoutSystem')
  const styleDisplayPath = createLayoutPropertyPath('display')

  let withUpdatedLayoutSystem: EditorModel = editor
  switch (layoutSystem) {
    case 'flex':
      withUpdatedLayoutSystem = setPropertyOnTarget(
        withUpdatedLayoutSystem,
        target,
        (attributes) => {
          return unsetJSXValueAtPath(attributes, layoutSystemPath)
        },
      )
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
        layoutSystemPath,
        createLayoutPropertyPath('PinnedLeft'),
        createLayoutPropertyPath('PinnedTop'),
        createLayoutPropertyPath('PinnedRight'),
        createLayoutPropertyPath('PinnedBottom'),
        createLayoutPropertyPath('PinnedCenterX'),
        createLayoutPropertyPath('PinnedCenterY'),
        createLayoutPropertyPath('position'),
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
            createLayoutPropertyPath('position'),
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
        jsxMetadata: MetadataUtils.unsetPropertyDirectlyIntoMetadata(
          withUpdatedLayoutSystem.jsxMetadata,
          target,
          layoutSystemPath,
        ),
      }
      withUpdatedLayoutSystem = {
        ...withUpdatedLayoutSystem,
        jsxMetadata: MetadataUtils.setPropertyDirectlyIntoMetadata(
          withUpdatedLayoutSystem.jsxMetadata,
          target,
          styleDisplayPath, // TODO LAYOUT investigate if we should use also update the DOM walker specialSizeMeasurements
          'flex',
        ),
      }
      withUpdatedLayoutSystem = {
        ...withUpdatedLayoutSystem,
        jsxMetadata: MetadataUtils.setPropertyDirectlyIntoMetadata(
          withUpdatedLayoutSystem.jsxMetadata,
          target,
          createLayoutPropertyPath('position'), // TODO LAYOUT investigate if we should use also update the DOM walker specialSizeMeasurements
          'relative',
        ),
      }
      break
    case 'flow':
      withUpdatedLayoutSystem = {
        ...withUpdatedLayoutSystem,
        jsxMetadata: MetadataUtils.unsetPropertyDirectlyIntoMetadata(
          withUpdatedLayoutSystem.jsxMetadata,
          target,
          layoutSystemPath,
        ),
      }
      break
    case LayoutSystem.PinSystem:
      withUpdatedLayoutSystem = {
        ...withUpdatedLayoutSystem,
        jsxMetadata: MetadataUtils.unsetPropertyDirectlyIntoMetadata(
          withUpdatedLayoutSystem.jsxMetadata,
          target,
          layoutSystemPath,
        ),
      }
      withUpdatedLayoutSystem = {
        ...withUpdatedLayoutSystem,
        jsxMetadata: MetadataUtils.setPropertyDirectlyIntoMetadata(
          withUpdatedLayoutSystem.jsxMetadata,
          target,
          createLayoutPropertyPath('position'), // TODO LAYOUT investigate if we should use also update the DOM walker specialSizeMeasurements
          'absolute',
        ),
      }
      break
    case LayoutSystem.Group:
    default:
      withUpdatedLayoutSystem = {
        ...withUpdatedLayoutSystem,
        jsxMetadata: MetadataUtils.unsetPropertyDirectlyIntoMetadata(
          withUpdatedLayoutSystem.jsxMetadata,
          target,
          styleDisplayPath,
        ),
      }
      withUpdatedLayoutSystem = {
        ...withUpdatedLayoutSystem,
        jsxMetadata: MetadataUtils.setPropertyDirectlyIntoMetadata(
          withUpdatedLayoutSystem.jsxMetadata,
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
      return maybeSwitchChildrenLayoutProps(target, editor.jsxMetadata, metadata, components)
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

  Utils.fastForEach(targetMetadata.children, (childPath) => {
    const child = MetadataUtils.findElementByElementPath(editor.jsxMetadata, childPath)
    if (child?.globalFrame != null) {
      // if the globalFrame is null, this child is a non-layoutable so just skip it
      const isParentOfChildFlex = MetadataUtils.isParentYogaLayoutedContainerAndElementParticipatesInLayout(
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
  newParentLayoutType: LayoutSystem | null,
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
  newParentLayoutSystem: LayoutSystem | null,
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
    domMetadata: [],
    jsxMetadata: poppedEditor.jsxMetadata,
    projectContents: poppedEditor.projectContents,
    nodeModules: currentEditor.nodeModules,
    codeResultCache: currentEditor.codeResultCache,
    propertyControlsInfo: currentEditor.propertyControlsInfo,
    selectedViews: poppedEditor.selectedViews,
    highlightedViews: poppedEditor.highlightedViews,
    hiddenInstances: poppedEditor.hiddenInstances,
    warnedInstances: poppedEditor.warnedInstances,
    mode: EditorModes.selectMode(),
    focusedPanel: currentEditor.focusedPanel,
    keysPressed: {},
    openPopupId: null,
    toasts: poppedEditor.toasts,
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
      visible: currentEditor.canvas.visible,
      dragState: null,
      scale: currentEditor.canvas.scale,
      snappingThreshold: currentEditor.canvas.snappingThreshold,
      realCanvasOffset: currentEditor.canvas.realCanvasOffset,
      roundedCanvasOffset: currentEditor.canvas.roundedCanvasOffset,
      textEditor: null,
      selectionControlsVisible: currentEditor.canvas.selectionControlsVisible,
      animationsEnabled: currentEditor.canvas.animationsEnabled,
      highlightsEnabled: true,
      cursor: null,
      duplicationState: null,
      base64Blobs: {},
      mountCount: currentEditor.canvas.mountCount, // QUESTION should undo-redo forcibly remount the canvas?
      canvasContentInvalidateCount: currentEditor.canvas.canvasContentInvalidateCount + 1,
      domWalkerInvalidateCount: currentEditor.canvas.domWalkerInvalidateCount + 1,
      openFile: currentEditor.canvas.openFile,
      scrollAnimation: currentEditor.canvas.scrollAnimation,
    },
    inspector: {
      visible: currentEditor.inspector.visible,
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
      position: currentEditor.navigator.position,
      dropTargetHint: {
        target: null,
        type: null,
      },
      collapsedViews: poppedEditor.navigator.collapsedViews,
      renamingTarget: null,
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
    focusedElementPath: currentEditor.focusedElementPath,
    config: defaultConfig(),
  }
}

export function restoreDerivedState(history: StateHistory): DerivedState {
  const poppedDerived = history.current.derived

  return {
    navigatorTargets: poppedDerived.navigatorTargets,
    visibleNavigatorTargets: poppedDerived.visibleNavigatorTargets,
    canvas: {
      descendantsOfHiddenInstances: poppedDerived.canvas.descendantsOfHiddenInstances,
      controls: [],
      transientState: produceCanvasTransientState(
        poppedDerived.canvas.transientState.selectedViews,
        history.current.editor,
        true,
      ),
    },
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
    temporaryExpandedViews: {
      $set: collapsedWithChildrenSelected,
    },
  })
}

export function getNavigatorPositionNextState(editor: EditorState): 'hidden' | 'left' | 'right' {
  switch (editor.navigator.position) {
    case 'hidden':
      return 'right'
    case 'left':
      return 'hidden'
    case 'right':
      return editor.interfaceDesigner.codePaneVisible ? 'left' : 'hidden'
    default:
      const _exhaustiveCheck: never = editor.navigator.position
      throw new Error(`Cannot update navigator position to ${editor.navigator.position}`)
  }
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
  targetsAndZIndex.sort((a, b) => a.index - b.index)
  const orderedTargets = Utils.pluck(targetsAndZIndex, 'target')

  // keep direct children from reparenting
  let filteredTargets: Array<ElementPath> = []
  Utils.fastForEach(orderedTargets, (target) => {
    if (!orderedTargets.find((tp) => EP.pathsEqual(EP.parentPath(target), tp))) {
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
    workers.sendInitMessage(
      getDependencyTypeDefinitions(action.nodeModules),
      newModel.projectContents,
    )
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
        const parseResult = lintAndParse(filename, file.fileContents.code, null)
        const lastSavedFileContents = optionalMap((lastSaved) => {
          const lastSavedParseResult = lintAndParse(filename, lastSaved.code, null)
          return textFileContents(lastSaved.code, lastSavedParseResult, RevisionsState.BothMatch)
        }, file.lastSavedContents)
        return textFile(
          textFileContents(file.fileContents.code, parseResult, RevisionsState.BothMatch),
          lastSavedFileContents,
          Date.now(),
        )
      },
    )
    initVSCodeBridge(action.projectId, parsedProjectFiles, dispatch)
    const parsedModel = {
      ...migratedModel,
      projectContents: parsedProjectFiles,
    }
    const newModel: EditorModel = {
      ...editorModelFromPersistentModel(parsedModel, dispatch),
      projectName: action.title,
      id: action.projectId,
      vscodeBridgeId: action.projectId, // we assign a first value when loading a project. SET_PROJECT_ID will not change this, saving us from having to reload VSCode
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
    const openUIJSFile = getOpenUIJSFile(editor)
    if (openUIJSFile == null || !isParseSuccess(openUIJSFile.fileContents.parsed)) {
      return editor
    } else {
      const components = getUtopiaJSXComponentsFromSuccess(openUIJSFile.fileContents.parsed)
      const target = action.element
      const element = findElementAtPath(target, components)
      if (element == null || !isJSXElement(element)) {
        return editor
      } else {
        const updatedProps = unsetJSXValueAtPath(element.props, action.property)
        const updatedResult = foldEither(
          (failureMessage) => {
            const toastAction = showToast(notice(failureMessage, 'ERROR'))
            return UPDATE_FNS.ADD_TOAST(toastAction, editor, dispatch)
          },
          (updated) => {
            return modifyOpenJsxElementAtPath(
              target,
              (openElement) => {
                return {
                  ...openElement,
                  props: updated,
                }
              },
              editor,
            )
          },
          updatedProps,
        )
        return updatedResult
      }
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
    let index: number
    const uiFile = getOpenUIJSFile(editor)
    if (uiFile == null) {
      console.warn('Attempted to find the index of a view with no ui file open.')
      return editor
    } else {
      if (isParseSuccess(uiFile.fileContents.parsed)) {
        index = MetadataUtils.getViewZIndexFromMetadata(editor.jsxMetadata, targetPath)
      } else {
        console.warn(
          'Attempted to find the index of a view when the code currently does not parse.',
        )
        return editor
      }
    }
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
    )

    return {
      ...withMovedTemplate,
      selectedViews: newPaths,
      highlightedViews: [],
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
    ).editor
  },
  DELETE_SELECTED: (
    action: DeleteSelected,
    editorForAction: EditorModel,
    derived: DerivedState,
    dispatch: EditorDispatch,
  ): EditorModel => {
    return toastOnGeneratedElementsSelected(
      'Generated elements can only be deleted in code. ',
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
        return deleteElements(staticSelectedElements, editor)
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
    if (!action.toast.persistent) {
      setTimeout(() => dispatch([removeToast(action.toast.id)], 'everyone'), 5500)
    }

    const withOldToastRemoved = UPDATE_FNS.REMOVE_TOAST(removeToast(action.toast.id), editor)
    return {
      ...withOldToastRemoved,
      toasts: [...withOldToastRemoved.toasts, action.toast],
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
    const numberOfScenes = getNumberOfScenes(editor)
    const sceneUID = generateUidWithExistingComponents(editor.projectContents)
    const newSceneLabel = `Scene ${numberOfScenes}`
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
      (success) => {
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

        const updatedImports = mergeImports(success.imports, action.importsToAdd)
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
    return toastOnGeneratedElementsSelected(
      `Generated elements can't be wrapped into other elements.`,
      editorForAction,
      false,
      (editor) => {
        const uiFileKey = getOpenUIJSFileKey(editor)
        if (uiFileKey == null) {
          return editor
        }

        const newUID = generateUidWithExistingComponents(editor.projectContents)

        const orderedActionTargets = getZIndexOrderedViewsWithoutDirectChildren(
          action.targets,
          derived,
        )
        const parentPath = EP.getCommonParent(orderedActionTargets)
        if (parentPath === null) {
          return editor
        } else {
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
            parentPath,
          )
          const targetSuccess = normalisePathSuccessOrThrowError(underlyingTarget)

          const withWrapperViewAddedNoFrame = modifyParseSuccessAtPath(
            targetSuccess.filePath,
            editor,
            (parseSuccess) => {
              const elementToInsert: JSXElement = defaultTransparentViewElement(newUID)
              const utopiaJSXComponents = getUtopiaJSXComponentsFromSuccess(parseSuccess)
              const withTargetAdded: Array<UtopiaJSXComponent> = insertElementAtPath(
                editor.projectContents,
                editor.canvas.openFile?.filename ?? null,
                parentPath,
                elementToInsert,
                utopiaJSXComponents,
                null,
              )

              viewPath = EP.appendToPath(parentPath, newUID)

              return modifyParseSuccessWithSimple((success: SimpleParseSuccess) => {
                return {
                  ...success,
                  utopiaComponents: withTargetAdded,
                  imports: addImport(
                    'utopia-api',
                    null,
                    [importAlias('View')],
                    null,
                    success.imports,
                  ),
                }
              }, parseSuccess)
            },
          )

          if (viewPath == null) {
            return editor
          }

          const parent = MetadataUtils.findElementByElementPath(editor.jsxMetadata, parentPath)
          const isParentFlex =
            parent != null ? MetadataUtils.isFlexLayoutedContainer(parent) : false
          const frameChanges: Array<PinOrFlexFrameChange> = [
            getFrameChange(viewPath, boundingBox, isParentFlex),
          ]
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
        const children = MetadataUtils.getChildrenHandlingGroups(
          editor.jsxMetadata,
          action.target,
          true,
        )
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
      case 'navigatorPane':
        return {
          ...editor,
          navigator: {
            ...editor.navigator,
            position: action.visible ? 'right' : 'hidden',
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
      case 'navigatorPane':
        return {
          ...editor,
          navigator: {
            ...editor.navigator,
            position: getNavigatorPositionNextState(editor),
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
      return action.elements.reduce((workingEditorState, currentValue, index) => {
        return modifyUnderlyingForOpenFile(
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
              updatedComponents = maybeSwitchLayoutProps(
                newPath,
                originalPath,
                targetParent,
                action.targetOriginalContextMetadata,
                workingEditorState.jsxMetadata,
                components,
                null,
                null,
              ).components
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
      return update(editor, {
        navigator: {
          collapsedViews: {
            $set: editor.navigator.collapsedViews.filter(
              (element) => !EP.pathsEqual(element, action.target),
            ),
          },
        },
      })
    } else {
      return update(editor, {
        navigator: {
          collapsedViews: {
            $set: editor.navigator.collapsedViews.concat(action.target),
          },
        },
      })
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
      [FramePoint.Left]: frame.x,
      [FramePoint.Top]: frame.y,
      [FramePoint.Width]: frame.width,
      [FramePoint.Height]: frame.height,
    }

    let errorMessage: string | null = null

    const updatedEditor = modifyOpenJsxElementAtPath(
      target,
      (element: JSXElement) => {
        const updatedAttributes = PinLayoutHelpers.setLayoutPropsToPinsWithFrame(
          element.props,
          newLayout,
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
  SET_CANVAS_ANIMATIONS_ENABLED: (
    action: SetCanvasAnimationsEnabled,
    editor: EditorModel,
  ): EditorModel => {
    if (editor.canvas.animationsEnabled === action.value) {
      return editor
    }
    return {
      ...editor,
      canvas: {
        ...editor.canvas,
        animationsEnabled: action.value,
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
    if (
      element != null &&
      MetadataUtils.isTextAgainstImports(element) &&
      element.props.textSizing == 'auto'
    ) {
      const alignment = element.props.style.textAlign
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
      projectFile = assetFile()
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
        requireFn: action.codeResultCache.requireFn,
        resolve: action.codeResultCache.resolve,
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
  SET_HIGHLIGHTS_ENABLED: (action: SetHighlightsEnabled, editor: EditorModel): EditorModel => {
    if (editor.canvas.highlightsEnabled === action.value) {
      return editor
    }
    return {
      ...editor,
      canvas: {
        ...editor.canvas,
        highlightsEnabled: action.value,
      },
    }
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
  UPDATE_FILE: (action: UpdateFile, editor: EditorModel, dispatch: EditorDispatch): EditorModel => {
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

    let packageLoadingStatus: PackageStatusMap = {}

    // Ensure dependencies are updated if the `package.json` file has been changed.
    if (action.filePath === '/package.json' && isTextFile(file)) {
      const deps = dependenciesFromPackageJsonContents(file.fileContents.code)
      if (deps != null) {
        packageLoadingStatus = deps.reduce((packageStatus: PackageStatusMap, dep) => {
          packageStatus[dep.name] = { status: 'loading' }
          return packageStatus
        }, {})

        fetchNodeModules(deps, 'canvas').then((fetchNodeModulesResult) => {
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
            updateNodeModulesContents(fetchNodeModulesResult.nodeModules, 'full-build'),
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
        packageStatus: {
          ...editor.nodeModules.packageStatus,
          ...packageLoadingStatus,
        },
      },
    }
  },
  UPDATE_FROM_WORKER: (
    action: UpdateFromWorker,
    editor: EditorModel,
    derived: DerivedState,
  ): EditorModel => {
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
              // The worker should only ever cause one side to catch up to the other
              if (!codeNeedsPrinting(existing.fileContents.revisionsState)) {
                continue
              } else {
                // we use the new highlightBounds coming from the action
                code = fileUpdate.code
                updatedContents = updateParsedTextFileHighlightBounds(
                  existing.fileContents.parsed,
                  fileUpdate.highlightBounds,
                )
              }
              break
            }
            case 'WORKER_PARSED_UPDATE': {
              // The worker should only ever cause one side to catch up to the other
              if (!codeNeedsParsing(existing.fileContents.revisionsState)) {
                continue
              } else {
                anyParsedUpdates = true

                // we use the new highlightBounds coming from the action
                code = existing.fileContents.code
                const highlightBounds = getHighlightBoundsFromParseResult(fileUpdate.parsed)
                updatedContents = updateParsedTextFileHighlightBounds(
                  fileUpdate.parsed,
                  highlightBounds,
                )
              }
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
              existing.lastRevisedTime,
            )
          } else {
            updatedFile = textFile(
              textFileContents(code, updatedContents, RevisionsState.BothMatch),
              existing.lastSavedContents,
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
        PP.clearPropertyPathCache()
        EP.clearElementPathCache()
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

      updatedFile = textFile(contents, lastSavedContents, Date.now())
    } else {
      updatedFile = updateFileContents(code, existing, manualSave)
    }

    const updateAction = updateFile(action.filePath, updatedFile, true)
    return UPDATE_FNS.UPDATE_FILE(updateAction, editor, dispatch)
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
    const newFolderKey = uniqueProjectContentID(pathPrefix + 'folder', editor.projectContents)
    return {
      ...editor,
      projectContents: addFileToProjectContents(editor.projectContents, newFolderKey, directory()),
      fileBrowser: {
        ...editor.fileBrowser,
        renamingTarget: newFolderKey,
      },
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
      fileBrowser: {
        ...editor.fileBrowser,
        renamingTarget: newFileKey,
      },
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
        const filesToDelete: Array<string> = Object.keys(
          updatedEditor.projectContents,
        ).filter((key) => oldFolderRegex.test(key))
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

    // Keep the size of the spy collector down to some manageable level.
    cullSpyCollector(spyCollector, action.elementMetadata)

    // Calculate the spy metadata given what has been collected.
    const spyResult = spyCollector.current.spyValues.metadata

    const finalDomMetadata = arrayDeepEquality(ElementInstanceMetadataKeepDeepEquality())(
      editor.domMetadata,
      action.elementMetadata as Array<ElementInstanceMetadata>, // we convert a ReadonlyArray to a regular array – it'd be nice to make more arrays readonly in the future
    ).value
    const finalSpyMetadata = ElementInstanceMetadataMapKeepDeepEquality()(
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
      }
    }
  },
  SET_PROP: (action: SetProp, editor: EditorModel): EditorModel => {
    return setPropertyOnTarget(editor, action.target, (props) => {
      return mapEither(
        roundAttributeLayoutValues,
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
      return switchAndUpdateFrames(working, target, action.layoutSystem)
    }, editor)
  },
  UPDATE_JSX_ELEMENT_NAME: (action: UpdateJSXElementName, editor: EditorModel): EditorModel => {
    const updatedEditor = UPDATE_FNS.ADD_IMPORTS(addImports(action.importsToAdd), editor)

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
    return modifyOpenParseSuccess((success) => {
      return {
        ...success,
        imports: mergeImports(success.imports, action.importsToAdd),
      }
    }, editor)
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
  RESET_PROP_TO_DEFAULT: (action: ResetPropToDefault, editor: EditorModel): EditorModel => {
    const openFilePath = getOpenUIJSFileKey(editor)
    if (openFilePath != null) {
      const target = action.target
      const propertyControls = getPropertyControlsForTargetFromEditor(target, editor)
      let element: JSXElement | null = null
      forUnderlyingTargetFromEditorState(
        action.target,
        editor,
        (_underlyingSuccess, underlyingElement) => {
          element = underlyingElement
        },
      )
      if (element == null) {
        return editor
      }
      let defaultProps: { [key: string]: any } = {}
      if (propertyControls != null) {
        Utils.fastForEach(Object.keys(propertyControls), (key) => {
          const defaultValue = (propertyControls[key] as any).defaultValue
          if (defaultValue != null) {
            defaultProps[key] = defaultValue
          }
        })
      }

      const pathToUpdate: PropertyPath | null = action.path

      const propsForPath =
        action.path == null ? defaultProps : defaultProps[PP.toString(action.path)]

      if (pathToUpdate == null) {
        return setPropertyOnTarget(editor, target, (props) => {
          let updatedProps = jsxAttributesFromMap(
            objectMap((value) => jsxAttributeValue(value, emptyComments), defaultProps),
          )
          const dataUID = getJSXAttribute(props, 'data-uid')
          if (dataUID != null) {
            updatedProps = setJSXAttributesAttribute(updatedProps, 'data-uid', dataUID)
          }
          return right(updatedProps)
        })
      } else {
        return setPropertyOnTarget(editor, target, (props) => {
          return setJSXValueAtPath(
            props,
            pathToUpdate!,
            jsxAttributeValue(propsForPath, emptyComments),
          )
        })
      }
    } else {
      return editor
    }
  },
  UPDATE_NODE_MODULES_CONTENTS: (
    action: UpdateNodeModulesContents,
    editor: EditorState,
    dispatch: EditorDispatch,
  ): EditorState => {
    let result: EditorState
    if (action.buildType === 'full-build') {
      result = produce(editor, (draft) => {
        draft.nodeModules.files = action.contentsToAdd
      })
    } else {
      result = produce(editor, (draft) => {
        draft.nodeModules.files = {
          ...draft.nodeModules.files,
          ...action.contentsToAdd,
        }
      })
    }

    let onlyProjectFiles: boolean = true
    for (const key of Object.keys(action.contentsToAdd)) {
      if (key.startsWith('/node_modules')) {
        onlyProjectFiles = false
        break
      }
    }

    result = {
      ...result,
      codeResultCache: generateCodeResultCache(
        result.projectContents,
        editor.codeResultCache.projectModules,
        codeCacheToBuildResult(result.codeResultCache.cache),
        result.codeResultCache.exportsInfo,
        result.nodeModules.files,
        dispatch,
        editor.codeResultCache.evaluationCache,
        action.buildType,
        onlyProjectFiles,
      ),
    }

    return result
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
    return {
      ...editor,
      propertyControlsInfo: action.propertyControlsInfo,
    }
  },
  PROPERTY_CONTROLS_IFRAME_READY: (
    _action: PropertyControlsIFrameReady,
    editor: EditorModel,
  ): EditorModel => {
    // Internal side effect.
    setPropertyControlsIFrameReady(true)
    return editor
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
    const allElementPaths = derived.navigatorTargets
    const highlightBoundsForUids = getHighlightBoundsForFile(editor, action.filePath)
    const newlySelectedElements = getElementPathsInBounds(
      action.line,
      highlightBoundsForUids,
      allElementPaths,
    )
    return UPDATE_FNS.SELECT_COMPONENTS(
      selectComponents(newlySelectedElements, false),
      editor,
      dispatch,
    )
  },
  SEND_CODE_EDITOR_INITIALISATION: (
    action: SendCodeEditorInitialisation,
    editor: EditorModel,
  ): EditorModel => {
    // Side effects.
    sendCodeEditorDecorations(editor)
    sendSelectedElement(editor)
    return editor
  },
  SET_FOCUSED_ELEMENT: (action: SetFocusedElement, editor: EditorModel): EditorModel => {
    if (EP.pathsEqual(editor.focusedElementPath, action.focusedElementPath)) {
      return editor
    }

    return {
      ...editor,
      focusedElementPath: action.focusedElementPath,
      canvas: {
        ...editor.canvas,
        domWalkerInvalidateCount: editor.canvas.domWalkerInvalidateCount + 1,
      },
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
      const isNavigatorOnTop = editor.navigator.position === 'right'
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
        const isVisible = rectangleIntersection(containerRectangle, targetElementCoords)
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
      const isParentFlex = MetadataUtils.isParentYogaLayoutedContainerAndElementParticipatesInLayout(
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

export async function newProject(
  dispatch: EditorDispatch,
  renderEditorRoot: () => void,
): Promise<void> {
  const defaultPersistentModel = defaultProject()
  const npmDependencies = dependenciesWithEditorRequirements(defaultPersistentModel.projectContents)
  const fetchNodeModulesResult = await fetchNodeModules(npmDependencies, 'canvas')

  const nodeModules: NodeModules = fetchNodeModulesResult.nodeModules
  const packageResult: PackageStatusMap = createLoadedPackageStatusMapFromDependencies(
    npmDependencies,
    fetchNodeModulesResult.dependenciesWithError,
    fetchNodeModulesResult.dependenciesNotFound,
  )

  const codeResultCache = generateCodeResultCache(
    defaultPersistentModel.projectContents,
    {},
    SampleFileBuildResult,
    SampleFileBundledExportsInfo,
    nodeModules,
    dispatch,
    {},
    'full-build',
    false,
  )

  renderEditorRoot()
  dispatch(
    [
      {
        action: 'NEW',
        nodeModules: nodeModules,
        persistentModel: defaultPersistentModel,
        codeResultCache: codeResultCache,
        packageResult: packageResult,
      },
    ],
    'everyone',
  )
}

export async function loadSampleProject(
  projectID: string,
  dispatch: EditorDispatch,
  workers: UtopiaTsWorkers,
  renderEditorRoot: () => void,
): Promise<void> {
  const sampleProject = sampleProjectForId(projectID)
  if (sampleProject == null) {
    return newProject(dispatch, renderEditorRoot)
  } else {
    const { name, model } = sampleProject
    return load(dispatch, model, name, projectID, workers, renderEditorRoot)
  }
}

export async function load(
  dispatch: EditorDispatch,
  model: PersistentModel,
  title: string,
  projectId: string,
  workers: UtopiaTsWorkers,
  renderEditorRoot: () => void,
  retryFetchNodeModules: boolean = true,
): Promise<void> {
  // this action is now async!
  const migratedModel = applyMigrations(model)
  const npmDependencies = dependenciesWithEditorRequirements(migratedModel.projectContents)
  const fetchNodeModulesResult = await fetchNodeModules(
    npmDependencies,
    'canvas',
    retryFetchNodeModules,
  )

  const nodeModules: NodeModules = fetchNodeModulesResult.nodeModules
  const packageResult: PackageStatusMap = createLoadedPackageStatusMapFromDependencies(
    npmDependencies,
    fetchNodeModulesResult.dependenciesWithError,
    fetchNodeModulesResult.dependenciesNotFound,
  )

  const typeDefinitions = getDependencyTypeDefinitions(nodeModules)

  let codeResultCache: CodeResultCache
  if (migratedModel.exportsInfo.length > 0) {
    workers.sendInitMessage(typeDefinitions, migratedModel.projectContents)
    codeResultCache = generateCodeResultCache(
      migratedModel.projectContents,
      {},
      {},
      migratedModel.exportsInfo,
      nodeModules,
      dispatch,
      {},
      'full-build',
      false,
    )
  } else {
    codeResultCache = await loadCodeResult(
      workers,
      nodeModules,
      dispatch,
      typeDefinitions,
      migratedModel.projectContents,
    )
  }

  const storedState = await loadStoredState(projectId)

  const safeMode = await localforage.getItem<boolean>(getProjectLockedKey(projectId))

  renderEditorRoot()

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

function loadCodeResult(
  workers: UtopiaTsWorkers,
  nodeModules: NodeModules,
  dispatch: EditorDispatch,
  typeDefinitions: TypeDefinitions,
  projectContents: ProjectContentTreeRoot,
): Promise<CodeResultCache> {
  return new Promise((resolve, reject) => {
    const handleMessage = (e: MessageEvent) => {
      const data = e.data as OutgoingWorkerMessage
      switch (data.type) {
        case 'build': {
          const codeResultCache = generateCodeResultCache(
            projectContents,
            {},
            data.buildResult,
            data.exportsInfo,
            nodeModules,
            dispatch,
            {},
            'full-build',
            false,
          )
          resolve(codeResultCache)
          workers.removeBundleResultEventListener(handleMessage)
          break
        }
      }
    }

    workers.addBundleResultEventListener(handleMessage)
    workers.sendInitMessage(typeDefinitions, projectContents)
  })
}

export function isSendPreviewModel(action: any): action is SendPreviewModel {
  return action != null && (action as SendPreviewModel).action === 'SEND_PREVIEW_MODEL'
}

export function isPropertyControlsIFrameReady(
  action: unknown,
): action is PropertyControlsIFrameReady {
  const parseResult = objectKeyParser(parseString, 'action')(action)
  return foldEither(
    (_) => false,
    (prop) => prop === 'PROPERTY_CONTROLS_IFRAME_READY',
    parseResult,
  )
}

export function isUpdatePropertyControlsInfo(
  action: unknown,
): action is UpdatePropertyControlsInfo {
  const parseResult = objectKeyParser(parseString, 'action')(action)
  return foldEither(
    (_) => false,
    (prop) => prop === 'UPDATE_PROPERTY_CONTROLS_INFO',
    parseResult,
  )
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
