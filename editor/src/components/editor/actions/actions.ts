import { produce } from 'immer'
import * as update from 'immutability-helper'
import * as R from 'ramda'
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
import { colorTheme } from 'uuiui'
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
  convertMetadataMap,
} from '../../../core/model/element-metadata-utils'
import {
  ComponentMetadata,
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
  isUtopiaJSXComponent,
  utopiaJSXComponent,
  JSXMetadata,
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
} from '../../../core/shared/either'
import type {
  RequireFn,
  TypeDefinitions,
  PackageStatus,
  PackageStatusMap,
  RequestedNpmDependency,
} from '../../../core/shared/npm-dependency-types'
import {
  InstancePath,
  isParseSuccess,
  isTextFile,
  LayoutWrapper,
  ParsedTextFile,
  ParseSuccess,
  ProjectContents,
  ProjectFile,
  PropertyPath,
  RevisionsState,
  ScenePath,
  StaticElementPath,
  TemplatePath,
  TextFile,
  importAlias,
  isAssetFile,
  ESCodeFile,
  esCodeFile,
  NodeModules,
  Imports,
  importDetails,
  StaticInstancePath,
  textFileContents,
  textFile,
  codeFile,
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
} from '../../../core/shared/math-utils'
import {
  addFileToProjectContents,
  contentsToTree,
  ensureDirectoriesExist,
  getContentsTreeFileFromString,
  ProjectContentTreeRoot,
  removeFromProjectContents,
  treeToContents,
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
  filterMultiSelectScenes,
  getFrameChange,
  moveTemplate,
  produceCanvasTransientState,
  SkipFrameChange,
  updateFramesOfScenesAndComponents,
  cullSpyCollector,
} from '../../canvas/canvas-utils'
import { CodeEditorTheme } from '../../code-editor/code-editor-themes'
import { EditorPane, EditorPanel, ResizeLeftPane, SetFocus } from '../../common/actions'
import { openMenu } from '../../context-menu-wrapper'
import {
  CodeResultCache,
  generateCodeResultCache,
  codeCacheToBuildResult,
  PropertyControlsInfo,
} from '../../custom-code/code-file'
import { ElementContextMenuInstance } from '../../element-context-menu'
import { getFilePathToImport } from '../../filebrowser/filepath-utils'
import { FontSettings } from '../../inspector/common/css-utils'
import { CSSTarget } from '../../inspector/sections/header-section/target-selector'
import {
  LeftMenuTab,
  LeftPaneDefaultWidth,
  LeftPaneMinimumWidth,
  setLeftMenuTabFromFocusedPanel,
  updateLeftMenuExpanded,
  updateSelectedLeftMenuTab,
} from '../../navigator/left-pane'
import * as PP from '../../../core/shared/property-path'
import * as TP from '../../../core/shared/template-path'
import {
  AddTextFile,
  AddFolder,
  Alignment,
  AlignSelectedViews,
  Atomic,
  ClearHighlightedViews,
  ClearImageFileBlob,
  ClearParseOrPrintInFlight,
  CloseEditorTab,
  ClosePopup,
  CloseTextEditor,
  CopySelectionToClipboard,
  DeleteFile,
  DeleteSelected,
  DeleteView,
  DeleteViews,
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
  OpenEditorTab,
  OpenPopup,
  OpenTextEditor,
  PasteJSXElements,
  Redo,
  RedrawOldCanvasControls,
  RegenerateThumbnail,
  RenameComponent,
  RenameStyleSelector,
  ReorderEditorTabs,
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
  SetCodeEditorTheme,
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
  SetProp,
  SetPropWithElementPath,
  SetSceneProp,
  SetStoredFontSettings,
  PushToast,
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
  UnsetSceneProp,
  UnwrapGroupOrView,
  UnwrapLayoutable,
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
  WrapInLayoutable,
  WrapInView,
  SetSafeMode,
  SaveImageDetails,
  SetSaveError,
  PopToast,
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
  applyParseAndEditorChanges,
  applyUtopiaJSXComponentsChanges,
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
  getOpenEditorTab,
  getOpenFilename,
  getOpenImportsFromState,
  getOpenTextFileKey,
  getOpenUIJSFile,
  getOpenUIJSFileKey,
  getOpenUtopiaJSXComponentsFromState,
  insertElementAtPath,
  mergeStoredEditorStateIntoEditorState,
  ModalDialog,
  modifyOpenJsxElementAtPath,
  modifyOpenJSXElements,
  modifyOpenJSXElementsAndMetadata,
  modifyOpenParseSuccess,
  modifyOpenSceneAtPath,
  modifyOpenScenesAndJSXElements,
  modifyParseSuccessWithSimple,
  OriginalFrame,
  ParseSuccessAndEditorChanges,
  PersistentModel,
  removeElementAtPath,
  SimpleParseSuccess,
  UIFileBase64Blobs,
  updateMainUIInEditorState,
  addNewScene,
  removeScene,
  getNumberOfScenes,
  getStoryboardTemplatePath,
  addSceneToJSXComponents,
  UserState,
  UserConfiguration,
} from '../store/editor-state'
import { loadStoredState } from '../stored-state'
import { applyMigrations } from './migrations/migrations'
import { fastForEach, getProjectLockedKey } from '../../../core/shared/utils'
import {
  PathForSceneDataLabel,
  createSceneTemplatePath,
  PathForSceneComponent,
  PathForSceneProps,
  fishOutUtopiaCanvasFromTopLevelElements,
  createSceneFromComponent,
  BakedInStoryboardVariableName,
} from '../../../core/model/scene-utils'
import { addUtopiaUtilsImportIfUsed } from '../import-utils'
import { getFrameAndMultiplier } from '../../images'
import { arrayToMaybe, forceNotNull } from '../../../core/shared/optional-utils'

import {
  updateRightMenuExpanded,
  updateSelectedRightMenuTab,
  RightMenuTab,
} from '../../canvas/right-menu'

import { Notice } from '../../common/notices'
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
import {
  addStoryboardFileToProject,
  StoryboardFilePath,
} from '../../../core/model/storyboard-utils'
import { keepDeepReferenceEqualityIfPossible } from '../../../utils/react-performance'
import { arrayDeepEquality } from '../../../utils/deep-equality'
import {
  ElementInstanceMetadataKeepDeepEquality,
  JSXMetadataKeepDeepEquality,
} from '../store/store-deep-equality-instances'
import {
  showToast,
  popToast,
  setPropWithElementPath_UNSAFE,
  clearImageFileBlob,
  updateFile,
  enableInsertModeForJSXElement,
  insertJSXElement,
  updateThumbnailGenerated,
  openEditorTab,
  setPackageStatus,
  updateNodeModulesContents,
  finishCheckpointTimer,
} from './action-creators'
import { EditorTab, isOpenFileTab, openFileTab } from '../store/editor-tabs'

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
  target: InstancePath,
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
  target: StaticElementPath,
  updateFn: (props: JSXAttributes) => Either<any, JSXAttributes>,
): EditorModel {
  return modifyOpenJSXElements((components) => {
    return transformJSXComponentAtElementPath(components, target, (e: JSXElement) =>
      applyUpdateToJSXElement(e, updateFn),
    )
  }, editor)
}

function setSpecialSizeMeasurementParentLayoutSystemOnAllChildren(
  scenes: JSXMetadata,
  parentPath: InstancePath,
  value: DetectedLayoutSystem,
): JSXMetadata {
  const allChildren = MetadataUtils.getImmediateChildren(scenes, parentPath)
  return allChildren.reduce((transformedScenes, child) => {
    return switchLayoutMetadata(transformedScenes, child.templatePath, value, undefined, undefined)
  }, scenes)
}

function switchAndUpdateFrames(
  editor: EditorModel,
  target: InstancePath,
  layoutSystem: SettableLayoutSystem,
): EditorModel {
  const targetMetadata = Utils.forceNotNull(
    `Could not find metadata for ${JSON.stringify(target)}`,
    MetadataUtils.getElementByInstancePathMaybe(editor.jsxMetadataKILLME.elements, target),
  )
  if (targetMetadata.globalFrame == null) {
    // The target is a non-layoutable
    return editor
  }

  const layoutSystemPath = createLayoutPropertyPath('LayoutSystem')
  const styleDisplayPath = createLayoutPropertyPath('display')

  const originalComponents = getOpenUtopiaJSXComponentsFromState(editor)

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
          return setJSXValueAtPath(attributes, styleDisplayPath, jsxAttributeValue('flex'))
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
            jsxAttributeValue('absolute'),
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
        jsxMetadataKILLME: MetadataUtils.unsetPropertyDirectlyIntoMetadata(
          withUpdatedLayoutSystem.jsxMetadataKILLME,
          target,
          layoutSystemPath,
        ),
      }
      withUpdatedLayoutSystem = {
        ...withUpdatedLayoutSystem,
        jsxMetadataKILLME: MetadataUtils.setPropertyDirectlyIntoMetadata(
          withUpdatedLayoutSystem.jsxMetadataKILLME,
          target,
          styleDisplayPath, // TODO LAYOUT investigate if we should use also update the DOM walker specialSizeMeasurements
          'flex',
        ),
      }
      withUpdatedLayoutSystem = {
        ...withUpdatedLayoutSystem,
        jsxMetadataKILLME: MetadataUtils.setPropertyDirectlyIntoMetadata(
          withUpdatedLayoutSystem.jsxMetadataKILLME,
          target,
          createLayoutPropertyPath('position'), // TODO LAYOUT investigate if we should use also update the DOM walker specialSizeMeasurements
          'relative',
        ),
      }
      break
    case 'flow':
      withUpdatedLayoutSystem = {
        ...withUpdatedLayoutSystem,
        jsxMetadataKILLME: MetadataUtils.unsetPropertyDirectlyIntoMetadata(
          withUpdatedLayoutSystem.jsxMetadataKILLME,
          target,
          layoutSystemPath,
        ),
      }
      break
    case LayoutSystem.PinSystem:
      withUpdatedLayoutSystem = {
        ...withUpdatedLayoutSystem,
        jsxMetadataKILLME: MetadataUtils.unsetPropertyDirectlyIntoMetadata(
          withUpdatedLayoutSystem.jsxMetadataKILLME,
          target,
          layoutSystemPath,
        ),
      }
      withUpdatedLayoutSystem = {
        ...withUpdatedLayoutSystem,
        jsxMetadataKILLME: MetadataUtils.setPropertyDirectlyIntoMetadata(
          withUpdatedLayoutSystem.jsxMetadataKILLME,
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
        jsxMetadataKILLME: MetadataUtils.unsetPropertyDirectlyIntoMetadata(
          withUpdatedLayoutSystem.jsxMetadataKILLME,
          target,
          styleDisplayPath,
        ),
      }
      withUpdatedLayoutSystem = {
        ...withUpdatedLayoutSystem,
        jsxMetadataKILLME: MetadataUtils.setPropertyDirectlyIntoMetadata(
          withUpdatedLayoutSystem.jsxMetadataKILLME,
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
    jsxMetadataKILLME: setSpecialSizeMeasurementParentLayoutSystemOnAllChildren(
      withUpdatedLayoutSystem.jsxMetadataKILLME,
      target,
      layoutSystemToSet(),
    ),
  }
  withUpdatedLayoutSystem = {
    ...withUpdatedLayoutSystem,
    jsxMetadataKILLME: switchLayoutMetadata(
      withUpdatedLayoutSystem.jsxMetadataKILLME,
      target,
      undefined,
      layoutSystemToSet(),
      undefined,
    ),
  }

  let withChildrenUpdated = modifyOpenJSXElementsAndMetadata((components, metadata) => {
    return maybeSwitchChildrenLayoutProps(
      target,
      editor.jsxMetadataKILLME,
      metadata,
      originalComponents,
      components,
    )
  }, withUpdatedLayoutSystem)

  let framesAndTargets: Array<PinOrFlexFrameChange> = []
  if (layoutSystem !== 'flow') {
    const isParentFlex = MetadataUtils.isParentYogaLayoutedContainerAndElementParticipatesInLayout(
      target,
      withChildrenUpdated.jsxMetadataKILLME,
    )
    framesAndTargets.push(getFrameChange(target, targetMetadata.globalFrame, isParentFlex))
  }

  Utils.fastForEach(targetMetadata.children, (childPath) => {
    const child = MetadataUtils.getElementByInstancePathMaybe(
      editor.jsxMetadataKILLME.elements,
      childPath,
    )
    if (child?.globalFrame != null) {
      // if the globalFrame is null, this child is a non-layoutable so just skip it
      const isParentOfChildFlex = MetadataUtils.isParentYogaLayoutedContainerAndElementParticipatesInLayout(
        child.templatePath,
        withChildrenUpdated.jsxMetadataKILLME,
      )
      framesAndTargets.push(
        getFrameChange(child.templatePath, child.globalFrame, isParentOfChildFlex),
      )
    }
  })
  return setCanvasFramesInnerNew(withChildrenUpdated, framesAndTargets, null)
}

export function editorMoveMultiSelectedTemplates(
  targets: TemplatePath[],
  indexPosition: IndexPosition,
  newParentPath: TemplatePath | null,
  parentFrame: CanvasRectangle | null,
  editor: EditorModel,
  newParentLayoutType: LayoutSystem | null,
): {
  editor: EditorModel
  newPaths: Array<TemplatePath>
} {
  let updatedTargets: Array<TemplatePath> = [...targets]
  let newPaths: Array<TemplatePath> = []
  const updatedEditor = targets.reduce((working, target, i) => {
    if (TP.isScenePath(target)) {
      // TODO Scene Implementation
      return working
    }
    const frame = MetadataUtils.getFrameInCanvasCoords(target, editor.jsxMetadataKILLME)

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
        const newChildPath = TP.replaceIfAncestor(path, templateToMove, newPath)
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
  target: TemplatePath,
  originalPath: TemplatePath,
  newFrame: CanvasRectangle | typeof SkipFrameChange | null,
  indexPosition: IndexPosition,
  newParentPath: TemplatePath | null,
  parentFrame: CanvasRectangle | null,
  editor: EditorModel,
  newParentLayoutSystem: LayoutSystem | null,
): {
  editor: EditorModel
  newPath: TemplatePath | null
} {
  function noChanges(): { editor: EditorModel; newPath: TemplatePath | null } {
    return {
      editor: editor,
      newPath: null,
    }
  }

  function getChanges(
    editorForChanges: EditorState,
    successForChanges: ParseSuccess,
  ): ParseSuccessAndEditorChanges<TemplatePath | null> {
    const componentsIncludingScenes = getUtopiaJSXComponentsFromSuccess(successForChanges)
    const moveResult = moveTemplate(
      target,
      originalPath,
      newFrame,
      indexPosition,
      newParentPath,
      parentFrame,
      componentsIncludingScenes,
      editorForChanges.jsxMetadataKILLME,
      editorForChanges.selectedViews,
      editorForChanges.highlightedViews,
      newParentLayoutSystem,
    )
    return {
      parseSuccessTransform: (success: ParseSuccess) => {
        // Sync these back up.
        const topLevelElements = applyUtopiaJSXComponentsChanges(
          success.topLevelElements,
          moveResult.utopiaComponentsIncludingScenes,
        )

        return {
          ...success,
          topLevelElements: topLevelElements,
        }
      },
      editorStateTransform: (editorState: EditorState) => {
        return {
          ...editorState,
          selectedViews: moveResult.selectedViews,
          highlightedViews: moveResult.highlightedViews,
        }
      },
      additionalData: moveResult.newPath,
    }
  }

  const openUIFile = getOpenUIJSFile(editor)
  if (openUIFile == null) {
    return noChanges()
  } else {
    const editorAndAdditionalData = applyParseAndEditorChanges(getChanges, editor)

    if (editorAndAdditionalData.additionalData == null) {
      return noChanges()
    } else {
      return {
        newPath: editorAndAdditionalData.additionalData,
        editor: editorAndAdditionalData.editor,
      }
    }
  }
}

function restoreEditorState(currentEditor: EditorModel, history: StateHistory): EditorModel {
  // FIXME Ask Team Components to check over these
  const poppedEditor = history.current.editor
  return {
    id: currentEditor.id,
    appID: currentEditor.appID,
    projectName: currentEditor.projectName,
    projectVersion: currentEditor.projectVersion,
    isLoaded: currentEditor.isLoaded,
    spyMetadataKILLME: poppedEditor.spyMetadataKILLME,
    domMetadataKILLME: [],
    jsxMetadataKILLME: poppedEditor.jsxMetadataKILLME,
    projectContents: poppedEditor.projectContents,
    nodeModules: currentEditor.nodeModules,
    openFiles: poppedEditor.openFiles,
    selectedFile: poppedEditor.selectedFile,
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
      mountCount: currentEditor.canvas.mountCount + 1,
    },
    inspector: {
      visible: currentEditor.inspector.visible,
    },
    fileBrowser: {
      minimised: currentEditor.fileBrowser.minimised,
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
      visible: currentEditor.navigator.visible,
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
    codeEditorTheme: poppedEditor.codeEditorTheme,
    safeMode: currentEditor.safeMode,
    saveError: currentEditor.saveError,
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
      transientState: produceCanvasTransientState(history.current.editor, true),
    },
    elementWarnings: poppedDerived.elementWarnings,
  }
}

function deleteElements(targets: TemplatePath[], editor: EditorModel): EditorModel {
  const openUIJSFile = getOpenUIJSFile(editor)
  if (openUIJSFile == null) {
    console.error(`Attempted to delete element(s) with no UI file open.`)
    return editor
  } else {
    const metadata = editor.jsxMetadataKILLME

    const isElementToBeDeleted = (element: ElementInstanceMetadata) => {
      return targets.some((target) => TP.pathsEqual(element.templatePath, target))
    }

    const isEmptyOrContainsDeleted = (element: ElementInstanceMetadata): boolean => {
      if (!MetadataUtils.isAutoSizingViewFromComponents(metadata, element.templatePath)) {
        return false
      }

      return element.children.every((childPath) => {
        const child = MetadataUtils.getElementByInstancePathMaybe(metadata.elements, childPath)
        child == null || isElementToBeDeleted(child) || isEmptyOrContainsDeleted(child)
      })
    }
    const emptyGroups = MetadataUtils.findElements(
      metadata.elements,
      (element: ElementInstanceMetadata) => isEmptyOrContainsDeleted(element),
    )
    const emptyGroupTemplatePaths = emptyGroups.map((group) => group.templatePath)

    const extendedTargets = [...targets, ...emptyGroupTemplatePaths]

    const updatedEditor = extendedTargets.reduce((working, target) => {
      if (TP.isScenePath(target)) {
        return removeScene(working, target)
      } else {
        return modifyOpenParseSuccess((parseSuccess) => {
          const utopiaComponents = getUtopiaJSXComponentsFromSuccess(parseSuccess)
          const element = findElementAtPath(target, utopiaComponents)
          if (element == null) {
            return parseSuccess
          } else {
            const withTargetRemoved: Array<UtopiaJSXComponent> = removeElementAtPath(
              target,
              utopiaComponents,
            )
            return modifyParseSuccessWithSimple((success: SimpleParseSuccess) => {
              return {
                ...success,
                utopiaComponents: withTargetRemoved,
              }
            }, parseSuccess)
          }
        }, working)
      }
    }, editor)
    return {
      ...updatedEditor,
      selectedViews: TP.filterPaths(updatedEditor.selectedViews, extendedTargets),
    }
  }
}

function duplicateMany(paths: TemplatePath[], editor: EditorModel): EditorModel {
  const targetParent = TP.getCommonParent(paths)
  const duplicateResult = duplicate(paths, targetParent, editor)
  if (duplicateResult == null) {
    return editor
  } else {
    return modifyOpenJSXElements((_) => duplicateResult.utopiaComponents, {
      ...editor,
      jsxMetadataKILLME: duplicateResult.metadata,
      selectedViews: duplicateResult.selectedViews,
    })
  }
}

function indexPositionForAdjustment(
  target: StaticInstancePath | InstancePath,
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
      const openUIJSFile = getOpenUIJSFile(editor)
      if (openUIJSFile != null && isParseSuccess(openUIJSFile.fileContents.parsed)) {
        const current = getZIndexOfElement(
          openUIJSFile.fileContents.parsed.topLevelElements,
          TP.asStatic(target),
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
      if (TP.isScenePath(selectedView)) {
        // TODO Scene Implementation
        return working
      }

      const indexPosition = indexPositionForAdjustment(selectedView, working, index)
      return editorMoveTemplate(
        selectedView,
        selectedView,
        SkipFrameChange,
        indexPosition,
        TP.parentPath(selectedView),
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
  selectedViews: Array<TemplatePath>,
  navigator: EditorModel['navigator'],
): EditorModel['navigator'] {
  const allCollapsedViews = navigator.collapsedViews
  let collapsedWithChildrenSelected: TemplatePath[] = []
  let collapsedNoChildrenSelected: TemplatePath[] = []
  selectedViews.forEach((selectedView) => {
    allCollapsedViews.forEach((collapsedView) => {
      if (
        TP.isAncestorOf(selectedView, collapsedView) &&
        !TP.pathsEqual(selectedView, collapsedView)
      ) {
        if (!TP.containsPath(collapsedView, collapsedWithChildrenSelected)) {
          collapsedWithChildrenSelected.push(collapsedView)
        }
      } else {
        if (!TP.containsPath(collapsedView, collapsedNoChildrenSelected)) {
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
        (path) => !TP.containsPath(path, collapsedWithChildrenSelected),
      ),
    },
    temporaryExpandedViews: {
      $set: collapsedWithChildrenSelected,
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
  Utils.fastForEach(Object.keys(projectContents), (key) => {
    if (oldFolderRegex.test(key)) {
      const projectFile = projectContents[key]
      const newFilePath = key.replace(oldPath, newPath)
      const fileType = isDirectory(projectFile) ? 'DIRECTORY' : fileTypeFromFileName(newFilePath)
      if (fileType == null) {
        // Can't identify the file type.
        error = `Can't rename ${key} to ${newFilePath}.`
      } else {
        const updatedProjectFile = switchToFileType(projectFile, fileType)
        if (updatedProjectFile == null) {
          // Appears this file can't validly be changed.
          error = `Can't rename ${key} to ${newFilePath}.`
        } else {
          // Remove the old file.
          delete updatedProjectContents[key]
          updatedProjectContents[newFilePath] = updatedProjectFile
          updatedFiles.push({ oldPath: key, newPath: newFilePath })
        }
      }
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
  targets: Array<TemplatePath>,
  derived: DerivedState,
): Array<TemplatePath> {
  let targetsAndZIndex: Array<{ target: TemplatePath; index: number }> = []
  Utils.fastForEach(targets, (target) => {
    const index = derived.navigatorTargets.findIndex((tp) => TP.pathsEqual(tp, target))
    targetsAndZIndex.push({ target: target, index: index })
  })
  targetsAndZIndex.sort((a, b) => a.index - b.index)
  const orderedTargets = Utils.pluck(targetsAndZIndex, 'target')

  // keep direct children from reparenting
  let filteredTargets: Array<TemplatePath> = []
  Utils.fastForEach(orderedTargets, (target) => {
    if (!orderedTargets.find((tp) => TP.pathsEqual(TP.parentPath(target), tp))) {
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
  targets: TemplatePath[],
  editor: EditorState,
  allowActionRegardless: boolean,
  actionOtherwise: (e: EditorState) => EditorState,
  dispatch: EditorDispatch,
): EditorState {
  const generatedElementsTargeted = areGeneratedElementsTargeted(targets, editor)
  let result: EditorState = editor
  if (generatedElementsTargeted) {
    const showToastAction = showToast({ message: message })
    result = UPDATE_FNS.PUSH_TOAST(showToastAction, result, dispatch)
  }

  if (!generatedElementsTargeted || allowActionRegardless) {
    result = actionOtherwise(result)
  }

  return result
}

let checkpointTimeoutId: number | undefined = undefined

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
        const parseResult = lintAndParse(filename, file.fileContents.code)
        return textFile(
          textFileContents(file.fileContents.code, parseResult, RevisionsState.BothMatch),
          null,
          Date.now(),
        )
      },
    )
    const parsedModel = {
      ...migratedModel,
      projectContents: parsedProjectFiles,
    }
    const newModel: EditorModel = {
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
    const newModelMergedWithStoredState: EditorModel = mergeStoredEditorStateIntoEditorState(
      action.storedState,
      newModel,
    )
    return loadModel(newModelMergedWithStoredState, oldEditor)
  },
  SET_HIGHLIGHTED_VIEW: (action: SetHighlightedView, editor: EditorModel): EditorModel => {
    if (
      editor.highlightedViews.length > 0 &&
      TP.containsPath(action.target, editor.highlightedViews)
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
            const toastAction = showToast({ message: failureMessage, level: 'ERROR' })
            return UPDATE_FNS.PUSH_TOAST(toastAction, editor, dispatch)
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
        index = MetadataUtils.getViewZIndexFromMetadata(editor.jsxMetadataKILLME, targetPath)
      } else {
        console.warn(
          'Attempted to find the index of a view when the code currently does not parse.',
        )
        return editor
      }
    }
    let indexPosition: IndexPosition
    let newParentPath: TemplatePath | null
    switch (dropTarget.type) {
      case 'MOVE_ROW_BEFORE':
        indexPosition = {
          type: 'before',
          index: index,
        }
        newParentPath = TP.parentPath(targetPath)
        break
      case 'MOVE_ROW_AFTER':
        indexPosition = {
          type: 'after',
          index: index,
        }
        newParentPath = TP.parentPath(targetPath)
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
        : MetadataUtils.getFrameInCanvasCoords(newParentPath, editor.jsxMetadataKILLME)
    const { editor: withMovedTemplate, newPaths } = editorMoveMultiSelectedTemplates(
      R.reverse(getZIndexOrderedViewsWithoutDirectChildren(dragSources, derived)),
      indexPosition,
      newParentPath,
      newParentSize,
      editor,
      null,
    )

    return {
      ...withMovedTemplate,
      selectedViews: filterMultiSelectScenes(newPaths),
      highlightedViews: [],
    }
  },
  SET_Z_INDEX: (action: SetZIndex, editor: EditorModel, derived: DerivedState): EditorModel => {
    return editorMoveTemplate(
      action.target,
      action.target,
      SkipFrameChange,
      action.indexPosition,
      TP.parentPath(action.target),
      null,
      editor,
      null,
    ).editor
  },
  DELETE_SELECTED: (
    action: DeleteSelected,
    editorForAction: EditorModel,
    dispatch: EditorDispatch,
  ): EditorModel => {
    return toastOnGeneratedElementsSelected(
      'Generated elements can only be deleted in code. ',
      editorForAction,
      true,
      (editor) => {
        const components = getOpenUtopiaJSXComponentsFromState(editor)
        const staticSelectedElements = MetadataUtils.staticElementsOnly(
          components,
          editor.selectedViews,
        )
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
        const newSelection = TP.parentPath(action.target)
        return {
          ...updatedEditor,
          selectedViews:
            newSelection != null && !TP.isScenePath(newSelection) ? [newSelection] : [],
        }
      },
      dispatch,
    )
  },
  DELETE_VIEWS: (
    action: DeleteViews,
    editor: EditorModel,
    dispatch: EditorDispatch,
  ): EditorModel => {
    return toastOnGeneratedElementsTargeted(
      'Generated elements can only be deleted in code.',
      action.targets,
      editor,
      true,
      (editorState) => {
        const components = getOpenUtopiaJSXComponentsFromState(editorState)
        const staticSelectedElements = MetadataUtils.staticElementsOnly(components, action.targets)
        return deleteElements(staticSelectedElements, editorState)
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
    let newlySelectedPaths: Array<TemplatePath>
    if (action.addToSelection) {
      newlySelectedPaths = action.target.reduce((working, path) => {
        return TP.addPathIfMissing(path, working)
      }, editor.selectedViews)
    } else {
      newlySelectedPaths = action.target
    }
    const newHighlightedViews = editor.highlightedViews.filter(
      (path) => !TP.containsPath(path, newlySelectedPaths),
    )

    const filteredNewlySelectedPaths = filterMultiSelectScenes(newlySelectedPaths)
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
      const showToastAction = showToast({
        message: `Only one scene can be selected`,
        level: 'WARNING',
      })
      return UPDATE_FNS.PUSH_TOAST(showToastAction, updatedEditor, dispatch)
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
    const uniqueParents = R.uniqBy(
      TP.toComponentId,
      Utils.stripNulls(selectedElements.map(TP.parentPath)),
    )
    const additionalTargets = Utils.flatMapArray((uniqueParent) => {
      const children = MetadataUtils.getImmediateChildren(editor.jsxMetadataKILLME, uniqueParent)
      return children
        .map((child) => child.templatePath)
        .filter((childPath) => {
          return !TP.containsPath(childPath, selectedElements)
        })
    }, uniqueParents)

    return {
      ...editor,
      selectedViews: filterMultiSelectScenes([...editor.selectedViews, ...additionalTargets]),
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
  PUSH_TOAST: (action: PushToast, editor: EditorModel, dispatch: EditorDispatch): EditorModel => {
    setTimeout(() => dispatch([popToast()], 'everyone'), 5500)

    return {
      ...editor,
      toasts: [...editor.toasts, action.toast],
    }
  },
  POP_TOAST: (action: PopToast, editor: EditorModel): EditorModel => {
    return {
      ...editor,
      toasts: editor.toasts.slice(1),
    }
  },
  TOGGLE_HIDDEN: (action: ToggleHidden, editor: EditorModel): EditorModel => {
    const targets = action.targets.length > 0 ? action.targets : editor.selectedViews
    return targets.reduce((working, target) => {
      if (working.hiddenInstances.some((element) => TP.pathsEqual(element, target))) {
        return update(working, {
          hiddenInstances: {
            $set: working.hiddenInstances.filter((element) => !TP.pathsEqual(element, target)),
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
    const { target, name } = action
    let propsTransform: (props: JSXAttributes) => Either<string, JSXAttributes>
    if (name == null) {
      propsTransform = (props) => unsetJSXValueAtPath(props, PathForSceneDataLabel)
    } else {
      propsTransform = (props) =>
        setJSXValueAtPath(props, PathForSceneDataLabel, jsxAttributeValue(name))
    }
    if (TP.isScenePath(target)) {
      return modifyOpenSceneAtPath(
        target,
        (scene): JSXElement => {
          const updatedSceneProps = propsTransform(scene.props)
          return foldEither(
            () => scene,
            (sceneProps) => {
              return {
                ...scene,
                props: sceneProps,
              }
            },
            updatedSceneProps,
          )
        },
        editor,
      )
    } else {
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
    }
  },
  INSERT_SCENE: (action: InsertScene, editor: EditorModel): EditorModel => {
    const numberOfScenes = getNumberOfScenes(editor)
    const components = getOpenUtopiaJSXComponentsFromState(editor)
    const sceneUID = generateUidWithExistingComponents(components)
    const newSceneLabel = `Scene ${numberOfScenes}`
    const newScene: JSXElement = defaultSceneElement(
      sceneUID,
      null,
      canvasFrameToNormalisedFrame(action.frame),
      newSceneLabel,
    )
    const storyBoardPath = getStoryboardTemplatePath(components)
    const newSelection =
      storyBoardPath != null ? [TP.scenePath([TP.toUid(storyBoardPath), sceneUID])] : []
    return {
      ...addNewScene(editor, newScene),
      selectedViews: newSelection,
    }
  },
  SET_SCENE_PROP: (action: SetSceneProp, editor: EditorModel): EditorModel => {
    return modifyOpenSceneAtPath(
      action.scenePath,
      (scene): JSXElement => {
        const updatedAttributes = setJSXValueAtPath(scene.props, action.propertyPath, action.value)
        if (isRight(updatedAttributes)) {
          return {
            ...scene,
            props: updatedAttributes.value,
          }
        } else {
          return scene
        }
      },
      editor,
    )
  },
  UNSET_SCENE_PROP: (action: UnsetSceneProp, editor: EditorModel): EditorModel => {
    return modifyOpenSceneAtPath(
      action.scenePath,
      (scene): JSXElement => {
        const updatedAttributes = unsetJSXValueAtPath(scene.props, action.propertyPath)
        if (isRight(updatedAttributes)) {
          return {
            ...scene,
            props: updatedAttributes.value,
          }
        } else {
          return scene
        }
      },
      editor,
    )
  },
  INSERT_JSX_ELEMENT: (action: InsertJSXElement, editor: EditorModel): EditorModel => {
    const editorWithAddedImport = modifyOpenParseSuccess((success) => {
      const updatedImports = mergeImports(success.imports, action.importsToAdd)
      return {
        ...success,
        imports: updatedImports,
      }
    }, editor)

    const newSelectedViews: TemplatePath[] = []

    const withNewElement = modifyOpenJSXElements((elements) => {
      const targetParent =
        action.parent == null
          ? // action.parent == null means Canvas, which means storyboard root element
            getStoryboardTemplatePath(elements)
          : action.parent

      if (targetParent == null || TP.isScenePath(targetParent)) {
        // TODO Scene Implementation
        return elements
      }

      const withInsertedElement = insertElementAtPath(
        targetParent,
        action.jsxElement,
        elements,
        null,
      )

      const uid = getUtopiaID(action.jsxElement)
      const newPath = TP.appendToPath(targetParent, uid)
      newSelectedViews.push(newPath)
      return withInsertedElement
    }, editorWithAddedImport)

    return {
      ...withNewElement,
      selectedViews: filterMultiSelectScenes(newSelectedViews),
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
        const uiFile = getOpenUIJSFile(editor)
        if (uiFile == null || !isParseSuccess(uiFile.fileContents.parsed)) {
          return editor
        }

        const utopiaComponents = getOpenUtopiaJSXComponentsFromState(editor)
        const newUID = generateUidWithExistingComponents(utopiaComponents)

        const orderedActionTargets = getZIndexOrderedViewsWithoutDirectChildren(
          action.targets,
          derived,
        )
        const parentPath = TP.getCommonParent(orderedActionTargets)
        if (parentPath === null) {
          return editor
        } else {
          const canvasFrames = action.targets.map((target) => {
            return MetadataUtils.getFrameInCanvasCoords(target, editor.jsxMetadataKILLME)
          })

          const boundingBox = Utils.boundingRectangleArray(canvasFrames)

          if (boundingBox == null) {
            // TODO Should this wrap in a zero sized rectangle so the user can then manually resize that?
            // we are trying to wrap something that is non-layoutable, just give up early
            return editor
          }

          let viewPath: TemplatePath | null = null
          const withWrapperViewAddedNoFrame = modifyOpenParseSuccess((parseSuccess) => {
            const elementToInsert: JSXElement = defaultTransparentViewElement(newUID)
            const utopiaJSXComponents = getUtopiaJSXComponentsFromSuccess(parseSuccess)
            const withTargetAdded: Array<UtopiaJSXComponent> = insertElementAtPath(
              parentPath,
              elementToInsert,
              utopiaJSXComponents,
              null,
            )

            viewPath = TP.appendToPath(parentPath, newUID)

            return modifyParseSuccessWithSimple((success: SimpleParseSuccess) => {
              return {
                ...success,
                utopiaComponents: withTargetAdded,
              }
            }, parseSuccess)
          }, editor)

          if (viewPath == null) {
            return editor
          }

          const parent = TP.isInstancePath(parentPath)
            ? MetadataUtils.getElementByInstancePathMaybe(
                editor.jsxMetadataKILLME.elements,
                parentPath,
              )
            : null
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
        if (TP.isScenePath(action.target)) {
          return editor
        }

        const element = MetadataUtils.getElementByInstancePathMaybe(
          editor.jsxMetadataKILLME.elements,
          action.target,
        )
        const children = MetadataUtils.getChildrenHandlingGroups(
          editor.jsxMetadataKILLME,
          action.target,
          true,
        )
        const imports = getOpenImportsFromState(editorForAction)
        if (children.length === 0 || !MetadataUtils.isViewAgainstImports(imports, element)) {
          return editor
        }

        const parentPath = TP.parentPath(action.target)
        const parentFrame =
          parentPath == null
            ? (Utils.zeroRectangle as CanvasRectangle)
            : MetadataUtils.getFrameInCanvasCoords(parentPath, editor.jsxMetadataKILLME)
        const indexPosition: IndexPosition = indexPositionForAdjustment(
          action.target,
          editor,
          'forward',
        )
        const withChildrenMoved = children.reduce((working, child) => {
          const childFrame = MetadataUtils.getFrameInCanvasCoords(
            child.templatePath,
            editor.jsxMetadataKILLME,
          )
          return editorMoveTemplate(
            child.templatePath,
            child.templatePath,
            childFrame,
            indexPosition,
            parentPath,
            parentFrame,
            working,
            null,
          ).editor
        }, editor)
        const withViewDeleted = deleteElements([action.target], withChildrenMoved)

        return withViewDeleted
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
      case 'uicodeeditor':
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
            visible: !editor.navigator.visible,
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

      case 'uicodeeditor':
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
    const imports = getOpenImportsFromState(editor)
    const targetParent = MetadataUtils.getTargetParentForPaste(
      imports,
      editor.selectedViews,
      editor.jsxMetadataKILLME,
      editor.pasteTargetsToIgnore,
    )

    let insertionAllowed: boolean = true
    if (targetParent != null) {
      const parentOriginType = MetadataUtils.getElementOriginType(
        getOpenUtopiaJSXComponentsFromState(editor),
        targetParent,
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
      const pasteToParseSuccess = (parseSuccess: ParseSuccess) => {
        const utopiaComponents = getUtopiaJSXComponentsFromSuccess(parseSuccess)
        const withTargetAdded = action.elements.reduce((accumulator, currentValue, index) => {
          const newUID = generateUidWithExistingComponents(accumulator)
          const elementToAdd = setUtopiaID(currentValue, newUID)
          const originalPath = action.originalTemplatePaths[index]
          if (TP.isScenePath(originalPath)) {
            const numberOfScenes = getNumberOfScenes(editor)
            const newSceneLabel = `Scene ${numberOfScenes}`
            const props = {
              ...currentValue.props,
              'data-label': jsxAttributeValue(newSceneLabel),
              'data-uid': jsxAttributeValue(newUID),
            }
            const newSceneElement = {
              ...currentValue,
              props: props,
            }
            return addSceneToJSXComponents(accumulator, newSceneElement)
          } else {
            const components = insertElementAtPath(targetParent, elementToAdd, accumulator, null)
            if (targetParent == null || TP.isScenePath(targetParent)) {
              return components
            } else {
              const newPath = TP.appendToPath(targetParent, newUID)
              return maybeSwitchLayoutProps(
                newPath,
                originalPath,
                targetParent,
                action.targetOriginalContextMetadata,
                editor.jsxMetadataKILLME,
                utopiaComponents,
                components,
                null,
                null,
              ).components
            }
          }
        }, utopiaComponents)
        return modifyParseSuccessWithSimple((success: SimpleParseSuccess) => {
          return {
            ...success,
            utopiaComponents: withTargetAdded,
          }
        }, parseSuccess)
      }

      return modifyOpenParseSuccess(pasteToParseSuccess, editor)
    } else {
      const showToastAction = showToast({
        message: `Unable to paste into a generated element.`,
        level: 'WARNING',
      })
      return UPDATE_FNS.PUSH_TOAST(showToastAction, editor, dispatch)
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
          templatePath: action.target,
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
    if (editor.navigator.collapsedViews.some((element) => TP.pathsEqual(element, action.target))) {
      return update(editor, {
        navigator: {
          collapsedViews: {
            $set: editor.navigator.collapsedViews.filter(
              (element) => !TP.pathsEqual(element, action.target),
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
      ? UPDATE_FNS.PUSH_TOAST(
          showToast({ message: 'Code editor hidden. Use the menu or resize to get it back.' }),
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
      : UPDATE_FNS.PUSH_TOAST(
          showToast({ message: 'Code editor hidden. Use the menu or resize to get it back.' }),
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
    const frame = MetadataUtils.getFrame(target, editor.jsxMetadataKILLME)

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
      const toastAction = showToast({ message: errorMessage!, level: 'WARNING' })
      return UPDATE_FNS.PUSH_TOAST(toastAction, updatedEditor, dispatch)
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
    const initialFrame = MetadataUtils.getFrame(action.element, editor.jsxMetadataKILLME)

    if (initialFrame == null) {
      return editor
    }

    let frame = {
      x: initialFrame.x,
      y: initialFrame.y,
      width: action.width,
      height: action.height,
    } as LocalRectangle

    if (TP.isInstancePath(action.element)) {
      const element = MetadataUtils.getElementByInstancePathMaybe(
        editor.jsxMetadataKILLME.elements,
        action.element,
      )
      const imports = getOpenImportsFromState(editor)
      if (
        element != null &&
        MetadataUtils.isTextAgainstImports(imports, element) &&
        element.props.textSizing == 'auto'
      ) {
        const alignment = element.props.style.textAlign
        if (alignment === 'center') {
          frame = Utils.setRectCenterX(frame, initialFrame.x + initialFrame.width / 2)
        } else if (alignment === 'right') {
          frame = Utils.setRectRightX(frame, initialFrame.x + initialFrame.width)
        }
      }
    }

    const parentPath = TP.parentPath(action.element)
    let offset = { x: 0, y: 0 } as CanvasPoint
    if (parentPath != null) {
      const parentFrame = MetadataUtils.getFrameInCanvasCoords(parentPath, editor.jsxMetadataKILLME)
      if (parentFrame != null) {
        offset = { x: parentFrame.x, y: parentFrame.y } as CanvasPoint
      }
    }
    const canvasFrame = Utils.getCanvasRectangleWithCanvasOffset(offset, frame)

    const components = getOpenUtopiaJSXComponentsFromState(editor)
    const isParentFlex = MetadataUtils.isParentYogaLayoutedContainerAndElementParticipatesInLayout(
      action.element,
      editor.jsxMetadataKILLME,
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
    const imageAttribute = jsxAttributeValue(imageURL)

    const utopiaComponents = getOpenUtopiaJSXComponentsFromState(editor)
    const newUID = generateUidWithExistingComponents(utopiaComponents)
    const openUIJSFile = getOpenUIJSFileKey(editor)

    let actionsToRunAfterSave: Array<EditorAction> = []
    // Bit weird, but when replacing an image, we need to change the URLs only once the image has been saved.
    if (action.imageDetails != null) {
      if (replaceImage) {
        const imageWithoutHashURL = imagePathURL(assetFilename)
        const components = getOpenUtopiaJSXComponentsFromState(editor)
        const propertyPath = PP.create(['src'])
        walkElements(components, (element, elementPath) => {
          if (isJSXElement(element)) {
            const srcAttribute = element.props['src']
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
    if (isLoggedIn(userState.loginState) && editor.id != null) {
      saveAssetToServer(notNullProjectID, action.fileType, action.base64, assetFilename)
        .then(() => {
          dispatch(
            [
              ...actionsToRunAfterSave,
              showToast({ message: `Succesfully uploaded ${assetFilename}`, level: 'INFO' }),
            ],
            'everyone',
          )
        })
        .catch(() => {
          dispatch([showToast({ message: `Failed to upload ${assetFilename}`, level: 'ERROR' })])
        })
    } else {
      dispatch([
        showToast({
          message: `Please log in to upload assets`,
          level: 'ERROR',
          persistent: true,
        }),
      ])
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
            {
              alt: jsxAttributeValue(''),
              src: imageAttribute,
              style: jsxAttributeValue({ width: width, height: height }),
              'data-uid': jsxAttributeValue(newUID),
              'data-aspect-ratio-locked': jsxAttributeValue(true),
            },
            [],
          )
          const size = width != null && height != null ? { width: width, height: height } : null
          const switchMode = enableInsertModeForJSXElement(imageElement, newUID, {}, size)
          const editorInsertEnabled = UPDATE_FNS.SWITCH_EDITOR_MODE(switchMode, editor, derived)
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
            editor.jsxMetadataKILLME,
            action.imageDetails.afterSave.frame,
          )

          const imageElement = jsxElement(
            jsxElementName('img', []),
            {
              alt: jsxAttributeValue(''),
              src: imageAttribute,
              style: jsxAttributeValue({
                left: relativeFrame.x,
                top: relativeFrame.y,
                width: relativeFrame.width,
                height: relativeFrame.height,
              }),
              'data-uid': jsxAttributeValue(newUID),
              'data-aspect-ratio-locked': jsxAttributeValue(true),
            },
            [],
          )

          const insertJSXElementAction = insertJSXElement(imageElement, parent, {})

          const withComponentCreated = UPDATE_FNS.INSERT_JSX_ELEMENT(insertJSXElementAction, {
            ...editor,
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
          const toastAction = showToast({
            message: 'Assets replaced. You may need to reload the editor to see changes.',
            level: 'WARNING',
            persistent: true,
          })
          return UPDATE_FNS.PUSH_TOAST(toastAction, editor, dispatch)
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
      const utopiaComponents = getOpenUtopiaJSXComponentsFromState(editor)
      const newUID = generateUidWithExistingComponents(utopiaComponents)
      const imageURL = imagePathURL(action.imagePath)
      const imageSrcAttribute = jsxAttributeValue(imageURL)
      const width = Utils.optionalMap((w) => w / 2, possiblyAnImage.width)
      const height = Utils.optionalMap((h) => h / 2, possiblyAnImage.height)
      const imageElement = jsxElement(
        jsxElementName('img', []),
        {
          alt: jsxAttributeValue(''),
          src: imageSrcAttribute,
          style: jsxAttributeValue({
            width: width,
            height: height,
          }),
          'data-uid': jsxAttributeValue(newUID),
          'data-label': jsxAttributeValue('Image'),
          'data-aspect-ratio-locked': jsxAttributeValue(true),
        },
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
        projectModules: action.codeResultCache.projectModules,
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
      const toastAction = showToast({
        message: replaceFilePathResults.errorMessage,
        level: 'ERROR',
        persistent: true,
      })
      return UPDATE_FNS.PUSH_TOAST(toastAction, editor, dispatch)
    } else {
      const { projectContents, updatedFiles } = replaceFilePathResults
      const mainUIFile = getMainUIFromModel(editor)
      let updateUIFile: (e: EditorModel) => EditorModel = R.identity
      let selectedFile = getOpenEditorTab(editor)
      let updatedOpenFiles = [...editor.openFiles]
      Utils.fastForEach(updatedFiles, (updatedFile) => {
        const { oldPath, newPath } = updatedFile
        // If the main UI file is what we have renamed, update that later.
        if (oldPath === mainUIFile) {
          updateUIFile = (e: EditorModel) => {
            return updateMainUIInEditorState(e, newPath)
          }
        }
        // update open file array
        const indexOfTabToUpdate = updatedOpenFiles.findIndex((editorTab) => {
          return isOpenFileTab(editorTab) && editorTab.filename === oldPath
        })
        if (indexOfTabToUpdate > -1) {
          updatedOpenFiles[indexOfTabToUpdate] = openFileTab(newPath)
        }
        // update currently open file
        if (
          selectedFile != null &&
          isOpenFileTab(selectedFile) &&
          selectedFile.filename === oldPath
        ) {
          selectedFile = openFileTab(newPath)
        }
        const oldContent = getContentsTreeFileFromString(editor.projectContents, oldPath)
        if (oldContent != null && (isImageFile(oldContent) || isAssetFile(oldContent))) {
          // Update assets.
          if (isLoggedIn(userState.loginState) && editor.id != null) {
            updateAssetFileName(editor.id, action.oldPath, action.newPath)
          }
        }
      })

      return updateUIFile({
        ...editor,
        openFiles: updatedOpenFiles,
        selectedFile:
          selectedFile == null
            ? null
            : {
                tab: selectedFile,
                initialCursorPosition:
                  editor.selectedFile == null ? null : editor.selectedFile.initialCursorPosition,
              },
        projectContents: projectContents,
        codeEditorErrors: {
          buildErrors: {},
          lintErrors: {},
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
  OPEN_EDITOR_TAB: (action: OpenEditorTab, editor: EditorModel): EditorModel => {
    const currentOpenFile = getOpenEditorTab(editor)
    const selectedFile = isOpenFileTab(action.editorTab)
      ? getFileForName(action.editorTab.filename, editor)
      : null
    const fileOpensInCanvas = selectedFile == null || isTextFile(selectedFile)
    const focusedPanel: EditorPanel = fileOpensInCanvas ? 'canvas' : 'misccodeeditor'

    const keepSelectedViews = R.equals(currentOpenFile, action.editorTab)

    return setLeftMenuTabFromFocusedPanel({
      ...editor,
      openFiles: R.uniq([...editor.openFiles, action.editorTab]),
      selectedFile: {
        tab: action.editorTab,
        initialCursorPosition: action.cursorPosition,
      },
      focusedPanel: focusedPanel,
      selectedViews: keepSelectedViews ? editor.selectedViews : [],
      highlightedViews: keepSelectedViews ? editor.highlightedViews : [],
    })
  },
  CLOSE_FILE: (action: CloseEditorTab, editor: EditorModel): EditorModel => {
    const updatedOpenFiles = editor.openFiles.filter(
      (editorTab) => !R.equals(editorTab, action.editorTab),
    )
    let revertedProjectContents: ProjectContentTreeRoot
    if (isOpenFileTab(action.editorTab)) {
      revertedProjectContents = revertFileInProjectContents(
        editor.projectContents,
        action.editorTab.filename,
      )
    } else {
      revertedProjectContents = editor.projectContents
    }
    if (R.equals(getOpenEditorTab(editor), action.editorTab)) {
      if (updatedOpenFiles.length >= 1) {
        const nextSelectedFile = updatedOpenFiles[updatedOpenFiles.length - 1]
        const updatedEditor = {
          ...editor,
          projectContents: revertedProjectContents,
          openFiles: updatedOpenFiles,
        }
        return UPDATE_FNS.OPEN_EDITOR_TAB(openEditorTab(nextSelectedFile, null), updatedEditor)
      } else {
        return editor
      }
    } else {
      return {
        ...editor,
        projectContents: revertedProjectContents,
        openFiles: updatedOpenFiles,
      }
    }
  },
  REORDER_EDITOR_TABS: (action: ReorderEditorTabs, editor: EditorModel): EditorModel => {
    const oldIndex = editor.openFiles.findIndex((tab) => R.equals(tab, action.editorTab))
    let updatedOpenFiles = [...editor.openFiles]
    updatedOpenFiles[oldIndex] = editor.openFiles[action.newIndex]
    updatedOpenFiles[action.newIndex] = editor.openFiles[oldIndex]
    return {
      ...editor,
      openFiles: updatedOpenFiles,
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

        fetchNodeModules(deps).then((fetchNodeModulesResult) => {
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
        mountCount: editor.canvas.mountCount + (isTextFile(file) ? 0 : 1),
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
    const existing = getContentsTreeFileFromString(editor.projectContents, action.filePath)
    if (existing == null || !isTextFile(existing)) {
      // The worker shouldn't be recreating deleted files or reformated files
      console.error(`Worker thread is trying to update an invalid file ${action.filePath}`)
      return editor
    }

    if (!editor.parseOrPrintInFlight) {
      // We've received this update after an editor undo action, so we should discard it
      return editor
    }

    // The worker should only ever cause one side to catch up to the other
    if (action.codeOrModel === 'Code' && !codeNeedsPrinting(existing.fileContents.revisionsState)) {
      return editor
    } else if (
      action.codeOrModel === 'Model' &&
      !codeNeedsParsing(existing.fileContents.revisionsState)
    ) {
      return editor
    }

    let updatedFile: TextFile
    let updatedContents: ParsedTextFile
    let code: string
    switch (action.codeOrModel) {
      case 'Code': {
        // we use the new highlightBounds coming from the action
        code = action.file.fileContents.code
        const highlightBounds = getHighlightBoundsFromParseResult(action.file.fileContents.parsed)
        updatedContents = updateParsedTextFileHighlightBounds(
          existing.fileContents.parsed,
          highlightBounds,
        )
        break
      }
      case 'Model': {
        // Clear any cached paths since UIDs will have been regenerated and property paths may no longer exist
        PP.clearPropertyPathCache()
        TP.clearTemplatePathCache()

        // we use the new highlightBounds coming from the action
        code = existing.fileContents.code
        const highlightBounds = getHighlightBoundsFromParseResult(action.file.fileContents.parsed)
        updatedContents = updateParsedTextFileHighlightBounds(
          action.file.fileContents.parsed,
          highlightBounds,
        )
        break
      }
      default:
        const _exhaustiveCheck: never = action.codeOrModel
        throw new Error(`Invalid flag used: ${action.codeOrModel}`)
    }

    if (isOlderThan(action.file, existing)) {
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

    const updatedProjectContents = addFileToProjectContents(
      editor.projectContents,
      action.filePath,
      updatedFile,
    )
    return {
      ...editor,
      projectContents: updatedProjectContents,
      canvas: {
        ...editor.canvas,
        mountCount:
          action.codeOrModel === 'Model' ? editor.canvas.mountCount + 1 : editor.canvas.mountCount,
      },
      parseOrPrintInFlight: false, // only ever clear it here
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
    const newTab = openFileTab(newFileKey)
    const updatedEditor: EditorModel = {
      ...editor,
      selectedFile: {
        tab: newTab,
        initialCursorPosition: null,
      },
      openFiles: [...editor.openFiles, newTab],
      projectContents: updatedProjectContents,
      fileBrowser: {
        ...editor.fileBrowser,
        renamingTarget: newFileKey,
      },
    }
    return updatedEditor
  },
  DELETE_FILE: (
    action: DeleteFile,
    editor: EditorModel,
    derived: DerivedState,
    userState: UserState,
  ): EditorModel => {
    const file = getContentsTreeFileFromString(editor.projectContents, action.filename)
    const updatedProjectContents = removeFromProjectContents(
      editor.projectContents,
      action.filename,
    )
    const updatedOpenFiles = editor.openFiles.filter(
      (tab) => !isOpenFileTab(tab) || tab.filename !== action.filename,
    )
    const selectedFile = getOpenFilename(editor)
    const updatedSelectedFile =
      selectedFile === action.filename
        ? updatedOpenFiles.length > 0
          ? {
              tab: updatedOpenFiles[0],
              initialCursorPosition: null,
            }
          : null
        : editor.selectedFile

    // Don't delete package.json, otherwise it will bring about the end of days.
    if (file == null || action.filename === 'package.json') {
      return editor
    }

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
          openFiles: updatedOpenFiles,
          selectedFile: updatedSelectedFile,
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
          openFiles: updatedOpenFiles,
          selectedFile: updatedSelectedFile,
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
    const spyResult = convertMetadataMap(
      spyCollector.current.spyValues.metadata,
      spyCollector.current.spyValues.scenes,
    )

    const finalDomMetadata = arrayDeepEquality(ElementInstanceMetadataKeepDeepEquality())(
      editor.domMetadataKILLME,
      action.elementMetadata,
    ).value
    const finalSpyMetadata = JSXMetadataKeepDeepEquality()(editor.spyMetadataKILLME, spyResult)
      .value

    const stayedTheSame =
      editor.domMetadataKILLME === finalDomMetadata && editor.spyMetadataKILLME === finalSpyMetadata

    if (stayedTheSame) {
      return editor
    } else {
      return {
        ...editor,
        domMetadataKILLME: finalDomMetadata,
        spyMetadataKILLME: finalSpyMetadata,
      }
    }
  },
  SET_PROP: (action: SetProp, editor: EditorModel): EditorModel => {
    return addUtopiaUtilsImportIfUsed(
      setPropertyOnTarget(editor, action.target, (props) => {
        return mapEither(
          roundAttributeLayoutValues,
          setJSXValueAtPath(props, action.propertyPath, action.value),
        )
      }),
    )
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
            jsxAttributeValue(originalValue.value),
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
      if (TP.isInstancePath(target)) {
        return switchAndUpdateFrames(working, target, action.layoutSystem)
      } else {
        return working
      }
    }, editor)
  },
  WRAP_IN_LAYOUTABLE: (action: WrapInLayoutable, editor: EditorModel): EditorModel => {
    const editorWithAddedImport = modifyOpenParseSuccess((success) => {
      const updatedImport = addImport(
        'utopia-api',
        null,
        [importAlias(action.wrapper)],
        null,
        success.imports,
      )
      return {
        ...success,
        imports: updatedImport,
      }
    }, editor)

    return modifyOpenJsxElementAtPath(
      action.target,
      (element) => {
        const component = element.name.baseVariable
        if (component !== action.wrapper) {
          const wrappedComponent =
            element.props.wrappedComponent == null
              ? jsxAttributeOtherJavaScript(component, component, [], null)
              : element.props.wrappedComponent
          return {
            ...element,
            name: {
              ...element.name,
              baseVariable: action.wrapper,
            },
            props: {
              ...element.props,
              wrappedComponent: wrappedComponent,
            },
          }
        } else {
          return element
        }
      },
      editorWithAddedImport,
    )
  },
  UNWRAP_LAYOUTABLE: (action: UnwrapLayoutable, editor: EditorModel): EditorModel => {
    const targetMetadata = Utils.forceNotNull(
      `Could not find metadata for ${JSON.stringify(action.target)}`,
      MetadataUtils.getElementByInstancePathMaybe(editor.jsxMetadataKILLME.elements, action.target),
    )

    return modifyOpenJsxElementAtPath(
      action.target,
      (element) => {
        const imports = getOpenImportsFromState(editor)
        if (
          MetadataUtils.isLayoutWrapperAgainstImports(imports, targetMetadata) &&
          element.props.wrappedComponent != null
        ) {
          if (element.props.wrappedComponent.type === 'ATTRIBUTE_OTHER_JAVASCRIPT') {
            const updatedProps = { ...element.props }
            delete updatedProps['wrappedComponent']
            return {
              ...element,
              name: {
                ...element.name,
                baseVariable: element.props.wrappedComponent.javascript,
              },
              props: updatedProps,
            }
          } else {
            return element
          }
        } else {
          return element
        }
      },
      editor,
    )
  },
  UPDATE_JSX_ELEMENT_NAME: (action: UpdateJSXElementName, editor: EditorModel): EditorModel => {
    const targetMetadata = Utils.forceNotNull(
      `Could not find metadata for ${JSON.stringify(action.target)}`,
      MetadataUtils.getElementByInstancePathMaybe(editor.jsxMetadataKILLME.elements, action.target),
    )

    let updatedEditor = editor
    if (action.elementName.baseVariable === 'animated') {
      updatedEditor = modifyOpenParseSuccess((success) => {
        const updatedImport = addImport(
          'react-spring',
          null,
          [importAlias('animated')],
          null,
          success.imports,
        )
        return {
          ...success,
          imports: updatedImport,
        }
      }, editor)
    }

    return modifyOpenJsxElementAtPath(
      action.target,
      (element) => {
        const imports = getOpenImportsFromState(editor)
        if (
          MetadataUtils.isLayoutWrapperAgainstImports(imports, targetMetadata) &&
          element.props.wrappedComponent != null
        ) {
          if (element.props.wrappedComponent.type === 'ATTRIBUTE_OTHER_JAVASCRIPT') {
            const nameAsString = getJSXElementNameAsString(action.elementName)
            return {
              ...element,
              props: {
                ...element.props,
                wrappedComponent: jsxAttributeOtherJavaScript(nameAsString, nameAsString, [], null),
              },
            }
          } else {
            return element
          }
        } else {
          return {
            ...element,
            name: action.elementName,
          }
        }
      },
      updatedEditor,
    )
  },
  SET_ASPECT_RATIO_LOCK: (action: SetAspectRatioLock, editor: EditorModel): EditorModel => {
    return modifyOpenJsxElementAtPath(
      action.target,
      (element) => {
        const locked = jsxAttributeValue(action.locked)
        const updatedProps = eitherToMaybe(
          setJSXValueAtPath(element.props, PP.create(['data-aspect-ratio-locked']), locked),
        )
        return {
          ...element,
          props: updatedProps ?? {},
        }
      },
      editor,
    )
  },
  SET_CODE_EDITOR_THEME: (action: SetCodeEditorTheme, editor: EditorModel): EditorModel => {
    return {
      ...editor,
      codeEditorTheme: action.value,
    }
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
      const utopiaComponents = getOpenUtopiaJSXComponentsFromState(editor)
      const newUID = generateUidWithExistingComponents(utopiaComponents)
      const imageAttribute = jsxAttributeValue(imagePathURL(action.imagePath))
      const size: Size = {
        width: projectContent.width ?? 100,
        height: projectContent.height ?? 100,
      }
      const { frame } = getFrameAndMultiplier(action.position, action.imagePath, size, null)
      let parentShiftX: number = 0
      let parentShiftY: number = 0
      if (parent != null) {
        const frameOfParent = MetadataUtils.getFrameInCanvasCoords(parent, editor.jsxMetadataKILLME)
        if (frameOfParent != null) {
          parentShiftX = -frameOfParent.x
          parentShiftY = -frameOfParent.y
        }
      }
      const imageElement = jsxElement(
        jsxElementName('img', []),
        {
          alt: jsxAttributeValue(''),
          src: imageAttribute,
          style: jsxAttributeValue({
            left: parentShiftX + frame.x,
            top: parentShiftY + frame.y,
            width: frame.width,
            height: frame.height,
          }),
          'data-uid': jsxAttributeValue(newUID),
          'data-aspect-ratio-locked': jsxAttributeValue(true),
        },
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
      const propertyControls = getPropertyControlsForTargetFromEditor(action.target, editor)
      let elementName
      if (TP.isScenePath(action.target)) {
        const element = findJSXElementChildAtPath(
          getOpenUtopiaJSXComponentsFromState(editor),
          createSceneTemplatePath(action.target),
        )
        if (element != null && isJSXElement(element)) {
          elementName = foldEither(
            (l) => null,
            (r) => (r != null && r.type === 'ATTRIBUTE_OTHER_JAVASCRIPT' ? r.javascript : null),
            getModifiableJSXAttributeAtPath(element.props, PathForSceneComponent),
          )
        }
      } else {
        const element = findJSXElementAtPath(
          action.target,
          getOpenUtopiaJSXComponentsFromState(editor),
        )
        if (element != null) {
          elementName = getJSXElementNameAsString(element.name)
        }
      }
      if (elementName == null) {
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

      let pathToUpdate: PropertyPath | null
      if (TP.isScenePath(action.target)) {
        pathToUpdate =
          action.path == null ? PathForSceneProps : PP.append(PathForSceneProps, action.path)
      } else {
        pathToUpdate = action.path
      }

      const propsForPath =
        action.path == null ? defaultProps : defaultProps[PP.toString(action.path)]
      const target = TP.isScenePath(action.target)
        ? createSceneTemplatePath(action.target)
        : action.target

      if (pathToUpdate != null) {
        return setPropertyOnTarget(editor, target, (props) => {
          return setJSXValueAtPath(props, pathToUpdate!, jsxAttributeValue(propsForPath))
        })
      } else {
        return setPropertyOnTarget(editor, target, (props) => {
          const updatedProps = objectMap(jsxAttributeValue, defaultProps)
          updatedProps['data-uid'] = props['data-uid'] as JSXAttributeValue<string>
          return right(updatedProps)
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
        action.buildType,
        getMainUIFromModel(result),
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
      const openTab = openEditorTab(openFileTab(StoryboardFilePath), null)
      return UPDATE_FNS.OPEN_EDITOR_TAB(openTab, updatedEditor)
    }
  },
}

/** DO NOT USE outside of actions.ts, only exported for testing purposes */
export function alignOrDistributeSelectedViews(
  editor: EditorModel,
  derived: DerivedState,
  alignmentOrDistribution: Alignment | Distribution,
): EditorModel {
  const instancePaths = editor.selectedViews.filter(TP.isInstancePath)

  if (instancePaths.length > 0) {
    // this array of canvasFrames excludes the non-layoutables. it means in a multiselect, they will not be considered
    const canvasFrames: Array<{ target: TemplatePath; frame: CanvasRectangle }> = Utils.stripNulls(
      instancePaths.map((target) => {
        const instanceGlobalFrame = MetadataUtils.getFrameInCanvasCoords(
          target,
          editor.jsxMetadataKILLME,
        )
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
      const parentPath = TP.parentPath(instancePaths[0])
      const sourceIsParent = instancePaths.length === 1 && parentPath != null
      let source: CanvasRectangle
      if (sourceIsParent) {
        const parentFrame = MetadataUtils.getFrameInCanvasCoords(
          parentPath,
          editor.jsxMetadataKILLME,
        )
        // if the parent frame is null, that means we probably ran into some error state,
        // as it means the child's globalFrame should also be null, so we shouldn't be in this branch
        source = Utils.forceNotNull(
          `found no parent global frame for ${TP.toComponentId(parentPath!)}`,
          parentFrame,
        )
      } else {
        source = Utils.boundingRectangleArray(Utils.pluck(canvasFrames, 'frame'))! // I know this can't be null because we checked the canvasFrames array is non-empty
      }
      const components = getOpenUtopiaJSXComponentsFromState(editor)
      const updatedCanvasFrames = alignOrDistributeCanvasRects(
        components,
        editor.jsxMetadataKILLME,
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
  components: Array<UtopiaJSXComponent>,
  componentMetadata: JSXMetadata,
  targets: CanvasFrameAndTarget[],
  source: CanvasRectangle,
  alignmentOrDistribution: Alignment | Distribution,
  sourceIsParent: boolean,
): Array<PinOrFlexFrameChange> {
  let results: Array<PinOrFlexFrameChange> = []

  function addChange(target: TemplatePath, frame: CanvasRectangle | null): void {
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
      let toOperateOn: Array<{ target: TemplatePath; frame: CanvasRectangle }> = []
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
      let toOperateOn: Array<{ target: TemplatePath; frame: CanvasRectangle }> = []
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
  return modifyOpenScenesAndJSXElements((components) => {
    return updateFramesOfScenesAndComponents(
      components,
      editor.jsxMetadataKILLME,
      framesAndTargets,
      optionalParentFrame,
    )
  }, editor)
}

export async function newProject(
  dispatch: EditorDispatch,
  renderEditorRoot: () => void,
): Promise<void> {
  const defaultPersistentModel = defaultProject()
  const npmDependencies = dependenciesWithEditorRequirements(defaultPersistentModel.projectContents)
  const fetchNodeModulesResult = await fetchNodeModules(npmDependencies)

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
    'full-build',
    null,
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
  projectId: string | null,
  workers: UtopiaTsWorkers,
  renderEditorRoot: () => void,
  retryFetchNodeModules: boolean = true,
): Promise<void> {
  // this action is now async!
  const migratedModel = applyMigrations(model)
  const npmDependencies = dependenciesWithEditorRequirements(migratedModel.projectContents)
  const fetchNodeModulesResult = await fetchNodeModules(npmDependencies, retryFetchNodeModules)

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
      'full-build',
      null,
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

  const safeMode =
    projectId != null ? await localforage.getItem<boolean>(getProjectLockedKey(projectId)) : false

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
            'full-build',
            null,
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
