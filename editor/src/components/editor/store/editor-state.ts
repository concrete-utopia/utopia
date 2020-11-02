import * as json5 from 'json5'
import * as R from 'ramda'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import {
  ComponentMetadata,
  ElementInstanceMetadata,
  getElementsByUIDFromTopLevelElements,
  isUtopiaJSXComponent,
  JSXElement,
  JSXElementChild,
  TopLevelElement,
  UtopiaJSXComponent,
  isJSXElement,
} from '../../../core/shared/element-template'
import {
  insertJSXElementChild,
  removeJSXElementChild,
  transformJSXComponentAtPath,
  getUtopiaID,
} from '../../../core/model/element-template-utils'
import {
  correctProjectContentsPath,
  getOrDefaultScenes,
  getUtopiaJSXComponentsFromSuccess,
  saveTextFileContents,
  getHighlightBoundsFromParseResult,
} from '../../../core/model/project-file-utils'
import { ErrorMessage } from '../../../core/shared/error-messages'
import type { PackageStatusMap } from '../../../core/shared/npm-dependency-types'
import {
  Imports,
  InstancePath,
  ParsedTextFile,
  ParseSuccess,
  ProjectContents,
  ProjectFile,
  RevisionsState,
  SceneMetadata,
  ScenePath,
  StaticInstancePath,
  TemplatePath,
  TextFile,
  isTextFile,
  StaticTemplatePath,
  NodeModules,
  foldParsedTextFile,
  mapParsedTextFile,
  textFileContents,
  isParseSuccess,
  codeFile,
  isParseFailure,
  isParsedTextFile,
} from '../../../core/shared/project-file-types'
import { diagnosticToErrorMessage } from '../../../core/workers/ts/ts-utils'
import { ExportsInfo, MultiFileBuildResult } from '../../../core/workers/ts/ts-worker'
import { UtopiaTsWorkers } from '../../../core/workers/common/worker-types'
import { BaseSnappingThreshold } from '../../../templates/editor-canvas'
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
} from '../../assets'
import {
  CanvasModel,
  CSSCursor,
  DragState,
  FrameAndTarget,
  HigherOrderControl,
} from '../../canvas/canvas-types'
import { produceCanvasTransientState } from '../../canvas/canvas-utils'
import { CodeEditorTheme, DefaultTheme } from '../../code-editor/code-editor-themes'
import { CursorPosition } from '../../code-editor/code-editor-utils'
import { EditorPanel } from '../../common/actions/index'
import {
  CodeResultCache,
  generateCodeResultCache,
  PropertyControlsInfo,
} from '../../custom-code/code-file'
import { EditorModes, Mode } from '../editor-modes'
import { FontSettings } from '../../inspector/common/css-utils'
import { DefaultPackagesList, PackageDetails } from '../../navigator/dependency-list'
import { LeftMenuTab, LeftPaneDefaultWidth } from '../../navigator/left-pane'
import { DropTargetHint } from '../../navigator/navigator'
import { EditorDispatch, LoginState, ProjectListing } from '../action-types'
import { CURRENT_PROJECT_VERSION } from '../actions/migrations/migrations'
import { StateHistory } from '../history'
import {
  createSceneTemplatePath,
  isSceneElement,
  BakedInStoryboardVariableName,
  EmptyScenePathForStoryboard,
  isDynamicSceneChildWidthHeightPercentage,
} from '../../../core/model/scene-utils'

import { RightMenuTab } from '../../canvas/right-menu'

import {
  staticInstancePath,
  instancePath,
  isInstancePath,
  toUid,
  toString,
} from '../../../core/shared/template-path'

import { Notice } from '../../common/notices'
import { emptyComplexMap, ComplexMap, addToComplexMap } from '../../../utils/map'
import * as friendlyWords from 'friendly-words'
import { fastForEach } from '../../../core/shared/utils'
import { ShortcutConfiguration } from '../shortcut-definitions'
import { notLoggedIn } from '../../../common/user'
import {
  dependenciesWithEditorRequirements,
  immediatelyResolvableDependenciesWithEditorRequirements,
} from '../npm-dependency/npm-dependency'
import { getControlsForExternalDependencies } from '../../../core/property-controls/property-controls-utils'
import { parseSuccess } from '../../../core/workers/common/project-file-utils'

export interface OriginalPath {
  originalTP: TemplatePath
  currentTP: TemplatePath
}

export interface UserConfiguration {
  shortcutConfig: ShortcutConfiguration | null
}

export function emptyUserConfiguration(): UserConfiguration {
  return {
    shortcutConfig: null,
  }
}

export interface UserState extends UserConfiguration {
  loginState: LoginState
}

export const defaultUserState: UserState = {
  loginState: notLoggedIn,
  shortcutConfig: {},
}

export type EditorStore = {
  editor: EditorState
  derived: DerivedState
  history: StateHistory
  userState: UserState
  workers: UtopiaTsWorkers
  dispatch: EditorDispatch
}

export interface FileDeleteModal {
  type: 'file-delete'
  filePath: string
}
export interface FileCloseModal {
  type: 'file-close'
  editorTab: EditorTab
}

export type ModalDialog = FileDeleteModal | FileCloseModal

export type CursorImportanceLevel = 'fixed' | 'mouseOver' // only one fixed cursor can exist, mouseover is a bit less important
export type CursorStackItem = {
  id: string
  importance: CursorImportanceLevel
  cursor: CSSCursor
}
export type CursorStack = Array<CursorStackItem>
export type CanvasCursor = {
  fixed: CursorStackItem | null
  mouseOver: CursorStack
}

export interface DuplicationState {
  duplicateRoots: Array<OriginalPath>
}

export interface ImageBlob {
  base64: string
}

export type UIFileBase64Blobs = { [key: string]: ImageBlob }

export type CanvasBase64Blobs = { [key: string]: UIFileBase64Blobs }

export type ErrorMessages = { [filename: string]: Array<ErrorMessage> }

export interface OpenFileTab {
  type: 'OPEN_FILE_TAB'
  filename: string
}

export function openFileTab(filename: string): OpenFileTab {
  return {
    type: 'OPEN_FILE_TAB',
    filename: filename,
  }
}

export interface ReleaseNotesTab {
  type: 'RELEASE_NOTES_TAB'
}

export function releaseNotesTab(): ReleaseNotesTab {
  return {
    type: 'RELEASE_NOTES_TAB',
  }
}

export interface UserConfigurationTab {
  type: 'USER_CONFIGURATION_TAB'
}

export function userConfigurationTab(): UserConfigurationTab {
  return {
    type: 'USER_CONFIGURATION_TAB',
  }
}

export type EditorTab = OpenFileTab | ReleaseNotesTab | UserConfigurationTab

export function isOpenFileTab(editorTab: EditorTab): editorTab is OpenFileTab {
  return editorTab.type === 'OPEN_FILE_TAB'
}

export function isReleaseNotesTab(editorTab: EditorTab): editorTab is ReleaseNotesTab {
  return editorTab.type === 'RELEASE_NOTES_TAB'
}

export function isUserConfigurationTab(editorTab: EditorTab): editorTab is UserConfigurationTab {
  return editorTab.type === 'USER_CONFIGURATION_TAB'
}

export interface ConsoleLog {
  method: string
  data: Array<any>
}

// FIXME We need to pull out ProjectState from here
export interface EditorState {
  id: string | null
  appID: string | null
  projectName: string
  projectVersion: number
  isLoaded: boolean
  openFiles: Array<EditorTab>
  cursorPositions: { [key: string]: CursorPosition }
  selectedFile: {
    tab: EditorTab
    initialCursorPosition: CursorPosition | null
  } | null
  spyMetadataKILLME: ComponentMetadata[] // this is coming from the canvas spy report.
  domMetadataKILLME: ElementInstanceMetadata[] // this is coming from the dom walking report.
  jsxMetadataKILLME: ComponentMetadata[] // this is a merged result of the two above.
  projectContents: ProjectContentTreeRoot
  codeResultCache: CodeResultCache
  propertyControlsInfo: PropertyControlsInfo
  nodeModules: {
    skipDeepFreeze: true // when we evaluate the code files we plan to mutate the content with the eval result
    files: NodeModules
    projectFilesBuildResults: MultiFileBuildResult
    packageStatus: PackageStatusMap
  }
  selectedViews: Array<TemplatePath>
  highlightedViews: Array<TemplatePath>
  hiddenInstances: Array<TemplatePath>
  warnedInstances: Array<TemplatePath>
  mode: Mode
  focusedPanel: EditorPanel | null
  keysPressed: KeysPressed
  openPopupId: string | null
  toasts: ReadonlyArray<Notice>
  cursorStack: CanvasCursor
  leftMenu: {
    selectedTab: LeftMenuTab
    expanded: boolean
    paneWidth: number
  }
  rightMenu: {
    selectedTab: RightMenuTab
    expanded: boolean
  }
  interfaceDesigner: {
    codePaneWidth: number
    codePaneVisible: boolean
    restorableCodePaneWidth: number
    layoutReversed: boolean
    additionalControls: boolean
  }
  canvas: {
    visible: boolean
    dragState: DragState | null
    scale: number
    snappingThreshold: number
    realCanvasOffset: CanvasVector
    roundedCanvasOffset: CanvasVector
    textEditor: {
      templatePath: InstancePath
      triggerMousePosition: WindowPoint | null
    } | null
    selectionControlsVisible: boolean
    animationsEnabled: boolean
    highlightsEnabled: boolean
    cursor: CSSCursor | null
    duplicationState: DuplicationState | null
    base64Blobs: CanvasBase64Blobs
    mountCount: number
  }
  inspector: {
    visible: boolean
  }
  fileBrowser: {
    minimised: boolean
    renamingTarget: string | null
  }
  dependencyList: {
    minimised: boolean
  }
  genericExternalResources: {
    minimised: boolean
  }
  googleFontsResources: {
    minimised: boolean
  }
  projectSettings: {
    minimised: boolean
  }
  navigator: {
    minimised: boolean
    dropTargetHint: DropTargetHint
    collapsedViews: TemplatePath[]
    renamingTarget: TemplatePath | null
  }
  preview: {
    visible: boolean
    connected: boolean
  }
  home: {
    visible: boolean
  }
  lastUsedFont: FontSettings | null
  modal: ModalDialog | null
  localProjectList: Array<ProjectListing>
  projectList: Array<ProjectListing>
  showcaseProjects: Array<ProjectListing>
  codeEditingEnabled: boolean
  codeEditorErrors: {
    buildErrors: ErrorMessages
    lintErrors: ErrorMessages
  }
  thumbnailLastGenerated: number
  pasteTargetsToIgnore: TemplatePath[]
  parseOrPrintInFlight: boolean
  codeEditorTheme: CodeEditorTheme
  safeMode: boolean
  saveError: boolean
}

export interface StoredEditorState {
  selectedFile: EditorTab | null
  selectedViews: Array<TemplatePath>
}

export function storedEditorStateFromEditorState(editorState: EditorState): StoredEditorState {
  return {
    selectedFile: getOpenEditorTab(editorState),
    selectedViews: editorState.selectedViews,
  }
}

export function mergeStoredEditorStateIntoEditorState(
  storedEditorState: StoredEditorState | null,
  editorState: EditorState,
): EditorState {
  if (storedEditorState == null) {
    return editorState
  } else {
    return {
      ...editorState,
      selectedFile:
        storedEditorState.selectedFile == null
          ? null
          : { tab: storedEditorState.selectedFile, initialCursorPosition: null },
      selectedViews: storedEditorState.selectedViews,
    }
  }
}

export function getOpenEditorTab(model: EditorState): EditorTab | null {
  return model.selectedFile == null ? null : model.selectedFile.tab
}

export function getOpenFilename(model: EditorState): string | null {
  const openTab = getOpenEditorTab(model)
  return openTab != null && isOpenFileTab(openTab) ? openTab.filename : null
}

export function getOpenFile(model: EditorState): ProjectFile | null {
  const openEditorTab = getOpenEditorTab(model)
  if (openEditorTab == null) {
    return null
  } else {
    if (isOpenFileTab(openEditorTab)) {
      return getContentsTreeFileFromString(model.projectContents, openEditorTab.filename)
    } else {
      return null
    }
  }
}

export function getFileForName(filePath: string, model: EditorState): ProjectFile | null {
  return getContentsTreeFileFromString(model.projectContents, filePath)
}

export function getOpenTextFileKey(model: EditorState): string | null {
  const openFile = getOpenFile(model)
  const openEditorTab = getOpenEditorTab(model)
  if (
    openFile != null &&
    isTextFile(openFile) &&
    openEditorTab != null &&
    isOpenFileTab(openEditorTab)
  ) {
    return openEditorTab.filename
  } else {
    return null
  }
}

export function getOpenTextFile(model: EditorState): TextFile | null {
  const openTextFileKey = getOpenTextFileKey(model)
  if (openTextFileKey == null) {
    return null
  } else {
    const openTextFile = getContentsTreeFileFromString(model.projectContents, openTextFileKey)
    if (openTextFile != null && isTextFile(openTextFile)) {
      return openTextFile
    } else {
      throw new Error(
        `Inconsistency between expected open Text file ${openTextFileKey} and the file ${JSON.stringify(
          openTextFile,
        )}`,
      )
    }
  }
}

export function getOpenUIJSFileKey(model: EditorState): string | null {
  const openEditorTab = getOpenEditorTab(model)
  if (isOpenFileUiJs(model) && openEditorTab != null && isOpenFileTab(openEditorTab)) {
    return openEditorTab.filename
  } else {
    return null
  }
}

export function isOpenFileUiJs(model: EditorState): boolean {
  const openFile = getOpenFile(model)
  return openFile != null && isParsedTextFile(openFile)
}

export function getOpenUIJSFile(model: EditorState): TextFile | null {
  const openUIJSFileKey = getOpenUIJSFileKey(model)
  if (openUIJSFileKey == null) {
    return null
  } else {
    const openUIJSFile = getContentsTreeFileFromString(model.projectContents, openUIJSFileKey)
    if (openUIJSFile != null && isParsedTextFile(openUIJSFile)) {
      return openUIJSFile
    } else {
      throw new Error(
        `Inconsistency between expected open UI JS file ${openUIJSFileKey} and the file ${JSON.stringify(
          openUIJSFile,
        )}`,
      )
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
  )
}

export interface ParseSuccessAndEditorChanges<T> {
  parseSuccessTransform: (success: ParseSuccess) => ParseSuccess
  editorStateTransform: (editor: EditorState) => EditorState
  additionalData: T
}

export function applyParseAndEditorChanges<T>(
  getChanges: (e: EditorState, success: ParseSuccess) => ParseSuccessAndEditorChanges<T>,
  editor: EditorState,
): { editor: EditorState; additionalData: T | null } {
  const openUIJSFileKey = getOpenUIJSFileKey(editor)
  if (openUIJSFileKey == null) {
    return { editor: editor, additionalData: null }
  } else {
    const openUIJSFile = getOpenUIJSFile(editor)
    if (openUIJSFile == null) {
      return { editor: editor, additionalData: null }
    } else {
      const possibleChanges: ParseSuccessAndEditorChanges<T> | null = foldParsedTextFile(
        (_) => null,
        (success) => {
          return getChanges(editor, success)
        },
        (_) => null,
        openUIJSFile.fileContents.parsed,
      )
      if (possibleChanges == null) {
        return { editor: editor, additionalData: null }
      } else {
        const updatedContents: ParsedTextFile = mapParsedTextFile(
          possibleChanges.parseSuccessTransform,
          openUIJSFile.fileContents.parsed,
        )

        const updatedFile = saveTextFileContents(
          openUIJSFile,
          textFileContents(
            openUIJSFile.fileContents.code,
            updatedContents,
            RevisionsState.ParsedAhead,
          ),
          false,
        )

        const editorWithSuccessChanges = {
          ...editor,
          projectContents: addFileToProjectContents(
            editor.projectContents,
            openUIJSFileKey,
            updatedFile,
          ),
        }

        return {
          editor: possibleChanges.editorStateTransform(editorWithSuccessChanges),
          additionalData: possibleChanges.additionalData,
        }
      }
    }
  }
}

export function modifyOpenParseSuccess(
  transform: (success: ParseSuccess) => ParseSuccess,
  model: EditorState,
): EditorState {
  function getChanges(
    editor: EditorState,
    success: ParseSuccess,
  ): ParseSuccessAndEditorChanges<Record<string, never>> {
    return {
      parseSuccessTransform: transform,
      editorStateTransform: (e: EditorState) => e,
      additionalData: {},
    }
  }
  return applyParseAndEditorChanges<Record<string, never>>(getChanges, model).editor
}

export function applyUtopiaJSXComponentsChanges(
  topLevelElements: Array<TopLevelElement>,
  newUtopiaComponents: Array<UtopiaJSXComponent>,
): Array<TopLevelElement> {
  // Run through the old top level elements, replacing the exported elements with those in the
  // newly updated result with the same name.
  // If it doesn't exist in the updated result, delete it.
  // For any new items in the updated result, add them in.
  const addedSoFar: Set<string> = Utils.emptySet()
  let newTopLevelElements: Array<TopLevelElement> = []
  Utils.fastForEach(topLevelElements, (oldTopLevelElement) => {
    if (isUtopiaJSXComponent(oldTopLevelElement)) {
      const updatedElement = newUtopiaComponents.find((e) => e.name === oldTopLevelElement.name)
      if (updatedElement !== undefined) {
        addedSoFar.add(updatedElement.name)
        newTopLevelElements.push(updatedElement)
      }
    } else {
      newTopLevelElements.push(oldTopLevelElement)
    }
  })

  Utils.fastForEach(newUtopiaComponents, (updatedElement) => {
    if (!addedSoFar.has(updatedElement.name)) {
      newTopLevelElements.push(updatedElement)
    }
  })

  return newTopLevelElements
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
    componentMetadata: Array<ComponentMetadata>,
  ) => { components: Array<UtopiaJSXComponent>; componentMetadata: Array<ComponentMetadata> },
  model: EditorState,
): EditorState {
  let workingMetadata: Array<ComponentMetadata> = model.jsxMetadataKILLME
  const successTransform = (success: ParseSuccess) => {
    const oldUtopiaJSXComponents = getUtopiaJSXComponentsFromSuccess(success)
    // Apply the transformation.
    const transformResult = transform(oldUtopiaJSXComponents, model.jsxMetadataKILLME)
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
  return {
    ...modifyOpenParseSuccess(successTransform, model),
    jsxMetadataKILLME: workingMetadata,
  }
}

export function modifyOpenJsxElementAtPath(
  path: InstancePath,
  transform: (element: JSXElement) => JSXElement,
  model: EditorState,
): EditorState {
  const staticPath = MetadataUtils.dynamicPathToStaticPath(path)
  if (staticPath == null) {
    return model
  } else {
    return modifyOpenJSXElements((utopiaComponents) => {
      return transformJSXComponentAtPath(utopiaComponents, staticPath, transform)
    }, model)
  }
}

export function modifyOpenJsxElementAtStaticPath(
  path: StaticInstancePath,
  transform: (element: JSXElement) => JSXElement,
  model: EditorState,
): EditorState {
  return modifyOpenJSXElements((utopiaComponents) => {
    return transformJSXComponentAtPath(utopiaComponents, path, transform)
  }, model)
}

export function getOpenUtopiaJSXComponentsFromState(model: EditorState): Array<UtopiaJSXComponent> {
  const openUIJSFile = getOpenUIJSFile(model)
  if (openUIJSFile == null) {
    return []
  } else {
    if (isParseSuccess(openUIJSFile.fileContents.parsed)) {
      return getUtopiaJSXComponentsFromSuccess(openUIJSFile.fileContents.parsed)
    } else {
      return []
    }
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

export function modifyOpenSceneAtPath(
  path: ScenePath,
  transform: (scene: JSXElement) => JSXElement,
  model: EditorState,
): EditorState {
  return modifyOpenScenes_INTERNAL((components) => {
    return transformJSXComponentAtPath(components, createSceneTemplatePath(path), transform)
  }, model)
}

export function getNumberOfScenes(model: EditorState): number {
  return getSceneElements(model).length
}

export function getSceneElements(model: EditorState): JSXElement[] {
  const openUIJSFile = getOpenUIJSFile(model)
  if (openUIJSFile == null || !isParseSuccess(openUIJSFile.fileContents.parsed)) {
    return []
  } else {
    return getSceneElementsFromParseSuccess(openUIJSFile.fileContents.parsed)
  }
}

export function getSceneElementsFromParseSuccess(success: ParseSuccess): JSXElement[] {
  const rootElement = getOrDefaultScenes(success).rootElement
  if (!isJSXElement(rootElement) || rootElement.name.baseVariable !== 'Storyboard') {
    throw new Error('the root element must be a Storyboard component')
  }
  return rootElement.children.filter(
    (child): child is JSXElement => isJSXElement(child) && isSceneElement(child),
  )
}

export function addNewScene(model: EditorState, newSceneElement: JSXElement): EditorState {
  return modifyOpenScenes_INTERNAL(
    (components) => addSceneToJSXComponents(components, newSceneElement),
    model,
  )
}

export function addSceneToJSXComponents(
  components: UtopiaJSXComponent[],
  newSceneElement: JSXElement,
) {
  const storyoardComponentRootElement = components.find(
    (c) => c.name === BakedInStoryboardVariableName,
  )?.rootElement
  const storyboardComponentUID =
    storyoardComponentRootElement != null ? getUtopiaID(storyoardComponentRootElement) : null
  if (storyboardComponentUID != null) {
    const storyboardComponentTemplatePath = staticInstancePath(
      [storyboardComponentUID],
      [storyboardComponentUID],
    )
    return insertJSXElementChild(storyboardComponentTemplatePath, newSceneElement, components, null)
  } else {
    return components
  }
}

export function removeScene(model: EditorState, scenePath: ScenePath): EditorState {
  return modifyOpenScenes_INTERNAL((components) => {
    return removeJSXElementChild(createSceneTemplatePath(scenePath), components)
  }, model)
}

const emptyImports: Imports = {}

export function getOpenImportsFromState(model: EditorState): Imports {
  const openUIJSFile = getOpenUIJSFile(model)
  if (openUIJSFile == null) {
    return {}
  } else {
    return foldParsedTextFile(
      (_) => {
        return emptyImports
      },
      (r) => {
        return r.imports
      },
      (_) => {
        return emptyImports
      },
      openUIJSFile.fileContents.parsed,
    )
  }
}

export function removeElementAtPath(
  target: InstancePath,
  components: Array<UtopiaJSXComponent>,
): Array<UtopiaJSXComponent> {
  const staticTarget = MetadataUtils.dynamicPathToStaticPath(target)
  if (staticTarget == null) {
    return components
  } else {
    return removeJSXElementChild(staticTarget, components)
  }
}

export function insertElementAtPath(
  targetParent: TemplatePath | null,
  elementToInsert: JSXElementChild,
  components: Array<UtopiaJSXComponent>,
  indexPosition: IndexPosition | null,
): Array<UtopiaJSXComponent> {
  const staticTarget = MetadataUtils.templatePathToStaticTemplatePath(targetParent)
  return insertJSXElementChild(staticTarget, elementToInsert, components, indexPosition)
}

export function transformElementAtPath(
  components: Array<UtopiaJSXComponent>,
  target: InstancePath,
  transform: (elem: JSXElement) => JSXElement,
): Array<UtopiaJSXComponent> {
  const staticTarget = MetadataUtils.dynamicPathToStaticPath(target)
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

export interface TransientCanvasState {
  selectedViews: Array<TemplatePath>
  highlightedViews: Array<TemplatePath>
  fileState: TransientFileState | null
}

export function transientCanvasState(
  selectedViews: Array<TemplatePath>,
  highlightedViews: Array<TemplatePath>,
  fileState: TransientFileState | null,
): TransientCanvasState {
  return {
    selectedViews: selectedViews,
    highlightedViews: highlightedViews,
    fileState: fileState,
  }
}

export function getMetadata(editor: EditorState): Array<ComponentMetadata> {
  if (editor.canvas.dragState == null) {
    return editor.jsxMetadataKILLME
  } else {
    return editor.canvas.dragState.metadata
  }
}

export interface ElementWarnings {
  widthOrHeightZero: boolean
  absoluteWithUnpositionedParent: boolean
  dynamicSceneChildWidthHeightPercentage: boolean
}

export const defaultElementWarnings: ElementWarnings = {
  widthOrHeightZero: false,
  absoluteWithUnpositionedParent: false,
  dynamicSceneChildWidthHeightPercentage: false,
}

export interface DerivedState {
  navigatorTargets: Array<TemplatePath>
  canvas: {
    descendantsOfHiddenInstances: Array<TemplatePath>
    controls: Array<HigherOrderControl>
    transientState: TransientCanvasState
  }
  elementWarnings: ComplexMap<TemplatePath, ElementWarnings>
}

function emptyDerivedState(editorState: EditorState): DerivedState {
  return {
    navigatorTargets: [],
    canvas: {
      descendantsOfHiddenInstances: [],
      controls: [],
      transientState: produceCanvasTransientState(editorState, false),
    },
    elementWarnings: emptyComplexMap(),
  }
}

export interface PersistentModel {
  appID?: string | null
  projectVersion: number
  projectContents: ProjectContentTreeRoot
  exportsInfo: ReadonlyArray<ExportsInfo>
  lastUsedFont: FontSettings | null
  hiddenInstances: Array<TemplatePath>
  openFiles: Array<EditorTab>
  selectedFile: EditorTab | null
  codeEditorErrors: {
    buildErrors: ErrorMessages
    lintErrors: ErrorMessages
  }
  codeEditorTheme: CodeEditorTheme
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
    projectVersion: second.projectVersion,
    projectContents: {
      ...first.projectContents,
      ...second.projectContents,
    },
    exportsInfo: second.exportsInfo,
    lastUsedFont: second.lastUsedFont,
    hiddenInstances: [...first.hiddenInstances, ...second.hiddenInstances],
    openFiles: second.openFiles,
    selectedFile: second.selectedFile,
    codeEditorErrors: second.codeEditorErrors,
    codeEditorTheme: second.codeEditorTheme,
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
  }
}

export function createNewProjectName(): string {
  const friendlyWordsPredicate =
    friendlyWords.predicates[Math.floor(Math.random() * friendlyWords.predicates.length)]
  const friendlyWordsObject =
    friendlyWords.objects[Math.floor(Math.random() * friendlyWords.objects.length)]
  return `${friendlyWordsPredicate}-${friendlyWordsObject}`
}

export function createEditorState(dispatch: EditorDispatch): EditorState {
  return {
    id: null,
    appID: null,
    projectName: createNewProjectName(),
    projectVersion: CURRENT_PROJECT_VERSION,
    isLoaded: false,
    openFiles: [],
    cursorPositions: {},
    selectedFile: null,
    spyMetadataKILLME: [],
    domMetadataKILLME: [],
    jsxMetadataKILLME: [],
    projectContents: {},
    codeResultCache: generateCodeResultCache(
      {},
      {},
      {},
      [],
      {},
      dispatch,
      'full-build',
      null,
      true,
    ),
    propertyControlsInfo: {},
    nodeModules: {
      skipDeepFreeze: true,
      files: {},
      projectFilesBuildResults: {},
      packageStatus: {},
    },
    selectedViews: [],
    highlightedViews: [],
    hiddenInstances: [],
    warnedInstances: [],
    mode: EditorModes.selectMode(),
    focusedPanel: 'canvas',
    keysPressed: {},
    openPopupId: null,
    toasts: [],
    cursorStack: {
      fixed: null,
      mouseOver: [],
    },
    leftMenu: {
      selectedTab: LeftMenuTab.ProjectStructure,
      expanded: true,
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
      layoutReversed: true,
      additionalControls: true,
    },
    canvas: {
      dragState: null, // TODO change dragState if editorMode changes
      visible: true,
      scale: 1,
      snappingThreshold: BaseSnappingThreshold,
      realCanvasOffset: { x: 20, y: 60 } as CanvasPoint,
      roundedCanvasOffset: { x: 20, y: 60 } as CanvasPoint,
      textEditor: null,
      selectionControlsVisible: true,
      animationsEnabled: true,
      highlightsEnabled: true,
      cursor: null,
      duplicationState: null,
      base64Blobs: {},
      mountCount: 0,
    },
    inspector: {
      visible: true,
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
    codeEditorTheme: DefaultTheme,
    safeMode: false,
    saveError: false,
  }
}

export type OriginalFrame = FrameAndTarget<LocalRectangle>

export interface OriginalCanvasAndLocalFrame {
  target: TemplatePath
  frame?: LocalRectangle
  canvasFrame?: CanvasRectangle
}

type EditorAndDerivedState = {
  editor: EditorState
  derived: DerivedState
}

export function getElementWarnings(
  rootMetadata: Array<ComponentMetadata>,
): ComplexMap<TemplatePath, ElementWarnings> {
  let result: ComplexMap<TemplatePath, ElementWarnings> = emptyComplexMap()
  MetadataUtils.walkMetadata(
    rootMetadata,
    (elementMetadata: ElementInstanceMetadata, parentMetadata: ElementInstanceMetadata | null) => {
      // Check to see if this element is collapsed in one dimension.
      const globalFrame = elementMetadata.globalFrame
      const widthOrHeightZero =
        globalFrame != null ? globalFrame.width === 0 || globalFrame.height === 0 : false

      // Identify if this element looks to be trying to position itself with "pins", but
      // the parent element isn't appropriately configured.
      let absoluteWithUnpositionedParent: boolean = false
      if (parentMetadata != null) {
        if (
          elementMetadata.specialSizeMeasurements.position === 'absolute' &&
          !elementMetadata.specialSizeMeasurements.immediateParentProvidesLayout
        ) {
          absoluteWithUnpositionedParent = true
        }
      }

      // Build the warnings object and add it to the map.
      const elementWarnings: ElementWarnings = {
        widthOrHeightZero: widthOrHeightZero,
        absoluteWithUnpositionedParent: absoluteWithUnpositionedParent,
        dynamicSceneChildWidthHeightPercentage: false,
      }
      result = addToComplexMap(toString, result, elementMetadata.templatePath, elementWarnings)
    },
  )
  fastForEach(rootMetadata, (scene) => {
    const elementWarnings: ElementWarnings = {
      widthOrHeightZero:
        scene.globalFrame != null
          ? scene.globalFrame.width === 0 || scene.globalFrame.height === 0
          : false,
      absoluteWithUnpositionedParent: false,
      dynamicSceneChildWidthHeightPercentage: isDynamicSceneChildWidthHeightPercentage(scene),
    }
    result = addToComplexMap(toString, result, scene.scenePath, elementWarnings)
  })
  return result
}

export function deriveState(
  editor: EditorState,
  oldDerivedState: DerivedState | null,
  uidsChanged: boolean,
): EditorAndDerivedState {
  const derivedState = oldDerivedState == null ? emptyDerivedState(editor) : oldDerivedState

  const componentKeys = keepDeepReferenceEqualityIfPossible(
    derivedState.navigatorTargets,
    MetadataUtils.createOrderedTemplatePathsFromElements(editor.jsxMetadataKILLME),
  )

  const derived: DerivedState = {
    navigatorTargets: componentKeys,
    canvas: {
      descendantsOfHiddenInstances: editor.hiddenInstances, // FIXME This has been dead for like ever
      controls: derivedState.canvas.controls,
      transientState: produceCanvasTransientState(editor, true),
    },
    elementWarnings: keepDeepReferenceEqualityIfPossible(
      oldDerivedState?.elementWarnings,
      getElementWarnings(getMetadata(editor)),
    ),
  }

  let selectedViews: Array<TemplatePath> = []

  const currentFilePath = getOpenUIJSFileKey(editor)
  const currentFile = getOpenUIJSFile(editor)
  if (uidsChanged && currentFile != null && currentFilePath != null) {
    const cursorPosition = editor.cursorPositions[currentFilePath]
    if (cursorPosition != null) {
      const { line } = cursorPosition

      const highlightBounds = getHighlightBoundsFromParseResult(currentFile.fileContents.parsed)
      const sortedHighlightBounds = Object.values(highlightBounds).sort(
        (a, b) => b.startLine - a.startLine,
      )
      const targets = sortedHighlightBounds
        .filter((bounds) => {
          // TS line numbers are zero based, monaco is 1-based
          return line >= bounds.startLine + 1 && line <= bounds.endLine + 1
        })
        .map((bound) => bound.uid)

      if (targets.length > 0) {
        const target = targets[0]
        Utils.fastForEach(componentKeys, (path) => {
          if (isInstancePath(path)) {
            const staticPath = MetadataUtils.dynamicPathToStaticPath(path)
            const uid = staticPath != null ? toUid(staticPath) : null
            if (uid === target) {
              selectedViews.push(path)
            }
          }
        })
      }
    }
  } else {
    selectedViews = editor.selectedViews
  }

  return {
    editor: {
      ...editor,
      selectedViews: selectedViews,
    },
    derived: derived,
  }
}

export function createCanvasModelKILLME(
  editorState: EditorState,
  derivedState: DerivedState,
): CanvasModel {
  return {
    controls: derivedState.canvas.controls,
    dragState: editorState.canvas.dragState,
    keysPressed: editorState.keysPressed,
    mode: editorState.mode,
    scale: editorState.canvas.scale,
    highlightedviews: editorState.highlightedViews,
    selectedViews: editorState.selectedViews,
    canvasOffset: editorState.canvas.roundedCanvasOffset,
    focusedPanel: editorState.focusedPanel,
    editorState: editorState,
  }
}

export function editorModelFromPersistentModel(
  persistentModel: PersistentModel,
  dispatch: EditorDispatch,
): EditorState {
  const npmDependencies = immediatelyResolvableDependenciesWithEditorRequirements(
    persistentModel.projectContents,
  )
  const editor: EditorState = {
    id: null,
    appID: persistentModel.appID ?? null,
    projectName: createNewProjectName(),
    projectVersion: persistentModel.projectVersion,
    isLoaded: false,
    openFiles: persistentModel.openFiles,
    cursorPositions: {},
    spyMetadataKILLME: [],
    domMetadataKILLME: [],
    jsxMetadataKILLME: [],
    codeResultCache: generateCodeResultCache(
      persistentModel.projectContents,
      {},
      {},
      [],
      {},
      dispatch,
      'full-build',
      null,
      true,
    ),
    projectContents: persistentModel.projectContents,
    propertyControlsInfo: getControlsForExternalDependencies(npmDependencies),
    nodeModules: {
      skipDeepFreeze: true,
      files: {},
      projectFilesBuildResults: {},
      packageStatus: {},
    },
    selectedViews: [],
    highlightedViews: [],
    hiddenInstances: persistentModel.hiddenInstances,
    warnedInstances: [],
    mode: EditorModes.selectMode(),
    focusedPanel: 'canvas',
    keysPressed: {},
    openPopupId: null,
    toasts: [],
    cursorStack: {
      fixed: null,
      mouseOver: [],
    },
    leftMenu: {
      selectedTab: LeftMenuTab.ProjectStructure,
      expanded: true,
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
      layoutReversed: true,
      additionalControls: true,
    },
    canvas: {
      dragState: null, // TODO change dragState if editorMode changes
      visible: true,
      scale: 1,
      snappingThreshold: BaseSnappingThreshold,
      realCanvasOffset: { x: 20, y: 60 } as CanvasPoint,
      roundedCanvasOffset: { x: 20, y: 60 } as CanvasPoint,
      textEditor: null,
      selectionControlsVisible: true,
      animationsEnabled: true,
      highlightsEnabled: true,
      cursor: null,
      duplicationState: null,
      base64Blobs: {},
      mountCount: 0,
    },
    inspector: {
      visible: true,
    },
    dependencyList: persistentModel.dependencyList,
    genericExternalResources: {
      minimised: true,
    },
    googleFontsResources: {
      minimised: true,
    },
    projectSettings: persistentModel.projectSettings,
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
    codeEditorTheme: persistentModel.codeEditorTheme,
    safeMode: false,
    saveError: false,
    selectedFile: Utils.optionalMap((tab) => {
      return {
        tab: tab,
        initialCursorPosition: null,
      }
    }, persistentModel.selectedFile),
    navigator: {
      dropTargetHint: {
        target: null,
        type: null,
      },
      collapsedViews: [],
      renamingTarget: null,
      minimised: persistentModel.navigator.minimised,
    },
    fileBrowser: {
      renamingTarget: null,
      minimised: persistentModel.fileBrowser.minimised,
    },
    codeEditorErrors: persistentModel.codeEditorErrors,
  }
  return editor
}

export function persistentModelFromEditorModel(editor: EditorState): PersistentModel {
  const selectedFile = getOpenEditorTab(editor)
  return {
    appID: editor.appID,
    projectVersion: editor.projectVersion,
    projectContents: editor.projectContents,
    exportsInfo: editor.codeResultCache.exportsInfo,
    lastUsedFont: editor.lastUsedFont,
    hiddenInstances: editor.hiddenInstances,
    openFiles: editor.openFiles,
    selectedFile: selectedFile,
    codeEditorErrors: editor.codeEditorErrors,
    codeEditorTheme: editor.codeEditorTheme,
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
  }
}

export function persistentModelForProjectContents(
  projectContents: ProjectContentTreeRoot,
): PersistentModel {
  const selectedTab: EditorTab = releaseNotesTab()

  return {
    appID: null,
    projectVersion: CURRENT_PROJECT_VERSION,
    projectContents: projectContents,
    exportsInfo: [],
    openFiles: [selectedTab],
    selectedFile: selectedTab,
    codeEditorErrors: {
      buildErrors: {},
      lintErrors: {},
    },
    codeEditorTheme: DefaultTheme,
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
    'main-ui': 'src/app.js',
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
    const mainUI = R.path(['utopia', 'main-ui'], packageJsonContents.value)
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
    return JSON.stringify(R.assocPath(['utopia', 'main-ui'], mainUI, parsedPackageJson), null, 2)
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
  targets: Array<TemplatePath>,
  editor: EditorState,
): boolean {
  const components = getOpenUtopiaJSXComponentsFromState(editor)
  return targets.some((target) => {
    const originType = MetadataUtils.getElementOriginType(components, target)
    switch (originType) {
      case 'unknown-element':
      case 'generated-static-definition-present':
        return true
      default:
        return false
    }
  })
}

export function getAllCodeEditorErrors(
  editor: EditorState,
  onlyFatal: boolean,
  skipTsErrors: boolean,
): Array<ErrorMessage> {
  const allLintErrors = getAllLintErrors(editor)
  const allBuildErrors = getAllBuildErrors(editor)
  const errorsAndWarnings = skipTsErrors ? allLintErrors : [...allBuildErrors, ...allLintErrors]
  if (onlyFatal) {
    return errorsAndWarnings.filter((error) => error.severity === 'fatal')
  } else {
    return errorsAndWarnings
  }
}

export function getAllBuildErrors(editor: EditorState) {
  return getAllErrorsFromFiles(editor.codeEditorErrors.buildErrors)
}

export function getAllLintErrors(editor: EditorState) {
  return getAllErrorsFromFiles(editor.codeEditorErrors.lintErrors)
}

export function getAllErrorsFromFiles(errorsInFiles: ErrorMessages) {
  return Utils.flatMapArray((filename) => errorsInFiles[filename], Object.keys(errorsInFiles))
}

export function parseFailureAsErrorMessages(
  fileName: string | null,
  parseResult: TextFile | null,
): Array<ErrorMessage> {
  if (parseResult == null || !isParseFailure(parseResult.fileContents.parsed)) {
    return []
  } else {
    const parseFailure = parseResult.fileContents.parsed
    const fileNameString = fileName || ''
    let errors: Array<ErrorMessage> = []
    if (parseFailure.diagnostics != null && parseFailure.diagnostics.length > 0) {
      errors.push(...parseFailure.diagnostics.map(diagnosticToErrorMessage))
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

export function reconstructJSXMetadata(editor: EditorState): Array<ComponentMetadata> {
  const uiFile = getOpenUIJSFile(editor)
  if (uiFile == null) {
    return editor.jsxMetadataKILLME
  } else {
    return foldParsedTextFile(
      (_) => editor.jsxMetadataKILLME,
      (success) => {
        const elementsByUID = getElementsByUIDFromTopLevelElements(success.topLevelElements)
        const mergedMetadata = MetadataUtils.mergeComponentMetadata(
          elementsByUID,
          editor.spyMetadataKILLME,
          editor.domMetadataKILLME,
        )
        return mergedMetadata
      },
      (_) => editor.jsxMetadataKILLME,
      uiFile.fileContents.parsed,
    )
  }
}

export function getStoryboardUID(openComponents: UtopiaJSXComponent[]): string | null {
  const possiblyStoryboard = openComponents.find(
    (component) => component.name === BakedInStoryboardVariableName,
  )
  if (possiblyStoryboard != null) {
    return getUtopiaID(possiblyStoryboard.rootElement)
  }
  return null
}

export function getStoryboardTemplatePath(
  openComponents: UtopiaJSXComponent[],
): StaticInstancePath | null {
  const possiblyStoryboard = openComponents.find(
    (component) => component.name === BakedInStoryboardVariableName,
  )
  if (possiblyStoryboard != null) {
    const uid = getUtopiaID(possiblyStoryboard.rootElement)
    return staticInstancePath(EmptyScenePathForStoryboard, [uid])
  }
  return null
}

export function getStoryboardTemplatePathFromEditorState(
  editorState: EditorState,
): StaticTemplatePath | null {
  const openComponents = getOpenUtopiaJSXComponentsFromState(editorState)
  return getStoryboardTemplatePath(openComponents)
}
