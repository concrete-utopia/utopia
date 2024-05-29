import type { EditorDispatch } from '../components/editor/action-types'
import type {
  DerivedState,
  EditorState,
  PersistentModel,
} from '../components/editor/store/editor-state'
import {
  createEditorState,
  deriveState,
  getOpenUIJSFile,
  persistentModelFromEditorModel,
  DefaultPackageJson,
  StoryboardFilePath,
} from '../components/editor/store/editor-state'
import * as EP from '../core/shared/element-path'
import type {
  ElementInstanceMetadata,
  JSXElementChild,
  TopLevelElement,
  ElementInstanceMetadataMap,
  JSXElement,
} from '../core/shared/element-template'
import {
  emptySpecialSizeMeasurements,
  isJSXElement,
  isUtopiaJSXComponent,
  emptyComputedStyle,
  isJSXFragment,
  emptyJsxMetadata,
  emptyAttributeMetadata,
  jsxTestElement,
  getDefinedElsewhereFromAttributes,
  jsxElement,
  jsxAttributesFromMap,
  jsExpressionValue,
  getJSXElementNameAsString,
  walkElements,
  emptyComments,
} from '../core/shared/element-template'
import { jsxAttributesToProps } from '../core/shared/jsx-attributes'
import { getUtopiaJSXComponentsFromSuccess } from '../core/model/project-file-utils'
import {
  sampleImportsForTests,
  sampleJsxComponentWithScene,
} from '../core/model/test-ui-js-file.test-utils'
import type { ElementPath, ParseSuccess } from '../core/shared/project-file-types'
import {
  RevisionsState,
  foldParsedTextFile,
  textFile,
  textFileContents,
  unparsed,
  EmptyExportsDetail,
  parseSuccess,
} from '../core/shared/project-file-types'
import { foldEither, forEachLeft, right } from '../core/shared/either'
import Utils from './utils'
import type { SimpleRectangle } from '../core/shared/math-utils'
import { canvasRectangle, localRectangle, negate, offsetRect } from '../core/shared/math-utils'
import {
  createSceneUidFromIndex,
  BakedInStoryboardUID,
  PathForSceneDataLabel,
} from '../core/model/scene-utils'
import { NO_OP } from '../core/shared/utils'
import * as PP from '../core/shared/property-path'
import { mapArrayToDictionary } from '../core/shared/array-utils'
import type { MapLike } from 'typescript'
import { contentsToTree } from '../components/assets'
import { defaultSceneElement } from '../components/editor/defaults'
import { objectMap } from '../core/shared/object-utils'
import type { StrategyState } from '../components/canvas/canvas-strategies/interaction-state'
import { createEmptyStrategyState } from '../components/canvas/canvas-strategies/interaction-state'
import type { EditorRenderResult } from '../components/canvas/ui-jsx.test-utils'
import { selectComponents } from '../components/editor/actions/action-creators'
import { act, fireEvent, queryByAttribute } from '@testing-library/react'
import type { FeatureName } from './feature-switches'
import { isFeatureEnabled, setFeatureEnabled } from './feature-switches'
import { getUtopiaID } from '../core/shared/uid-utils'
import { unpatchedCreateRemixDerivedDataMemo } from '../components/editor/store/remix-derived-data'
import { getCanvasRectangleFromElement } from '../core/shared/dom-utils'
import { CanvasContainerID } from '../components/canvas/canvas-types'
import { CanvasToolbarSearchTestID } from '../components/editor/canvas-toolbar'
import { MetadataUtils } from '../core/model/element-metadata-utils'
import { editorStateToElementChildOptic } from '../core/model/common-optics'
import { toFirst } from '../core/shared/optics/optic-utilities'
import { emptyUiJsxCanvasContextData } from '../components/canvas/ui-jsx-canvas'
import type { RenderContext } from '../components/canvas/ui-jsx-canvas-renderer/ui-jsx-canvas-element-renderer-utils'
import { componentPickerFilterInputTestId } from '../components/navigator/navigator-item/component-picker'

export const testRenderContext: RenderContext = {
  rootScope: {},
  parentComponentInputProps: {},
  requireResult: Utils.NO_OP,
  hiddenInstances: [],
  displayNoneInstances: [],
  fileBlobs: {},
  validPaths: new Set(),
  reactChildren: undefined,
  metadataContext: emptyUiJsxCanvasContextData(),
  updateInvalidatedPaths: Utils.NO_OP,
  jsxFactoryFunctionName: null,
  shouldIncludeCanvasRootInTheSpy: false,
  filePath: 'test.js',
  imports: {},
  code: '',
  highlightBounds: null,
  editedText: null,
  variablesInScope: {},
  filePathMappings: [],
}

export function delay(time: number): Promise<void> {
  return new Promise((resolve) => setTimeout(resolve, time))
}

export function createPersistentModel(): PersistentModel {
  const editor: EditorState = {
    ...createEditorState(NO_OP),
    projectContents: contentsToTree({
      [StoryboardFilePath]: textFile(
        textFileContents(
          '',
          parseSuccess(
            sampleImportsForTests,
            sampleJsxComponentWithScene,
            {},
            null,
            null,
            EmptyExportsDetail,
            {},
          ),
          RevisionsState.ParsedAhead,
        ),
        null,
        null,
        0,
      ),
    }),
  }

  return persistentModelFromEditorModel(editor)
}

export interface CreateEditorStatesResult {
  editor: EditorState
  derivedState: DerivedState
  dispatch: EditorDispatch
  strategyState: StrategyState
}

export function createEditorStates(selectedViews: ElementPath[] = []): CreateEditorStatesResult {
  const editor: EditorState = {
    ...createEditorState(NO_OP),
    projectContents: contentsToTree({
      '/package.json': textFile(
        textFileContents(
          JSON.stringify(DefaultPackageJson, null, 2),
          unparsed,
          RevisionsState.CodeAhead,
        ),
        null,
        null,
        0,
      ),
      [StoryboardFilePath]: textFile(
        textFileContents(
          '',
          parseSuccess(
            sampleImportsForTests,
            sampleJsxComponentWithScene,
            {},
            null,
            null,
            EmptyExportsDetail,
            {},
          ),
          RevisionsState.ParsedAhead,
        ),
        null,
        null,
        0,
      ),
    }),
    selectedViews: selectedViews,
  }
  const derivedState = deriveState(editor, null, 'unpatched', unpatchedCreateRemixDerivedDataMemo)
  const componentMetadata = createFakeMetadataForEditor(editor)
  return {
    editor: {
      ...editor,
      jsxMetadata: componentMetadata,
    },
    derivedState: derivedState,
    strategyState: createEmptyStrategyState({}, {}, {}),
    dispatch: Utils.NO_OP,
  }
}

export function createFakeMetadataForEditor(editor: EditorState): ElementInstanceMetadataMap {
  const openUiJsFile = getOpenUIJSFile(editor)
  if (openUiJsFile == null) {
    return emptyJsxMetadata
  } else {
    const contents = openUiJsFile.fileContents.parsed
    return foldParsedTextFile(
      (_) => emptyJsxMetadata,
      createFakeMetadataForParseSuccess,
      (_) => emptyJsxMetadata,
      contents,
    )
  }
}

export function withFakeMetadataForEditor(editor: EditorState): EditorState {
  const componentMetadata = createFakeMetadataForEditor(editor)
  return {
    ...editor,
    jsxMetadata: componentMetadata,
  }
}

export function createFakeMetadataForParseSuccess(
  success: ParseSuccess,
): ElementInstanceMetadataMap {
  const utopiaComponents = getUtopiaJSXComponentsFromSuccess(success)
  let sceneElements: Array<JSXElement> = []
  walkElements(success.topLevelElements, (elementChild) => {
    if (isJSXElement(elementChild) && elementChild.name.baseVariable === 'Scene') {
      sceneElements.push(elementChild)
    }
  })
  let elements: ElementInstanceMetadataMap = {}
  const storyboardElementPath = EP.elementPath([[BakedInStoryboardUID]])

  sceneElements.forEach((scene, index) => {
    const descendantsMetadata = createFakeMetadataForJSXElement(
      scene,
      storyboardElementPath,
      {},
      utopiaComponents,
      false,
      false,
      { x: 0, y: 0, width: 400, height: 400 },
    )

    descendantsMetadata.forEach((individualMetadata) => {
      const descendantPath = individualMetadata.elementPath
      elements[EP.toString(descendantPath)] = individualMetadata
    })
  })

  elements[EP.toString(storyboardElementPath)] =
    createFakeMetadataForStoryboard(storyboardElementPath)

  return elements
}

export function createFakeMetadataForComponents(
  topLevelElements: Array<TopLevelElement>,
): ElementInstanceMetadataMap {
  let elements: ElementInstanceMetadataMap = {}
  const storyboardElementPath = EP.elementPath([[BakedInStoryboardUID]])

  Utils.fastForEach(topLevelElements, (component, index) => {
    if (isUtopiaJSXComponent(component)) {
      const sceneUID = createSceneUidFromIndex(index)
      const componentUID = `${component.name ?? 'default'}-${index}`
      const frame = { x: 0, y: 0, width: 100, height: 100 }
      const fakeScene = defaultSceneElement(
        sceneUID,
        { left: 0, top: 0, width: 100, height: 100 },
        `Scene ${index}`,
        [
          jsxElement(
            component.name ?? 'default',
            componentUID,
            jsxAttributesFromMap({
              'data-uid': jsExpressionValue(componentUID, emptyComments),
            }),
            [],
          ),
        ],
      )

      const descendantsMetadata = createFakeMetadataForJSXElement(
        fakeScene,
        storyboardElementPath,
        {},
        topLevelElements,
        false,
        false,
        frame,
      )

      descendantsMetadata.forEach((individualMetadata) => {
        const descendantPath = individualMetadata.elementPath
        elements[EP.toString(descendantPath)] = individualMetadata
      })
    }
  })

  elements[EP.toString(storyboardElementPath)] =
    createFakeMetadataForStoryboard(storyboardElementPath)

  return elements
}

function createFakeMetadataForJSXElement(
  element: JSXElementChild,
  rootPath: ElementPath,
  parentScope: MapLike<any>,
  topLevelElements: Array<TopLevelElement>,
  focused: boolean,
  rootOfInstance: boolean,
  frame: SimpleRectangle = Utils.zeroRectangle,
  textContents: string | null = null,
): Array<ElementInstanceMetadata> {
  let elements: Array<ElementInstanceMetadata> = []
  if (isJSXElement(element)) {
    const elementID = getUtopiaID(element)
    const elementPath = rootOfInstance
      ? EP.appendNewElementPath(rootPath, elementID)
      : EP.appendToPath(rootPath, elementID)
    const definedElsewhere = getDefinedElsewhereFromAttributes(element.props)
    const inScope = {
      ...mapArrayToDictionary(
        definedElsewhere,
        (elsewhere) => elsewhere,
        () => 'fake',
      ),
      ...parentScope,
    }
    const props = jsxAttributesToProps(
      inScope,
      element.props,
      null,
      testRenderContext,
      undefined,
      null,
    )
    const children = element.children.flatMap((child) =>
      createFakeMetadataForJSXElement(
        child,
        elementPath,
        {
          ...inScope,
          ...props,
        },
        topLevelElements,
        element.name.baseVariable === 'Scene',
        false,
      ),
    )

    if (focused) {
      const targetComponent = topLevelElements.find(
        (c) => isUtopiaJSXComponent(c) && c.name === element.name.baseVariable,
      )

      if (targetComponent != null && isUtopiaJSXComponent(targetComponent)) {
        const elementScenePath = elementPath

        const rootElementsMetadata = createFakeMetadataForJSXElement(
          targetComponent.rootElement,
          elementScenePath,
          {
            ...inScope,
            props: props,
          },
          topLevelElements,
          false,
          true,
        )

        elements.push(...rootElementsMetadata)
      }
    }

    elements.push({
      elementPath: elementPath,
      element: right(element),
      globalFrame: canvasRectangle(frame),
      localFrame: localRectangle(frame),
      nonRoundedGlobalFrame: canvasRectangle(frame),
      componentInstance: false,
      isEmotionOrStyledComponent: false,
      specialSizeMeasurements: emptySpecialSizeMeasurements,
      computedStyle: emptyComputedStyle,
      attributeMetadatada: emptyAttributeMetadata,
      label: props[PP.toString(PathForSceneDataLabel)],
      importInfo: null,
      conditionValue: 'not-a-conditional',
      textContent: textContents,
      earlyReturn: null,
      assignedToProp: null,
    })
    elements.push(...children)
  } else if (isJSXFragment(element)) {
    const children = element.children.flatMap((child) =>
      createFakeMetadataForJSXElement(
        child,
        rootPath,
        parentScope,
        topLevelElements,
        focused,
        rootOfInstance,
      ),
    )
    elements.push(...children)
  } else {
    throw new Error(`Not a JSX element ${JSON.stringify(element)}`)
  }

  return elements
}

function createFakeMetadataForStoryboard(elementPath: ElementPath): ElementInstanceMetadata {
  return {
    globalFrame: canvasRectangle({ x: 0, y: 0, width: 0, height: 0 }),
    localFrame: localRectangle({ x: 0, y: 0, width: 0, height: 0 }),
    nonRoundedGlobalFrame: canvasRectangle({ x: 0, y: 0, width: 0, height: 0 }),
    elementPath: elementPath,
    element: right(jsxTestElement('Storyboard', [], [])),
    componentInstance: true,
    isEmotionOrStyledComponent: false,
    specialSizeMeasurements: emptySpecialSizeMeasurements,
    computedStyle: emptyComputedStyle,
    attributeMetadatada: emptyAttributeMetadata,
    label: null,
    importInfo: null,
    conditionValue: 'not-a-conditional',
    textContent: null,
    earlyReturn: null,
    assignedToProp: null,
  }
}

export function wait(timeout: number): Promise<void> {
  return new Promise((resolve) => {
    setTimeout(resolve, timeout)
  })
}

interface SimplifiedMetadata {
  name: string
}

type SimplifiedMetadataMap = { [key: string]: SimplifiedMetadata }

export function simplifiedMetadata(elementMetadata: ElementInstanceMetadata): SimplifiedMetadata {
  return {
    name: foldEither(
      (name) => name,
      (element) =>
        isJSXElement(element) ? getJSXElementNameAsString(element.name) : 'not-jsx-element',
      elementMetadata.element,
    ),
  }
}

export function simplifiedMetadataMap(metadata: ElementInstanceMetadataMap): SimplifiedMetadataMap {
  const sanitizedSpyData = objectMap((elementMetadata, key) => {
    const elementPathAsReportedBySpy = EP.toString(elementMetadata.elementPath)
    if (elementPathAsReportedBySpy !== key) {
      throw new Error(`The reported template path should match what was used as key`)
    }

    return simplifiedMetadata(elementMetadata)
  }, metadata)
  return sanitizedSpyData
}

export function slightlyOffsetPointBecauseVeryWeirdIssue(point: { x: number; y: number }): {
  x: number
  y: number
} {
  // FIXME when running in headless chrome, the result of getBoundingClientRect will be slightly
  // offset for some unknown reason, meaning the inserted element will be 1 pixel of in each dimension
  return { x: point.x - 0.001, y: point.y - 0.001 }
}

async function expectNUndoStepsNSaves(
  editor: EditorRenderResult,
  undoSteps: number,
  saves: number,
  action: () => Promise<void>,
): Promise<void> {
  const saveCountBefore = editor.getEditorState().saveCountThisSession
  const historySizeBefore = editor.getEditorState().history.previous.length

  await action()

  const historySizeAfter = editor.getEditorState().history.previous.length
  const saveCountAfter = editor.getEditorState().saveCountThisSession

  expect(historySizeAfter - historySizeBefore).toEqual(undoSteps)
  expect(saveCountAfter - saveCountBefore).toEqual(saves)
}

export async function expectNoAction(
  editor: EditorRenderResult,
  action: () => Promise<void>,
): Promise<void> {
  return expectNUndoStepsNSaves(editor, 0, 0, action)
}

// FIXME We should really only be expecting a single save, but we currently save
// on changes to the parsed model as well as the printed code
export async function expectSingleUndo2Saves(
  editor: EditorRenderResult,
  action: () => Promise<void>,
): Promise<void> {
  return expectNUndoStepsNSaves(editor, 1, 2, action)
}

export async function expectSingleUndoNSaves(
  editor: EditorRenderResult,
  saves: number,
  action: () => Promise<void>,
): Promise<void> {
  return expectNUndoStepsNSaves(editor, 1, saves, action)
}

export async function selectComponentsForTest(
  editor: EditorRenderResult,
  paths: Array<ElementPath>,
): Promise<void> {
  for (const path of paths) {
    const element = MetadataUtils.findElementByElementPath(
      editor.getEditorState().editor.jsxMetadata,
      path,
    )
    if (element == null) {
      const underlyingElement = toFirst(
        editorStateToElementChildOptic(path),
        editor.getEditorState().editor,
      )
      forEachLeft(underlyingElement, () => {
        throw new Error(`Could not find ${EP.toString(path)} in metadata or in project contents.`)
      })
    }
  }
  await editor.dispatch([selectComponents(paths, false)], true)
  await editor.getDispatchFollowUpActionsFinished()
}

export async function hoverControlWithCheck(
  editor: EditorRenderResult,
  controlTestId: string,
  check: () => Promise<void>,
): Promise<void> {
  const control = (await editor.renderedDOM.findByTestId(controlTestId)) as HTMLInputElement
  fireEvent.mouseEnter(control)
  await editor.getDispatchFollowUpActionsFinished()
  await check()
}

export function setFeatureForBrowserTestsUseInDescribeBlockOnly(
  featureName: FeatureName,
  newValue: boolean,
): void {
  let originalFSValue: boolean = false
  before(() => {
    originalFSValue = isFeatureEnabled(featureName)
    setFeatureEnabled(featureName, newValue)
  })

  after(() => {
    setFeatureEnabled(featureName, originalFSValue)
  })
}

export function setFeatureForUnitTestsUseInDescribeBlockOnly(
  featureName: FeatureName,
  newValue: boolean,
): void {
  let originalFSValue: boolean = false
  beforeEach(() => {
    originalFSValue = isFeatureEnabled(featureName)
    setFeatureEnabled(featureName, newValue)
  })

  afterEach(() => {
    setFeatureEnabled(featureName, originalFSValue)
  })
}

function getElementsWithTestId(editor: EditorRenderResult, testId: string): HTMLElement[] {
  return editor.renderedDOM.queryAllByTestId(testId)
}

export const expectElementWithTestIdToBeRendered = (
  editor: EditorRenderResult,
  testId: string,
): void => {
  const foundElements = getElementsWithTestId(editor, testId)
  expect(foundElements.length).toEqual(1)
  expect(foundElements[0]?.style.display).not.toEqual('none')
}

export const expectElementWithTestIdToBeRenderedWithDisplayNone = (
  editor: EditorRenderResult,
  testId: string,
): void => {
  const foundElements = getElementsWithTestId(editor, testId)
  expect(foundElements.length).toEqual(1)
  expect(foundElements[0]?.style.display).toEqual('none')
}

export const expectElementWithTestIdNotToBeRendered = (
  editor: EditorRenderResult,
  testId: string,
): void => expect(getElementsWithTestId(editor, testId).length).toEqual(0)

export function boundingClientRectToCanvasRectangle(
  result: EditorRenderResult,
  elementBounds: DOMRect,
) {
  const canvasRootContainer = result.renderedDOM.getByTestId(CanvasContainerID)
  const canvasScale = result.getEditorState().editor.canvas.scale
  const canvasRootRectangle = getCanvasRectangleFromElement(
    canvasRootContainer,
    canvasScale,
    'without-content',
    'nearest-half',
  )
  const canvasBounds = offsetRect(canvasRectangle(elementBounds), negate(canvasRootRectangle))

  return canvasBounds
}

export async function searchInFloatingMenu(editor: EditorRenderResult, query: string) {
  const floatingMenu = editor.renderedDOM.getByTestId(CanvasToolbarSearchTestID)
  const searchBox = queryByAttribute('type', floatingMenu, 'text')!

  await act(() => {
    fireEvent.focus(searchBox)
    fireEvent.change(searchBox, { target: { value: query } })
    fireEvent.blur(searchBox)
    fireEvent.keyDown(searchBox, { key: 'Enter', keyCode: 13, metaKey: true })
  })
}

export async function searchInComponentPicker(editor: EditorRenderResult, query: string) {
  const searchBox = editor.renderedDOM.getByTestId(componentPickerFilterInputTestId)

  await act(() => {
    fireEvent.focus(searchBox)
    fireEvent.change(searchBox, { target: { value: query } })
    fireEvent.blur(searchBox)
    fireEvent.keyDown(searchBox, { key: 'Enter', keyCode: 13, metaKey: true })
  })
}
