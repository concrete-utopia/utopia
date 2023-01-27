import { EditorDispatch } from '../components/editor/action-types'
import {
  createEditorState,
  DerivedState,
  deriveState,
  EditorState,
  getOpenUIJSFile,
  PersistentModel,
  persistentModelFromEditorModel,
  DefaultPackageJson,
  StoryboardFilePath,
} from '../components/editor/store/editor-state'
import * as EP from '../core/shared/element-path'
import {
  ElementInstanceMetadata,
  emptySpecialSizeMeasurements,
  isJSXElement,
  isUtopiaJSXComponent,
  JSXElementChild,
  TopLevelElement,
  emptyComputedStyle,
  isJSXFragment,
  ElementInstanceMetadataMap,
  emptyJsxMetadata,
  emptyAttributeMetadatada,
  jsxTestElement,
  getDefinedElsewhereFromAttributes,
  jsxElement,
  jsxAttributesFromMap,
  jsxAttributeValue,
  getJSXElementNameAsString,
  JSXElement,
  walkElements,
  emptyComments,
} from '../core/shared/element-template'
import { getUtopiaID } from '../core/model/element-template-utils'
import { jsxAttributesToProps } from '../core/shared/jsx-attributes'
import { getUtopiaJSXComponentsFromSuccess } from '../core/model/project-file-utils'
import {
  sampleImportsForTests,
  sampleJsxComponentWithScene,
} from '../core/model/test-ui-js-file.test-utils'
import {
  RevisionsState,
  ElementPath,
  ParseSuccess,
  foldParsedTextFile,
  textFile,
  textFileContents,
  unparsed,
  EmptyExportsDetail,
  parseSuccess,
} from '../core/shared/project-file-types'
import { foldEither, right } from '../core/shared/either'
import Utils from './utils'
import { canvasRectangle, localRectangle, RectangleInner } from '../core/shared/math-utils'
import {
  createSceneUidFromIndex,
  BakedInStoryboardUID,
  PathForSceneDataLabel,
} from '../core/model/scene-utils'
import { NO_OP } from '../core/shared/utils'
import * as PP from '../core/shared/property-path'
import { mapArrayToDictionary, mapDropNulls } from '../core/shared/array-utils'
import { MapLike } from 'typescript'
import { contentsToTree } from '../components/assets'
import { defaultSceneElement } from '../components/editor/defaults'
import { objectMap } from '../core/shared/object-utils'
import {
  createEmptyStrategyState,
  StrategyState,
} from '../components/canvas/canvas-strategies/interaction-state'
import { EditorRenderResult } from '../components/canvas/ui-jsx.test-utils'

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

export function createEditorStates(selectedViews: ElementPath[] = []): {
  editor: EditorState
  derivedState: DerivedState
  dispatch: EditorDispatch
  strategyState: StrategyState
} {
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
  const derivedState = deriveState(editor, null)
  const componentMetadata = createFakeMetadataForEditor(editor)
  return {
    editor: {
      ...editor,
      jsxMetadata: componentMetadata,
    },
    derivedState: derivedState,
    strategyState: createEmptyStrategyState({}, {}),
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
              'data-uid': jsxAttributeValue(componentUID, emptyComments),
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
  frame: RectangleInner = Utils.zeroRectangle,
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
    const props = jsxAttributesToProps('test.js', inScope, element.props, Utils.NO_OP)
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
      componentInstance: false,
      isEmotionOrStyledComponent: false,
      specialSizeMeasurements: emptySpecialSizeMeasurements,
      computedStyle: emptyComputedStyle,
      attributeMetadatada: emptyAttributeMetadatada,
      label: props[PP.toString(PathForSceneDataLabel)],
      importInfo: null,
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
    throw new Error(`Not a JSX element ${element}`)
  }

  return elements
}

function createFakeMetadataForStoryboard(elementPath: ElementPath): ElementInstanceMetadata {
  return {
    globalFrame: canvasRectangle({ x: 0, y: 0, width: 0, height: 0 }),
    localFrame: localRectangle({ x: 0, y: 0, width: 0, height: 0 }),
    elementPath: elementPath,
    element: right(jsxTestElement('Storyboard', [], [])),
    componentInstance: true,
    isEmotionOrStyledComponent: false,
    specialSizeMeasurements: emptySpecialSizeMeasurements,
    computedStyle: emptyComputedStyle,
    attributeMetadatada: emptyAttributeMetadatada,
    label: null,
    importInfo: null,
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

export async function expectSingleUndoStep(
  editor: EditorRenderResult,
  action: () => Promise<void>,
): Promise<void> {
  const historySizeBefore = editor.getEditorState().history.previous.length
  await action()
  const historySizeAfter = editor.getEditorState().history.previous.length
  expect(historySizeAfter - historySizeBefore).toEqual(1)
}
