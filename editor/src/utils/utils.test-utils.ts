import { EditorDispatch } from '../components/editor/action-types'
import {
  createEditorState,
  DerivedState,
  deriveState,
  EditorState,
  getOpenUIJSFile,
  getSceneElementsFromParseSuccess,
  PersistentModel,
  persistentModelFromEditorModel,
  DefaultPackageJson,
  StoryboardFilePath,
} from '../components/editor/store/editor-state'
import * as TP from '../core/shared/template-path'
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
} from '../core/shared/element-template'
import { getUtopiaID } from '../core/model/element-template-utils'
import { jsxAttributesToProps } from '../core/shared/jsx-attributes'
import { getUtopiaJSXComponentsFromSuccess } from '../core/model/project-file-utils'
import { parseSuccess } from '../core/workers/common/project-file-utils'
import {
  sampleImportsForTests,
  sampleJsxComponentWithScene,
} from '../core/model/test-ui-js-file.test-utils'
import {
  RevisionsState,
  TemplatePath,
  ParseSuccess,
  foldParsedTextFile,
  textFile,
  textFileContents,
  unparsed,
  InstancePath,
  EmptyExportsDetail,
} from '../core/shared/project-file-types'
import { right } from '../core/shared/either'
import Utils from './utils'
import { canvasRectangle, localRectangle, RectangleInner } from '../core/shared/math-utils'
import {
  createSceneUidFromIndex,
  BakedInStoryboardUID,
  PathForSceneDataLabel,
  isSceneElement,
} from '../core/model/scene-utils'
import { NO_OP } from '../core/shared/utils'
import * as PP from '../core/shared/property-path'
import { mapArrayToDictionary, mapDropNulls } from '../core/shared/array-utils'
import { MapLike } from 'typescript'
import { contentsToTree } from '../components/assets'
import { defaultSceneElement } from '../components/editor/defaults'
import { emptyComments } from '../core/workers/parser-printer/parser-printer-comments'

export function delay<T>(time: number): Promise<T> {
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
        0,
      ),
    }),
  }

  return persistentModelFromEditorModel(editor)
}

export function createEditorStates(
  selectedViews: TemplatePath[] = [],
): {
  editor: EditorState
  derivedState: DerivedState
  dispatch: EditorDispatch
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

export function createFakeMetadataForParseSuccess(
  success: ParseSuccess,
): ElementInstanceMetadataMap {
  const utopiaComponents = getUtopiaJSXComponentsFromSuccess(success)
  const sceneElements = getSceneElementsFromParseSuccess(success)
  let elements: ElementInstanceMetadataMap = {}
  let storyboardChildren: InstancePath[] = []
  const storyboardTemplatePath = TP.instancePath(TP.emptyScenePath, [BakedInStoryboardUID])

  sceneElements.forEach((scene, index) => {
    const descendantsMetadata = createFakeMetadataForJSXElement(
      scene,
      storyboardTemplatePath,
      {},
      utopiaComponents,
      false,
      { x: 0, y: 0, width: 400, height: 400 },
    )

    descendantsMetadata.forEach((individualMetadata) => {
      const descendantPath = individualMetadata.templatePath
      elements[TP.toString(descendantPath)] = individualMetadata
      if (TP.isParentOf(storyboardTemplatePath, descendantPath)) {
        storyboardChildren.push(descendantPath)
      }
    })
  })

  elements[TP.toString(storyboardTemplatePath)] = createFakeMetadataForStoryboard(
    storyboardTemplatePath,
    storyboardChildren,
  )

  return elements
}

export function createFakeMetadataForComponents(
  topLevelElements: Array<TopLevelElement>,
): ElementInstanceMetadataMap {
  let elements: ElementInstanceMetadataMap = {}
  let storyboardChildren: InstancePath[] = []
  const storyboardTemplatePath = TP.instancePath(TP.emptyScenePath, [BakedInStoryboardUID])

  Utils.fastForEach(topLevelElements, (component, index) => {
    if (isUtopiaJSXComponent(component)) {
      const sceneUID = createSceneUidFromIndex(index)
      const componentUID = `${component.name}-${index}`
      const frame = { x: 0, y: 0, width: 100, height: 100 }
      const fakeScene = defaultSceneElement(
        sceneUID,
        { left: 0, top: 0, width: 100, height: 100 },
        `Scene ${index}`,
        [
          jsxElement(
            component.name,
            jsxAttributesFromMap({
              'data-uid': jsxAttributeValue(componentUID, emptyComments),
            }),
            [],
          ),
        ],
      )

      const descendantsMetadata = createFakeMetadataForJSXElement(
        fakeScene,
        storyboardTemplatePath,
        {},
        topLevelElements,
        false,
        frame,
      )

      descendantsMetadata.forEach((individualMetadata) => {
        const descendantPath = individualMetadata.templatePath
        elements[TP.toString(descendantPath)] = individualMetadata
        if (TP.isParentOf(storyboardTemplatePath, descendantPath)) {
          storyboardChildren.push(descendantPath)
        }
      })
    }
  })

  elements[TP.toString(storyboardTemplatePath)] = createFakeMetadataForStoryboard(
    storyboardTemplatePath,
    storyboardChildren,
  )

  return elements
}

function createFakeMetadataForJSXElement(
  element: JSXElementChild,
  rootPath: TemplatePath,
  parentScope: MapLike<any>,
  topLevelElements: Array<TopLevelElement>,
  focused: boolean,
  frame: RectangleInner = Utils.zeroRectangle,
): Array<ElementInstanceMetadata> {
  let elements: Array<ElementInstanceMetadata> = []
  if (isJSXElement(element)) {
    const elementID = getUtopiaID(element)
    const templatePath = TP.appendToPath(rootPath, elementID)
    const definedElsewhere = getDefinedElsewhereFromAttributes(element.props)
    const inScope = {
      ...mapArrayToDictionary(
        definedElsewhere,
        (elsewhere) => elsewhere,
        () => 'fake',
      ),
      ...parentScope,
    }
    const props = jsxAttributesToProps(inScope, element.props, Utils.NO_OP)
    const children = element.children.flatMap((child) =>
      createFakeMetadataForJSXElement(
        child,
        templatePath,
        {
          ...inScope,
          ...props,
        },
        topLevelElements,
        isSceneElement(element),
      ),
    )
    const childPaths = children.map((child) => child.templatePath)

    let rootElements: Array<InstancePath> = []
    if (focused) {
      const targetComponent = topLevelElements.find(
        (c) => isUtopiaJSXComponent(c) && c.name === element.name.baseVariable,
      )

      if (targetComponent != null && isUtopiaJSXComponent(targetComponent)) {
        const elementScenePath = TP.scenePathForElementAtPath(templatePath)

        const rootElementsMetadata = createFakeMetadataForJSXElement(
          targetComponent.rootElement,
          elementScenePath,
          {
            ...inScope,
            props: props,
          },
          topLevelElements,
          false,
        )

        elements.push(...rootElementsMetadata)
        rootElements = mapDropNulls((individualElementMetadata) => {
          const path = individualElementMetadata.templatePath
          return TP.isTopLevelInstancePath(path) && TP.isParentOf(elementScenePath, path)
            ? path
            : null
        }, rootElementsMetadata)
      }
    }

    elements.push({
      templatePath: templatePath,
      element: right(element),
      props: props,
      globalFrame: canvasRectangle(frame),
      localFrame: localRectangle(frame),
      children: childPaths,
      rootElements: rootElements,
      componentInstance: false,
      isEmotionOrStyledComponent: false,
      specialSizeMeasurements: emptySpecialSizeMeasurements,
      computedStyle: emptyComputedStyle,
      attributeMetadatada: emptyAttributeMetadatada,
      label: props[PP.toString(PathForSceneDataLabel)],
    })
    elements.push(...children)
  } else if (isJSXFragment(element)) {
    const children = element.children.flatMap((child) =>
      createFakeMetadataForJSXElement(child, rootPath, parentScope, topLevelElements, focused),
    )
    elements.push(...children)
  } else {
    throw new Error(`Not a JSX element ${element}`)
  }

  return elements
}

function createFakeMetadataForStoryboard(
  templatePath: InstancePath,
  children: Array<InstancePath>,
): ElementInstanceMetadata {
  return {
    globalFrame: canvasRectangle({ x: 0, y: 0, width: 0, height: 0 }),
    localFrame: localRectangle({ x: 0, y: 0, width: 0, height: 0 }),
    templatePath: templatePath,
    props: {},
    element: right(jsxTestElement('Storyboard', [], [])),
    children: children,
    rootElements: [],
    componentInstance: true,
    isEmotionOrStyledComponent: false,
    specialSizeMeasurements: emptySpecialSizeMeasurements,
    computedStyle: emptyComputedStyle,
    attributeMetadatada: emptyAttributeMetadatada,
    label: null,
  }
}

export function wait(timeout: number): Promise<void> {
  return new Promise((resolve) => {
    setTimeout(resolve, timeout)
  })
}
