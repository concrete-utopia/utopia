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
  ElementPath,
  ParseSuccess,
  foldParsedTextFile,
  textFile,
  textFileContents,
  unparsed,
  EmptyExportsDetail,
} from '../core/shared/project-file-types'
import { foldEither, right } from '../core/shared/either'
import Utils from './utils'
import { canvasRectangle, localRectangle, RectangleInner } from '../core/shared/math-utils'
import {
  createSceneUidFromIndex,
  BakedInStoryboardUID,
  PathForSceneDataLabel,
  isSceneElementIgnoringImports,
} from '../core/model/scene-utils'
import { NO_OP } from '../core/shared/utils'
import * as PP from '../core/shared/property-path'
import { mapArrayToDictionary, mapDropNulls } from '../core/shared/array-utils'
import { MapLike } from 'typescript'
import { contentsToTree } from '../components/assets'
import { defaultSceneElement } from '../components/editor/defaults'
import { emptyComments } from '../core/workers/parser-printer/parser-printer-comments'
import { objectMap } from '../core/shared/object-utils'

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
  selectedViews: ElementPath[] = [],
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
  let storyboardChildren: ElementPath[] = []
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
      if (EP.isParentOf(storyboardElementPath, descendantPath)) {
        storyboardChildren.push(descendantPath)
      }
    })
  })

  elements[EP.toString(storyboardElementPath)] = createFakeMetadataForStoryboard(
    storyboardElementPath,
    storyboardChildren,
  )

  return elements
}

export function createFakeMetadataForComponents(
  topLevelElements: Array<TopLevelElement>,
): ElementInstanceMetadataMap {
  let elements: ElementInstanceMetadataMap = {}
  let storyboardChildren: ElementPath[] = []
  const storyboardElementPath = EP.elementPath([[BakedInStoryboardUID]])

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
        if (EP.isParentOf(storyboardElementPath, descendantPath)) {
          storyboardChildren.push(descendantPath)
        }
      })
    }
  })

  elements[EP.toString(storyboardElementPath)] = createFakeMetadataForStoryboard(
    storyboardElementPath,
    storyboardChildren,
  )

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
    const props = jsxAttributesToProps(inScope, element.props, Utils.NO_OP)
    const children = element.children.flatMap((child) =>
      createFakeMetadataForJSXElement(
        child,
        elementPath,
        {
          ...inScope,
          ...props,
        },
        topLevelElements,
        isSceneElementIgnoringImports(element),
        false,
      ),
    )
    const childPaths = children.map((child) => child.elementPath)

    let rootElements: Array<ElementPath> = []
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
        rootElements = mapDropNulls((individualElementMetadata) => {
          const path = individualElementMetadata.elementPath
          return EP.isRootElementOfInstance(path) && EP.isParentOf(elementScenePath, path)
            ? path
            : null
        }, rootElementsMetadata)
      }
    }

    elements.push({
      elementPath: elementPath,
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

function createFakeMetadataForStoryboard(
  elementPath: ElementPath,
  children: Array<ElementPath>,
): ElementInstanceMetadata {
  return {
    globalFrame: canvasRectangle({ x: 0, y: 0, width: 0, height: 0 }),
    localFrame: localRectangle({ x: 0, y: 0, width: 0, height: 0 }),
    elementPath: elementPath,
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

interface SimplifiedMetadata {
  children: string[]
  name: string
  rootElements: string[]
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
    children: elementMetadata.children.map(EP.toString),
    rootElements: elementMetadata.rootElements.map(EP.toString),
  }
}

export function simplifiedMetadataMap(metadata: ElementInstanceMetadataMap): SimplifiedMetadataMap {
  const sanitizedSpyData = objectMap((elementMetadata, key) => {
    const elementPathAsReportedBySpy = EP.toString(elementMetadata.elementPath)
    if (elementPathAsReportedBySpy !== key) {
      fail(`The reported template path should match what was used as key`)
    }

    return simplifiedMetadata(elementMetadata)
  }, metadata)
  return sanitizedSpyData
}

export function domWalkerMetadataToSimplifiedMetadataMap(
  metadata: Array<ElementInstanceMetadata>,
): SimplifiedMetadataMap {
  return mapArrayToDictionary(
    metadata,
    (elementMetadata: ElementInstanceMetadata) => EP.toString(elementMetadata.elementPath),
    simplifiedMetadata,
  )
}
