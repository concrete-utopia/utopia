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
  ComponentMetadata,
  ElementInstanceMetadata,
  emptySpecialSizeMeasurements,
  isJSXElement,
  isUtopiaJSXComponent,
  JSXElementChild,
  TopLevelElement,
  emptyComputedStyle,
  walkElements,
  isJSXFragment,
  JSXMetadata,
  emptyJsxMetadata,
  ElementInstanceMetadataMap,
  jsxMetadata,
  getJSXElementNameAsString,
  walkElement,
  getJSXAttribute,
  getJSXAttributeForced,
  emptyAttributeMetadatada,
} from '../core/shared/element-template'
import { getUtopiaID } from '../core/model/element-template-utils'
import { jsxAttributesToProps, jsxSimpleAttributeToValue } from '../core/shared/jsx-attributes'
import { getUtopiaJSXComponentsFromSuccess } from '../core/model/project-file-utils'
import { parseSuccess } from '../core/workers/common/project-file-utils'
import { sampleImportsForTests, sampleJsxComponentWithScene } from '../core/model/test-ui-js-file'
import {
  RevisionsState,
  TemplatePath,
  isParseFailure,
  ParseSuccess,
  foldParsedTextFile,
  textFile,
  textFileContents,
  unparsed,
  isTextFile,
  isParseSuccess,
  ProjectFile,
  InstancePath,
  EmptyExportsDetail,
  StaticElementPath,
} from '../core/shared/project-file-types'
import { right, eitherToMaybe, isLeft } from '../core/shared/either'
import Utils from './utils'
import { CanvasRectangle, LocalRectangle } from '../core/shared/math-utils'
import {
  createSceneUidFromIndex,
  BakedInStoryboardUID,
  BakedInStoryboardVariableName,
  PathForSceneComponent,
  PathForSceneDataLabel,
  PathForSceneDataUid,
  PathForResizeContent,
} from '../core/model/scene-utils'
import { fastForEach, NO_OP } from '../core/shared/utils'
import * as PP from '../core/shared/property-path'
import { getSimpleAttributeAtPath } from '../core/model/element-metadata-utils'
import { mapArrayToDictionary } from '../core/shared/array-utils'
import { MapLike } from 'typescript'
import { contentsToTree } from '../components/assets'
import { forceNotNull } from '../core/shared/optional-utils'

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
      jsxMetadataKILLME: componentMetadata,
    },
    derivedState: derivedState,
    dispatch: Utils.NO_OP,
  }
}

export function createFakeMetadataForEditor(editor: EditorState): JSXMetadata {
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

export function createFakeMetadataForParseSuccess(success: ParseSuccess): JSXMetadata {
  const utopiaComponents = getUtopiaJSXComponentsFromSuccess(success)
  const sceneElements = getSceneElementsFromParseSuccess(success)
  let elements: ElementInstanceMetadataMap = {}

  const components: ComponentMetadata[] = sceneElements.map((scene, index) => {
    const props = mapArrayToDictionary(
      scene.props,
      (entry) => entry.key,
      (entry) => {
        const attr = entry.value
        const simpleValue = jsxSimpleAttributeToValue(attr)
        if (isLeft(simpleValue) && attr.type === 'ATTRIBUTE_OTHER_JAVASCRIPT') {
          return attr.javascript
        } else {
          return eitherToMaybe(simpleValue)
        }
      },
    )
    const component = utopiaComponents.find(
      (c) => c.name === props.component && isUtopiaJSXComponent(c),
    )
    const sceneResizesContent =
      Utils.path<boolean>(PP.getElements(PathForResizeContent), props) ?? true
    let rootElements: Array<InstancePath> = []
    if (component != null) {
      const elementMetadata = createFakeMetadataForJSXElement(
        component.rootElement,
        TP.scenePath([BakedInStoryboardUID, createSceneUidFromIndex(index)]),
        {
          props: {
            style: sceneResizesContent ? props.style : undefined,
            ...props.props,
          },
        },
        {},
      )

      rootElements = elementMetadata.map((individualElementMetadata) => {
        const path = individualElementMetadata.templatePath
        elements[TP.toString(path)] = individualElementMetadata
        return path
      })
    }

    return {
      component: props[PP.toString(PathForSceneComponent)],
      label: props[PP.toString(PathForSceneDataLabel)],
      scenePath: TP.scenePath([BakedInStoryboardUID, props[PP.toString(PathForSceneDataUid)]]),
      templatePath: TP.instancePath([], [BakedInStoryboardUID, createSceneUidFromIndex(index)]),
      globalFrame: { x: 0, y: 0, width: 400, height: 400 } as CanvasRectangle,
      sceneResizesContent: sceneResizesContent ?? true,
      style: {},
      rootElements: rootElements,
    }
  })

  return jsxMetadata(components, elements)
}

export function createFakeMetadataForComponents(
  topLevelElements: Array<TopLevelElement>,
): JSXMetadata {
  let components: ComponentMetadata[] = []
  let elements: ElementInstanceMetadataMap = {}
  Utils.fastForEach(topLevelElements, (component, index) => {
    if (isUtopiaJSXComponent(component)) {
      const elementMetadata = createFakeMetadataForJSXElement(
        component.rootElement,
        TP.scenePath([BakedInStoryboardUID, createSceneUidFromIndex(index)]),
        {},
        {},
      )

      const rootElements: Array<InstancePath> = elementMetadata.map((individualElementMetadata) => {
        const path = individualElementMetadata.templatePath
        elements[TP.toString(path)] = individualElementMetadata
        return path
      })

      components.push({
        scenePath: TP.scenePath([BakedInStoryboardUID, `scene-${index}`]),
        templatePath: TP.instancePath([], [BakedInStoryboardUID, `scene-${index}`]),
        component: component.name,
        globalFrame: { x: 0, y: 0, width: 100, height: 100 } as CanvasRectangle,
        sceneResizesContent: false,
        style: {},
        rootElements: rootElements,
      })
    }
  })
  return jsxMetadata(components, elements)
}

function createFakeMetadataForJSXElement(
  element: JSXElementChild,
  rootPath: TemplatePath,
  inScope: MapLike<any>,
  parentProps: MapLike<any>,
): Array<ElementInstanceMetadata> {
  let elements: Array<ElementInstanceMetadata> = []
  if (isJSXElement(element)) {
    const elementID = getUtopiaID(element)
    const templatePath = TP.appendToPath(rootPath, elementID)
    const props = jsxAttributesToProps(inScope, element.props, Utils.NO_OP)
    const children = element.children.flatMap((child) =>
      createFakeMetadataForJSXElement(child, templatePath, inScope, props),
    )
    const childPaths = children.map((child) => child.templatePath)

    elements.push({
      templatePath: templatePath,
      element: right(element),
      props: props,
      globalFrame: Utils.zeroRectangle as CanvasRectangle, // this could be parametrized to be able to set real rectangles
      localFrame: Utils.zeroRectangle as LocalRectangle,
      children: childPaths,
      componentInstance: false,
      specialSizeMeasurements: emptySpecialSizeMeasurements,
      computedStyle: emptyComputedStyle,
      attributeMetadatada: emptyAttributeMetadatada,
    })
    elements.push(...children)
  } else if (isJSXFragment(element)) {
    const children = element.children.flatMap((child) =>
      createFakeMetadataForJSXElement(child, rootPath, inScope, parentProps),
    )
    elements.push(...children)
  } else {
    throw new Error(`Not a JSX element ${element}`)
  }

  return elements
}

export function wait(timeout: number): Promise<void> {
  return new Promise((resolve) => {
    setTimeout(resolve, timeout)
  })
}
