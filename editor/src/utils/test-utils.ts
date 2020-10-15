import { LayoutSystem } from 'utopia-api'
import { EditorDispatch } from '../components/editor/action-types'
import {
  createEditorState,
  DerivedState,
  deriveState,
  EditorState,
  EditorTab,
  getOpenUIJSFile,
  openFileTab,
  getSceneElements,
  getSceneElementsFromParseSuccess,
  PersistentModel,
  persistentModelFromEditorModel,
  DefaultPackageJson,
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
} from '../core/shared/element-template'
import { getUtopiaID } from '../core/model/element-template-utils'
import { jsxAttributesToProps, jsxSimpleAttributeToValue } from '../core/shared/jsx-attributes'
import {
  getUtopiaJSXComponentsFromSuccess,
  uiJsFile,
  codeFile,
} from '../core/model/project-file-utils'
import { parseSuccess } from '../core/workers/common/project-file-utils'
import { sampleImportsForTests, sampleJsxComponentWithScene } from '../core/model/test-ui-js-file'
import {
  RevisionsState,
  TemplatePath,
  isParseFailure,
  ParseSuccess,
} from '../core/shared/project-file-types'
import { right, eitherToMaybe, isLeft } from '../core/shared/either'
import Utils from './utils'
import { CanvasRectangle, LocalRectangle } from '../core/shared/math-utils'
import {
  createSceneUidFromIndex,
  BakedInStoryboardUID,
  BakedInStoryboardVariableName,
  PathForSceneContainer,
  PathForSceneComponent,
  PathForSceneDataLabel,
  PathForSceneDataUid,
  PathForResizeContent,
} from '../core/model/scene-utils'
import { NO_OP } from '../core/shared/utils'
import * as PP from '../core/shared/property-path'
import { getSimpleAttributeAtPath } from '../core/model/element-metadata-utils'
import { mapArrayToDictionary } from '../core/shared/array-utils'
import { MapLike } from 'typescript'
import { contentsToTree } from '../components/assets'

export function delay<T>(time: number): Promise<T> {
  return new Promise((resolve) => setTimeout(resolve, time))
}

export function createPersistentModel(): PersistentModel {
  const editor: EditorState = {
    ...createEditorState(NO_OP),
    projectContents: contentsToTree({
      '/src/app.js': uiJsFile(
        right(
          parseSuccess(
            sampleImportsForTests,
            sampleJsxComponentWithScene,
            right({}),
            true,
            '',
            {},
            null,
            null,
          ),
        ),
        null,
        RevisionsState.BothMatch,
        0,
      ),
    }),
    selectedFile: {
      tab: openFileTab('/src/app.js'),
      initialCursorPosition: null,
    },
  }

  return persistentModelFromEditorModel(editor)
}

export function createEditorStates(
  selectedFileOrTab: string | EditorTab = '/src/app.js',
  selectedViews: TemplatePath[] = [],
): {
  editor: EditorState
  derivedState: DerivedState
  dispatch: EditorDispatch
} {
  const selectedTab: EditorTab =
    typeof selectedFileOrTab === 'string' ? openFileTab(selectedFileOrTab) : selectedFileOrTab
  const editor: EditorState = {
    ...createEditorState(NO_OP),
    projectContents: contentsToTree({
      '/package.json': codeFile(JSON.stringify(DefaultPackageJson, null, 2), null),
      '/src/app.js': uiJsFile(
        right(
          parseSuccess(
            sampleImportsForTests,
            sampleJsxComponentWithScene,
            right({}),
            true,
            '',
            {},
            null,
            null,
          ),
        ),
        null,
        RevisionsState.BothMatch,
        0,
      ),
    }),
    selectedFile: {
      tab: selectedTab,
      initialCursorPosition: null,
    },
    selectedViews: selectedViews,
  }
  const derivedResult = deriveState(editor, null, false)
  const componentMetadata = createFakeMetadataForEditor(editor)
  return {
    editor: {
      ...derivedResult.editor,
      jsxMetadataKILLME: componentMetadata,
    },
    derivedState: derivedResult.derived,
    dispatch: Utils.NO_OP,
  }
}

export function createFakeMetadataForEditor(editor: EditorState): Array<ComponentMetadata> {
  const openUiJsFile = getOpenUIJSFile(editor)
  if (openUiJsFile == null) {
    return []
  } else {
    const contents = openUiJsFile.fileContents
    if (isParseFailure(contents)) {
      return []
    } else {
      return createFakeMetadataForParseSuccess(contents.value)
    }
  }
}

export function createFakeMetadataForParseSuccess(success: ParseSuccess): Array<ComponentMetadata> {
  const components = getUtopiaJSXComponentsFromSuccess(success)
  const sceneElements = getSceneElementsFromParseSuccess(success)
  return sceneElements.map((scene, index) => {
    const props = mapArrayToDictionary(
      Object.keys(scene.props),
      (key) => key,
      (key) => {
        const attr = scene.props[key]
        const simpleValue = jsxSimpleAttributeToValue(scene.props[key])
        if (isLeft(simpleValue) && attr.type === 'ATTRIBUTE_OTHER_JAVASCRIPT') {
          return attr.javascript
        } else {
          return eitherToMaybe(simpleValue)
        }
      },
    )
    const component = components.find((c) => c.name === props.component && isUtopiaJSXComponent(c))
    const sceneResizesContent =
      Utils.path<boolean>(PP.getElements(PathForResizeContent), props) ?? true
    let elementMetadata: ElementInstanceMetadata | Array<ElementInstanceMetadata> | null = null
    if (component != null) {
      elementMetadata = createFakeMetadataForJSXElement(
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
    }
    return {
      component: props[PP.toString(PathForSceneComponent)],
      container: props[PP.toString(PathForSceneContainer)],
      label: props[PP.toString(PathForSceneDataLabel)],
      scenePath: TP.scenePath([BakedInStoryboardUID, props[PP.toString(PathForSceneDataUid)]]),
      templatePath: TP.instancePath([], [BakedInStoryboardUID, createSceneUidFromIndex(index)]),
      globalFrame: { x: 0, y: 0, width: 400, height: 400 } as CanvasRectangle,
      sceneResizesContent: sceneResizesContent ?? true,
      style: {},
      rootElements:
        elementMetadata == null
          ? []
          : Array.isArray(elementMetadata)
          ? elementMetadata
          : [elementMetadata],
    }
  })
}

export function createFakeMetadataForComponents(
  components: Array<TopLevelElement>,
): Array<ComponentMetadata> {
  let metadata: Array<ComponentMetadata> = []
  Utils.fastForEach(components, (component, index) => {
    if (isUtopiaJSXComponent(component)) {
      const elementMetadata = createFakeMetadataForJSXElement(
        component.rootElement,
        TP.scenePath([BakedInStoryboardUID, createSceneUidFromIndex(index)]),
        {},
        {},
      )
      metadata.push({
        scenePath: TP.scenePath([BakedInStoryboardUID, `scene-${index}`]),
        templatePath: TP.instancePath([], [BakedInStoryboardUID, `scene-${index}`]),
        component: component.name,
        globalFrame: { x: 0, y: 0, width: 100, height: 100 } as CanvasRectangle,
        container: { layoutSystem: LayoutSystem.PinSystem },
        sceneResizesContent: false,
        style: {},
        rootElements: Array.isArray(elementMetadata) ? elementMetadata : [elementMetadata],
      })
    }
  })
  return metadata
}

function createFakeMetadataForJSXElement(
  element: JSXElementChild,
  rootPath: TemplatePath,
  inScope: MapLike<any>,
  parentProps: MapLike<any>,
): ElementInstanceMetadata | Array<ElementInstanceMetadata> {
  if (isJSXElement(element)) {
    const elementID = getUtopiaID(element)
    const templatePath = TP.appendToPath(rootPath, elementID)
    const props = jsxAttributesToProps(inScope, element.props, Utils.NO_OP)
    return {
      templatePath: templatePath,
      element: right(element),
      props: props,
      globalFrame: Utils.zeroRectangle as CanvasRectangle, // this could be parametrized to be able to set real rectangles
      localFrame: Utils.zeroRectangle as LocalRectangle,
      children: element.children.flatMap((child) =>
        createFakeMetadataForJSXElement(child, templatePath, inScope, props),
      ),
      componentInstance: false,
      specialSizeMeasurements: emptySpecialSizeMeasurements,
      computedStyle: emptyComputedStyle,
    }
  } else if (isJSXFragment(element)) {
    return element.children.flatMap((child) => {
      return createFakeMetadataForJSXElement(child, rootPath, inScope, parentProps)
    })
  } else {
    throw new Error(`Not a JSX element ${element}`)
  }
}

export function wait(timeout: number): Promise<void> {
  return new Promise((resolve) => {
    setTimeout(resolve, timeout)
  })
}

export function elementsStructure(elements: Array<TopLevelElement>): string {
  let structureResults: Array<string> = []
  walkElements(elements, (element, path) => {
    const isElement = isJSXElement(element)
    // Adjustment to cater for things which are not elements
    // appearing one level too high because they do not modify the
    // path from walkElements.
    const depth = path.length + (isElement ? 0 : 1)
    let elementResult: string = ''
    for (let index = 0; index < depth; index++) {
      elementResult += '  '
    }
    elementResult += element.type
    if (isJSXElement(element)) {
      elementResult += ` - ${getUtopiaID(element)}`
    }
    structureResults.push(elementResult)
  })
  return structureResults.join('\n')
}
