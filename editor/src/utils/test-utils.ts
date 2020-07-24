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
import { jsxAttributesToProps, getJSXAttributeAtPath } from '../core/shared/jsx-attributes'
import { getUtopiaJSXComponentsFromSuccess, uiJsFile } from '../core/model/project-file-utils'
import { parseSuccess } from '../core/workers/common/project-file-utils'
import { sampleImportsForTests, sampleJsxComponentWithScene } from '../core/model/test-ui-js-file'
import {
  RevisionsState,
  TemplatePath,
  isParseFailure,
  ParseSuccess,
} from '../core/shared/project-file-types'
import { right } from '../core/shared/either'
import Utils from './utils'
import { CanvasRectangle, LocalRectangle } from '../core/shared/math-utils'
import {
  unmapScene,
  createSceneUidFromIndex,
  BakedInStoryboardUID,
  BakedInStoryboardVariableName,
} from '../core/model/scene-utils'
import { NO_OP } from '../core/shared/utils'
import { create } from '../core/shared/property-path'
import { getSimpleAttributeAtPath } from '../core/model/element-metadata-utils'

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
    projectContents: {
      '/src/app.js': uiJsFile(
        right(
          parseSuccess(
            sampleImportsForTests,
            sampleJsxComponentWithScene,
            right({}),
            true,
            '',
            {},
            [],
            null,
          ),
        ),
        null,
        RevisionsState.BothMatch,
        0,
      ),
    },
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
  const sceneElements = Utils.stripNulls(getSceneElementsFromParseSuccess(success).map(unmapScene))
  return sceneElements.map((scene, index) => {
    const component = components.find((c) => c.name === scene.component && isUtopiaJSXComponent(c))
    let elementMetadata: ElementInstanceMetadata | Array<ElementInstanceMetadata> | null = null
    if (component != null) {
      elementMetadata = createFakeMetadataForJSXElement(
        component.rootElement,
        TP.scenePath([BakedInStoryboardUID, createSceneUidFromIndex(index)]),
      )
    }
    return {
      ...scene,
      scenePath: TP.scenePath([BakedInStoryboardUID, scene.uid]),
      templatePath: TP.instancePath([], [BakedInStoryboardUID, `scene-${index}`]),
      globalFrame: { x: 0, y: 0, width: 400, height: 400 } as CanvasRectangle,
      type: 'static',
      rootElement:
        elementMetadata == null
          ? null
          : Array.isArray(elementMetadata)
          ? elementMetadata[0]
          : elementMetadata,
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
      )
      metadata.push({
        scenePath: TP.scenePath([BakedInStoryboardUID, `scene-${index}`]),
        templatePath: TP.instancePath([], [BakedInStoryboardUID, `scene-${index}`]),
        component: component.name,
        globalFrame: { x: 0, y: 0, width: 100, height: 100 } as CanvasRectangle,
        container: { layoutSystem: LayoutSystem.PinSystem },
        type: 'static',
        rootElement: Array.isArray(elementMetadata) ? elementMetadata[0] : elementMetadata,
      })
    }
  })
  return metadata
}

function createFakeMetadataForJSXElement(
  element: JSXElementChild,
  rootPath: TemplatePath,
): ElementInstanceMetadata | Array<ElementInstanceMetadata> {
  if (isJSXElement(element)) {
    const elementID = getUtopiaID(element)
    const templatePath = TP.appendToPath(rootPath, elementID)
    return {
      templatePath: templatePath,
      element: right(element),
      props: jsxAttributesToProps({}, element.props, {}, Utils.NO_OP, Utils.NO_OP),
      globalFrame: Utils.zeroRectangle as CanvasRectangle, // this could be parametrized to be able to set real rectangles
      localFrame: Utils.zeroRectangle as LocalRectangle,
      children: element.children.flatMap((child) =>
        createFakeMetadataForJSXElement(child, templatePath),
      ),
      componentInstance: false,
      specialSizeMeasurements: emptySpecialSizeMeasurements,
      computedStyle: emptyComputedStyle,
    }
  } else if (isJSXFragment(element)) {
    return element.children.flatMap((child) => {
      return createFakeMetadataForJSXElement(child, rootPath)
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
