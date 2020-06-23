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
} from '../core/shared/element-template'
import { getUtopiaID } from '../core/model/element-template-utils'
import { jsxAttributesToProps } from '../core/shared/jsx-attributes'
import { getUtopiaJSXComponentsFromSuccess, uiJsFile } from '../core/model/project-file-utils'
import { parseSuccess } from '../core/workers/common/project-file-utils'
import { sampleImportsForTests, sampleJsxComponentWithScene } from '../core/model/test-ui-js-file'
import { RevisionsState, TemplatePath, isParseFailure } from '../core/shared/project-file-types'
import { right } from '../core/shared/either'
import Utils from './utils'
import { CanvasRectangle, LocalRectangle } from '../core/shared/math-utils'
import {
  unmapScene,
  createSceneUidFromIndex,
  BakedInStoryboardUID,
} from '../core/model/scene-utils'
import { NO_OP } from '../core/shared/utils'

export function createEditorStates(
  selectedFileOrTab: string | EditorTab = '/src/app.ui.js',
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
      '/src/app.ui.js': uiJsFile(
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
      const success = contents.value
      const components = getUtopiaJSXComponentsFromSuccess(success)
      const sceneElements = Utils.stripNulls(getSceneElements(editor).map(unmapScene))
      return sceneElements.map((scene, index) => {
        const component = components.find(
          (c) => c.name === scene.component && isUtopiaJSXComponent(c),
        )
        return {
          ...scene,
          scenePath: TP.scenePath([BakedInStoryboardUID, scene.uid]),
          rootElement:
            component == null
              ? null
              : createFakeMetadataForJSXElement(
                  component.rootElement,
                  TP.scenePath([BakedInStoryboardUID, createSceneUidFromIndex(index)]),
                ),
        }
      })
    }
  }
}

export function createFakeMetadataForComponents(
  components: Array<TopLevelElement>,
): Array<ComponentMetadata> {
  let metadata: Array<ComponentMetadata> = []
  Utils.fastForEach(components, (component, index) => {
    if (isUtopiaJSXComponent(component)) {
      metadata.push({
        scenePath: TP.scenePath([BakedInStoryboardUID, `scene-${index}`]),
        component: component.name,
        frame: { left: 0, top: 0, width: 100, height: 100 },
        container: { layoutSystem: LayoutSystem.PinSystem },
        rootElement: createFakeMetadataForJSXElement(
          component.rootElement,
          TP.scenePath([BakedInStoryboardUID, createSceneUidFromIndex(index)]),
        ),
      })
    }
  })
  return metadata
}

function createFakeMetadataForJSXElement(
  element: JSXElementChild,
  rootPath: TemplatePath,
): ElementInstanceMetadata {
  if (isJSXElement(element)) {
    const elementID = getUtopiaID(element)
    const templatePath = TP.appendToPath(rootPath, elementID)
    return {
      templatePath: templatePath,
      element: right(element),
      props: jsxAttributesToProps({}, element.props, {}, Utils.NO_OP, Utils.NO_OP),
      globalFrame: Utils.zeroRectangle as CanvasRectangle, // this could be parametrized to be able to set real rectangles
      localFrame: Utils.zeroRectangle as LocalRectangle,
      children: element.children.map((child) =>
        createFakeMetadataForJSXElement(child, templatePath),
      ),
      componentInstance: false,
      specialSizeMeasurements: emptySpecialSizeMeasurements,
    }
  } else {
    throw new Error(`Not a JSX element ${element}`)
  }
}
