import { act, render } from '@testing-library/react'
import React from 'react'
import create from 'zustand'
import { subscribeWithSelector } from 'zustand/middleware'
import { notLoggedIn } from '../../common/user'
import {
  ElementInstanceMetadata,
  ElementInstanceMetadataMap,
} from '../../core/shared/element-template'
import {
  FakeLinterWorker,
  FakeParserPrinterWorker,
  FakeWatchdogWorker,
} from '../../core/workers/test-workers'
import { UtopiaTsWorkersImplementation } from '../../core/workers/workers'
import { EditorRoot } from '../../templates/editor'
import { left } from '../../core/shared/either'
import { EditorDispatch } from '../editor/action-types'
import { load } from '../editor/actions/actions'
import * as History from '../editor/history'
import { editorDispatch } from '../editor/store/dispatch'
import {
  createEditorState,
  deriveState,
  EditorStoreFull,
  EditorStorePatched,
  patchedStoreFromFullStore,
} from '../editor/store/editor-state'
import Utils from '../../utils/utils'
import { BakedInStoryboardUID } from '../../core/model/scene-utils'
import { NO_OP } from '../../core/shared/utils'
import { mapValues } from '../../core/shared/object-utils'
import { emptyUiJsxCanvasContextData } from './ui-jsx-canvas'
import { TestAppUID, TestSceneUID } from './ui-jsx.test-utils'
import { createTestProjectWithCode } from '../../sample-projects/sample-project-utils.test-utils'
import { DummyPersistenceMachine } from '../editor/persistence/persistence.test-utils'
import { disableStoredStateforTests } from '../editor/stored-state'
import { matchInlineSnapshotBrowser } from '../../../test/karma-snapshots'
import { createBuiltInDependenciesList } from '../../core/es-modules/package-manager/built-in-dependencies-list'
import { createEmptyStrategyState } from './canvas-strategies/interaction-state'

disableStoredStateforTests()

function sanitizeElementMetadata(element: ElementInstanceMetadata): ElementInstanceMetadata {
  delete element.props['children']
  return {
    ...element,
    element: left('REMOVED_FROM_TEST'),
  }
}

function sanitizeJsxMetadata(metadata: ElementInstanceMetadataMap) {
  return mapValues(sanitizeElementMetadata, metadata)
}

async function renderTestEditorWithCode(appUiJsFileCode: string) {
  let emptyEditorState = createEditorState(NO_OP)
  const derivedState = deriveState(emptyEditorState, null)

  const history = History.init(emptyEditorState, derivedState)
  const spyCollector = emptyUiJsxCanvasContextData()

  const dispatch: EditorDispatch = (actions) => {
    const result = editorDispatch(dispatch, actions, editorStore, spyCollector)
    editorStore = result
    storeHook.setState(patchedStoreFromFullStore(result))
  }

  let editorStore: EditorStoreFull = {
    strategyState: createEmptyStrategyState(),
    unpatchedEditor: emptyEditorState,
    patchedEditor: emptyEditorState,
    unpatchedDerived: derivedState,
    patchedDerived: derivedState,
    history: history,
    userState: {
      loginState: notLoggedIn,
      shortcutConfig: {},
    },
    workers: new UtopiaTsWorkersImplementation(
      new FakeParserPrinterWorker(),
      new FakeLinterWorker(),
      new FakeWatchdogWorker(),
    ),
    persistence: DummyPersistenceMachine,
    dispatch: dispatch,
    alreadySaved: false,
    builtInDependencies: createBuiltInDependenciesList(null),
  }

  const storeHook = create<EditorStorePatched>(
    subscribeWithSelector((set) => patchedStoreFromFullStore(editorStore)),
  )

  render(<EditorRoot api={storeHook} useStore={storeHook} spyCollector={spyCollector} />)

  await act(async () => {
    await load(
      dispatch,
      createTestProjectWithCode(appUiJsFileCode),
      'Test',
      '0',
      editorStore.builtInDependencies,
      false,
    )
  })
  const sanitizedMetadata = sanitizeJsxMetadata(storeHook.getState().editor.jsxMetadata)
  return sanitizedMetadata
}

describe('DOM Walker tests', () => {
  it('Simple Project with one child View', async () => {
    const sanitizedMetadata = await renderTestEditorWithCode(
      `
      import * as React from 'react'
      import {
        View,
        Scene,
        Storyboard,
      } from 'utopia-api'
      export var App = (props) => {
        return (
          <View style={{ ...props.style, backgroundColor: '#FFFFFF'}} data-uid={'05c'}>
            <View
              style={{ backgroundColor: '#DDDDDD', position: 'absolute', left: 55, top: 98, width: 266, height: 124  }}
              data-uid={'ef0'}
            >
              <View
                style={{ backgroundColor: '#DDDDDD', position: 'absolute', left: 71, top: 27, width: 125, height: 70 }}
                data-uid={'488'}
              />
            </View>
          </View>
        )
      }
      export var storyboard = (props) => {
        return (
          <Storyboard data-uid={'${BakedInStoryboardUID}'}>
            <Scene
              style={{ position: 'relative', left: 0, top: 0, width: 375, height: 812 }}
              data-uid={'${TestSceneUID}'}
            >
              <App
                data-uid='${TestAppUID}' 
                style={{ position: 'absolute', bottom: 0, left: 0, right: 0, top: 0 }}
              />
            </Scene>
          </Storyboard>
        )
      }
      `,
    )
    matchInlineSnapshotBrowser(
      sanitizedMetadata,
      `
    Object {
      "utopia-storyboard-uid": Object {
        "attributeMetadatada": Object {},
        "componentInstance": true,
        "computedStyle": Object {},
        "element": Object {
          "type": "LEFT",
          "value": "REMOVED_FROM_TEST",
        },
        "elementPath": Object {
          "parts": Array [
            Array [
              "utopia-storyboard-uid",
            ],
          ],
          "type": "elementpath",
        },
        "globalFrame": Object {
          "height": 0,
          "width": 0,
          "x": 0,
          "y": 0,
        },
        "importInfo": Object {
          "type": "RIGHT",
          "value": Object {
            "originalName": "Storyboard",
            "path": "utopia-api",
            "variableName": "Storyboard",
          },
        },
        "isEmotionOrStyledComponent": false,
        "label": null,
        "localFrame": Object {
          "height": 0,
          "width": 0,
          "x": 0,
          "y": 0,
        },
        "props": Object {
          "data-path": "utopia-storyboard-uid",
          "data-uid": "utopia-storyboard-uid",
          "skipDeepFreeze": true,
        },
        "specialSizeMeasurements": Object {
          "clientHeight": 0,
          "clientWidth": 0,
          "coordinateSystemBounds": Object {
            "height": 0,
            "width": 0,
            "x": 0,
            "y": 0,
          },
          "display": "initial",
          "flexDirection": null,
          "globalContentBox": null,
          "htmlElementName": "div",
          "immediateParentBounds": Object {
            "height": 0,
            "width": 0,
            "x": 0,
            "y": 0,
          },
          "immediateParentProvidesLayout": true,
          "layoutSystemForChildren": "flow",
          "margin": Object {},
          "naturalHeight": null,
          "naturalWidth": null,
          "offset": Object {
            "x": 0,
            "y": 0,
          },
          "padding": Object {},
          "parentFlexDirection": null,
          "parentLayoutSystem": "flow",
          "position": "static",
          "providesBoundsForChildren": false,
          "renderedChildrenCount": 0,
          "usesParentBounds": false,
        },
      },
      "utopia-storyboard-uid/scene-aaa": Object {
        "attributeMetadatada": null,
        "componentInstance": true,
        "computedStyle": null,
        "element": Object {
          "type": "LEFT",
          "value": "REMOVED_FROM_TEST",
        },
        "elementPath": Object {
          "parts": Array [
            Array [
              "utopia-storyboard-uid",
              "scene-aaa",
            ],
          ],
          "type": "elementpath",
        },
        "globalFrame": Object {
          "height": 812,
          "width": 375,
          "x": 0,
          "y": 0,
        },
        "importInfo": Object {
          "type": "RIGHT",
          "value": Object {
            "originalName": "Scene",
            "path": "utopia-api",
            "variableName": "Scene",
          },
        },
        "isEmotionOrStyledComponent": false,
        "label": null,
        "localFrame": Object {
          "height": 812,
          "width": 375,
          "x": 0,
          "y": 0,
        },
        "props": Object {
          "data-path": "utopia-storyboard-uid/scene-aaa",
          "data-uid": "scene-aaa",
          "data-utopia-scene-id": "utopia-storyboard-uid/scene-aaa",
          "skipDeepFreeze": true,
          "style": Object {
            "height": 812,
            "left": 0,
            "position": "relative",
            "top": 0,
            "width": 375,
          },
        },
        "specialSizeMeasurements": Object {
          "clientHeight": 812,
          "clientWidth": 375,
          "coordinateSystemBounds": Object {
            "height": 812,
            "width": 375,
            "x": 0,
            "y": 0,
          },
          "display": "block",
          "flexDirection": "row",
          "globalContentBox": Object {
            "height": 812,
            "width": 375,
            "x": 0,
            "y": 0,
          },
          "htmlElementName": "div",
          "immediateParentBounds": Object {
            "height": 812,
            "width": 375,
            "x": 0,
            "y": 0,
          },
          "immediateParentProvidesLayout": true,
          "layoutSystemForChildren": "flow",
          "margin": Object {
            "bottom": 0,
            "left": 0,
            "right": 0,
            "top": 0,
          },
          "naturalHeight": null,
          "naturalWidth": null,
          "offset": Object {
            "x": 0,
            "y": 0,
          },
          "padding": Object {
            "bottom": 0,
            "left": 0,
            "right": 0,
            "top": 0,
          },
          "parentFlexDirection": "row",
          "parentLayoutSystem": "flow",
          "position": "relative",
          "providesBoundsForChildren": true,
          "renderedChildrenCount": 1,
          "usesParentBounds": true,
        },
      },
      "utopia-storyboard-uid/scene-aaa/app-entity": Object {
        "attributeMetadatada": null,
        "componentInstance": true,
        "computedStyle": null,
        "element": Object {
          "type": "LEFT",
          "value": "REMOVED_FROM_TEST",
        },
        "elementPath": Object {
          "parts": Array [
            Array [
              "utopia-storyboard-uid",
              "scene-aaa",
              "app-entity",
            ],
          ],
          "type": "elementpath",
        },
        "globalFrame": Object {
          "height": 812,
          "width": 375,
          "x": 0,
          "y": 0,
        },
        "importInfo": Object {
          "type": "LEFT",
          "value": "NOT_IMPORTED",
        },
        "isEmotionOrStyledComponent": false,
        "label": null,
        "localFrame": Object {
          "height": 812,
          "width": 375,
          "x": 0,
          "y": 0,
        },
        "props": Object {
          "data-path": "utopia-storyboard-uid/scene-aaa/app-entity",
          "data-uid": "app-entity",
          "data-utopia-instance-path": Object {
            "parts": Array [
              Array [
                "utopia-storyboard-uid",
                "scene-aaa",
                "app-entity",
              ],
            ],
            "type": "elementpath",
          },
          "skipDeepFreeze": true,
          "style": Object {
            "bottom": 0,
            "left": 0,
            "position": "absolute",
            "right": 0,
            "top": 0,
          },
        },
        "specialSizeMeasurements": Object {
          "clientHeight": 812,
          "clientWidth": 375,
          "coordinateSystemBounds": Object {
            "height": 812,
            "width": 375,
            "x": 0,
            "y": 0,
          },
          "display": "block",
          "flexDirection": "row",
          "globalContentBox": Object {
            "height": 812,
            "width": 375,
            "x": 0,
            "y": 0,
          },
          "htmlElementName": "div",
          "immediateParentBounds": Object {
            "height": 812,
            "width": 375,
            "x": 0,
            "y": 0,
          },
          "immediateParentProvidesLayout": true,
          "layoutSystemForChildren": "flow",
          "margin": Object {
            "bottom": 0,
            "left": 0,
            "right": 0,
            "top": 0,
          },
          "naturalHeight": null,
          "naturalWidth": null,
          "offset": Object {
            "x": 0,
            "y": 0,
          },
          "padding": Object {
            "bottom": 0,
            "left": 0,
            "right": 0,
            "top": 0,
          },
          "parentFlexDirection": "row",
          "parentLayoutSystem": "flow",
          "position": "absolute",
          "providesBoundsForChildren": true,
          "renderedChildrenCount": 1,
          "usesParentBounds": true,
        },
      },
      "utopia-storyboard-uid/scene-aaa/app-entity:05c": Object {
        "attributeMetadatada": null,
        "componentInstance": true,
        "computedStyle": null,
        "element": Object {
          "type": "LEFT",
          "value": "REMOVED_FROM_TEST",
        },
        "elementPath": Object {
          "parts": Array [
            Array [
              "utopia-storyboard-uid",
              "scene-aaa",
              "app-entity",
            ],
            Array [
              "05c",
            ],
          ],
          "type": "elementpath",
        },
        "globalFrame": Object {
          "height": 812,
          "width": 375,
          "x": 0,
          "y": 0,
        },
        "importInfo": Object {
          "type": "RIGHT",
          "value": Object {
            "originalName": "View",
            "path": "utopia-api",
            "variableName": "View",
          },
        },
        "isEmotionOrStyledComponent": false,
        "label": null,
        "localFrame": Object {
          "height": 812,
          "width": 375,
          "x": 0,
          "y": 0,
        },
        "props": Object {
          "data-path": "utopia-storyboard-uid/scene-aaa/app-entity:05c",
          "data-uid": "05c app-entity",
          "skipDeepFreeze": true,
          "style": Object {
            "backgroundColor": "#FFFFFF",
            "bottom": 0,
            "left": 0,
            "position": "absolute",
            "right": 0,
            "top": 0,
          },
        },
        "specialSizeMeasurements": Object {
          "clientHeight": 812,
          "clientWidth": 375,
          "coordinateSystemBounds": Object {
            "height": 812,
            "width": 375,
            "x": 0,
            "y": 0,
          },
          "display": "block",
          "flexDirection": "row",
          "globalContentBox": Object {
            "height": 812,
            "width": 375,
            "x": 0,
            "y": 0,
          },
          "htmlElementName": "div",
          "immediateParentBounds": Object {
            "height": 812,
            "width": 375,
            "x": 0,
            "y": 0,
          },
          "immediateParentProvidesLayout": true,
          "layoutSystemForChildren": "flow",
          "margin": Object {
            "bottom": 0,
            "left": 0,
            "right": 0,
            "top": 0,
          },
          "naturalHeight": null,
          "naturalWidth": null,
          "offset": Object {
            "x": 0,
            "y": 0,
          },
          "padding": Object {
            "bottom": 0,
            "left": 0,
            "right": 0,
            "top": 0,
          },
          "parentFlexDirection": "row",
          "parentLayoutSystem": "flow",
          "position": "absolute",
          "providesBoundsForChildren": true,
          "renderedChildrenCount": 1,
          "usesParentBounds": true,
        },
      },
      "utopia-storyboard-uid/scene-aaa/app-entity:05c/ef0": Object {
        "attributeMetadatada": null,
        "componentInstance": true,
        "computedStyle": null,
        "element": Object {
          "type": "LEFT",
          "value": "REMOVED_FROM_TEST",
        },
        "elementPath": Object {
          "parts": Array [
            Array [
              "utopia-storyboard-uid",
              "scene-aaa",
              "app-entity",
            ],
            Array [
              "05c",
              "ef0",
            ],
          ],
          "type": "elementpath",
        },
        "globalFrame": Object {
          "height": 124,
          "width": 266,
          "x": 55,
          "y": 98,
        },
        "importInfo": Object {
          "type": "RIGHT",
          "value": Object {
            "originalName": "View",
            "path": "utopia-api",
            "variableName": "View",
          },
        },
        "isEmotionOrStyledComponent": false,
        "label": null,
        "localFrame": Object {
          "height": 124,
          "width": 266,
          "x": 55,
          "y": 98,
        },
        "props": Object {
          "data-path": "utopia-storyboard-uid/scene-aaa/app-entity:05c/ef0",
          "data-uid": "ef0",
          "skipDeepFreeze": true,
          "style": Object {
            "backgroundColor": "#DDDDDD",
            "height": 124,
            "left": 55,
            "position": "absolute",
            "top": 98,
            "width": 266,
          },
        },
        "specialSizeMeasurements": Object {
          "clientHeight": 124,
          "clientWidth": 266,
          "coordinateSystemBounds": Object {
            "height": 812,
            "width": 375,
            "x": 0,
            "y": 0,
          },
          "display": "block",
          "flexDirection": "row",
          "globalContentBox": Object {
            "height": 124,
            "width": 266,
            "x": 55,
            "y": 98,
          },
          "htmlElementName": "div",
          "immediateParentBounds": Object {
            "height": 812,
            "width": 375,
            "x": 0,
            "y": 0,
          },
          "immediateParentProvidesLayout": true,
          "layoutSystemForChildren": "flow",
          "margin": Object {
            "bottom": 0,
            "left": 0,
            "right": 0,
            "top": 0,
          },
          "naturalHeight": null,
          "naturalWidth": null,
          "offset": Object {
            "x": 55,
            "y": 98,
          },
          "padding": Object {
            "bottom": 0,
            "left": 0,
            "right": 0,
            "top": 0,
          },
          "parentFlexDirection": "row",
          "parentLayoutSystem": "flow",
          "position": "absolute",
          "providesBoundsForChildren": true,
          "renderedChildrenCount": 1,
          "usesParentBounds": true,
        },
      },
      "utopia-storyboard-uid/scene-aaa/app-entity:05c/ef0/488": Object {
        "attributeMetadatada": null,
        "componentInstance": true,
        "computedStyle": null,
        "element": Object {
          "type": "LEFT",
          "value": "REMOVED_FROM_TEST",
        },
        "elementPath": Object {
          "parts": Array [
            Array [
              "utopia-storyboard-uid",
              "scene-aaa",
              "app-entity",
            ],
            Array [
              "05c",
              "ef0",
              "488",
            ],
          ],
          "type": "elementpath",
        },
        "globalFrame": Object {
          "height": 70,
          "width": 125,
          "x": 126,
          "y": 125,
        },
        "importInfo": Object {
          "type": "RIGHT",
          "value": Object {
            "originalName": "View",
            "path": "utopia-api",
            "variableName": "View",
          },
        },
        "isEmotionOrStyledComponent": false,
        "label": null,
        "localFrame": Object {
          "height": 70,
          "width": 125,
          "x": 71,
          "y": 27,
        },
        "props": Object {
          "data-path": "utopia-storyboard-uid/scene-aaa/app-entity:05c/ef0/488",
          "data-uid": "488",
          "skipDeepFreeze": true,
          "style": Object {
            "backgroundColor": "#DDDDDD",
            "height": 70,
            "left": 71,
            "position": "absolute",
            "top": 27,
            "width": 125,
          },
        },
        "specialSizeMeasurements": Object {
          "clientHeight": 70,
          "clientWidth": 125,
          "coordinateSystemBounds": Object {
            "height": 124,
            "width": 266,
            "x": 55,
            "y": 98,
          },
          "display": "block",
          "flexDirection": "row",
          "globalContentBox": Object {
            "height": 70,
            "width": 125,
            "x": 126,
            "y": 125,
          },
          "htmlElementName": "div",
          "immediateParentBounds": Object {
            "height": 124,
            "width": 266,
            "x": 55,
            "y": 98,
          },
          "immediateParentProvidesLayout": true,
          "layoutSystemForChildren": "flow",
          "margin": Object {
            "bottom": 0,
            "left": 0,
            "right": 0,
            "top": 0,
          },
          "naturalHeight": null,
          "naturalWidth": null,
          "offset": Object {
            "x": 71,
            "y": 27,
          },
          "padding": Object {
            "bottom": 0,
            "left": 0,
            "right": 0,
            "top": 0,
          },
          "parentFlexDirection": "row",
          "parentLayoutSystem": "flow",
          "position": "absolute",
          "providesBoundsForChildren": true,
          "renderedChildrenCount": 0,
          "usesParentBounds": true,
        },
      },
    }
    `,
    )
  })

  it('Simple Project with divs', async () => {
    const sanitizedMetadata = await renderTestEditorWithCode(
      `
      import * as React from 'react'
      import {
        View,
        Scene,
        Storyboard,
      } from 'utopia-api'
      export var App = (props) => {
        return (
          <div style={{ ...props.style, backgroundColor: '#FFFFFF' }} data-uid={'05c'}>
            <div
              style={{ backgroundColor: '#DDDDDD', position: 'fixed', padding: 20, left: 55, top: 98, width: 266, height: 124 }}
              data-uid={'ef0'}
            >
              <div
                style={{ backgroundColor: '#DDDDDD', position: 'absolute', left: 71, top: 27, width: 125, height: 70 }}
                data-uid={'488'}
              />
            </div>
          </div>
        )
      }
      export var storyboard = (props) => {
        return (
          <Storyboard data-uid={'${BakedInStoryboardUID}'}>
            <Scene
              style={{ position: 'relative', left: 0, top: 0, width: 375, height: 812 }}
              data-uid={'${TestSceneUID}'}
            >
              <App
                data-uid='${TestAppUID}' 
                style={{ position: 'absolute', bottom: 0, left: 0, right: 0, top: 0 }}
              />
            </Scene>
          </Storyboard>
        )
      }
      `,
    )
    matchInlineSnapshotBrowser(
      sanitizedMetadata,
      `
    Object {
      "utopia-storyboard-uid": Object {
        "attributeMetadatada": Object {},
        "componentInstance": true,
        "computedStyle": Object {},
        "element": Object {
          "type": "LEFT",
          "value": "REMOVED_FROM_TEST",
        },
        "elementPath": Object {
          "parts": Array [
            Array [
              "utopia-storyboard-uid",
            ],
          ],
          "type": "elementpath",
        },
        "globalFrame": Object {
          "height": 0,
          "width": 0,
          "x": 0,
          "y": 0,
        },
        "importInfo": Object {
          "type": "RIGHT",
          "value": Object {
            "originalName": "Storyboard",
            "path": "utopia-api",
            "variableName": "Storyboard",
          },
        },
        "isEmotionOrStyledComponent": false,
        "label": null,
        "localFrame": Object {
          "height": 0,
          "width": 0,
          "x": 0,
          "y": 0,
        },
        "props": Object {
          "data-path": "utopia-storyboard-uid",
          "data-uid": "utopia-storyboard-uid",
          "skipDeepFreeze": true,
        },
        "specialSizeMeasurements": Object {
          "clientHeight": 0,
          "clientWidth": 0,
          "coordinateSystemBounds": Object {
            "height": 0,
            "width": 0,
            "x": 0,
            "y": 0,
          },
          "display": "initial",
          "flexDirection": null,
          "globalContentBox": null,
          "htmlElementName": "div",
          "immediateParentBounds": Object {
            "height": 0,
            "width": 0,
            "x": 0,
            "y": 0,
          },
          "immediateParentProvidesLayout": true,
          "layoutSystemForChildren": "flow",
          "margin": Object {},
          "naturalHeight": null,
          "naturalWidth": null,
          "offset": Object {
            "x": 0,
            "y": 0,
          },
          "padding": Object {},
          "parentFlexDirection": null,
          "parentLayoutSystem": "flow",
          "position": "static",
          "providesBoundsForChildren": false,
          "renderedChildrenCount": 0,
          "usesParentBounds": false,
        },
      },
      "utopia-storyboard-uid/scene-aaa": Object {
        "attributeMetadatada": null,
        "componentInstance": true,
        "computedStyle": null,
        "element": Object {
          "type": "LEFT",
          "value": "REMOVED_FROM_TEST",
        },
        "elementPath": Object {
          "parts": Array [
            Array [
              "utopia-storyboard-uid",
              "scene-aaa",
            ],
          ],
          "type": "elementpath",
        },
        "globalFrame": Object {
          "height": 812,
          "width": 375,
          "x": 0,
          "y": 0,
        },
        "importInfo": Object {
          "type": "RIGHT",
          "value": Object {
            "originalName": "Scene",
            "path": "utopia-api",
            "variableName": "Scene",
          },
        },
        "isEmotionOrStyledComponent": false,
        "label": null,
        "localFrame": Object {
          "height": 812,
          "width": 375,
          "x": 0,
          "y": 0,
        },
        "props": Object {
          "data-path": "utopia-storyboard-uid/scene-aaa",
          "data-uid": "scene-aaa",
          "data-utopia-scene-id": "utopia-storyboard-uid/scene-aaa",
          "skipDeepFreeze": true,
          "style": Object {
            "height": 812,
            "left": 0,
            "position": "relative",
            "top": 0,
            "width": 375,
          },
        },
        "specialSizeMeasurements": Object {
          "clientHeight": 812,
          "clientWidth": 375,
          "coordinateSystemBounds": Object {
            "height": 812,
            "width": 375,
            "x": 0,
            "y": 0,
          },
          "display": "block",
          "flexDirection": "row",
          "globalContentBox": Object {
            "height": 812,
            "width": 375,
            "x": 0,
            "y": 0,
          },
          "htmlElementName": "div",
          "immediateParentBounds": Object {
            "height": 812,
            "width": 375,
            "x": 0,
            "y": 0,
          },
          "immediateParentProvidesLayout": true,
          "layoutSystemForChildren": "flow",
          "margin": Object {
            "bottom": 0,
            "left": 0,
            "right": 0,
            "top": 0,
          },
          "naturalHeight": null,
          "naturalWidth": null,
          "offset": Object {
            "x": 0,
            "y": 0,
          },
          "padding": Object {
            "bottom": 0,
            "left": 0,
            "right": 0,
            "top": 0,
          },
          "parentFlexDirection": "row",
          "parentLayoutSystem": "flow",
          "position": "relative",
          "providesBoundsForChildren": true,
          "renderedChildrenCount": 1,
          "usesParentBounds": true,
        },
      },
      "utopia-storyboard-uid/scene-aaa/app-entity": Object {
        "attributeMetadatada": null,
        "componentInstance": true,
        "computedStyle": null,
        "element": Object {
          "type": "LEFT",
          "value": "REMOVED_FROM_TEST",
        },
        "elementPath": Object {
          "parts": Array [
            Array [
              "utopia-storyboard-uid",
              "scene-aaa",
              "app-entity",
            ],
          ],
          "type": "elementpath",
        },
        "globalFrame": Object {
          "height": 812,
          "width": 375,
          "x": 0,
          "y": 0,
        },
        "importInfo": Object {
          "type": "LEFT",
          "value": "NOT_IMPORTED",
        },
        "isEmotionOrStyledComponent": false,
        "label": null,
        "localFrame": Object {
          "height": 812,
          "width": 375,
          "x": 0,
          "y": 0,
        },
        "props": Object {
          "data-path": "utopia-storyboard-uid/scene-aaa/app-entity",
          "data-uid": "app-entity",
          "data-utopia-instance-path": Object {
            "parts": Array [
              Array [
                "utopia-storyboard-uid",
                "scene-aaa",
                "app-entity",
              ],
            ],
            "type": "elementpath",
          },
          "skipDeepFreeze": true,
          "style": Object {
            "bottom": 0,
            "left": 0,
            "position": "absolute",
            "right": 0,
            "top": 0,
          },
        },
        "specialSizeMeasurements": Object {
          "clientHeight": 812,
          "clientWidth": 375,
          "coordinateSystemBounds": Object {
            "height": 812,
            "width": 375,
            "x": 0,
            "y": 0,
          },
          "display": "block",
          "flexDirection": "row",
          "globalContentBox": Object {
            "height": 812,
            "width": 375,
            "x": 0,
            "y": 0,
          },
          "htmlElementName": "div",
          "immediateParentBounds": Object {
            "height": 812,
            "width": 375,
            "x": 0,
            "y": 0,
          },
          "immediateParentProvidesLayout": true,
          "layoutSystemForChildren": "flow",
          "margin": Object {
            "bottom": 0,
            "left": 0,
            "right": 0,
            "top": 0,
          },
          "naturalHeight": null,
          "naturalWidth": null,
          "offset": Object {
            "x": 0,
            "y": 0,
          },
          "padding": Object {
            "bottom": 0,
            "left": 0,
            "right": 0,
            "top": 0,
          },
          "parentFlexDirection": "row",
          "parentLayoutSystem": "flow",
          "position": "absolute",
          "providesBoundsForChildren": true,
          "renderedChildrenCount": 1,
          "usesParentBounds": true,
        },
      },
      "utopia-storyboard-uid/scene-aaa/app-entity:05c": Object {
        "attributeMetadatada": null,
        "componentInstance": false,
        "computedStyle": null,
        "element": Object {
          "type": "LEFT",
          "value": "REMOVED_FROM_TEST",
        },
        "elementPath": Object {
          "parts": Array [
            Array [
              "utopia-storyboard-uid",
              "scene-aaa",
              "app-entity",
            ],
            Array [
              "05c",
            ],
          ],
          "type": "elementpath",
        },
        "globalFrame": Object {
          "height": 812,
          "width": 375,
          "x": 0,
          "y": 0,
        },
        "importInfo": Object {
          "type": "LEFT",
          "value": "NOT_IMPORTED",
        },
        "isEmotionOrStyledComponent": false,
        "label": null,
        "localFrame": Object {
          "height": 812,
          "width": 375,
          "x": 0,
          "y": 0,
        },
        "props": Object {
          "data-path": "utopia-storyboard-uid/scene-aaa/app-entity:05c",
          "data-uid": "05c app-entity",
          "skipDeepFreeze": true,
          "style": Object {
            "backgroundColor": "#FFFFFF",
            "bottom": 0,
            "left": 0,
            "position": "absolute",
            "right": 0,
            "top": 0,
          },
        },
        "specialSizeMeasurements": Object {
          "clientHeight": 812,
          "clientWidth": 375,
          "coordinateSystemBounds": Object {
            "height": 812,
            "width": 375,
            "x": 0,
            "y": 0,
          },
          "display": "block",
          "flexDirection": "row",
          "globalContentBox": Object {
            "height": 812,
            "width": 375,
            "x": 0,
            "y": 0,
          },
          "htmlElementName": "div",
          "immediateParentBounds": Object {
            "height": 812,
            "width": 375,
            "x": 0,
            "y": 0,
          },
          "immediateParentProvidesLayout": true,
          "layoutSystemForChildren": "flow",
          "margin": Object {
            "bottom": 0,
            "left": 0,
            "right": 0,
            "top": 0,
          },
          "naturalHeight": null,
          "naturalWidth": null,
          "offset": Object {
            "x": 0,
            "y": 0,
          },
          "padding": Object {
            "bottom": 0,
            "left": 0,
            "right": 0,
            "top": 0,
          },
          "parentFlexDirection": "row",
          "parentLayoutSystem": "flow",
          "position": "absolute",
          "providesBoundsForChildren": true,
          "renderedChildrenCount": 1,
          "usesParentBounds": true,
        },
      },
      "utopia-storyboard-uid/scene-aaa/app-entity:05c/ef0": Object {
        "attributeMetadatada": null,
        "componentInstance": false,
        "computedStyle": null,
        "element": Object {
          "type": "LEFT",
          "value": "REMOVED_FROM_TEST",
        },
        "elementPath": Object {
          "parts": Array [
            Array [
              "utopia-storyboard-uid",
              "scene-aaa",
              "app-entity",
            ],
            Array [
              "05c",
              "ef0",
            ],
          ],
          "type": "elementpath",
        },
        "globalFrame": Object {
          "height": 164,
          "width": 306,
          "x": 55,
          "y": 98,
        },
        "importInfo": Object {
          "type": "LEFT",
          "value": "NOT_IMPORTED",
        },
        "isEmotionOrStyledComponent": false,
        "label": null,
        "localFrame": Object {
          "height": 164,
          "width": 306,
          "x": 55,
          "y": 98,
        },
        "props": Object {
          "data-path": "utopia-storyboard-uid/scene-aaa/app-entity:05c/ef0",
          "data-uid": "ef0",
          "skipDeepFreeze": true,
          "style": Object {
            "backgroundColor": "#DDDDDD",
            "height": 124,
            "left": 55,
            "padding": 20,
            "position": "fixed",
            "top": 98,
            "width": 266,
          },
        },
        "specialSizeMeasurements": Object {
          "clientHeight": 164,
          "clientWidth": 306,
          "coordinateSystemBounds": null,
          "display": "block",
          "flexDirection": "row",
          "globalContentBox": Object {
            "height": 164,
            "width": 306,
            "x": 55,
            "y": 98,
          },
          "htmlElementName": "div",
          "immediateParentBounds": Object {
            "height": 812,
            "width": 375,
            "x": 0,
            "y": 0,
          },
          "immediateParentProvidesLayout": false,
          "layoutSystemForChildren": "flow",
          "margin": Object {
            "bottom": 0,
            "left": 0,
            "right": 0,
            "top": 0,
          },
          "naturalHeight": null,
          "naturalWidth": null,
          "offset": Object {
            "x": 55,
            "y": 98,
          },
          "padding": Object {
            "bottom": 20,
            "left": 20,
            "right": 20,
            "top": 20,
          },
          "parentFlexDirection": "row",
          "parentLayoutSystem": "flow",
          "position": "fixed",
          "providesBoundsForChildren": true,
          "renderedChildrenCount": 1,
          "usesParentBounds": true,
        },
      },
      "utopia-storyboard-uid/scene-aaa/app-entity:05c/ef0/488": Object {
        "attributeMetadatada": null,
        "componentInstance": false,
        "computedStyle": null,
        "element": Object {
          "type": "LEFT",
          "value": "REMOVED_FROM_TEST",
        },
        "elementPath": Object {
          "parts": Array [
            Array [
              "utopia-storyboard-uid",
              "scene-aaa",
              "app-entity",
            ],
            Array [
              "05c",
              "ef0",
              "488",
            ],
          ],
          "type": "elementpath",
        },
        "globalFrame": Object {
          "height": 70,
          "width": 125,
          "x": 126,
          "y": 125,
        },
        "importInfo": Object {
          "type": "LEFT",
          "value": "NOT_IMPORTED",
        },
        "isEmotionOrStyledComponent": false,
        "label": null,
        "localFrame": Object {
          "height": 70,
          "width": 125,
          "x": 71,
          "y": 27,
        },
        "props": Object {
          "data-path": "utopia-storyboard-uid/scene-aaa/app-entity:05c/ef0/488",
          "data-uid": "488",
          "skipDeepFreeze": true,
          "style": Object {
            "backgroundColor": "#DDDDDD",
            "height": 70,
            "left": 71,
            "position": "absolute",
            "top": 27,
            "width": 125,
          },
        },
        "specialSizeMeasurements": Object {
          "clientHeight": 70,
          "clientWidth": 125,
          "coordinateSystemBounds": Object {
            "height": 164,
            "width": 306,
            "x": 55,
            "y": 98,
          },
          "display": "block",
          "flexDirection": "row",
          "globalContentBox": Object {
            "height": 70,
            "width": 125,
            "x": 126,
            "y": 125,
          },
          "htmlElementName": "div",
          "immediateParentBounds": Object {
            "height": 164,
            "width": 306,
            "x": 55,
            "y": 98,
          },
          "immediateParentProvidesLayout": true,
          "layoutSystemForChildren": "flow",
          "margin": Object {
            "bottom": 0,
            "left": 0,
            "right": 0,
            "top": 0,
          },
          "naturalHeight": null,
          "naturalWidth": null,
          "offset": Object {
            "x": 71,
            "y": 27,
          },
          "padding": Object {
            "bottom": 0,
            "left": 0,
            "right": 0,
            "top": 0,
          },
          "parentFlexDirection": "row",
          "parentLayoutSystem": "flow",
          "position": "absolute",
          "providesBoundsForChildren": true,
          "renderedChildrenCount": 0,
          "usesParentBounds": true,
        },
      },
    }
    `,
    )
  })

  it('Simple Project with flex parent', async () => {
    const sanitizedMetadata = await renderTestEditorWithCode(
      `
      import * as React from 'react'
      import {
        View,
        Scene,
        Storyboard,
      } from 'utopia-api'
      export var App = (props) => {
        return (
          <div style={{ ...props.style, backgroundColor: '#FFFFFF', display: 'flex' }} data-uid={'05c'}>
            <div
              style={{ backgroundColor: '#DDDDDD', position: 'fixed', padding: 20, left: 55, top: 98, width: 266, height: 124 }}
              data-uid={'ef0'}
            >
              <div
                style={{ backgroundColor: '#DDDDDD', position: 'absolute', left: 71, top: 27, width: 125, height: 70 }}
                data-uid={'488'}
              />
            </div>
          </div>
        )
      }
      export var storyboard = (props) => {
        return (
          <Storyboard data-uid={'${BakedInStoryboardUID}'}>
            <Scene
              style={{ position: 'relative', left: 0, top: 0, width: 375, height: 812 }}
              data-uid={'${TestSceneUID}'}
            >
              <App
                data-uid='${TestAppUID}' 
                style={{ position: 'absolute', bottom: 0, left: 0, right: 0, top: 0 }}
              />
            </Scene>
          </Storyboard>
        )
      }
      `,
    )
    matchInlineSnapshotBrowser(
      sanitizedMetadata,
      `
    Object {
      "utopia-storyboard-uid": Object {
        "attributeMetadatada": Object {},
        "componentInstance": true,
        "computedStyle": Object {},
        "element": Object {
          "type": "LEFT",
          "value": "REMOVED_FROM_TEST",
        },
        "elementPath": Object {
          "parts": Array [
            Array [
              "utopia-storyboard-uid",
            ],
          ],
          "type": "elementpath",
        },
        "globalFrame": Object {
          "height": 0,
          "width": 0,
          "x": 0,
          "y": 0,
        },
        "importInfo": Object {
          "type": "RIGHT",
          "value": Object {
            "originalName": "Storyboard",
            "path": "utopia-api",
            "variableName": "Storyboard",
          },
        },
        "isEmotionOrStyledComponent": false,
        "label": null,
        "localFrame": Object {
          "height": 0,
          "width": 0,
          "x": 0,
          "y": 0,
        },
        "props": Object {
          "data-path": "utopia-storyboard-uid",
          "data-uid": "utopia-storyboard-uid",
          "skipDeepFreeze": true,
        },
        "specialSizeMeasurements": Object {
          "clientHeight": 0,
          "clientWidth": 0,
          "coordinateSystemBounds": Object {
            "height": 0,
            "width": 0,
            "x": 0,
            "y": 0,
          },
          "display": "initial",
          "flexDirection": null,
          "globalContentBox": null,
          "htmlElementName": "div",
          "immediateParentBounds": Object {
            "height": 0,
            "width": 0,
            "x": 0,
            "y": 0,
          },
          "immediateParentProvidesLayout": true,
          "layoutSystemForChildren": "flow",
          "margin": Object {},
          "naturalHeight": null,
          "naturalWidth": null,
          "offset": Object {
            "x": 0,
            "y": 0,
          },
          "padding": Object {},
          "parentFlexDirection": null,
          "parentLayoutSystem": "flow",
          "position": "static",
          "providesBoundsForChildren": false,
          "renderedChildrenCount": 0,
          "usesParentBounds": false,
        },
      },
      "utopia-storyboard-uid/scene-aaa": Object {
        "attributeMetadatada": null,
        "componentInstance": true,
        "computedStyle": null,
        "element": Object {
          "type": "LEFT",
          "value": "REMOVED_FROM_TEST",
        },
        "elementPath": Object {
          "parts": Array [
            Array [
              "utopia-storyboard-uid",
              "scene-aaa",
            ],
          ],
          "type": "elementpath",
        },
        "globalFrame": Object {
          "height": 812,
          "width": 375,
          "x": 0,
          "y": 0,
        },
        "importInfo": Object {
          "type": "RIGHT",
          "value": Object {
            "originalName": "Scene",
            "path": "utopia-api",
            "variableName": "Scene",
          },
        },
        "isEmotionOrStyledComponent": false,
        "label": null,
        "localFrame": Object {
          "height": 812,
          "width": 375,
          "x": 0,
          "y": 0,
        },
        "props": Object {
          "data-path": "utopia-storyboard-uid/scene-aaa",
          "data-uid": "scene-aaa",
          "data-utopia-scene-id": "utopia-storyboard-uid/scene-aaa",
          "skipDeepFreeze": true,
          "style": Object {
            "height": 812,
            "left": 0,
            "position": "relative",
            "top": 0,
            "width": 375,
          },
        },
        "specialSizeMeasurements": Object {
          "clientHeight": 812,
          "clientWidth": 375,
          "coordinateSystemBounds": Object {
            "height": 812,
            "width": 375,
            "x": 0,
            "y": 0,
          },
          "display": "block",
          "flexDirection": "row",
          "globalContentBox": Object {
            "height": 812,
            "width": 375,
            "x": 0,
            "y": 0,
          },
          "htmlElementName": "div",
          "immediateParentBounds": Object {
            "height": 812,
            "width": 375,
            "x": 0,
            "y": 0,
          },
          "immediateParentProvidesLayout": true,
          "layoutSystemForChildren": "flow",
          "margin": Object {
            "bottom": 0,
            "left": 0,
            "right": 0,
            "top": 0,
          },
          "naturalHeight": null,
          "naturalWidth": null,
          "offset": Object {
            "x": 0,
            "y": 0,
          },
          "padding": Object {
            "bottom": 0,
            "left": 0,
            "right": 0,
            "top": 0,
          },
          "parentFlexDirection": "row",
          "parentLayoutSystem": "flow",
          "position": "relative",
          "providesBoundsForChildren": true,
          "renderedChildrenCount": 1,
          "usesParentBounds": true,
        },
      },
      "utopia-storyboard-uid/scene-aaa/app-entity": Object {
        "attributeMetadatada": null,
        "componentInstance": true,
        "computedStyle": null,
        "element": Object {
          "type": "LEFT",
          "value": "REMOVED_FROM_TEST",
        },
        "elementPath": Object {
          "parts": Array [
            Array [
              "utopia-storyboard-uid",
              "scene-aaa",
              "app-entity",
            ],
          ],
          "type": "elementpath",
        },
        "globalFrame": Object {
          "height": 812,
          "width": 375,
          "x": 0,
          "y": 0,
        },
        "importInfo": Object {
          "type": "LEFT",
          "value": "NOT_IMPORTED",
        },
        "isEmotionOrStyledComponent": false,
        "label": null,
        "localFrame": Object {
          "height": 812,
          "width": 375,
          "x": 0,
          "y": 0,
        },
        "props": Object {
          "data-path": "utopia-storyboard-uid/scene-aaa/app-entity",
          "data-uid": "app-entity",
          "data-utopia-instance-path": Object {
            "parts": Array [
              Array [
                "utopia-storyboard-uid",
                "scene-aaa",
                "app-entity",
              ],
            ],
            "type": "elementpath",
          },
          "skipDeepFreeze": true,
          "style": Object {
            "bottom": 0,
            "left": 0,
            "position": "absolute",
            "right": 0,
            "top": 0,
          },
        },
        "specialSizeMeasurements": Object {
          "clientHeight": 812,
          "clientWidth": 375,
          "coordinateSystemBounds": Object {
            "height": 812,
            "width": 375,
            "x": 0,
            "y": 0,
          },
          "display": "flex",
          "flexDirection": "row",
          "globalContentBox": Object {
            "height": 812,
            "width": 375,
            "x": 0,
            "y": 0,
          },
          "htmlElementName": "div",
          "immediateParentBounds": Object {
            "height": 812,
            "width": 375,
            "x": 0,
            "y": 0,
          },
          "immediateParentProvidesLayout": true,
          "layoutSystemForChildren": "flex",
          "margin": Object {
            "bottom": 0,
            "left": 0,
            "right": 0,
            "top": 0,
          },
          "naturalHeight": null,
          "naturalWidth": null,
          "offset": Object {
            "x": 0,
            "y": 0,
          },
          "padding": Object {
            "bottom": 0,
            "left": 0,
            "right": 0,
            "top": 0,
          },
          "parentFlexDirection": "row",
          "parentLayoutSystem": "flow",
          "position": "absolute",
          "providesBoundsForChildren": true,
          "renderedChildrenCount": 1,
          "usesParentBounds": true,
        },
      },
      "utopia-storyboard-uid/scene-aaa/app-entity:05c": Object {
        "attributeMetadatada": null,
        "componentInstance": false,
        "computedStyle": null,
        "element": Object {
          "type": "LEFT",
          "value": "REMOVED_FROM_TEST",
        },
        "elementPath": Object {
          "parts": Array [
            Array [
              "utopia-storyboard-uid",
              "scene-aaa",
              "app-entity",
            ],
            Array [
              "05c",
            ],
          ],
          "type": "elementpath",
        },
        "globalFrame": Object {
          "height": 812,
          "width": 375,
          "x": 0,
          "y": 0,
        },
        "importInfo": Object {
          "type": "LEFT",
          "value": "NOT_IMPORTED",
        },
        "isEmotionOrStyledComponent": false,
        "label": null,
        "localFrame": Object {
          "height": 812,
          "width": 375,
          "x": 0,
          "y": 0,
        },
        "props": Object {
          "data-path": "utopia-storyboard-uid/scene-aaa/app-entity:05c",
          "data-uid": "05c app-entity",
          "skipDeepFreeze": true,
          "style": Object {
            "backgroundColor": "#FFFFFF",
            "bottom": 0,
            "display": "flex",
            "left": 0,
            "position": "absolute",
            "right": 0,
            "top": 0,
          },
        },
        "specialSizeMeasurements": Object {
          "clientHeight": 812,
          "clientWidth": 375,
          "coordinateSystemBounds": Object {
            "height": 812,
            "width": 375,
            "x": 0,
            "y": 0,
          },
          "display": "flex",
          "flexDirection": "row",
          "globalContentBox": Object {
            "height": 812,
            "width": 375,
            "x": 0,
            "y": 0,
          },
          "htmlElementName": "div",
          "immediateParentBounds": Object {
            "height": 812,
            "width": 375,
            "x": 0,
            "y": 0,
          },
          "immediateParentProvidesLayout": true,
          "layoutSystemForChildren": "flex",
          "margin": Object {
            "bottom": 0,
            "left": 0,
            "right": 0,
            "top": 0,
          },
          "naturalHeight": null,
          "naturalWidth": null,
          "offset": Object {
            "x": 0,
            "y": 0,
          },
          "padding": Object {
            "bottom": 0,
            "left": 0,
            "right": 0,
            "top": 0,
          },
          "parentFlexDirection": "row",
          "parentLayoutSystem": "flow",
          "position": "absolute",
          "providesBoundsForChildren": true,
          "renderedChildrenCount": 1,
          "usesParentBounds": true,
        },
      },
      "utopia-storyboard-uid/scene-aaa/app-entity:05c/ef0": Object {
        "attributeMetadatada": null,
        "componentInstance": false,
        "computedStyle": null,
        "element": Object {
          "type": "LEFT",
          "value": "REMOVED_FROM_TEST",
        },
        "elementPath": Object {
          "parts": Array [
            Array [
              "utopia-storyboard-uid",
              "scene-aaa",
              "app-entity",
            ],
            Array [
              "05c",
              "ef0",
            ],
          ],
          "type": "elementpath",
        },
        "globalFrame": Object {
          "height": 164,
          "width": 306,
          "x": 55,
          "y": 98,
        },
        "importInfo": Object {
          "type": "LEFT",
          "value": "NOT_IMPORTED",
        },
        "isEmotionOrStyledComponent": false,
        "label": null,
        "localFrame": Object {
          "height": 164,
          "width": 306,
          "x": 55,
          "y": 98,
        },
        "props": Object {
          "data-path": "utopia-storyboard-uid/scene-aaa/app-entity:05c/ef0",
          "data-uid": "ef0",
          "skipDeepFreeze": true,
          "style": Object {
            "backgroundColor": "#DDDDDD",
            "height": 124,
            "left": 55,
            "padding": 20,
            "position": "fixed",
            "top": 98,
            "width": 266,
          },
        },
        "specialSizeMeasurements": Object {
          "clientHeight": 164,
          "clientWidth": 306,
          "coordinateSystemBounds": null,
          "display": "block",
          "flexDirection": "row",
          "globalContentBox": Object {
            "height": 164,
            "width": 306,
            "x": 55,
            "y": 98,
          },
          "htmlElementName": "div",
          "immediateParentBounds": Object {
            "height": 812,
            "width": 375,
            "x": 0,
            "y": 0,
          },
          "immediateParentProvidesLayout": false,
          "layoutSystemForChildren": "flow",
          "margin": Object {
            "bottom": 0,
            "left": 0,
            "right": 0,
            "top": 0,
          },
          "naturalHeight": null,
          "naturalWidth": null,
          "offset": Object {
            "x": 55,
            "y": 98,
          },
          "padding": Object {
            "bottom": 20,
            "left": 20,
            "right": 20,
            "top": 20,
          },
          "parentFlexDirection": "row",
          "parentLayoutSystem": "flex",
          "position": "fixed",
          "providesBoundsForChildren": true,
          "renderedChildrenCount": 1,
          "usesParentBounds": true,
        },
      },
      "utopia-storyboard-uid/scene-aaa/app-entity:05c/ef0/488": Object {
        "attributeMetadatada": null,
        "componentInstance": false,
        "computedStyle": null,
        "element": Object {
          "type": "LEFT",
          "value": "REMOVED_FROM_TEST",
        },
        "elementPath": Object {
          "parts": Array [
            Array [
              "utopia-storyboard-uid",
              "scene-aaa",
              "app-entity",
            ],
            Array [
              "05c",
              "ef0",
              "488",
            ],
          ],
          "type": "elementpath",
        },
        "globalFrame": Object {
          "height": 70,
          "width": 125,
          "x": 126,
          "y": 125,
        },
        "importInfo": Object {
          "type": "LEFT",
          "value": "NOT_IMPORTED",
        },
        "isEmotionOrStyledComponent": false,
        "label": null,
        "localFrame": Object {
          "height": 70,
          "width": 125,
          "x": 71,
          "y": 27,
        },
        "props": Object {
          "data-path": "utopia-storyboard-uid/scene-aaa/app-entity:05c/ef0/488",
          "data-uid": "488",
          "skipDeepFreeze": true,
          "style": Object {
            "backgroundColor": "#DDDDDD",
            "height": 70,
            "left": 71,
            "position": "absolute",
            "top": 27,
            "width": 125,
          },
        },
        "specialSizeMeasurements": Object {
          "clientHeight": 70,
          "clientWidth": 125,
          "coordinateSystemBounds": Object {
            "height": 164,
            "width": 306,
            "x": 55,
            "y": 98,
          },
          "display": "block",
          "flexDirection": "row",
          "globalContentBox": Object {
            "height": 70,
            "width": 125,
            "x": 126,
            "y": 125,
          },
          "htmlElementName": "div",
          "immediateParentBounds": Object {
            "height": 164,
            "width": 306,
            "x": 55,
            "y": 98,
          },
          "immediateParentProvidesLayout": true,
          "layoutSystemForChildren": "flow",
          "margin": Object {
            "bottom": 0,
            "left": 0,
            "right": 0,
            "top": 0,
          },
          "naturalHeight": null,
          "naturalWidth": null,
          "offset": Object {
            "x": 71,
            "y": 27,
          },
          "padding": Object {
            "bottom": 0,
            "left": 0,
            "right": 0,
            "top": 0,
          },
          "parentFlexDirection": "row",
          "parentLayoutSystem": "flow",
          "position": "absolute",
          "providesBoundsForChildren": true,
          "renderedChildrenCount": 0,
          "usesParentBounds": true,
        },
      },
    }
    `,
    )
  })

  it('Label carried through for normal elements', async () => {
    const sanitizedMetadata = await renderTestEditorWithCode(
      `
      import * as React from 'react'
      import {
        View,
        Scene,
        Storyboard,
      } from 'utopia-api'
      export var App = (props) => {
        return <div style={{ ...props.style}} data-uid={'aaa'} data-label={'Hat'} />
      }
      export var storyboard = (props) => {
        return (
          <Storyboard data-uid={'${BakedInStoryboardUID}'}>
            <Scene
              style={{ position: 'relative', left: 0, top: 0, width: 375, height: 812 }}
              data-uid={'${TestSceneUID}'}
            >
              <App
                data-uid='${TestAppUID}' 
                style={{ position: 'absolute', bottom: 0, left: 0, right: 0, top: 0 }}
              />
            </Scene>
          </Storyboard>
        )
      }
      `,
    )
    matchInlineSnapshotBrowser(
      sanitizedMetadata,
      `
    Object {
      "utopia-storyboard-uid": Object {
        "attributeMetadatada": Object {},
        "componentInstance": true,
        "computedStyle": Object {},
        "element": Object {
          "type": "LEFT",
          "value": "REMOVED_FROM_TEST",
        },
        "elementPath": Object {
          "parts": Array [
            Array [
              "utopia-storyboard-uid",
            ],
          ],
          "type": "elementpath",
        },
        "globalFrame": Object {
          "height": 0,
          "width": 0,
          "x": 0,
          "y": 0,
        },
        "importInfo": Object {
          "type": "RIGHT",
          "value": Object {
            "originalName": "Storyboard",
            "path": "utopia-api",
            "variableName": "Storyboard",
          },
        },
        "isEmotionOrStyledComponent": false,
        "label": null,
        "localFrame": Object {
          "height": 0,
          "width": 0,
          "x": 0,
          "y": 0,
        },
        "props": Object {
          "data-path": "utopia-storyboard-uid",
          "data-uid": "utopia-storyboard-uid",
          "skipDeepFreeze": true,
        },
        "specialSizeMeasurements": Object {
          "clientHeight": 0,
          "clientWidth": 0,
          "coordinateSystemBounds": Object {
            "height": 0,
            "width": 0,
            "x": 0,
            "y": 0,
          },
          "display": "initial",
          "flexDirection": null,
          "globalContentBox": null,
          "htmlElementName": "div",
          "immediateParentBounds": Object {
            "height": 0,
            "width": 0,
            "x": 0,
            "y": 0,
          },
          "immediateParentProvidesLayout": true,
          "layoutSystemForChildren": "flow",
          "margin": Object {},
          "naturalHeight": null,
          "naturalWidth": null,
          "offset": Object {
            "x": 0,
            "y": 0,
          },
          "padding": Object {},
          "parentFlexDirection": null,
          "parentLayoutSystem": "flow",
          "position": "static",
          "providesBoundsForChildren": false,
          "renderedChildrenCount": 0,
          "usesParentBounds": false,
        },
      },
      "utopia-storyboard-uid/scene-aaa": Object {
        "attributeMetadatada": null,
        "componentInstance": true,
        "computedStyle": null,
        "element": Object {
          "type": "LEFT",
          "value": "REMOVED_FROM_TEST",
        },
        "elementPath": Object {
          "parts": Array [
            Array [
              "utopia-storyboard-uid",
              "scene-aaa",
            ],
          ],
          "type": "elementpath",
        },
        "globalFrame": Object {
          "height": 812,
          "width": 375,
          "x": 0,
          "y": 0,
        },
        "importInfo": Object {
          "type": "RIGHT",
          "value": Object {
            "originalName": "Scene",
            "path": "utopia-api",
            "variableName": "Scene",
          },
        },
        "isEmotionOrStyledComponent": false,
        "label": null,
        "localFrame": Object {
          "height": 812,
          "width": 375,
          "x": 0,
          "y": 0,
        },
        "props": Object {
          "data-path": "utopia-storyboard-uid/scene-aaa",
          "data-uid": "scene-aaa",
          "data-utopia-scene-id": "utopia-storyboard-uid/scene-aaa",
          "skipDeepFreeze": true,
          "style": Object {
            "height": 812,
            "left": 0,
            "position": "relative",
            "top": 0,
            "width": 375,
          },
        },
        "specialSizeMeasurements": Object {
          "clientHeight": 812,
          "clientWidth": 375,
          "coordinateSystemBounds": Object {
            "height": 812,
            "width": 375,
            "x": 0,
            "y": 0,
          },
          "display": "block",
          "flexDirection": "row",
          "globalContentBox": Object {
            "height": 812,
            "width": 375,
            "x": 0,
            "y": 0,
          },
          "htmlElementName": "div",
          "immediateParentBounds": Object {
            "height": 812,
            "width": 375,
            "x": 0,
            "y": 0,
          },
          "immediateParentProvidesLayout": true,
          "layoutSystemForChildren": "flow",
          "margin": Object {
            "bottom": 0,
            "left": 0,
            "right": 0,
            "top": 0,
          },
          "naturalHeight": null,
          "naturalWidth": null,
          "offset": Object {
            "x": 0,
            "y": 0,
          },
          "padding": Object {
            "bottom": 0,
            "left": 0,
            "right": 0,
            "top": 0,
          },
          "parentFlexDirection": "row",
          "parentLayoutSystem": "flow",
          "position": "relative",
          "providesBoundsForChildren": true,
          "renderedChildrenCount": 1,
          "usesParentBounds": true,
        },
      },
      "utopia-storyboard-uid/scene-aaa/app-entity": Object {
        "attributeMetadatada": null,
        "componentInstance": true,
        "computedStyle": null,
        "element": Object {
          "type": "LEFT",
          "value": "REMOVED_FROM_TEST",
        },
        "elementPath": Object {
          "parts": Array [
            Array [
              "utopia-storyboard-uid",
              "scene-aaa",
              "app-entity",
            ],
          ],
          "type": "elementpath",
        },
        "globalFrame": Object {
          "height": 812,
          "width": 375,
          "x": 0,
          "y": 0,
        },
        "importInfo": Object {
          "type": "LEFT",
          "value": "NOT_IMPORTED",
        },
        "isEmotionOrStyledComponent": false,
        "label": null,
        "localFrame": Object {
          "height": 812,
          "width": 375,
          "x": 0,
          "y": 0,
        },
        "props": Object {
          "data-path": "utopia-storyboard-uid/scene-aaa/app-entity",
          "data-uid": "app-entity",
          "data-utopia-instance-path": Object {
            "parts": Array [
              Array [
                "utopia-storyboard-uid",
                "scene-aaa",
                "app-entity",
              ],
            ],
            "type": "elementpath",
          },
          "skipDeepFreeze": true,
          "style": Object {
            "bottom": 0,
            "left": 0,
            "position": "absolute",
            "right": 0,
            "top": 0,
          },
        },
        "specialSizeMeasurements": Object {
          "clientHeight": 812,
          "clientWidth": 375,
          "coordinateSystemBounds": Object {
            "height": 812,
            "width": 375,
            "x": 0,
            "y": 0,
          },
          "display": "block",
          "flexDirection": "row",
          "globalContentBox": Object {
            "height": 812,
            "width": 375,
            "x": 0,
            "y": 0,
          },
          "htmlElementName": "div",
          "immediateParentBounds": Object {
            "height": 812,
            "width": 375,
            "x": 0,
            "y": 0,
          },
          "immediateParentProvidesLayout": true,
          "layoutSystemForChildren": "flow",
          "margin": Object {
            "bottom": 0,
            "left": 0,
            "right": 0,
            "top": 0,
          },
          "naturalHeight": null,
          "naturalWidth": null,
          "offset": Object {
            "x": 0,
            "y": 0,
          },
          "padding": Object {
            "bottom": 0,
            "left": 0,
            "right": 0,
            "top": 0,
          },
          "parentFlexDirection": "row",
          "parentLayoutSystem": "flow",
          "position": "absolute",
          "providesBoundsForChildren": true,
          "renderedChildrenCount": 0,
          "usesParentBounds": true,
        },
      },
      "utopia-storyboard-uid/scene-aaa/app-entity:aaa": Object {
        "attributeMetadatada": null,
        "componentInstance": false,
        "computedStyle": null,
        "element": Object {
          "type": "LEFT",
          "value": "REMOVED_FROM_TEST",
        },
        "elementPath": Object {
          "parts": Array [
            Array [
              "utopia-storyboard-uid",
              "scene-aaa",
              "app-entity",
            ],
            Array [
              "aaa",
            ],
          ],
          "type": "elementpath",
        },
        "globalFrame": Object {
          "height": 812,
          "width": 375,
          "x": 0,
          "y": 0,
        },
        "importInfo": Object {
          "type": "LEFT",
          "value": "NOT_IMPORTED",
        },
        "isEmotionOrStyledComponent": false,
        "label": null,
        "localFrame": Object {
          "height": 812,
          "width": 375,
          "x": 0,
          "y": 0,
        },
        "props": Object {
          "data-label": "Hat",
          "data-path": "utopia-storyboard-uid/scene-aaa/app-entity:aaa",
          "data-uid": "aaa app-entity",
          "skipDeepFreeze": true,
          "style": Object {
            "bottom": 0,
            "left": 0,
            "position": "absolute",
            "right": 0,
            "top": 0,
          },
        },
        "specialSizeMeasurements": Object {
          "clientHeight": 812,
          "clientWidth": 375,
          "coordinateSystemBounds": Object {
            "height": 812,
            "width": 375,
            "x": 0,
            "y": 0,
          },
          "display": "block",
          "flexDirection": "row",
          "globalContentBox": Object {
            "height": 812,
            "width": 375,
            "x": 0,
            "y": 0,
          },
          "htmlElementName": "div",
          "immediateParentBounds": Object {
            "height": 812,
            "width": 375,
            "x": 0,
            "y": 0,
          },
          "immediateParentProvidesLayout": true,
          "layoutSystemForChildren": "flow",
          "margin": Object {
            "bottom": 0,
            "left": 0,
            "right": 0,
            "top": 0,
          },
          "naturalHeight": null,
          "naturalWidth": null,
          "offset": Object {
            "x": 0,
            "y": 0,
          },
          "padding": Object {
            "bottom": 0,
            "left": 0,
            "right": 0,
            "top": 0,
          },
          "parentFlexDirection": "row",
          "parentLayoutSystem": "flow",
          "position": "absolute",
          "providesBoundsForChildren": true,
          "renderedChildrenCount": 0,
          "usesParentBounds": true,
        },
      },
    }
    `,
    )
  })

  it('Label carried through for generated elements', async () => {
    const sanitizedMetadata = await renderTestEditorWithCode(
      `
      import * as React from 'react'
      import {
        View,
        Scene,
        Storyboard,
      } from 'utopia-api'
      export var App = (props) => {
        return <div style={{ ...props.style}} data-uid={'aaa'}>
          {[1, 2, 3].map(n => {
            return <div data-uid={'bbb'} data-label={'Plane'} />
          })}
        </div>
      }
      export var storyboard = (props) => {
        return (
          <Storyboard data-uid={'${BakedInStoryboardUID}'}>
            <Scene
              style={{ position: 'relative', left: 0, top: 0, width: 375, height: 812 }}
              data-uid={'${TestSceneUID}'}
            >
              <App
                data-uid='${TestAppUID}' 
                style={{ position: 'absolute', bottom: 0, left: 0, right: 0, top: 0 }}
              />
            </Scene>
          </Storyboard>
        )
      }
      `,
    )
    matchInlineSnapshotBrowser(
      sanitizedMetadata,
      `
    Object {
      "utopia-storyboard-uid": Object {
        "attributeMetadatada": Object {},
        "componentInstance": true,
        "computedStyle": Object {},
        "element": Object {
          "type": "LEFT",
          "value": "REMOVED_FROM_TEST",
        },
        "elementPath": Object {
          "parts": Array [
            Array [
              "utopia-storyboard-uid",
            ],
          ],
          "type": "elementpath",
        },
        "globalFrame": Object {
          "height": 0,
          "width": 0,
          "x": 0,
          "y": 0,
        },
        "importInfo": Object {
          "type": "RIGHT",
          "value": Object {
            "originalName": "Storyboard",
            "path": "utopia-api",
            "variableName": "Storyboard",
          },
        },
        "isEmotionOrStyledComponent": false,
        "label": null,
        "localFrame": Object {
          "height": 0,
          "width": 0,
          "x": 0,
          "y": 0,
        },
        "props": Object {
          "data-path": "utopia-storyboard-uid",
          "data-uid": "utopia-storyboard-uid",
          "skipDeepFreeze": true,
        },
        "specialSizeMeasurements": Object {
          "clientHeight": 0,
          "clientWidth": 0,
          "coordinateSystemBounds": Object {
            "height": 0,
            "width": 0,
            "x": 0,
            "y": 0,
          },
          "display": "initial",
          "flexDirection": null,
          "globalContentBox": null,
          "htmlElementName": "div",
          "immediateParentBounds": Object {
            "height": 0,
            "width": 0,
            "x": 0,
            "y": 0,
          },
          "immediateParentProvidesLayout": true,
          "layoutSystemForChildren": "flow",
          "margin": Object {},
          "naturalHeight": null,
          "naturalWidth": null,
          "offset": Object {
            "x": 0,
            "y": 0,
          },
          "padding": Object {},
          "parentFlexDirection": null,
          "parentLayoutSystem": "flow",
          "position": "static",
          "providesBoundsForChildren": false,
          "renderedChildrenCount": 0,
          "usesParentBounds": false,
        },
      },
      "utopia-storyboard-uid/scene-aaa": Object {
        "attributeMetadatada": null,
        "componentInstance": true,
        "computedStyle": null,
        "element": Object {
          "type": "LEFT",
          "value": "REMOVED_FROM_TEST",
        },
        "elementPath": Object {
          "parts": Array [
            Array [
              "utopia-storyboard-uid",
              "scene-aaa",
            ],
          ],
          "type": "elementpath",
        },
        "globalFrame": Object {
          "height": 812,
          "width": 375,
          "x": 0,
          "y": 0,
        },
        "importInfo": Object {
          "type": "RIGHT",
          "value": Object {
            "originalName": "Scene",
            "path": "utopia-api",
            "variableName": "Scene",
          },
        },
        "isEmotionOrStyledComponent": false,
        "label": null,
        "localFrame": Object {
          "height": 812,
          "width": 375,
          "x": 0,
          "y": 0,
        },
        "props": Object {
          "data-path": "utopia-storyboard-uid/scene-aaa",
          "data-uid": "scene-aaa",
          "data-utopia-scene-id": "utopia-storyboard-uid/scene-aaa",
          "skipDeepFreeze": true,
          "style": Object {
            "height": 812,
            "left": 0,
            "position": "relative",
            "top": 0,
            "width": 375,
          },
        },
        "specialSizeMeasurements": Object {
          "clientHeight": 812,
          "clientWidth": 375,
          "coordinateSystemBounds": Object {
            "height": 812,
            "width": 375,
            "x": 0,
            "y": 0,
          },
          "display": "block",
          "flexDirection": "row",
          "globalContentBox": Object {
            "height": 812,
            "width": 375,
            "x": 0,
            "y": 0,
          },
          "htmlElementName": "div",
          "immediateParentBounds": Object {
            "height": 812,
            "width": 375,
            "x": 0,
            "y": 0,
          },
          "immediateParentProvidesLayout": true,
          "layoutSystemForChildren": "flow",
          "margin": Object {
            "bottom": 0,
            "left": 0,
            "right": 0,
            "top": 0,
          },
          "naturalHeight": null,
          "naturalWidth": null,
          "offset": Object {
            "x": 0,
            "y": 0,
          },
          "padding": Object {
            "bottom": 0,
            "left": 0,
            "right": 0,
            "top": 0,
          },
          "parentFlexDirection": "row",
          "parentLayoutSystem": "flow",
          "position": "relative",
          "providesBoundsForChildren": true,
          "renderedChildrenCount": 1,
          "usesParentBounds": true,
        },
      },
      "utopia-storyboard-uid/scene-aaa/app-entity": Object {
        "attributeMetadatada": null,
        "componentInstance": true,
        "computedStyle": null,
        "element": Object {
          "type": "LEFT",
          "value": "REMOVED_FROM_TEST",
        },
        "elementPath": Object {
          "parts": Array [
            Array [
              "utopia-storyboard-uid",
              "scene-aaa",
              "app-entity",
            ],
          ],
          "type": "elementpath",
        },
        "globalFrame": Object {
          "height": 812,
          "width": 375,
          "x": 0,
          "y": 0,
        },
        "importInfo": Object {
          "type": "LEFT",
          "value": "NOT_IMPORTED",
        },
        "isEmotionOrStyledComponent": false,
        "label": null,
        "localFrame": Object {
          "height": 812,
          "width": 375,
          "x": 0,
          "y": 0,
        },
        "props": Object {
          "data-path": "utopia-storyboard-uid/scene-aaa/app-entity",
          "data-uid": "app-entity",
          "data-utopia-instance-path": Object {
            "parts": Array [
              Array [
                "utopia-storyboard-uid",
                "scene-aaa",
                "app-entity",
              ],
            ],
            "type": "elementpath",
          },
          "skipDeepFreeze": true,
          "style": Object {
            "bottom": 0,
            "left": 0,
            "position": "absolute",
            "right": 0,
            "top": 0,
          },
        },
        "specialSizeMeasurements": Object {
          "clientHeight": 812,
          "clientWidth": 375,
          "coordinateSystemBounds": Object {
            "height": 812,
            "width": 375,
            "x": 0,
            "y": 0,
          },
          "display": "block",
          "flexDirection": "row",
          "globalContentBox": Object {
            "height": 812,
            "width": 375,
            "x": 0,
            "y": 0,
          },
          "htmlElementName": "div",
          "immediateParentBounds": Object {
            "height": 812,
            "width": 375,
            "x": 0,
            "y": 0,
          },
          "immediateParentProvidesLayout": true,
          "layoutSystemForChildren": "flow",
          "margin": Object {
            "bottom": 0,
            "left": 0,
            "right": 0,
            "top": 0,
          },
          "naturalHeight": null,
          "naturalWidth": null,
          "offset": Object {
            "x": 0,
            "y": 0,
          },
          "padding": Object {
            "bottom": 0,
            "left": 0,
            "right": 0,
            "top": 0,
          },
          "parentFlexDirection": "row",
          "parentLayoutSystem": "flow",
          "position": "absolute",
          "providesBoundsForChildren": true,
          "renderedChildrenCount": 3,
          "usesParentBounds": true,
        },
      },
      "utopia-storyboard-uid/scene-aaa/app-entity:aaa": Object {
        "attributeMetadatada": null,
        "componentInstance": false,
        "computedStyle": null,
        "element": Object {
          "type": "LEFT",
          "value": "REMOVED_FROM_TEST",
        },
        "elementPath": Object {
          "parts": Array [
            Array [
              "utopia-storyboard-uid",
              "scene-aaa",
              "app-entity",
            ],
            Array [
              "aaa",
            ],
          ],
          "type": "elementpath",
        },
        "globalFrame": Object {
          "height": 812,
          "width": 375,
          "x": 0,
          "y": 0,
        },
        "importInfo": Object {
          "type": "LEFT",
          "value": "NOT_IMPORTED",
        },
        "isEmotionOrStyledComponent": false,
        "label": null,
        "localFrame": Object {
          "height": 812,
          "width": 375,
          "x": 0,
          "y": 0,
        },
        "props": Object {
          "data-path": "utopia-storyboard-uid/scene-aaa/app-entity:aaa",
          "data-uid": "aaa app-entity",
          "skipDeepFreeze": true,
          "style": Object {
            "bottom": 0,
            "left": 0,
            "position": "absolute",
            "right": 0,
            "top": 0,
          },
        },
        "specialSizeMeasurements": Object {
          "clientHeight": 812,
          "clientWidth": 375,
          "coordinateSystemBounds": Object {
            "height": 812,
            "width": 375,
            "x": 0,
            "y": 0,
          },
          "display": "block",
          "flexDirection": "row",
          "globalContentBox": Object {
            "height": 812,
            "width": 375,
            "x": 0,
            "y": 0,
          },
          "htmlElementName": "div",
          "immediateParentBounds": Object {
            "height": 812,
            "width": 375,
            "x": 0,
            "y": 0,
          },
          "immediateParentProvidesLayout": true,
          "layoutSystemForChildren": "flow",
          "margin": Object {
            "bottom": 0,
            "left": 0,
            "right": 0,
            "top": 0,
          },
          "naturalHeight": null,
          "naturalWidth": null,
          "offset": Object {
            "x": 0,
            "y": 0,
          },
          "padding": Object {
            "bottom": 0,
            "left": 0,
            "right": 0,
            "top": 0,
          },
          "parentFlexDirection": "row",
          "parentLayoutSystem": "flow",
          "position": "absolute",
          "providesBoundsForChildren": true,
          "renderedChildrenCount": 3,
          "usesParentBounds": true,
        },
      },
      "utopia-storyboard-uid/scene-aaa/app-entity:aaa/bbb~~~1": Object {
        "attributeMetadatada": null,
        "componentInstance": false,
        "computedStyle": null,
        "element": Object {
          "type": "LEFT",
          "value": "REMOVED_FROM_TEST",
        },
        "elementPath": Object {
          "parts": Array [
            Array [
              "utopia-storyboard-uid",
              "scene-aaa",
              "app-entity",
            ],
            Array [
              "aaa",
              "bbb~~~1",
            ],
          ],
          "type": "elementpath",
        },
        "globalFrame": Object {
          "height": 0,
          "width": 375,
          "x": 0,
          "y": 0,
        },
        "importInfo": Object {
          "type": "LEFT",
          "value": "NOT_IMPORTED",
        },
        "isEmotionOrStyledComponent": false,
        "label": null,
        "localFrame": Object {
          "height": 0,
          "width": 375,
          "x": 0,
          "y": 0,
        },
        "props": Object {
          "data-label": "Plane",
          "data-path": "utopia-storyboard-uid/scene-aaa/app-entity:aaa/bbb~~~1",
          "data-uid": "bbb~~~1",
          "skipDeepFreeze": true,
        },
        "specialSizeMeasurements": Object {
          "clientHeight": 0,
          "clientWidth": 375,
          "coordinateSystemBounds": Object {
            "height": 812,
            "width": 375,
            "x": 0,
            "y": 0,
          },
          "display": "block",
          "flexDirection": "row",
          "globalContentBox": Object {
            "height": 0,
            "width": 375,
            "x": 0,
            "y": 0,
          },
          "htmlElementName": "div",
          "immediateParentBounds": Object {
            "height": 812,
            "width": 375,
            "x": 0,
            "y": 0,
          },
          "immediateParentProvidesLayout": true,
          "layoutSystemForChildren": "flow",
          "margin": Object {
            "bottom": 0,
            "left": 0,
            "right": 0,
            "top": 0,
          },
          "naturalHeight": null,
          "naturalWidth": null,
          "offset": Object {
            "x": 0,
            "y": 0,
          },
          "padding": Object {
            "bottom": 0,
            "left": 0,
            "right": 0,
            "top": 0,
          },
          "parentFlexDirection": "row",
          "parentLayoutSystem": "flow",
          "position": "static",
          "providesBoundsForChildren": false,
          "renderedChildrenCount": 0,
          "usesParentBounds": true,
        },
      },
      "utopia-storyboard-uid/scene-aaa/app-entity:aaa/bbb~~~2": Object {
        "attributeMetadatada": null,
        "componentInstance": false,
        "computedStyle": null,
        "element": Object {
          "type": "LEFT",
          "value": "REMOVED_FROM_TEST",
        },
        "elementPath": Object {
          "parts": Array [
            Array [
              "utopia-storyboard-uid",
              "scene-aaa",
              "app-entity",
            ],
            Array [
              "aaa",
              "bbb~~~2",
            ],
          ],
          "type": "elementpath",
        },
        "globalFrame": Object {
          "height": 0,
          "width": 375,
          "x": 0,
          "y": 0,
        },
        "importInfo": Object {
          "type": "LEFT",
          "value": "NOT_IMPORTED",
        },
        "isEmotionOrStyledComponent": false,
        "label": null,
        "localFrame": Object {
          "height": 0,
          "width": 375,
          "x": 0,
          "y": 0,
        },
        "props": Object {
          "data-label": "Plane",
          "data-path": "utopia-storyboard-uid/scene-aaa/app-entity:aaa/bbb~~~2",
          "data-uid": "bbb~~~2",
          "skipDeepFreeze": true,
        },
        "specialSizeMeasurements": Object {
          "clientHeight": 0,
          "clientWidth": 375,
          "coordinateSystemBounds": Object {
            "height": 812,
            "width": 375,
            "x": 0,
            "y": 0,
          },
          "display": "block",
          "flexDirection": "row",
          "globalContentBox": Object {
            "height": 0,
            "width": 375,
            "x": 0,
            "y": 0,
          },
          "htmlElementName": "div",
          "immediateParentBounds": Object {
            "height": 812,
            "width": 375,
            "x": 0,
            "y": 0,
          },
          "immediateParentProvidesLayout": true,
          "layoutSystemForChildren": "flow",
          "margin": Object {
            "bottom": 0,
            "left": 0,
            "right": 0,
            "top": 0,
          },
          "naturalHeight": null,
          "naturalWidth": null,
          "offset": Object {
            "x": 0,
            "y": 0,
          },
          "padding": Object {
            "bottom": 0,
            "left": 0,
            "right": 0,
            "top": 0,
          },
          "parentFlexDirection": "row",
          "parentLayoutSystem": "flow",
          "position": "static",
          "providesBoundsForChildren": false,
          "renderedChildrenCount": 0,
          "usesParentBounds": true,
        },
      },
      "utopia-storyboard-uid/scene-aaa/app-entity:aaa/bbb~~~3": Object {
        "attributeMetadatada": null,
        "componentInstance": false,
        "computedStyle": null,
        "element": Object {
          "type": "LEFT",
          "value": "REMOVED_FROM_TEST",
        },
        "elementPath": Object {
          "parts": Array [
            Array [
              "utopia-storyboard-uid",
              "scene-aaa",
              "app-entity",
            ],
            Array [
              "aaa",
              "bbb~~~3",
            ],
          ],
          "type": "elementpath",
        },
        "globalFrame": Object {
          "height": 0,
          "width": 375,
          "x": 0,
          "y": 0,
        },
        "importInfo": Object {
          "type": "LEFT",
          "value": "NOT_IMPORTED",
        },
        "isEmotionOrStyledComponent": false,
        "label": null,
        "localFrame": Object {
          "height": 0,
          "width": 375,
          "x": 0,
          "y": 0,
        },
        "props": Object {
          "data-label": "Plane",
          "data-path": "utopia-storyboard-uid/scene-aaa/app-entity:aaa/bbb~~~3",
          "data-uid": "bbb~~~3",
          "skipDeepFreeze": true,
        },
        "specialSizeMeasurements": Object {
          "clientHeight": 0,
          "clientWidth": 375,
          "coordinateSystemBounds": Object {
            "height": 812,
            "width": 375,
            "x": 0,
            "y": 0,
          },
          "display": "block",
          "flexDirection": "row",
          "globalContentBox": Object {
            "height": 0,
            "width": 375,
            "x": 0,
            "y": 0,
          },
          "htmlElementName": "div",
          "immediateParentBounds": Object {
            "height": 812,
            "width": 375,
            "x": 0,
            "y": 0,
          },
          "immediateParentProvidesLayout": true,
          "layoutSystemForChildren": "flow",
          "margin": Object {
            "bottom": 0,
            "left": 0,
            "right": 0,
            "top": 0,
          },
          "naturalHeight": null,
          "naturalWidth": null,
          "offset": Object {
            "x": 0,
            "y": 0,
          },
          "padding": Object {
            "bottom": 0,
            "left": 0,
            "right": 0,
            "top": 0,
          },
          "parentFlexDirection": "row",
          "parentLayoutSystem": "flow",
          "position": "static",
          "providesBoundsForChildren": false,
          "renderedChildrenCount": 0,
          "usesParentBounds": true,
        },
      },
    }
    `,
    )
  })
})
