import React from 'react'
import { renderHook } from '@testing-library/react-hooks'
import { EditorStateContext } from '../../editor/store/store-hook'
import { useGetPropertyControlsForSelectedComponents } from './property-controls-hooks'
import {
  InspectorCallbackContext,
  InspectorCallbackContextData,
  InspectorPropsContext,
} from './property-path-hooks'
import create from 'zustand'
import {
  createEditorState,
  editorModelFromPersistentModel,
  EditorState,
  EditorStore,
  StoryboardFilePath,
} from '../../editor/store/editor-state'
import { NO_OP } from '../../../core/shared/utils'
import * as EP from '../../../core/shared/element-path'
import { projectContentFile } from '../../assets'
import { textFile, textFileContents } from '../../../core/shared/project-file-types'
import { createTestProjectWithCode } from '../../../sample-projects/sample-project-utils.test-utils'
import { TestAppUID, TestSceneUID } from '../../canvas/ui-jsx.test-utils'
import { BakedInStoryboardUID } from '../../../core/model/scene-utils'
import { PropertyControls } from 'utopia-api'

function callPropertyControlsHook() {
  const propertyControlsForTest: PropertyControls = {
    kutya: {
      type: 'string',
      title: 'Kutya',
      defaultValue: 'doggie',
    },
  }

  const persistentModel = createTestProjectWithCode(`
  import * as React from 'react'
  import {
    View,
    Scene,
    Storyboard,
  } from 'utopia-api'
  export var App = (props) => {
    return (
      <div data-uid={'aaa'}/>
    )
  }
  // Note: for this test, we are not running the property controls parser. I'm only including the code here for posterity
  App.propertyControls = ${JSON.stringify(propertyControlsForTest)}


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
  `)

  const selectedViews = [EP.elementPath([[BakedInStoryboardUID, TestSceneUID, TestAppUID]])]

  const initialEditorState = editorModelFromPersistentModel(persistentModel, NO_OP)
  const editorState: EditorState = {
    ...initialEditorState,
    selectedViews: selectedViews,
    propertyControlsInfo: {
      ...initialEditorState.propertyControlsInfo,
      '/utopia/storyboard': { App: propertyControlsForTest },
    },
  }

  const initialEditorStore: EditorStore = {
    editor: editorState,
    derived: null as any,
    history: null as any,
    userState: null as any,
    workers: null as any,
    persistence: null as any,
    dispatch: null as any,
    alreadySaved: null as any,
  }

  const storeHook = create<EditorStore>((set) => initialEditorStore)

  const inspectorCallbackContext: InspectorCallbackContextData = {
    selectedViewsRef: { current: selectedViews },
    onSubmitValue: null as any,
    onUnsetValue: null as any,
  }

  const contextProvider = ({ children }: any) => (
    <EditorStateContext.Provider value={{ useStore: storeHook, api: storeHook }}>
      <InspectorCallbackContext.Provider value={inspectorCallbackContext}>
        {children}
      </InspectorCallbackContext.Provider>
    </EditorStateContext.Provider>
  )

  const { result } = renderHook(() => useGetPropertyControlsForSelectedComponents(), {
    wrapper: contextProvider,
  })
  return { result: result.current, getEditorState: () => storeHook.getState() }
}

describe('useGetPropertyControlsForSelectedComponents', () => {
  it('uh huh', () => {
    const { result } = callPropertyControlsHook()
    expect(result).toMatchInlineSnapshot(`
      Array [
        Object {
          "controls": Object {
            "type": "RIGHT",
            "value": Object {
              "kutya": Object {
                "type": "RIGHT",
                "value": Object {
                  "defaultValue": "doggie",
                  "title": "Kutya",
                  "type": "string",
                },
              },
            },
          },
          "detectedPropsAndValuesWithoutControls": Object {},
          "detectedPropsWithNoValue": Array [],
          "targets": Array [
            Object {
              "parts": Array [
                Array [
                  "utopia-storyboard-uid",
                  "scene-aaa",
                  "app-entity",
                ],
              ],
              "type": "elementpath",
            },
          ],
        },
      ]
    `)
  })
})
