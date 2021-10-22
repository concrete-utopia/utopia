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
import { ElementPath, textFile, textFileContents } from '../../../core/shared/project-file-types'
import { createTestProjectWithCode } from '../../../sample-projects/sample-project-utils.test-utils'
import { TestAppUID, TestSceneUID } from '../../canvas/ui-jsx.test-utils'
import { BakedInStoryboardUID } from '../../../core/model/scene-utils'
import { PropertyControls } from 'utopia-api'
import {
  elementInstanceMetadata,
  ElementInstanceMetadataMap,
} from '../../../core/shared/element-template'
import { right } from '../../../core/shared/either'
import { objectMap } from '../../../core/shared/object-utils'

const TestAppUID2 = 'app-entity-2'

const propertyControlsForTest: PropertyControls = {
  propWithControlButNoValue: {
    type: 'string',
    title: 'No Value',
    defaultValue: 'doggie',
  },
}
function callPropertyControlsHook(selectedViews: ElementPath[]) {
  const persistentModel = createTestProjectWithCode(`
  import * as React from 'react'
  import {
    View,
    Scene,
    Storyboard,
  } from 'utopia-api'
  export var App = (props) => {
    const propToDetect = props.testDetectedPropertyWithNoValue
    return (
      <div data-uid={'aaa'}/>
    )
  }
  // Note: for this test, we are not running the property controls parser. Otherwise this is where App.propertyControls would come
 

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
            testPropWithoutControl='yes'
          />
          <App
            data-uid='${TestAppUID2}' 
            style={{ position: 'absolute', bottom: 0, left: 0, right: 0, top: 0 }}
            propWithControlButNoValue='but there is a value!'
          />
        </Scene>
      </Storyboard>
    )
  }
  `)

  let metadata: ElementInstanceMetadataMap = {
    [EP.toString(selectedViews[0])]: elementInstanceMetadata(
      selectedViews[0],
      null as any,
      { testPropWithoutControl: 'yes' },
      null,
      null,
      true,
      false,
      null as any,
      null,
      null,
      null,
      null,
    ),
  }

  if (selectedViews.length > 1) {
    metadata[EP.toString(selectedViews[1])] = elementInstanceMetadata(
      selectedViews[1],
      null as any,
      { propWithControlButNoValue: 'but there is a value!' },
      null,
      null,
      true,
      false,
      null as any,
      null,
      null,
      null,
      null,
    )
  }

  const initialEditorState = editorModelFromPersistentModel(persistentModel, NO_OP)
  const editorState: EditorState = {
    ...initialEditorState,
    selectedViews: selectedViews,
    propertyControlsInfo: {
      ...initialEditorState.propertyControlsInfo,
      '/utopia/storyboard': { App: propertyControlsForTest },
    },
    jsxMetadata: metadata,
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
  it('single select', () => {
    const selectedViews = [EP.elementPath([[BakedInStoryboardUID, TestSceneUID, TestAppUID]])]
    const { result } = callPropertyControlsHook(selectedViews)

    expect(result[0].controls).toEqual(right(objectMap(right, propertyControlsForTest)))
    expect(result[0].detectedPropsAndValuesWithoutControls).toEqual({
      testPropWithoutControl: 'yes',
    })
    expect(result[0].detectedPropsWithNoValue).toEqual(['testDetectedPropertyWithNoValue'])
    expect(result[0].propsWithControlsButNoValue).toEqual(['propWithControlButNoValue'])
    expect(result[0].targets).toEqual(selectedViews)
  })

  it('multiselect', () => {
    const selectedViews = [
      EP.elementPath([[BakedInStoryboardUID, TestSceneUID, TestAppUID]]),
      EP.elementPath([[BakedInStoryboardUID, TestSceneUID, TestAppUID2]]),
    ]
    const { result } = callPropertyControlsHook(selectedViews)

    expect(result[0].controls).toEqual(right(objectMap(right, propertyControlsForTest)))
    expect(result[0].detectedPropsAndValuesWithoutControls).toEqual({
      testPropWithoutControl: 'yes',
    })
    expect(result[0].detectedPropsWithNoValue).toEqual(['testDetectedPropertyWithNoValue'])
    expect(result[0].propsWithControlsButNoValue).toEqual([])
    expect(result[0].targets).toEqual(selectedViews)
  })

  // TODO for next week
  xit('TODO: a test with multiple _different_ results', () => {})
})
