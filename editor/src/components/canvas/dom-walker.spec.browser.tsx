jest.mock('../editor/stored-state', () => ({
  loadStoredState: () => Promise.resolve(null),
  saveStoredState: () => Promise.resolve(),
}))

import { act, render } from '@testing-library/react'
import * as React from 'react'
import create from 'zustand'
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
import { HotRoot } from '../../templates/editor'
import { left } from '../../core/shared/either'
import { EditorDispatch } from '../editor/action-types'
import { load } from '../editor/actions/actions'
import * as History from '../editor/history'
import { editorDispatch } from '../editor/store/dispatch'
import { createEditorState, deriveState, EditorStore } from '../editor/store/editor-state'
import Utils from '../../utils/utils'
import { BakedInStoryboardUID } from '../../core/model/scene-utils'
import { NO_OP } from '../../core/shared/utils'
import { mapValues } from '../../core/shared/object-utils'
import { emptyUiJsxCanvasContextData } from './ui-jsx-canvas'
import { TestAppUID, TestSceneUID } from './ui-jsx.test-utils'
import { createTestProjectWithCode } from '../../sample-projects/sample-project-utils.test-utils'
import { DummyPersistenceMachine } from '../editor/persistence/persistence.test-utils'

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
    const result = editorDispatch(dispatch, actions, storeHook.getState(), spyCollector)
    storeHook.setState(result)
  }

  const initialEditorStore: EditorStore = {
    editor: emptyEditorState,
    derived: derivedState,
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
  }

  const storeHook = create<EditorStore>((set) => initialEditorStore)

  render(
    <HotRoot
      api={storeHook}
      useStore={storeHook}
      spyCollector={spyCollector}
      propertyControlsInfoSupported={false}
      vscodeBridgeReady={false}
    />,
  )

  await act(async () => {
    await load(dispatch, createTestProjectWithCode(appUiJsFileCode), 'Test', '0', false)
  })
  const sanitizedMetadata = sanitizeJsxMetadata(storeHook.getState().editor.jsxMetadata)
  expect(sanitizedMetadata).toMatchSnapshot()
}

describe('DOM Walker tests', () => {
  it('Simple Project with one child View', async () => {
    await renderTestEditorWithCode(
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
  })

  it('Simple Project with divs', async () => {
    await renderTestEditorWithCode(
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
  })

  it('Simple Project with flex parent', async () => {
    await renderTestEditorWithCode(
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
  })

  it('Label carried through for normal elements', async () => {
    await renderTestEditorWithCode(
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
  })

  it('Label carried through for generated elements', async () => {
    await renderTestEditorWithCode(
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
  })
})
