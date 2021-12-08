import React from 'react'
import * as ReactDOM from 'react-dom'
import create from 'zustand'

import { DndProvider } from 'react-dnd'
import Backend from 'react-dnd-html5-backend'

import { DesignPanelRoot } from '../src/components/canvas/design-panel-root'
import {
  emptyUiJsxCanvasContextData,
  UiJsxCanvasContextData,
  UiJsxCanvasCtxAtom,
} from '../src/components/canvas/ui-jsx-canvas'
import { DispatchPriority, EditorAction } from '../src/components/editor/action-types'
import { load } from '../src/components/editor/actions/actions'
import {
  createEditorState,
  defaultUserState,
  deriveState,
  EditorStore,
  PersistentModel,
} from '../src/components/editor/store/editor-state'
import {
  EditorStateContext,
  UtopiaStoreAPI,
  UtopiaStoreHook,
} from '../src/components/editor/store/store-hook'
import * as History from '../src/components/editor/history'
import { createBuiltInDependenciesList } from '../src/core/es-modules/package-manager/built-in-dependencies-list'
import { UtopiaTsWorkersImplementation } from '../src/core/workers/workers'
import {
  FakeLinterWorker,
  FakeParserPrinterWorker,
  FakeWatchdogWorker,
} from '../src/core/workers/test-workers'
import { DummyPersistenceMachine } from '../src/components/editor/persistence/persistence.test-utils'
import { editorDispatch } from '../src/components/editor/store/dispatch'
import { TestProject } from './test-project'

const boundDispatch = (
  dispatchedActions: readonly EditorAction[],
  priority?: DispatchPriority,
): {
  entireUpdateFinished: Promise<any>
} => {
  const runDispatch = () => {
    const result = editorDispatch(boundDispatch, dispatchedActions, storedState, spyCollector)
    storedState = result

    if (!result.nothingChanged) {
      // we update the zustand store with the new editor state. this will trigger a re-render in the EditorComponent
      storeHook.setState({
        ...result,
      })
    }
    return { entireUpdateFinished: result.entireUpdateFinished }
  }
  return runDispatch()
}

const initialState = {
  editor: createEditorState(boundDispatch),
  derived: deriveState(createEditorState(boundDispatch), null),
  history: History.init(
    createEditorState(boundDispatch),
    deriveState(createEditorState(boundDispatch), null),
  ),
  userState: defaultUserState,
  workers: new UtopiaTsWorkersImplementation(
    new FakeParserPrinterWorker(),
    new FakeLinterWorker(),
    new FakeWatchdogWorker(),
  ),
  persistence: DummyPersistenceMachine,
  dispatch: boundDispatch,
  builtInDependencies: createBuiltInDependenciesList(null),
  alreadySaved: false,
}

let storedState = initialState

const onCreatedOrLoadedProject = (
  projectId: string,
  projectName: string,
  project: PersistentModel,
) =>
  load(storedState.dispatch, project, projectName, projectId, createBuiltInDependenciesList(null))

const storeHook = create<EditorStore>((set) => storedState)

const spyCollector = emptyUiJsxCanvasContextData()

onCreatedOrLoadedProject('test', 'Test Project', TestProject as any)

export const EditorRoot: React.FunctionComponent = () => {
  return (
    <EditorStateContext.Provider value={{ api: storeHook, useStore: storeHook }}>
      <UiJsxCanvasCtxAtom.Provider value={spyCollector}>
        <DndProvider backend={Backend}>
          <div style={{ width: '100vw', height: '100vh', display: 'flex' }}>
            <DesignPanelRoot />
          </div>
        </DndProvider>
      </UiJsxCanvasCtxAtom.Provider>
    </EditorStateContext.Provider>
  )
}
