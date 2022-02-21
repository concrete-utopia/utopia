import create from 'zustand'
import { createBuiltInDependenciesList } from '../../../core/es-modules/package-manager/built-in-dependencies-list'
import { NO_OP } from '../../../core/shared/utils'
import {
  FakeLinterWorker,
  FakeParserPrinterWorker,
  FakeWatchdogWorker,
} from '../../../core/workers/test-workers'
import { UtopiaTsWorkersImplementation } from '../../../core/workers/workers'
import { createEmptySessionStateState } from '../../../interactions_proposal'
import { setProperty } from '../../canvas/commands/commands'
import { emptyUiJsxCanvasContextData } from '../../canvas/ui-jsx-canvas'
import { EditorDispatch, notLoggedIn } from '../action-types'
import * as History from '../history'
import { DummyPersistenceMachine } from '../persistence/persistence.test-utils'
import { DispatchResult, editorDispatch } from './dispatch'
import { interactionCancel } from './dispatch-strategies'
import { createEditorState, deriveState, EditorStore } from './editor-state'
import * as EP from '../../../core/shared/element-path'
import * as PP from '../../../core/shared/property-path'
import { emptyComments, jsxAttributeValue } from '../../../core/shared/element-template'

function createEditorStore(): EditorStore {
  let emptyEditorState = createEditorState(NO_OP)
  const derivedState = deriveState(emptyEditorState, null)

  const history = History.init(emptyEditorState, derivedState)
  const spyCollector = emptyUiJsxCanvasContextData()

  const dispatch: EditorDispatch = (actions) => {
    const result = editorDispatch(dispatch, actions, storeHook.getState(), spyCollector)
    storeHook.setState(result)
  }

  const initialEditorStore: EditorStore = {
    unpatchedEditor: emptyEditorState,
    patchedEditor: emptyEditorState,
    editor: emptyEditorState,
    derived: derivedState,
    sessionStateState: createEmptySessionStateState(),
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

  const storeHook = create<EditorStore>((set) => initialEditorStore)

  return initialEditorStore
}

function dispatchResultFromEditorStore(editorStore: EditorStore): DispatchResult {
  return {
    ...editorStore,
    nothingChanged: true,
    entireUpdateFinished: Promise.resolve(),
  }
}

describe('interactionCancel', () => {
  it('returns a clean state', () => {
    let editorStore = createEditorStore()
    editorStore.sessionStateState.accumulatedCommands = [
      {
        commands: [
          setProperty(
            'permanent',
            EP.fromString('aaa/bbb'),
            PP.create(['style', 'left']),
            jsxAttributeValue(100, emptyComments),
          ),
        ],
        strategy: null,
      },
    ]
    const actualResult = interactionCancel(editorStore, dispatchResultFromEditorStore(editorStore))
    expect(actualResult.newSessionStateState.accumulatedCommands).toHaveLength(0)
    expect(actualResult.newSessionStateState.commandDescriptions).toHaveLength(0)
    expect(actualResult.newSessionStateState.currentStrategyCommands).toHaveLength(0)
    expect(actualResult.newSessionStateState.currentStrategy).toBeNull()
  })
})
