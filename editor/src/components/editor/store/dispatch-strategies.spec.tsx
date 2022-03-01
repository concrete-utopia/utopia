import create from 'zustand'
import { createBuiltInDependenciesList } from '../../../core/es-modules/package-manager/built-in-dependencies-list'
import { NO_OP } from '../../../core/shared/utils'
import {
  FakeLinterWorker,
  FakeParserPrinterWorker,
  FakeWatchdogWorker,
} from '../../../core/workers/test-workers'
import { UtopiaTsWorkersImplementation } from '../../../core/workers/workers'
import { createEmptyStrategyState } from '../../canvas/canvas-strategies/interaction-state'
import { wildcardPatch } from '../../canvas/commands/wildcard-patch-command'
import { emptyUiJsxCanvasContextData } from '../../canvas/ui-jsx-canvas'
import { EditorDispatch, notLoggedIn } from '../action-types'
import * as History from '../history'
import { DummyPersistenceMachine } from '../persistence/persistence.test-utils'
import { DispatchResult, editorDispatch } from './dispatch'
import { interactionCancel } from './dispatch-strategies'
import { createEditorState, deriveState, EditorStoreFull } from './editor-state'

function createEditorStore(): EditorStoreFull {
  let emptyEditorState = createEditorState(NO_OP)
  const derivedState = deriveState(emptyEditorState, null)

  const history = History.init(emptyEditorState, derivedState)
  const spyCollector = emptyUiJsxCanvasContextData()

  const dispatch: EditorDispatch = (actions) => {
    const result = editorDispatch(dispatch, actions, storeHook.getState(), spyCollector)
    storeHook.setState(result)
  }

  const initialEditorStore: EditorStoreFull = {
    unpatchedEditor: emptyEditorState,
    patchedEditor: emptyEditorState,
    unpatchedDerived: derivedState,
    patchedDerived: derivedState,
    strategyState: createEmptyStrategyState(),
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

  const storeHook = create<EditorStoreFull>((set) => initialEditorStore)

  return initialEditorStore
}

function dispatchResultFromEditorStore(editorStore: EditorStoreFull): DispatchResult {
  return {
    ...editorStore,
    nothingChanged: true,
    entireUpdateFinished: Promise.resolve(),
  }
}

describe('interactionCancel', () => {
  it('returns a clean state', () => {
    let editorStore = createEditorStore()
    editorStore.strategyState.accumulatedCommands = [
      {
        commands: [wildcardPatch('permanent', { selectedViews: { $set: [] } })],
        strategy: null,
      },
    ]
    const actualResult = interactionCancel(editorStore, dispatchResultFromEditorStore(editorStore))
    expect(actualResult.newStrategyState.accumulatedCommands).toHaveLength(0)
    expect(actualResult.newStrategyState.commandDescriptions).toHaveLength(0)
    expect(actualResult.newStrategyState.currentStrategyCommands).toHaveLength(0)
    expect(actualResult.newStrategyState.currentStrategy).toBeNull()
  })
})
