import { disableStoredStateforTests } from '../editor/stored-state'
import fastDeepEquals from 'fast-deep-equal'
try {
  jest.mock('../editor/stored-state', () => ({
    loadStoredState: () => Promise.resolve(null),
    saveStoredState: () => Promise.resolve(),
  }))
  jest.mock('../code-editor/console-and-errors-pane', () => ({
    ConsoleAndErrorsPane: () => null,
  }))
} catch (e) {
  // not jest env, disable stored state manually
  disableStoredStateforTests()
}

import React from 'react'

///// IMPORTANT NOTE - THIS MUST BE BELOW THE REACT IMPORT AND ABOVE ALL OTHER IMPORTS
const realCreateElement = React.createElement
let renderCount = 0
const renderInfo: { current: Array<string> } = { current: [] }
const monkeyCreateElement = (...params: any[]) => {
  renderCount++
  const el = (realCreateElement as any)(...params)
  renderInfo.current.push(getNamedPath(el))
  return el
}
;(React as any).createElement = monkeyCreateElement

try {
  jest.setTimeout(10000) // in milliseconds
} catch (e) {
  // probably not Jest env
}

import type { RenderResult } from '@testing-library/react'
import { act, render } from '@testing-library/react'
import * as Prettier from 'prettier/standalone'
import type {
  ElementPath,
  ParsedTextFile,
  ParseSuccess,
} from '../../core/shared/project-file-types'
import {
  codeFile,
  foldParsedTextFile,
  isParseFailure,
  isParseSuccess,
  isTextFile,
  isUnparsed,
  RevisionsState,
  textFile,
  textFileContents,
} from '../../core/shared/project-file-types'
import { PrettierConfig } from 'utopia-vscode-common'
import {
  FakeLinterWorker,
  FakeParserPrinterWorker,
  FakeWatchdogWorker,
} from '../../core/workers/test-workers'
import { UtopiaTsWorkersImplementation } from '../../core/workers/workers'
import { EditorRoot } from '../../templates/editor'
import Utils from '../../utils/utils'
import { getNamedPath } from '../../utils/react-helpers'
import type {
  DispatchPriority,
  EditorAction,
  EditorDispatch,
  LoginState,
} from '../editor/action-types'
import { notLoggedIn } from '../editor/action-types'
import { load } from '../editor/actions/actions'
import * as History from '../editor/history'
import type { DispatchResult } from '../editor/store/dispatch'
import {
  editorDispatchActionRunner,
  editorDispatchClosingOut,
  resetDispatchGlobals,
} from '../editor/store/dispatch'
import type {
  EditorState,
  EditorStoreFull,
  EditorStorePatched,
  PersistentModel,
} from '../editor/store/editor-state'
import {
  createEditorState,
  deriveState,
  emptyCollaborativeEditingSupport,
  patchedStoreFromFullStore,
  persistentModelForProjectContents,
  StoryboardFilePath,
} from '../editor/store/editor-state'
import { BakedInStoryboardUID, BakedInStoryboardVariableName } from '../../core/model/scene-utils'
import { elementPath } from '../../core/shared/element-path'
import { CanvasContextMenuPortalTargetID, NO_OP } from '../../core/shared/utils'
import { emptyUiJsxCanvasContextData } from './ui-jsx-canvas'
import { testParseCode } from '../../core/workers/parser-printer/parser-printer.test-utils'
import type { SteganographyMode } from '../../core/workers/parser-printer/parser-printer'
import { printCode, printCodeOptions } from '../../core/workers/parser-printer/parser-printer'
import type { PathAndFileEntry, ProjectContentTreeRoot } from '../assets'
import {
  contentsToTree,
  contentsTreeOptic,
  getProjectFileByFilePath,
  treeToContents,
} from '../assets'
import { testStaticElementPath } from '../../core/shared/element-path.test-utils'
import { createFakeMetadataForParseSuccess, wait } from '../../utils/utils.test-utils'
import {
  mergeWithPrevUndo,
  saveDOMReport,
  setPanelVisibility,
  switchEditorMode,
  updateMetadataInEditorState,
  updateNodeModulesContents,
} from '../editor/actions/action-creators'
import { EditorModes } from '../editor/editor-modes'
import {
  hasReactRouterErrorBeenLogged,
  useUpdateOnRuntimeErrors,
} from '../../core/shared/runtime-report-logs'
import type { RuntimeErrorInfo } from '../../core/shared/code-exec-utils'
import { clearListOfEvaluatedFiles } from '../../core/shared/code-exec-utils'
import { createTestProjectWithCode } from '../../sample-projects/sample-project-utils.test-utils'
import { DummyPersistenceMachine } from '../editor/persistence/persistence.test-utils'
import type { BuiltInDependencies } from '../../core/es-modules/package-manager/built-in-dependencies-list'
import { createBuiltInDependenciesList } from '../../core/es-modules/package-manager/built-in-dependencies-list'
import { createEmptyStrategyState } from './canvas-strategies/interaction-state'
import type { DomWalkerMutableStateData } from './dom-walker'
import {
  createDomWalkerMutableState,
  invalidateDomWalkerIfNecessary,
  resubscribeObservers,
} from './dom-walker'
import { flushSync } from 'react-dom'
import { shouldUpdateLowPriorityUI } from '../inspector/inspector'
import { SampleNodeModules } from '../custom-code/code-file.test-utils'
import type { MetaCanvasStrategy } from './canvas-strategies/canvas-strategies'
import { RegisteredCanvasStrategies } from './canvas-strategies/canvas-strategies'
import type { UtopiaStoreAPI } from '../editor/store/store-hook'
import { createStoresAndState } from '../editor/store/store-hook'
import { checkAnyWorkerUpdates, simpleStringifyActions } from '../editor/actions/action-utils'
import { modify } from '../../core/shared/optics/optic-utilities'
import { fromField } from '../../core/shared/optics/optic-creators'
import type { DuplicateUIDsResult } from '../../core/model/get-uid-mappings'
import { getUidMappings } from '../../core/model/get-uid-mappings'
import { carryDispatchResultFields } from './editor-dispatch-flow'
import type { FeatureName } from '../../utils/feature-switches'
import { setFeatureEnabled } from '../../utils/feature-switches'
import { unpatchedCreateRemixDerivedDataMemo } from '../editor/store/remix-derived-data'
import {
  emptyProjectServerState,
  getUpdateProjectServerStateInStoreRunCount,
} from '../editor/store/project-server-state'
import { uniqBy } from '../../core/shared/array-utils'
import { InitialOnlineState } from '../editor/online-status'
import { RadixComponentsPortalId } from '../../uuiui/radix-components'
import {
  resetDomSamplerExecutionCounts,
  runDomSamplerGroups,
  runDomSamplerRegular,
} from './dom-sampler'
import {
  ElementInstanceMetadataKeepDeepEquality,
  ElementInstanceMetadataMapKeepDeepEquality,
} from '../editor/store/store-deep-equality-instances'

// eslint-disable-next-line no-unused-expressions
typeof process !== 'undefined' &&
  process.on('unhandledRejection', (reason, promise) => {
    console.warn(
      'Unhandled promise rejection:',
      promise,
      'reason:',
      (reason as any)?.stack ?? reason,
    )
  })

try {
  jest.mock('../../core/vscode/vscode-bridge')
} catch (e) {
  // mock fails don't care
}

const FailJestOnCanvasError = () => {
  const stableCallback = React.useCallback((newRuntimeErrors: Array<RuntimeErrorInfo>) => {
    // we have new runtime errors, let's take the tests down
    if (newRuntimeErrors.length > 0) {
      console.error('Canvas Error!!!!!', newRuntimeErrors[0]?.error)
      throw newRuntimeErrors[0]?.error
      expect(newRuntimeErrors[0]?.error ?? null).toEqual(null)
    }
  }, [])

  useUpdateOnRuntimeErrors(stableCallback)

  return null
}

type ActionsCausingDuplicateUIDs = Array<{
  actions: ReadonlyArray<EditorAction>
  duplicateUIDs: DuplicateUIDsResult
}>

export type AsyncEditorDispatch = (
  actions: ReadonlyArray<EditorAction>,
  waitForDOMReport: boolean,
  overrideDefaultStrategiesArray?: Array<MetaCanvasStrategy>,
) => Promise<void>

export interface EditorRenderResult {
  dispatch: AsyncEditorDispatch
  getDispatchFollowUpActionsFinished: () => Promise<void>
  getEditorState: () => EditorStorePatched
  renderedDOM: RenderResult
  getNumberOfCommits: () => number
  getNumberOfRenders: () => number
  clearRenderInfo: () => void
  getRenderInfo: () => Array<string>
  clearRecordedActions: () => void
  getRecordedActions: () => ReadonlyArray<EditorAction>
  getDomWalkerState: () => DomWalkerMutableStateData
  getActionsCausingDuplicateUIDs: () => ActionsCausingDuplicateUIDs
}

function formatAllCodeInModel(model: PersistentModel): PersistentModel {
  // Call formatTestProjectCode on every code file to ensure that simply re-printing and
  // re-parsing the file will have no effect
  const combinedOptic = fromField<PersistentModel, 'projectContents'>('projectContents').compose(
    contentsTreeOptic,
  )
  return modify(
    combinedOptic,
    (pathAndFile: PathAndFileEntry) => {
      const { fullPath, file } = pathAndFile
      if (isTextFile(file) && (fullPath.endsWith('.js') || fullPath.endsWith('.jsx'))) {
        return {
          fullPath: pathAndFile.fullPath,
          file: codeFile(formatTestProjectCode(file.fileContents.code), null),
        }
      } else {
        return pathAndFile
      }
    },
    model,
  )
}

type StartingFeatureSwitches = Partial<{
  [F in FeatureName]: boolean
}>

export const DefaultStartingFeatureSwitches: StartingFeatureSwitches = {
  'Debug - Print UIDs': true,
}

interface RenderTestEditorWithCodeOptions {
  strategiesToUse: Array<MetaCanvasStrategy>
  startingFeatureSwitches: StartingFeatureSwitches
  applySteganography: SteganographyMode
}

const DefaultRenderTestEditorWithCodeOptions: RenderTestEditorWithCodeOptions = {
  strategiesToUse: RegisteredCanvasStrategies,
  startingFeatureSwitches: DefaultStartingFeatureSwitches,
  applySteganography: 'do-not-apply-steganography',
}

export async function renderTestEditorWithCode(
  appUiJsFileCode: string,
  awaitFirstDomReport: 'await-first-dom-report' | 'dont-await-first-dom-report',
  options: Partial<RenderTestEditorWithCodeOptions> = {},
): Promise<EditorRenderResult> {
  const { strategiesToUse, startingFeatureSwitches } = {
    ...DefaultRenderTestEditorWithCodeOptions,
    ...options,
  }
  return renderTestEditorWithModel(
    createTestProjectWithCode(appUiJsFileCode, options.applySteganography),
    awaitFirstDomReport,
    startingFeatureSwitches,
    undefined,
    strategiesToUse,
  )
}
export async function renderTestEditorWithProjectContent(
  projectContent: ProjectContentTreeRoot,
  awaitFirstDomReport: 'await-first-dom-report' | 'dont-await-first-dom-report',
  strategiesToUse: Array<MetaCanvasStrategy> = RegisteredCanvasStrategies,
  loginState: LoginState = notLoggedIn,
): Promise<EditorRenderResult> {
  return renderTestEditorWithModel(
    persistentModelForProjectContents(projectContent),
    awaitFirstDomReport,
    DefaultStartingFeatureSwitches,
    undefined,
    strategiesToUse,
    loginState,
  )
}

const optedInToCheckFileTimestamps = { current: true }
export function optOutFromCheckFileTimestamps() {
  beforeEach(() => {
    optedInToCheckFileTimestamps.current = false
  })
  afterEach(() => {
    optedInToCheckFileTimestamps.current = true
  })
}

let prevDomWalkerMutableState: DomWalkerMutableStateData | null = null

export async function renderTestEditorWithModel(
  rawModel: PersistentModel,
  awaitFirstDomReport: 'await-first-dom-report' | 'dont-await-first-dom-report',
  startingFeatureSwitches: StartingFeatureSwitches = DefaultStartingFeatureSwitches,
  mockBuiltInDependencies?: BuiltInDependencies,
  strategiesToUse: Array<MetaCanvasStrategy> = RegisteredCanvasStrategies,
  loginState: LoginState = notLoggedIn,
  failOnCanvasError: boolean = true,
): Promise<EditorRenderResult> {
  for (const [key, value] of Object.entries(startingFeatureSwitches)) {
    setFeatureEnabled(key as FeatureName, value)
  }
  const model = formatAllCodeInModel(rawModel)
  const renderCountBaseline = renderCount
  let recordedActions: Array<EditorAction> = []
  let actionsCausingDuplicateUIDs: ActionsCausingDuplicateUIDs = []

  let emptyEditorState = createEditorState(NO_OP)
  const derivedState = deriveState(
    emptyEditorState,
    null,
    'unpatched',
    unpatchedCreateRemixDerivedDataMemo,
  )

  const history = History.init(emptyEditorState, derivedState)

  let editorDispatchPromises: Array<Promise<void>> = []
  async function getDispatchFollowUpActionsFinished(): Promise<void> {
    return Promise.all(editorDispatchPromises).then(NO_OP)
  }

  let workingEditorState: DispatchResult

  const spyCollector = emptyUiJsxCanvasContextData()

  // Reset canvas globals
  resetDispatchGlobals()
  clearListOfEvaluatedFiles()

  const asyncTestDispatch = async (
    actions: ReadonlyArray<EditorAction>,
    priority?: DispatchPriority, // priority is not used in the editorDispatch now, but we didn't delete this param yet
    waitForDispatchEntireUpdate = false,
    innerStrategiesToUse: Array<MetaCanvasStrategy> = strategiesToUse,
  ) => {
    resetDomSamplerExecutionCounts()
    recordedActions.push(...actions)
    const originalEditorState = workingEditorState
    const result = editorDispatchActionRunner(
      asyncTestDispatch,
      actions,
      workingEditorState,
      spyCollector,
      innerStrategiesToUse,
    )

    const duplicateUIDs = getUidMappings(result.patchedEditor.projectContents).duplicateIDs
    if (Object.keys(duplicateUIDs).length > 0) {
      actionsCausingDuplicateUIDs.push({
        actions: actions,
        duplicateUIDs: duplicateUIDs,
      })
    }

    const anyWorkerUpdates = checkAnyWorkerUpdates(actions)
    const anyUndoOrRedoOrPostAction = actions.some(
      (action) =>
        action.action === 'UNDO' ||
        action.action === 'REDO' ||
        action.action === 'EXECUTE_POST_ACTION_MENU_CHOICE',
    )
    const shouldCheckFileTimestamps =
      optedInToCheckFileTimestamps.current && !(anyWorkerUpdates || anyUndoOrRedoOrPostAction)

    if (shouldCheckFileTimestamps) {
      // We compare both the patched and unpatched versions of the new project contents
      // against only the unpatched version of the old project contents. This is because
      // we only care about the total change here, as e.g. a continuous drag might introduce
      // and then remove a property change, resulting in the patch collapsing and the file
      // reverting to the current unpatched state.
      expectUpdatedFilesUpdateTimestamp(
        workingEditorState.unpatchedEditor.projectContents,
        result.unpatchedEditor.projectContents,
        actions,
        'unpatched',
      )
      expectUpdatedFilesUpdateTimestamp(
        workingEditorState.unpatchedEditor.projectContents,
        result.patchedEditor.projectContents,
        actions,
        'patched',
      )
    }

    expectNoActionsCausedDuplicateUids(actionsCausingDuplicateUIDs)

    editorDispatchPromises.push(result.entireUpdateFinished)
    invalidateDomWalkerIfNecessary(
      domWalkerMutableState,
      workingEditorState.patchedEditor,
      result.patchedEditor,
    )

    workingEditorState = result
    if (waitForDispatchEntireUpdate) {
      await Utils.timeLimitPromise(
        getDispatchFollowUpActionsFinished(),
        2000,
        'Follow up actions took too long.',
      )
    }
    const reactRouterErrorPreviouslyLogged = hasReactRouterErrorBeenLogged()

    flushSync(() => {
      canvasStoreHook.setState(patchedStoreFromFullStore(workingEditorState, 'canvas-store'))
    })

    // run dom SAMPLER

    {
      resubscribeObservers(domWalkerMutableState)

      const metadataResult = runDomSamplerRegular({
        elementsToFocusOn: workingEditorState.patchedEditor.canvas.elementsToRerender,
        domWalkerAdditionalElementsToFocusOn:
          workingEditorState.patchedEditor.canvas.domWalkerAdditionalElementsToUpdate,
        scale: workingEditorState.patchedEditor.canvas.scale,
        metadataToUpdate: workingEditorState.elementMetadata,
        selectedViews: workingEditorState.patchedEditor.selectedViews,
        spyCollector: spyCollector,
      })

      const deepEqualityResult = ElementInstanceMetadataMapKeepDeepEquality(
        workingEditorState.elementMetadata,
        metadataResult.metadata,
      )

      workingEditorState.elementMetadata = deepEqualityResult.value

      if (!deepEqualityResult.areEqual) {
        const saveDomReportAction = updateMetadataInEditorState(
          workingEditorState.elementMetadata,
          metadataResult.tree,
        )
        recordedActions.push(saveDomReportAction)
        const editorWithNewMetadata = editorDispatchActionRunner(
          asyncTestDispatch,
          [saveDomReportAction],
          workingEditorState,
          spyCollector,
        )
        workingEditorState = carryDispatchResultFields(workingEditorState, editorWithNewMetadata)
      }
    }

    // true-up groups if needed
    if (workingEditorState.unpatchedEditor.trueUpElementsAfterDomWalkerRuns.length > 0) {
      ;(() => {
        // updated editor with trued up groups
        const projectContentsBeforeGroupTrueUp = workingEditorState.unpatchedEditor.projectContents
        const dispatchResultWithTruedUpGroups = editorDispatchActionRunner(
          asyncTestDispatch,
          [{ action: 'TRUE_UP_ELEMENTS' }],
          workingEditorState,
          spyCollector,
        )
        workingEditorState = carryDispatchResultFields(
          workingEditorState,
          dispatchResultWithTruedUpGroups,
        )

        editorDispatchPromises.push(dispatchResultWithTruedUpGroups.entireUpdateFinished)

        if (
          projectContentsBeforeGroupTrueUp === workingEditorState.unpatchedEditor.projectContents
        ) {
          // no group-related re-render / re-measure is needed, bail out
          return
        }

        // re-render the canvas
        {
          // TODO run fixElementsToRerender and set ElementsToRerenderGLOBAL

          flushSync(() => {
            canvasStoreHook.setState(patchedStoreFromFullStore(workingEditorState, 'canvas-store'))
          })
        }

        // re-run the dom SAMPLER

        {
          resubscribeObservers(domWalkerMutableState)

          const metadataResult = runDomSamplerGroups({
            elementsToFocusOn: workingEditorState.patchedEditor.canvas.elementsToRerender,
            domWalkerAdditionalElementsToFocusOn:
              workingEditorState.patchedEditor.canvas.domWalkerAdditionalElementsToUpdate,
            scale: workingEditorState.patchedEditor.canvas.scale,
            metadataToUpdate: workingEditorState.elementMetadata,
            selectedViews: workingEditorState.patchedEditor.selectedViews,
            spyCollector: spyCollector,
          })

          if (metadataResult != null) {
            const saveDomReportAction = updateMetadataInEditorState(
              metadataResult.metadata,
              metadataResult.tree,
            )
            recordedActions.push(saveDomReportAction)
            const editorWithNewMetadata = editorDispatchActionRunner(
              asyncTestDispatch,
              [saveDomReportAction],
              workingEditorState,
              spyCollector,
            )
            workingEditorState = carryDispatchResultFields(
              workingEditorState,
              editorWithNewMetadata,
            )
          }
        }
      })()
    }

    workingEditorState = editorDispatchClosingOut(
      asyncTestDispatch,
      actions,
      originalEditorState,
      workingEditorState,
      reactRouterErrorPreviouslyLogged,
    )

    // update state with new metadata

    flushSync(() => {
      storeHook.setState(patchedStoreFromFullStore(workingEditorState, 'editor-store'))
      if (
        shouldUpdateLowPriorityUI(
          workingEditorState.strategyState,
          workingEditorState.patchedEditor.canvas.elementsToRerender,
        )
      ) {
        lowPriorityStoreHook.setState(
          patchedStoreFromFullStore(workingEditorState, 'low-priority-store'),
        )
      }
    })
  }

  const workers = new UtopiaTsWorkersImplementation(
    [new FakeParserPrinterWorker()],
    new FakeLinterWorker(),
    new FakeWatchdogWorker(),
  )

  const builtInDependencies =
    mockBuiltInDependencies != null
      ? mockBuiltInDependencies
      : createBuiltInDependenciesList(workers)
  const initialEditorStore: EditorStoreFull = {
    strategyState: createEmptyStrategyState({}, {}, {}),
    unpatchedEditor: emptyEditorState,
    patchedEditor: emptyEditorState,
    unpatchedDerived: derivedState,
    patchedDerived: derivedState,
    history: history,
    userState: {
      loginState: loginState,
      shortcutConfig: {},
      themeConfig: 'system',
      githubState: {
        authenticated: false,
        gitRepoToLoad: null,
      },
    },
    workers: workers,
    persistence: DummyPersistenceMachine,
    saveCountThisSession: 0,
    builtInDependencies: builtInDependencies,
    elementMetadata: {},
    postActionInteractionSession: null,
    projectServerState: {
      ...emptyProjectServerState(),
      isMyProject: 'yes',
    },
    collaborativeEditingSupport: emptyCollaborativeEditingSupport(),
    onlineState: InitialOnlineState,
  }

  const canvasStoreHook: UtopiaStoreAPI = createStoresAndState(
    patchedStoreFromFullStore(initialEditorStore, 'canvas-store'),
  )

  prevDomWalkerMutableState?.resizeObserver.disconnect()
  prevDomWalkerMutableState?.mutationObserver.disconnect()
  const domWalkerMutableState = createDomWalkerMutableState(canvasStoreHook, asyncTestDispatch)
  prevDomWalkerMutableState = domWalkerMutableState

  const lowPriorityStoreHook: UtopiaStoreAPI = createStoresAndState(
    patchedStoreFromFullStore(initialEditorStore, 'low-priority-store'),
  )

  const storeHook: UtopiaStoreAPI = createStoresAndState(
    patchedStoreFromFullStore(initialEditorStore, 'editor-store'),
  )

  // initializing the local editor state
  workingEditorState = {
    ...initialEditorStore,
    nothingChanged: true,
    entireUpdateFinished: Promise.resolve(true),
  }

  let numberOfCommits = 0

  // This results in the portal element existing before the subsequent render,
  // which is necessary as any component that attempts to look for the portal as part of that render
  // will not find it as it is not yet in the DOM.
  const renderTargetDiv = document.createElement('div')
  document.body.appendChild(renderTargetDiv)
  render(
    <React.Profiler onRender={NO_OP} id='editor-root'>
      <div id='utopia-editor-root'>
        <div id={CanvasContextMenuPortalTargetID}></div>
        <div id={RadixComponentsPortalId}></div>
      </div>
    </React.Profiler>,
    { container: renderTargetDiv },
  )
  const result = render(
    <React.Profiler
      id='editor-root'
      /* eslint-disable-next-line react/jsx-no-bind */
      onRender={(id, phase, actualDuration, baseDuration, startTime, commitTime, interactions) => {
        numberOfCommits++
      }}
    >
      <div id='utopia-editor-root'>
        <div id={CanvasContextMenuPortalTargetID}></div>
        <div id={RadixComponentsPortalId}></div>
        {failOnCanvasError ? <FailJestOnCanvasError /> : null}
        <style>{`
div,
span,
img,
ul,
li,
label {
  box-sizing: border-box !important;
}`}</style>
        <EditorRoot
          dispatch={asyncTestDispatch as EditorDispatch}
          mainStore={storeHook}
          canvasStore={canvasStoreHook}
          spyCollector={spyCollector}
          lowPriorityStore={lowPriorityStoreHook}
          domWalkerMutableState={domWalkerMutableState}
        />
      </div>
    </React.Profiler>,
    { container: renderTargetDiv },
  )

  // Capture how many times the project server state update has been triggered.
  const beforeLoadUpdateStoreRunCount = getUpdateProjectServerStateInStoreRunCount()

  await act(async () => {
    await new Promise<void>((resolve, reject) => {
      void load(
        async (actions) => {
          try {
            await asyncTestDispatch(
              [
                ...actions,
                switchEditorMode(EditorModes.selectMode(null, false, 'none')),
                setPanelVisibility('codeEditor', false),
                updateNodeModulesContents(SampleNodeModules),
              ],
              undefined,
              true,
              strategiesToUse,
            )
            resolve()
          } catch (e) {
            reject(e)
          }
        },
        model,
        'Test',
        '0',
        initialEditorStore.builtInDependencies,
        false,
      )
    })
  })

  // Check that the project server state update has been triggered twice, which (currently at least)
  // is the number of times that it runs as a result of the above logic. Once without a project ID and
  // the second time with the project ID '0', which should then mean that it has stabilised and unless other
  // changes occur the `UPDATE_PROJECT_SERVER_STATE` action shouldn't be triggered anymore after that.
  while (getUpdateProjectServerStateInStoreRunCount() <= beforeLoadUpdateStoreRunCount + 1) {
    // eslint-disable-next-line no-await-in-loop
    await wait(1)
  }

  return {
    dispatch: async (
      actions: ReadonlyArray<EditorAction>,
      waitForDOMReport: boolean,
      innerStrategiesToUse: Array<MetaCanvasStrategy> = strategiesToUse,
    ) => {
      return act(async () => asyncTestDispatch(actions, 'everyone', true, innerStrategiesToUse))
    },
    getDispatchFollowUpActionsFinished: getDispatchFollowUpActionsFinished,
    getEditorState: () => storeHook.getState(),
    renderedDOM: result,
    getNumberOfCommits: () => numberOfCommits,
    getNumberOfRenders: () => renderCount - renderCountBaseline,
    clearRenderInfo: () => (renderInfo.current = []),
    getRenderInfo: () => renderInfo.current,
    clearRecordedActions: () => {
      recordedActions = []
    },
    getRecordedActions: () => recordedActions,
    getDomWalkerState: () => domWalkerMutableState,
    getActionsCausingDuplicateUIDs: () => actionsCausingDuplicateUIDs,
  }
}

function expectUpdatedFilesUpdateTimestamp(
  before: ProjectContentTreeRoot,
  after: ProjectContentTreeRoot,
  actions: ReadonlyArray<EditorAction>,
  unpatchedOrPatched: 'unpatched' | 'patched',
) {
  if (before === after) {
    return
  } else {
    const beforeAsMap = treeToContents(before)
    const afterAsMap = treeToContents(after)

    Object.entries(afterAsMap).forEach(([fileName, newProjectFile]) => {
      const oldProjectFile = beforeAsMap[fileName]

      // Check every file in the new project contents against the corresponding version in the old project contents.
      // If a file has been updated in any way (except for updates from the worker) we need to check that the
      // versionNumber has been updated, otherwise an update from the worker could overwrite it.

      if (
        oldProjectFile != null &&
        oldProjectFile !== newProjectFile &&
        isTextFile(newProjectFile) &&
        isTextFile(oldProjectFile) &&
        newProjectFile.versionNumber <= oldProjectFile.versionNumber
      ) {
        if (
          // We use fastDeepEquals here because it is far too easy to blow referential equality
          !fastDeepEquals(newProjectFile.fileContents, oldProjectFile.fileContents) ||
          !fastDeepEquals(newProjectFile.lastSavedContents, oldProjectFile.lastSavedContents)
        ) {
          throw new Error(
            `Invalid file update in ${unpatchedOrPatched} editor state caused by ${simpleStringifyActions(
              actions,
            )}`,
          )
        }
      }
    })
  }
}

function expectNoActionsCausedDuplicateUids(
  actionsCausingDuplicateUIDs: ActionsCausingDuplicateUIDs,
) {
  if (actionsCausingDuplicateUIDs.length > 0) {
    expect({
      actionsCausingDuplicateUIDs: actionsCausingDuplicateUIDs,
      message: 'No actions have introduced duplicate uids',
    }).toEqual({
      actionsCausingDuplicateUIDs: actionsCausingDuplicateUIDs,
      message: 'Some actions have introduced duplicate uids!',
    })
  }
}

export function getPrintedUiJsCode(
  store: EditorStorePatched,
  filePath: string = StoryboardFilePath,
): string {
  const file = getProjectFileByFilePath(store.editor.projectContents, filePath)
  if (file != null && isTextFile(file)) {
    return file.fileContents.code
  } else {
    throw new Error('File is not a text file.')
  }
}

export function getPrintedUiJsCodeWithoutUIDs(
  store: EditorStorePatched,
  filePath: string = StoryboardFilePath,
): string {
  const file = getProjectFileByFilePath(store.editor.projectContents, filePath)
  if (file != null && isTextFile(file) && isParseSuccess(file.fileContents.parsed)) {
    return printCode(
      StoryboardFilePath,
      printCodeOptions(false, true, false, true),
      file.fileContents.parsed.imports,
      file.fileContents.parsed.topLevelElements,
      file.fileContents.parsed.jsxFactoryFunction,
      file.fileContents.parsed.exportsDetail,
    )
  } else {
    throw new Error('File is not a text file.')
  }
}

export const TestSceneUID = 'scene-aaa'
export const TestAppUID = 'app-entity'
export const TestStoryboardPath = elementPath([[BakedInStoryboardUID]])
export const TestSceneElementPaths = [[BakedInStoryboardUID, TestSceneUID, TestAppUID]]
export const TestScenePath = elementPath(TestSceneElementPaths)
export const TestStaticScenePath = testStaticElementPath(TestSceneElementPaths)

export function formatTestProjectCode(code: string): string {
  return Prettier.format(code, PrettierConfig)
}

export function makeTestProjectCodeWithComponentInnards(componentInnards: string): string {
  const code = `
  import * as React from 'react'
  import { Scene, Storyboard, View, Group } from 'utopia-api'

  export var App = (props) => {
${componentInnards}
  }

  export var ${BakedInStoryboardVariableName} = (props) => {
    return (
      <Storyboard data-uid='${BakedInStoryboardUID}'>
        <Scene
          style={{ left: 0, top: 0, width: 400, height: 400 }}
          data-uid='${TestSceneUID}'
        >
          <App
            data-uid='${TestAppUID}'
            style={{ position: 'absolute', bottom: 0, left: 0, right: 0, top: 0 }}
          />
        </Scene>
      </Storyboard>
    )
  }
`
  return formatTestProjectCode(code)
}

export function makeTestProjectCodeWithSnippet(snippet: string): string {
  return makeTestProjectCodeWithComponentInnards(`
  return (
${snippet}
  )
`)
}

export function makeTestProjectCodeWithComponentInnardsWithoutUIDs(
  componentInnards: string,
): string {
  const code = `
  import * as React from 'react'
  import { Scene, Storyboard, View, Group } from 'utopia-api'

  export var App = (props) => {
${componentInnards}
  }

  export var ${BakedInStoryboardVariableName} = (props) => {
    return (
      <Storyboard>
        <Scene
          style={{ left: 0, top: 0, width: 400, height: 400 }}
        >
          <App
            style={{ position: 'absolute', bottom: 0, left: 0, right: 0, top: 0 }}
          />
        </Scene>
      </Storyboard>
    )
  }
`
  return formatTestProjectCode(code)
}

export function makeTestProjectCodeWithSnippetWithoutUIDs(snippet: string): string {
  return makeTestProjectCodeWithComponentInnardsWithoutUIDs(`
  return (
${snippet}
  )
`)
}

export function makeTestProjectCodeWithSnippetStyledComponents(snippet: string): string {
  const code = `
  /** @jsx jsx */
  import * as React from 'react'
  import { css, jsx } from '@emotion/react'
  import { Scene, Storyboard, View, Group } from 'utopia-api'

  export var App = (props) => {
    return (
${snippet}
    )
  }

  export var ${BakedInStoryboardVariableName} = (props) => {
    return (
      <Storyboard data-uid='${BakedInStoryboardUID}'>
        <Scene
          style={{ left: 0, top: 0, width: 400, height: 400 }}
          data-uid='${TestSceneUID}'
        >
          <App
            data-uid='${TestAppUID}'
            style={{ position: 'absolute', bottom: 0, left: 0, right: 0, top: 0 }}
          />
        </Scene>
      </Storyboard>
    )
  }
`
  return formatTestProjectCode(code)
}

export function getTestParseSuccess(fileContents: string): ParseSuccess {
  const parseResult = testParseCode(fileContents)
  if (isParseFailure(parseResult)) {
    throw new Error(`Error parsing test input code: ${parseResult.errorMessage}`)
  } else if (isUnparsed(parseResult)) {
    throw new Error(`Unexpected unparsed file.`)
  } else {
    return parseResult
  }
}

export function getEditorState(fileContents: string): EditorState {
  const success = getTestParseSuccess(fileContents)
  const storyboardFile = textFile(
    textFileContents('', success, RevisionsState.ParsedAhead),
    null,
    success,
    0,
  )
  return {
    ...createEditorState(NO_OP),
    projectContents: contentsToTree({
      [StoryboardFilePath]: storyboardFile,
    }),
    jsxMetadata: createFakeMetadataForParseSuccess(success),
  }
}

export function getEditorStateWithSelectedViews(
  fileContents: string,
  selectedViews: Array<ElementPath>,
): EditorState {
  return {
    ...getEditorState(fileContents),
    selectedViews: selectedViews,
  }
}

export function editorStateToParseSuccess(
  editorState: EditorState,
  filePath: string = StoryboardFilePath,
): ParseSuccess {
  const file = getProjectFileByFilePath(editorState.projectContents, filePath)
  if (file == null) {
    throw new Error(`Cannot find storyboard file.`)
  } else if (isTextFile(file)) {
    if (isParseSuccess(file.fileContents.parsed)) {
      return file.fileContents.parsed
    } else {
      throw new Error(`Parsed storyboard is not a parse success.`)
    }
  } else {
    throw new Error(`Storyboard file was not a text file.`)
  }
}

export function testPrintCodeFromParseSuccess(
  filename: string,
  parseSuccess: ParseSuccess,
): string {
  return printCode(
    filename,
    printCodeOptions(false, true, true),
    parseSuccess.imports,
    parseSuccess.topLevelElements,
    parseSuccess.jsxFactoryFunction,
    parseSuccess.exportsDetail,
  )
}

export function testPrintCodeFromEditorState(
  editorState: EditorState,
  filePath: string = StoryboardFilePath,
): string {
  const parseSuccess = editorStateToParseSuccess(editorState, filePath)
  return testPrintCodeFromParseSuccess(filePath, parseSuccess)
}

export function testPrintParsedTextFile(filename: string, parsedTextFile: ParsedTextFile): string {
  return foldParsedTextFile(
    (_) => 'FAILURE',
    (success) => testPrintCodeFromParseSuccess(filename, success),
    (_) => 'UNPARSED',
    parsedTextFile,
  )
}

// TODO refactor all createBuiltInDependenciesList(null) calls with the use of createBuiltinDependenciesWithTestWorkers, simplify API
export function createBuiltinDependenciesWithTestWorkers(
  extraBuiltinDependencies: BuiltInDependencies,
): BuiltInDependencies {
  const workers = new UtopiaTsWorkersImplementation(
    [new FakeParserPrinterWorker()],
    new FakeLinterWorker(),
    new FakeWatchdogWorker(),
  )

  const defaultDependencies = createBuiltInDependenciesList(workers)
  return uniqBy(
    [...extraBuiltinDependencies, ...defaultDependencies], // ordered like this so the user's extra dependencies take priority over the default dependencies
    (l, r) => l.moduleName === r.moduleName,
  )
}
