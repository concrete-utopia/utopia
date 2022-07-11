import { disableStoredStateforTests } from '../editor/stored-state'
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
const monkeyCreateElement = (...params: any[]) => {
  renderCount++
  return (realCreateElement as any)(...params)
}
;(React as any).createElement = monkeyCreateElement

try {
  jest.setTimeout(10000) // in milliseconds
} catch (e) {
  // probably not Jest env
}

import { act, render, RenderResult } from '@testing-library/react'
import * as Prettier from 'prettier/standalone'
import create, { GetState, Mutate, SetState, StoreApi } from 'zustand'
import { subscribeWithSelector } from 'zustand/middleware'
import {
  ElementPath,
  foldParsedTextFile,
  isParseFailure,
  isParseSuccess,
  isTextFile,
  isUnparsed,
  ParsedTextFile,
  ParseSuccess,
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
import { DispatchPriority, EditorAction, notLoggedIn } from '../editor/action-types'
import { load } from '../editor/actions/actions'
import * as History from '../editor/history'
import { editorDispatch, resetDispatchGlobals } from '../editor/store/dispatch'
import {
  createEditorState,
  deriveState,
  EditorState,
  EditorStoreFull,
  EditorStorePatched,
  patchedStoreFromFullStore,
  PersistentModel,
  persistentModelForProjectContents,
  StoryboardFilePath,
} from '../editor/store/editor-state'
import { BakedInStoryboardUID, BakedInStoryboardVariableName } from '../../core/model/scene-utils'
import { elementPath } from '../../core/shared/element-path'
import { NO_OP } from '../../core/shared/utils'
import { emptyUiJsxCanvasContextData } from './ui-jsx-canvas'
import { testParseCode } from '../../core/workers/parser-printer/parser-printer.test-utils'
import { printCode, printCodeOptions } from '../../core/workers/parser-printer/parser-printer'
import { contentsToTree, getContentsTreeFileFromString, ProjectContentTreeRoot } from '../assets'
import { testStaticElementPath } from '../../core/shared/element-path.test-utils'
import { createFakeMetadataForParseSuccess } from '../../utils/utils.test-utils'
import {
  saveDOMReport,
  setPanelVisibility,
  switchEditorMode,
} from '../editor/actions/action-creators'
import { EditorModes } from '../editor/editor-modes'
import { useUpdateOnRuntimeErrors } from '../../core/shared/runtime-report-logs'
import { clearListOfEvaluatedFiles, RuntimeErrorInfo } from '../../core/shared/code-exec-utils'
import { createTestProjectWithCode } from '../../sample-projects/sample-project-utils.test-utils'
import { DummyPersistenceMachine } from '../editor/persistence/persistence.test-utils'
import {
  BuiltInDependencies,
  createBuiltInDependenciesList,
} from '../../core/es-modules/package-manager/built-in-dependencies-list'
import { clearAllRegisteredControls } from './canvas-globals'
import { createEmptyStrategyState } from './canvas-strategies/interaction-state'
import {
  createDomWalkerMutableState,
  invalidateDomWalkerIfNecessary,
  runDomWalker,
} from './dom-walker'
import { flushSync } from 'react-dom'
import { shouldInspectorUpdate } from '../inspector/inspector'

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

export interface EditorRenderResult {
  dispatch: (actions: ReadonlyArray<EditorAction>, waitForDOMReport: boolean) => Promise<void>
  getDispatchFollowUpActionsFinished: () => Promise<void>
  getEditorState: () => EditorStorePatched
  renderedDOM: RenderResult
  getNumberOfCommits: () => number
  getNumberOfRenders: () => number
  clearRecordedActions: () => void
  getRecordedActions: () => ReadonlyArray<EditorAction>
}

export async function renderTestEditorWithCode(
  appUiJsFileCode: string,
  awaitFirstDomReport: 'await-first-dom-report' | 'dont-await-first-dom-report',
) {
  return renderTestEditorWithModel(createTestProjectWithCode(appUiJsFileCode), awaitFirstDomReport)
}
export async function renderTestEditorWithProjectContent(
  projectContent: ProjectContentTreeRoot,
  awaitFirstDomReport: 'await-first-dom-report' | 'dont-await-first-dom-report',
) {
  return renderTestEditorWithModel(
    persistentModelForProjectContents(projectContent),
    awaitFirstDomReport,
  )
}

export async function renderTestEditorWithModel(
  model: PersistentModel,
  awaitFirstDomReport: 'await-first-dom-report' | 'dont-await-first-dom-report',
  mockBuiltInDependencies?: BuiltInDependencies,
): Promise<EditorRenderResult> {
  const renderCountBaseline = renderCount
  let recordedActions: Array<EditorAction> = []

  let emptyEditorState = createEditorState(NO_OP)
  const derivedState = deriveState(emptyEditorState, null)

  const history = History.init(emptyEditorState, derivedState)

  let editorDispatchPromises: Array<Promise<void>> = []
  async function getDispatchFollowUpActionsFinished(): Promise<void> {
    return Promise.all(editorDispatchPromises).then(NO_OP)
  }

  let workingEditorState: EditorStoreFull

  const spyCollector = emptyUiJsxCanvasContextData()

  // Reset canvas globals
  resetDispatchGlobals()
  clearAllRegisteredControls()
  clearListOfEvaluatedFiles()

  const asyncTestDispatch = async (
    actions: ReadonlyArray<EditorAction>,
    priority?: DispatchPriority, // priority is not used in the editorDispatch now, but we didn't delete this param yet
    waitForDispatchEntireUpdate = false,
  ) => {
    recordedActions.push(...actions)
    const result = editorDispatch(asyncTestDispatch, actions, workingEditorState, spyCollector)
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

    flushSync(() => {
      canvasStoreHook.setState(patchedStoreFromFullStore(workingEditorState))
    })

    // run dom walker

    const domWalkerResult = runDomWalker({
      domWalkerMutableState: domWalkerMutableState,
      selectedViews: workingEditorState.patchedEditor.selectedViews,
      elementsToFocusOn: workingEditorState.patchedEditor.canvas.elementsToRerender,
      scale: workingEditorState.patchedEditor.canvas.scale,
      additionalElementsToUpdate:
        workingEditorState.patchedEditor.canvas.domWalkerAdditionalElementsToUpdate,
      rootMetadataInStateRef: {
        current: workingEditorState.patchedEditor.domMetadata,
      },
    })

    if (domWalkerResult != null) {
      const saveDomReportAction = saveDOMReport(
        domWalkerResult.metadata,
        domWalkerResult.cachedPaths,
        domWalkerResult.invalidatedPaths,
      )
      recordedActions.push(saveDomReportAction)
      const editorWithNewMetadata = editorDispatch(
        asyncTestDispatch,
        [saveDomReportAction],
        workingEditorState,
        spyCollector,
      )
      workingEditorState = editorWithNewMetadata
    }

    // update state with new metadata

    flushSync(() => {
      storeHook.setState(patchedStoreFromFullStore(workingEditorState))
      if (shouldInspectorUpdate(workingEditorState.strategyState)) {
        inspectorStoreHook.setState(patchedStoreFromFullStore(workingEditorState))
      }
    })
  }

  const workers = new UtopiaTsWorkersImplementation(
    new FakeParserPrinterWorker(),
    new FakeLinterWorker(),
    new FakeWatchdogWorker(),
  )

  const builtInDependencies =
    mockBuiltInDependencies != null
      ? mockBuiltInDependencies
      : createBuiltInDependenciesList(workers)
  const initialEditorStore: EditorStoreFull = {
    strategyState: createEmptyStrategyState({}, {}),
    unpatchedEditor: emptyEditorState,
    patchedEditor: emptyEditorState,
    unpatchedDerived: derivedState,
    patchedDerived: derivedState,
    history: history,
    userState: {
      loginState: notLoggedIn,
      shortcutConfig: {},
    },
    workers: workers,
    persistence: DummyPersistenceMachine,
    dispatch: asyncTestDispatch,
    alreadySaved: false,
    builtInDependencies: builtInDependencies,
  }

  const canvasStoreHook = create<
    EditorStorePatched,
    SetState<EditorStorePatched>,
    GetState<EditorStorePatched>,
    Mutate<StoreApi<EditorStorePatched>, [['zustand/subscribeWithSelector', never]]>
  >(subscribeWithSelector((set) => patchedStoreFromFullStore(initialEditorStore)))

  const domWalkerMutableState = createDomWalkerMutableState(canvasStoreHook)

  const inspectorStoreHook = create<
    EditorStorePatched,
    SetState<EditorStorePatched>,
    GetState<EditorStorePatched>,
    Mutate<StoreApi<EditorStorePatched>, [['zustand/subscribeWithSelector', never]]>
  >(subscribeWithSelector((set) => patchedStoreFromFullStore(initialEditorStore)))

  const storeHook = create<
    EditorStorePatched,
    SetState<EditorStorePatched>,
    GetState<EditorStorePatched>,
    Mutate<StoreApi<EditorStorePatched>, [['zustand/subscribeWithSelector', never]]>
  >(subscribeWithSelector((set) => patchedStoreFromFullStore(initialEditorStore)))

  // initializing the local editor state
  workingEditorState = initialEditorStore

  let numberOfCommits = 0

  const result = render(
    <React.Profiler
      id='editor-root'
      /* eslint-disable-next-line react/jsx-no-bind */
      onRender={(id, phase, actualDuration, baseDuration, startTime, commitTime, interactions) => {
        numberOfCommits++
      }}
    >
      <FailJestOnCanvasError />
      <EditorRoot
        api={storeHook}
        useStore={storeHook}
        canvasStore={canvasStoreHook}
        spyCollector={spyCollector}
        inspectorStore={inspectorStoreHook}
        domWalkerMutableState={domWalkerMutableState}
      />
    </React.Profiler>,
    { legacyRoot: true },
  )

  await act(async () => {
    await new Promise<void>((resolve, reject) => {
      load(
        async (actions) => {
          try {
            await asyncTestDispatch(actions, undefined, true)
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

  await act(async () => {
    await asyncTestDispatch(
      [switchEditorMode(EditorModes.selectMode()), setPanelVisibility('codeEditor', false)],
      undefined,
      true,
    )
  })

  return {
    dispatch: async (actions: ReadonlyArray<EditorAction>, waitForDOMReport: boolean) => {
      return await act(async () => {
        await asyncTestDispatch(actions, 'everyone', true)
      })
    },
    getDispatchFollowUpActionsFinished: getDispatchFollowUpActionsFinished,
    getEditorState: () => storeHook.getState(),
    renderedDOM: result,
    getNumberOfCommits: () => numberOfCommits,
    getNumberOfRenders: () => renderCount - renderCountBaseline,
    clearRecordedActions: () => {
      recordedActions = []
    },
    getRecordedActions: () => recordedActions,
  }
}

export function getPrintedUiJsCode(
  store: EditorStorePatched,
  filePath: string = StoryboardFilePath,
): string {
  const file = getContentsTreeFileFromString(store.editor.projectContents, filePath)
  if (isTextFile(file)) {
    return file.fileContents.code
  } else {
    throw new Error('File is not a text file.')
  }
}

export function getPrintedUiJsCodeWithoutUIDs(store: EditorStorePatched): string {
  const file = getContentsTreeFileFromString(store.editor.projectContents, StoryboardFilePath)
  if (isTextFile(file) && isParseSuccess(file.fileContents.parsed)) {
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
  import { Scene, Storyboard, View } from 'utopia-api'

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

export function makeTestProjectCodeWithSnippetStyledComponents(snippet: string): string {
  const code = `
  /** @jsx jsx */
  import * as React from 'react'
  import { css, jsx } from '@emotion/react'
  import { Scene, Storyboard, View } from 'utopia-api'

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
  const file = getContentsTreeFileFromString(editorState.projectContents, filePath)
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
