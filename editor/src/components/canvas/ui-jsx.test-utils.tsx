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

import { act, render, RenderResult } from '@testing-library/react'
import * as Prettier from 'prettier/standalone'
import create, { GetState, Mutate, SetState, StoreApi } from 'zustand'
import { subscribeWithSelector } from 'zustand/middleware'
import {
  codeFile,
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
import { getNamedPath } from '../../utils/react-helpers'
import {
  DispatchPriority,
  EditorAction,
  EditorDispatch,
  LoginState,
  notLoggedIn,
} from '../editor/action-types'
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
import { CanvasContextMenuPortalTargetID, NO_OP } from '../../core/shared/utils'
import { emptyUiJsxCanvasContextData } from './ui-jsx-canvas'
import { testParseCode } from '../../core/workers/parser-printer/parser-printer.test-utils'
import { printCode, printCodeOptions } from '../../core/workers/parser-printer/parser-printer'
import {
  contentsToTree,
  contentsTreeOptic,
  getContentsTreeFileFromString,
  PathAndFileEntry,
  ProjectContentTreeRoot,
} from '../assets'
import { testStaticElementPath } from '../../core/shared/element-path.test-utils'
import { createFakeMetadataForParseSuccess } from '../../utils/utils.test-utils'
import {
  saveDOMReport,
  setPanelVisibility,
  switchEditorMode,
  updateNodeModulesContents,
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
import { SampleNodeModules } from '../custom-code/code-file.test-utils'
import { CanvasStrategy } from './canvas-strategies/canvas-strategy-types'
import {
  MetaCanvasStrategy,
  RegisteredCanvasStrategies,
} from './canvas-strategies/canvas-strategies'
import { createStoresAndState, UtopiaStoreAPI } from '../editor/store/store-hook'
import { isTransientAction } from '../editor/actions/action-utils'
import { modify } from '../../core/shared/optics/optic-utilities'
import { compose2Optics, Optic } from '../../core/shared/optics/optics'
import { fromField } from '../../core/shared/optics/optic-creators'

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
  dispatch: (
    actions: ReadonlyArray<EditorAction>,
    waitForDOMReport: boolean,
    overrideDefaultStrategiesArray?: Array<MetaCanvasStrategy>,
  ) => Promise<void>
  getDispatchFollowUpActionsFinished: () => Promise<void>
  getEditorState: () => EditorStorePatched
  renderedDOM: RenderResult
  getNumberOfCommits: () => number
  getNumberOfRenders: () => number
  clearRenderInfo: () => void
  getRenderInfo: () => Array<string>
  clearRecordedActions: () => void
  getRecordedActions: () => ReadonlyArray<EditorAction>
}

function formatAllCodeInModel(model: PersistentModel): PersistentModel {
  // Call formatTestProjectCode on every code file to ensure that simply re-printing and
  // re-parsing the file will have no effect
  const combinedOptic: Optic<PersistentModel, PathAndFileEntry> = compose2Optics(
    fromField('projectContents'),
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

export async function renderTestEditorWithCode(
  appUiJsFileCode: string,
  awaitFirstDomReport: 'await-first-dom-report' | 'dont-await-first-dom-report',
  strategiesToUse: Array<MetaCanvasStrategy> = RegisteredCanvasStrategies,
) {
  return renderTestEditorWithModel(
    createTestProjectWithCode(appUiJsFileCode),
    awaitFirstDomReport,
    undefined,
    strategiesToUse,
  )
}
export async function renderTestEditorWithProjectContent(
  projectContent: ProjectContentTreeRoot,
  awaitFirstDomReport: 'await-first-dom-report' | 'dont-await-first-dom-report',
  strategiesToUse: Array<MetaCanvasStrategy> = RegisteredCanvasStrategies,
  loginState: LoginState = notLoggedIn,
) {
  return renderTestEditorWithModel(
    persistentModelForProjectContents(projectContent),
    awaitFirstDomReport,
    undefined,
    strategiesToUse,
    loginState,
  )
}

export async function renderTestEditorWithModel(
  rawModel: PersistentModel,
  awaitFirstDomReport: 'await-first-dom-report' | 'dont-await-first-dom-report',
  mockBuiltInDependencies?: BuiltInDependencies,
  strategiesToUse: Array<MetaCanvasStrategy> = RegisteredCanvasStrategies,
  loginState: LoginState = notLoggedIn,
): Promise<EditorRenderResult> {
  const model = formatAllCodeInModel(rawModel)
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
    innerStrategiesToUse: Array<MetaCanvasStrategy> = strategiesToUse,
  ) => {
    recordedActions.push(...actions)
    const result = editorDispatch(
      asyncTestDispatch,
      actions,
      workingEditorState,
      spyCollector,
      innerStrategiesToUse,
    )
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
      canvasStoreHook.setState(patchedStoreFromFullStore(workingEditorState, 'canvas-store'))
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
      storeHook.setState(patchedStoreFromFullStore(workingEditorState, 'editor-store'))
      if (
        shouldInspectorUpdate(
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
    new FakeParserPrinterWorker(),
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
      },
    },
    workers: workers,
    persistence: DummyPersistenceMachine,
    saveCountThisSession: 0,
    builtInDependencies: builtInDependencies,
  }

  const canvasStoreHook: UtopiaStoreAPI = createStoresAndState(
    patchedStoreFromFullStore(initialEditorStore, 'canvas-store'),
  )

  const domWalkerMutableState = createDomWalkerMutableState(canvasStoreHook)

  const lowPriorityStoreHook: UtopiaStoreAPI = createStoresAndState(
    patchedStoreFromFullStore(initialEditorStore, 'low-priority-store'),
  )

  const storeHook: UtopiaStoreAPI = createStoresAndState(
    patchedStoreFromFullStore(initialEditorStore, 'editor-store'),
  )

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
      <div id={CanvasContextMenuPortalTargetID}></div>
      <FailJestOnCanvasError />
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
    </React.Profiler>,
    { legacyRoot: true },
  )

  await act(async () => {
    await new Promise<void>((resolve, reject) => {
      void load(
        async (actions) => {
          try {
            await asyncTestDispatch(
              [
                ...actions,
                switchEditorMode(EditorModes.selectMode()),
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
  }
}

export function getPrintedUiJsCode(
  store: EditorStorePatched,
  filePath: string = StoryboardFilePath,
): string {
  const file = getContentsTreeFileFromString(store.editor.projectContents, filePath)
  if (file != null && isTextFile(file)) {
    return file.fileContents.code
  } else {
    throw new Error('File is not a text file.')
  }
}

export function getPrintedUiJsCodeWithoutUIDs(store: EditorStorePatched): string {
  const file = getContentsTreeFileFromString(store.editor.projectContents, StoryboardFilePath)
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
