import { disableStoredStateforTests } from '../editor/stored-state'
try {
  jest.mock('../editor/stored-state', () => ({
    loadStoredState: () => Promise.resolve(null),
    saveStoredState: () => Promise.resolve(),
  }))
} catch (e) {
  // not jest env, disable stored state manually
  disableStoredStateforTests()
}

import * as React from 'react'

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
import create from 'zustand'
import {
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
  FakeBundlerWorker,
  FakeLinterWorker,
  FakeParserPrinterWorker,
  FakeWatchdogWorker,
} from '../../core/workers/test-workers'
import { UtopiaTsWorkersImplementation } from '../../core/workers/workers'
import { HotRoot } from '../../templates/editor'
import Utils from '../../utils/utils'
import { DispatchPriority, EditorAction, notLoggedIn } from '../editor/action-types'
import { load } from '../editor/actions/actions'
import * as History from '../editor/history'
import { editorDispatch } from '../editor/store/dispatch'
import {
  createEditorState,
  deriveState,
  EditorState,
  EditorStore,
  PersistentModel,
  persistentModelForProjectContents,
  StoryboardFilePath,
} from '../editor/store/editor-state'
import { createTestProjectWithCode } from './canvas-utils'
import { BakedInStoryboardUID, BakedInStoryboardVariableName } from '../../core/model/scene-utils'
import { elementPath } from '../../core/shared/element-path'
import { NO_OP } from '../../core/shared/utils'
import { emptyUiJsxCanvasContextData } from './ui-jsx-canvas'
import { testParseCode } from '../../core/workers/parser-printer/parser-printer.test-utils'
import { printCode, printCodeOptions } from '../../core/workers/parser-printer/parser-printer'
import { contentsToTree, getContentsTreeFileFromString, ProjectContentTreeRoot } from '../assets'
import { testStaticElementPath } from '../../core/shared/element-path.test-utils'
import { createFakeMetadataForParseSuccess } from '../../utils/utils.test-utils'
import { switchEditorMode } from '../editor/actions/action-creators'
import { EditorModes } from '../editor/editor-modes'

process.on('unhandledRejection', (reason, promise) => {
  console.warn('Unhandled promise rejection:', promise, 'reason:', (reason as any)?.stack ?? reason)
})

try {
  jest.mock('../../core/vscode/vscode-bridge')
} catch (e) {
  // mock fails don't care
}

export async function renderTestEditorWithCode(appUiJsFileCode: string) {
  return renderTestEditorWithModel(createTestProjectWithCode(appUiJsFileCode))
}
export async function renderTestEditorWithProjectContent(projectContent: ProjectContentTreeRoot) {
  return renderTestEditorWithModel(persistentModelForProjectContents(projectContent))
}

export async function renderTestEditorWithModel(
  model: PersistentModel,
): Promise<{
  dispatch: (actions: ReadonlyArray<EditorAction>, waitForDOMReport: boolean) => Promise<void>
  getDomReportDispatched: () => Promise<void>
  getDispatchFollowUpactionsFinished: () => Promise<void>
  getEditorState: () => EditorStore
  renderedDOM: RenderResult
  getNumberOfCommits: () => number
  getNumberOfRenders: () => number
  clearRecordedActions: () => void
  getRecordedActions: () => ReadonlyArray<EditorAction>
}> {
  const renderCountBaseline = renderCount
  let recordedActions: Array<EditorAction> = []

  let emptyEditorState = createEditorState(NO_OP)
  const derivedState = deriveState(emptyEditorState, null)

  const history = History.init(emptyEditorState, derivedState)

  let domReportDispatched = Utils.defer<void>()
  let dispatchFollowUpActionsFinished = Utils.defer<void>()

  function resetPromises() {
    domReportDispatched = Utils.defer()
    dispatchFollowUpActionsFinished = Utils.defer()
  }

  resetPromises()

  let workingEditorState: EditorStore

  function updateEditor() {
    storeHook.setState(workingEditorState)
  }

  const spyCollector = emptyUiJsxCanvasContextData()

  const asyncTestDispatch = async (
    actions: ReadonlyArray<EditorAction>,
    priority?: DispatchPriority, // priority is not used in the editorDispatch now, but we didn't delete this param yet
    waitForDispatchEntireUpdate = false,
    waitForADomReport = false,
  ) => {
    recordedActions.push(...actions)
    const result = editorDispatch(asyncTestDispatch, actions, workingEditorState, spyCollector)
    result.entireUpdateFinished.then(() => dispatchFollowUpActionsFinished.resolve())
    workingEditorState = result
    if (actions[0]?.action === 'SAVE_DOM_REPORT') {
      domReportDispatched.resolve()
    }
    if (waitForDispatchEntireUpdate) {
      await Utils.timeLimitPromise(
        dispatchFollowUpActionsFinished,
        2000,
        'Follow up actions took too long.',
      )
    }
    updateEditor()

    if (waitForADomReport) {
      await Utils.timeLimitPromise(domReportDispatched, 2000, 'DOM report took too long.')
      resetPromises() // I _think_ this is safe for concurrency, so long as all the test callsites `await` the dispatch
    }
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
      new FakeBundlerWorker(),
      new FakeParserPrinterWorker(),
      new FakeLinterWorker(),
      new FakeWatchdogWorker(),
    ),
    dispatch: asyncTestDispatch,
    alreadySaved: false,
  }

  const storeHook = create<EditorStore>((set) => initialEditorStore)

  // initializing the local editor state
  workingEditorState = storeHook.getState()

  let numberOfCommits = 0

  const result = render(
    <React.Profiler
      id='editor-root'
      /* eslint-disable-next-line react/jsx-no-bind */
      onRender={(id, phase, actualDuration, baseDuration, startTime, commitTime, interactions) => {
        numberOfCommits++
      }}
    >
      <HotRoot
        api={storeHook}
        useStore={storeHook}
        spyCollector={spyCollector}
        propertyControlsInfoSupported={false}
      />
    </React.Profiler>,
  )

  const noFileOpenText = result.getByText('No file open')
  expect(noFileOpenText).toBeDefined()

  await act(async () => {
    await new Promise((resolve, reject) => {
      load(
        async (actions) => {
          try {
            await asyncTestDispatch(actions, undefined, true, true)
            resolve()
          } catch (e) {
            reject(e)
          }
        },
        model,
        'Test',
        '0',
        initialEditorStore.workers,
        Utils.NO_OP,
        false,
      )
    })
  })

  await act(async () => {
    await asyncTestDispatch([switchEditorMode(EditorModes.selectMode())], undefined, true, false)
  })

  return {
    dispatch: async (actions: ReadonlyArray<EditorAction>, waitForDOMReport: boolean) => {
      return await act(async () => {
        await asyncTestDispatch(actions, 'everyone', true, waitForDOMReport)
      })
    },
    getDomReportDispatched: () => domReportDispatched,
    getDispatchFollowUpactionsFinished: () => dispatchFollowUpActionsFinished,
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
  store: EditorStore,
  filePath: string = StoryboardFilePath,
): string {
  const file = getContentsTreeFileFromString(store.editor.projectContents, filePath)
  if (isTextFile(file)) {
    return file.fileContents.code
  } else {
    fail('File is not a text file.')
  }
}

export function getPrintedUiJsCodeWithoutUIDs(store: EditorStore): string {
  const file = getContentsTreeFileFromString(store.editor.projectContents, StoryboardFilePath)
  if (isTextFile(file) && isParseSuccess(file.fileContents.parsed)) {
    return printCode(
      printCodeOptions(false, true, false, true),
      file.fileContents.parsed.imports,
      file.fileContents.parsed.topLevelElements,
      file.fileContents.parsed.jsxFactoryFunction,
      file.fileContents.parsed.exportsDetail,
    )
  } else {
    fail('File is not a text file.')
  }
}

export const TestSceneUID = 'scene-aaa'
export const TestAppUID = 'app-entity'
export const TestStoryboardPath = elementPath([[BakedInStoryboardUID]])
export const TestSceneElementPaths = [[BakedInStoryboardUID, TestSceneUID, TestAppUID]]
export const TestScenePath = elementPath(TestSceneElementPaths)
export const TestStaticScenePath = testStaticElementPath(TestSceneElementPaths)

export function makeTestProjectCodeWithSnippet(snippet: string): string {
  const code = `
  import * as React from 'react'
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
  return Prettier.format(code, PrettierConfig)
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
  return Prettier.format(code, PrettierConfig)
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

export function testPrintCodeFromParseSuccess(parseSuccess: ParseSuccess): string {
  return printCode(
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
  return testPrintCodeFromParseSuccess(parseSuccess)
}

export function testPrintParsedTextFile(parsedTextFile: ParsedTextFile): string {
  return foldParsedTextFile(
    (_) => 'FAILURE',
    testPrintCodeFromParseSuccess,
    (_) => 'UNPARSED',
    parsedTextFile,
  )
}
