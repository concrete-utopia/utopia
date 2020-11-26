// WATCH OUT! We are disabling the imports from monaco here, because it breaks jest
jest.mock('../code-editor/script-editor', () => ({
  ScriptEditor: (props: any) => {
    return <div>hi!</div>
  },
}))

jest.mock('../editor/stored-state', () => ({
  loadStoredState: () => Promise.resolve(null),
  saveStoredState: () => Promise.resolve(),
}))

import * as React from 'react'

///// IMPORTANT NOTE - THIS MUST BE BELOW THE REACT IMPORT AND ABOVE ALL OTHER IMPORTS
const realCreateElement = React.createElement
let renderCount = 0
const monkeyCreateElement = (...params: any[]) => {
  renderCount++
  return (realCreateElement as any)(...params)
}
;(React as any).createElement = monkeyCreateElement

jest.setTimeout(10000) // in milliseconds

import { act, render } from '@testing-library/react'
import * as Prettier from 'prettier'
import create from 'zustand'
import {
  ComponentMetadata,
  ElementInstanceMetadata,
  TopLevelElement,
} from '../../core/shared/element-template'
import {
  foldParsedTextFile,
  isParseFailure,
  isParseSuccess,
  isTextFile,
  isUnparsed,
  ParsedTextFile,
  ParseSuccess,
  TextFile,
} from '../../core/shared/project-file-types'
import { PrettierConfig } from '../../core/workers/parser-printer/prettier-utils'
import {
  FakeBundlerWorker,
  FakeLinterWorker,
  FakeParserPrinterWorker,
  FakeWatchdogWorker,
} from '../../core/workers/test-workers'
import { UtopiaTsWorkersImplementation } from '../../core/workers/workers'
import { HotRoot } from '../../templates/editor'
import { left, Right, isLeft } from '../../core/shared/either'
import Utils from '../../utils/utils'
import { DispatchPriority, EditorAction, notLoggedIn } from '../editor/action-types'
import { load } from '../editor/actions/actions'
import * as History from '../editor/history'
import { editorDispatch } from '../editor/store/dispatch'
import { createEditorState, deriveState, EditorStore } from '../editor/store/editor-state'
import { createTestProjectWithCode } from './canvas-utils'
import { BakedInStoryboardUID, BakedInStoryboardVariableName } from '../../core/model/scene-utils'
import { scenePath } from '../../core/shared/template-path'
import { NO_OP } from '../../core/shared/utils'
import { emptyUiJsxCanvasContextData } from './ui-jsx-canvas'
import { testParseCode } from '../../core/workers/parser-printer/parser-printer-test-utils'
import { printCode, printCodeOptions } from '../../core/workers/parser-printer/parser-printer'
import { setPropertyControlsIFrameAvailable } from '../../core/property-controls/property-controls-utils'
import { getContentsTreeFileFromString } from '../assets'

process.on('unhandledRejection', (reason, promise) => {
  console.warn('Unhandled promise rejection:', promise, 'reason:', (reason as any)?.stack || reason)
})

export async function renderTestEditorWithCode(appUiJsFileCode: string) {
  const renderCountBaseline = renderCount

  let emptyEditorState = createEditorState(NO_OP)
  const derivedState = deriveState(emptyEditorState, null)

  const history = History.init(emptyEditorState, derivedState)

  let domReportDispatched = Utils.defer()
  let dispatchFollowUpActionsFinished = Utils.defer()

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
    const result = editorDispatch(asyncTestDispatch, actions, workingEditorState, spyCollector)
    result.entireUpdateFinished.then(() => dispatchFollowUpActionsFinished.resolve())
    workingEditorState = result
    if (actions[0]?.action === 'SAVE_DOM_REPORT') {
      domReportDispatched.resolve(true)
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
    await new Promise<void>((resolve, reject) => {
      load(
        async (actions) => {
          try {
            await asyncTestDispatch(actions, undefined, true, true)
            resolve()
          } catch (e) {
            reject(e)
          }
        },
        createTestProjectWithCode(appUiJsFileCode),
        'Test',
        '0',
        initialEditorStore.workers,
        Utils.NO_OP,
        false,
      )
    })
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
  }
}

export function getPrintedUiJsCode(store: EditorStore): string {
  const file = getContentsTreeFileFromString(store.editor.projectContents, '/src/app.js')
  if (isTextFile(file)) {
    return file.fileContents.code
  } else {
    fail('File is not a text file.')
  }
}

const TestSceneUID = 'scene-aaa'
export const TestScenePath = scenePath([BakedInStoryboardUID, TestSceneUID])

export function makeTestProjectCodeWithSnippet(snippet: string): string {
  const code = `/** @jsx jsx */
  import * as React from 'react'
  import { Scene, Storyboard, View, jsx } from 'utopia-api'
  export var App = (props) => {
    return (
${snippet}
    )
  }
  export var ${BakedInStoryboardVariableName} = (props) => {
    return (
      <Storyboard data-uid={'${BakedInStoryboardUID}'}>
        <Scene
          style={{ left: 0, top: 0, width: 400, height: 400 }}
          component={App}
          static
          layout={{ layoutSystem: 'pinSystem' }}
          props={{ layout: { bottom: 0, left: 0, right: 0, top: 0 } }}
          data-uid={'${TestSceneUID}'}
        />
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

export function testPrintCode(parseSuccess: ParseSuccess): string {
  return printCode(
    printCodeOptions(false, true, true),
    parseSuccess.imports,
    parseSuccess.topLevelElements,
    parseSuccess.jsxFactoryFunction,
    parseSuccess.exportsDetail,
  )
}

export function testPrintParsedTextFile(parsedTextFile: ParsedTextFile): string {
  return foldParsedTextFile(
    (_) => 'FAILURE',
    testPrintCode,
    (_) => 'UNPARSED',
    parsedTextFile,
  )
}
