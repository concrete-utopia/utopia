const Prettier = jest != null ? require('prettier') : require('prettier/standalone') // TODO split these files, standalone prettier is not working in unit tests
import React from 'react'
import { applyUIDMonkeyPatch } from '../../utils/canvas-react-utils'
applyUIDMonkeyPatch()
import * as ReactDOMServer from 'react-dom/server'

import { FancyError, processErrorWithSourceMap } from '../../core/shared/code-exec-utils'
import { Either, isRight, left, right } from '../../core/shared/either'
import {
  ElementInstanceMetadata,
  clearJSXElementUniqueIDs,
  TopLevelElement,
  ArbitraryJSBlock,
} from '../../core/shared/element-template'
import { canvasPoint } from '../../core/shared/math-utils'
import { RequireFn } from '../../core/shared/npm-dependency-types'
import {
  Imports,
  foldParsedTextFile,
  codeFile,
  textFile,
  textFileContents,
  RevisionsState,
  ProjectContents,
  isParseSuccess,
} from '../../core/shared/project-file-types'
import { emptyImports } from '../../core/workers/common/project-file-utils'
import { testParseCode } from '../../core/workers/parser-printer/parser-printer.test-utils'
import { Utils } from '../../uuiui-deps'
import { normalizeName } from '../custom-code/custom-code-utils'
import { ConsoleLog, deriveState, EditorState } from '../editor/store/editor-state'
import {
  UiJsxCanvasProps,
  UiJsxCanvasContextData,
  emptyUiJsxCanvasContextData,
  CanvasReactErrorCallback,
  UiJsxCanvasPropsWithErrorCallback,
  UiJsxCanvasCtxAtom,
  UiJsxCanvas,
} from './ui-jsx-canvas'
import { CanvasErrorBoundary } from './canvas-component-entry'
import { EditorStateContext } from '../editor/store/store-hook'
import { getStoreHook } from '../inspector/common/inspector.test-utils'
import { NO_OP } from '../../core/shared/utils'
import { directory } from '../../core/model/project-file-utils'
import { contentsToTree, ProjectContentTreeRoot } from '../assets'
import { MapLike } from 'typescript'
import { getRequireFn } from '../../core/es-modules/package-manager/package-manager'
import type { ScriptLine } from '../../third-party/react-error-overlay/utils/stack-frame'
import type { CurriedResolveFn } from '../custom-code/code-file'
import * as path from 'path'
import { SampleNodeModules } from '../custom-code/code-file.test-utils'
import { UPDATE_FNS } from '../editor/actions/actions'
import { updateNodeModulesContents } from '../editor/actions/action-creators'

export interface PartialCanvasProps {
  hiddenInstances: UiJsxCanvasProps['hiddenInstances']
  editedTextElement: UiJsxCanvasProps['editedTextElement']
  mountCount: UiJsxCanvasProps['mountCount']
}

export const dumbResolveFn = (filenames: Array<string>): CurriedResolveFn => {
  return (_: ProjectContentTreeRoot) => (importOrigin: string, toImport: string) =>
    resolveTestFiles(filenames, importOrigin, toImport)
}

function resolveTestFiles(
  filenames: Array<string>,
  importOrigin: string,
  toImport: string,
): Either<string, string> {
  let normalizedName = normalizeName(importOrigin, toImport)
  // Partly restoring what `normalizeName` strips away.
  if (toImport.startsWith('.')) {
    normalizedName = path.normalize(`${importOrigin}/${normalizedName}`)
  } else if (toImport.startsWith('/')) {
    normalizedName = `/${normalizedName}`
  }
  for (const extension of ['.js', '.ts', '.jsx', '.tsx']) {
    const filenameWithExtension = `${normalizedName}${extension}`
    if (filenames.includes(filenameWithExtension)) {
      return right(filenameWithExtension)
    }
  }
  switch (normalizedName) {
    case 'utopia-api':
    case 'react':
    case 'react-dom':
    case 'uuiui':
    case 'antd':
      return right(`/node_modules/${normalizedName}/index.js`) // ¯\_(ツ)_/¯
    case UiFilePath:
      return right(UiFilePath)
    default:
      return left(`Test error, the dumbResolveFn did not know about this file: ${toImport}`)
  }
}

function stripUidsFromMetadata(metadata: ElementInstanceMetadata): ElementInstanceMetadata {
  if (isRight(metadata.element)) {
    return {
      ...metadata,
      element: right(clearJSXElementUniqueIDs(metadata.element.value)),
    }
  } else {
    return metadata
  }
}

function stripUnwantedDataFromMetadata(metadata: ElementInstanceMetadata): ElementInstanceMetadata {
  return stripUidsFromMetadata(metadata)
}

interface RuntimeErrorInfo {
  editedFile: string
  error: FancyError
  errorInfo?: React.ErrorInfo
}

const UiFilePath: UiJsxCanvasProps['uiFilePath'] = 'test.js'
export function renderCanvasReturnResultAndError(
  possibleProps: PartialCanvasProps | null,
  uiFileCode: string,
  codeFilesString: MapLike<string>,
) {
  const spyCollector: UiJsxCanvasContextData = emptyUiJsxCanvasContextData()

  const parsedUIFileCode = testParseCode(uiFileCode)
  let errorsReported: Array<RuntimeErrorInfo> = []
  const reportError: CanvasReactErrorCallback['reportError'] = (
    editedFile: string,
    error: FancyError,
    errorInfo?: React.ErrorInfo,
  ) => {
    errorsReported.push({ editedFile: editedFile, error: error, errorInfo: errorInfo })
  }
  const clearErrors: CanvasReactErrorCallback['clearErrors'] = Utils.NO_OP
  const imports: Imports = foldParsedTextFile(
    (_) => emptyImports(),
    (success) => success.imports,
    (_) => emptyImports(),
    parsedUIFileCode,
  )
  let canvasProps: UiJsxCanvasPropsWithErrorCallback
  let consoleLogs: Array<ConsoleLog> = []

  const storeHookForTest = getStoreHook(NO_OP)
  let projectContents: ProjectContents = {
    [UiFilePath]: textFile(
      textFileContents(uiFileCode, parsedUIFileCode, RevisionsState.BothMatch),
      null,
      isParseSuccess(parsedUIFileCode) ? parsedUIFileCode : null,
      1000,
    ),
  }
  for (const filename in codeFilesString) {
    const parsedCode = testParseCode(codeFilesString[filename])
    projectContents[filename] = textFile(
      textFileContents(codeFilesString[filename], parsedCode, RevisionsState.BothMatch),
      null,
      isParseSuccess(parsedCode) ? parsedCode : null,
      1000,
    )
  }
  const updatedContents = contentsToTree(projectContents)

  const curriedRequireFn = (innerProjectContents: ProjectContentTreeRoot) =>
    getRequireFn(
      NO_OP,
      innerProjectContents,
      SampleNodeModules,
      {},
      storeHookForTest.useStore().builtInDependencies,
    )

  storeHookForTest.updateStore((store) => {
    let updatedEditor: EditorState = {
      ...store.editor,
      canvas: {
        ...store.editor.canvas,
        openFile: {
          filename: UiFilePath,
        },
      },
      projectContents: updatedContents,
    }
    updatedEditor = UPDATE_FNS.UPDATE_NODE_MODULES_CONTENTS(
      updateNodeModulesContents(SampleNodeModules),
      updatedEditor,
      store.dispatch,
      store.builtInDependencies,
    )
    return {
      ...store,
      editor: updatedEditor,
      derived: deriveState(updatedEditor, store.derived),
    }
  })

  function clearConsoleLogs(): void {
    consoleLogs = []
  }
  function addToConsoleLogs(log: ConsoleLog): void {
    consoleLogs.push(log)
  }
  if (possibleProps == null) {
    canvasProps = {
      uiFilePath: UiFilePath,
      curriedRequireFn: curriedRequireFn,
      curriedResolveFn: dumbResolveFn(Object.keys(codeFilesString)),
      base64FileBlobs: {},
      clearErrors: clearErrors,
      hiddenInstances: [],
      editedTextElement: null,
      mountCount: 0,
      domWalkerInvalidateCount: 0,
      imports_KILLME: imports,
      canvasIsLive: false,
      shouldIncludeCanvasRootInTheSpy: false,
      clearConsoleLogs: clearConsoleLogs,
      addToConsoleLogs: addToConsoleLogs,
      linkTags: '',
      focusedElementPath: null,
      projectContents: storeHookForTest.api.getState().editor.projectContents,
      transientFilesState: storeHookForTest.api.getState().derived.transientState.filesState,
      propertyControlsInfo: {},
      dispatch: NO_OP,
      domWalkerAdditionalElementsToUpdate: [],
      elementsToRerender: 'rerender-all-elements',
    }
  } else {
    canvasProps = {
      ...possibleProps,
      uiFilePath: UiFilePath,
      curriedRequireFn: curriedRequireFn,
      curriedResolveFn: dumbResolveFn(Object.keys(codeFilesString)),
      base64FileBlobs: {},
      clearErrors: clearErrors,
      domWalkerInvalidateCount: 0,
      imports_KILLME: imports,
      canvasIsLive: false,
      shouldIncludeCanvasRootInTheSpy: false,
      clearConsoleLogs: clearConsoleLogs,
      addToConsoleLogs: addToConsoleLogs,
      linkTags: '',
      focusedElementPath: null,
      projectContents: storeHookForTest.api.getState().editor.projectContents,
      transientFilesState: storeHookForTest.api.getState().derived.transientState.filesState,
      propertyControlsInfo: {},
      dispatch: NO_OP,
      domWalkerAdditionalElementsToUpdate: [],
      elementsToRerender: 'rerender-all-elements',
    }
  }

  const canvasPropsSpyDisabled = {
    ...canvasProps,
    spyEnabled: false,
  }

  let formattedSpyEnabled
  let errorsReportedSpyEnabled: Array<RuntimeErrorInfo> = []
  try {
    const flatFormat = ReactDOMServer.renderToStaticMarkup(
      <EditorStateContext.Provider value={storeHookForTest}>
        <UiJsxCanvasCtxAtom.Provider value={spyCollector}>
          <CanvasErrorBoundary
            filePath={UiFilePath}
            projectContents={canvasProps.projectContents}
            // eslint-disable-next-line react/jsx-no-bind
            reportError={reportError}
            requireFn={canvasProps.curriedRequireFn}
          >
            <UiJsxCanvas {...canvasProps} />
          </CanvasErrorBoundary>
        </UiJsxCanvasCtxAtom.Provider>
      </EditorStateContext.Provider>,
    )
    formattedSpyEnabled = Prettier.format(flatFormat, { parser: 'html' })
    errorsReportedSpyEnabled = errorsReported
  } catch (e: any) {
    // TODO instead of relying on this hack here, we should create a new test function that runs the real react render instead of ReactDOMServer.renderToStaticMarkup
    processErrorWithSourceMap(UiFilePath, uiFileCode, e, true)
    errorsReportedSpyEnabled = [e]
  }
  errorsReported = []

  let formattedSpyDisabled
  let errorsReportedSpyDisabled: Array<RuntimeErrorInfo> = []

  try {
    const flatFormatSpyDisabled = ReactDOMServer.renderToStaticMarkup(
      <EditorStateContext.Provider value={storeHookForTest}>
        <UiJsxCanvasCtxAtom.Provider value={emptyUiJsxCanvasContextData()}>
          <UiJsxCanvas {...canvasPropsSpyDisabled} />
        </UiJsxCanvasCtxAtom.Provider>
      </EditorStateContext.Provider>,
    )
    formattedSpyDisabled = Prettier.format(flatFormatSpyDisabled, { parser: 'html' })
    errorsReportedSpyDisabled = errorsReported
  } catch (e: any) {
    // TODO instead of relying on this hack here, we should create a new test function that runs the real react render instead of ReactDOMServer.renderToStaticMarkup
    processErrorWithSourceMap(UiFilePath, uiFileCode, e, true)
    errorsReportedSpyDisabled = [e]
  }

  return {
    formattedSpyEnabled,
    formattedSpyDisabled,
    errorsReportedSpyEnabled,
    errorsReportedSpyDisabled,
    spyValues: spyCollector.current.spyValues,
  }
}

export function testCanvasRender(possibleProps: PartialCanvasProps | null, code: string): void {
  testCanvasRenderMultifile(possibleProps, code, {})
}

export function testCanvasRenderMultifile(
  possibleProps: PartialCanvasProps | null,
  uiFileCode: string,
  codeFilesString: MapLike<string>,
): void {
  const {
    formattedSpyEnabled,
    formattedSpyDisabled,
    errorsReportedSpyEnabled,
    errorsReportedSpyDisabled,
    spyValues,
  } = renderCanvasReturnResultAndError(possibleProps, uiFileCode, codeFilesString)
  if (errorsReportedSpyEnabled.length > 0) {
    throw new Error(`Canvas Tests, Spy Enabled: Errors reported: ${errorsReportedSpyEnabled}`)
  }
  if (errorsReportedSpyDisabled.length > 0) {
    throw new Error(`Canvas Tests, Spy Disabled: Errors reported: ${errorsReportedSpyDisabled}`)
  }

  // Spy enabled or disabled should have no effect on the rendered HTML
  expect(formattedSpyEnabled).toEqual(formattedSpyDisabled)

  expect(formattedSpyEnabled).toMatchSnapshot()

  const metadataWithoutUIDs = Utils.objectMap(stripUnwantedDataFromMetadata, spyValues.metadata)
  expect(metadataWithoutUIDs).toMatchSnapshot()
}

export function testCanvasRenderInline(
  possibleProps: PartialCanvasProps | null,
  code: string,
): string {
  return testCanvasRenderInlineMultifile(possibleProps, code, {})
}

export function testCanvasRenderInlineMultifile(
  possibleProps: PartialCanvasProps | null,
  uiFileCode: string,
  codeFilesString: MapLike<string>,
): string {
  const {
    formattedSpyEnabled,
    formattedSpyDisabled,
    errorsReportedSpyEnabled,
    errorsReportedSpyDisabled,
    spyValues,
  } = renderCanvasReturnResultAndError(possibleProps, uiFileCode, codeFilesString)
  if (errorsReportedSpyEnabled.length > 0) {
    console.error(errorsReportedSpyEnabled)
  }
  if (errorsReportedSpyEnabled.length > 0) {
    throw new Error(`Canvas Tests, Spy Enabled: Errors reported: ${errorsReportedSpyEnabled}`)
  }
  if (errorsReportedSpyDisabled.length > 0) {
    throw new Error(`Canvas Tests, Spy Disabled: Errors reported: ${errorsReportedSpyDisabled}`)
  }

  // Spy enabled or disabled should have no effect on the rendered HTML
  expect(formattedSpyEnabled).toEqual(formattedSpyDisabled)
  expect(formattedSpyEnabled).toBeDefined()

  return formattedSpyEnabled!
}

export function testCanvasError(possibleProps: PartialCanvasProps | null, code: string): void {
  testCanvasErrorMultifile(possibleProps, code, {})
}

export function testCanvasErrorMultifile(
  possibleProps: PartialCanvasProps | null,
  uiFileCode: string,
  codeFilesString: MapLike<string>,
): void {
  const errorsToCheck = testCanvasErrorInline(possibleProps, uiFileCode, codeFilesString)
  expect(errorsToCheck).toMatchSnapshot()
}

interface MappedStackFrame {
  fileName: string | null
  originalCode: ScriptLine[] | null
  lineNumber: number | null
  columnNumber: number | null
}

interface TestCanvasError {
  name: string
  message: string
  stackFrames: MappedStackFrame[]
}

export function testCanvasErrorInline(
  possibleProps: PartialCanvasProps | null,
  uiFileCode: string,
  codeFilesString: MapLike<string>,
): TestCanvasError[] {
  const { errorsReportedSpyEnabled, errorsReportedSpyDisabled } = renderCanvasReturnResultAndError(
    possibleProps,
    uiFileCode,
    codeFilesString,
  )

  expect(errorsReportedSpyEnabled.length).toEqual(errorsReportedSpyDisabled.length)
  expect(errorsReportedSpyEnabled.length).toBeGreaterThan(0)
  const errorsToCheck = errorsReportedSpyEnabled.map((error) => {
    let realError = error.error != null ? error.error : (error as unknown as FancyError) // is this conversion needed?
    return {
      name: realError.name,
      message: realError.message,
      stackFrames:
        realError.stackFrames?.map((stackFrame) => {
          return {
            fileName: stackFrame._originalFileName,
            originalCode: stackFrame._originalScriptCode,
            lineNumber: stackFrame._originalLineNumber,
            columnNumber: stackFrame._originalColumnNumber,
          }
        }) ?? [],
    }
  })
  return errorsToCheck
}
