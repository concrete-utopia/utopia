const Prettier = jest != null ? require('prettier') : require('prettier/standalone') // TODO split these files, standalone prettier is not working in unit tests
import React from 'react'
import { applyUIDMonkeyPatch } from '../../utils/canvas-react-utils'
applyUIDMonkeyPatch()
import * as ReactDOMServer from 'react-dom/server'

import type { FancyError } from '../../core/shared/code-exec-utils'
import { processErrorWithSourceMap } from '../../core/shared/code-exec-utils'
import type { Either } from '../../core/shared/either'
import { isRight, left, mapEither, right } from '../../core/shared/either'
import type { ElementInstanceMetadata } from '../../core/shared/element-template'
import { clearJSXElementChildUniqueIDs } from '../../core/shared/element-template'
import type { ProjectContents } from '../../core/shared/project-file-types'
import {
  textFile,
  textFileContents,
  RevisionsState,
  isParseSuccess,
} from '../../core/shared/project-file-types'
import {
  simplifyJSXElementChildAttributes,
  testParseCode,
} from '../../core/workers/parser-printer/parser-printer.test-utils'
import { Utils } from '../../uuiui-deps'
import { normalizeName } from '../custom-code/custom-code-utils'
import type { EditorState } from '../editor/store/editor-state'
import { deriveState } from '../editor/store/editor-state'
import type {
  UiJsxCanvasProps,
  UiJsxCanvasContextData,
  CanvasReactErrorCallback,
  UiJsxCanvasPropsWithErrorCallback,
} from './ui-jsx-canvas'
import { emptyUiJsxCanvasContextData, UiJsxCanvasCtxAtom, UiJsxCanvas } from './ui-jsx-canvas'
import { CanvasErrorBoundary } from './canvas-component-entry'
import { EditorStateContext, OriginalMainEditorStateContext } from '../editor/store/store-hook'
import { getStoreHook } from '../inspector/common/inspector.test-utils'
import { NO_OP } from '../../core/shared/utils'
import type { ProjectContentTreeRoot } from '../assets'
import { contentsToTree } from '../assets'
import type { MapLike } from 'typescript'
import { getRequireFn } from '../../core/es-modules/package-manager/package-manager'
import type { ScriptLine } from '../../third-party/react-error-overlay/utils/stack-frame'
import type { CurriedResolveFn } from '../custom-code/code-file'
import * as path from 'path'
import { SampleNodeModules } from '../custom-code/code-file.test-utils'
import { UPDATE_FNS } from '../editor/actions/actions'
import { updateNodeModulesContents } from '../editor/actions/action-creators'
import { unpatchedCreateRemixDerivedDataMemo } from '../editor/store/remix-derived-data'

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
      element: right(clearJSXElementChildUniqueIDs(metadata.element.value)),
    }
  } else {
    return metadata
  }
}

function stripUnwantedDataFromMetadata(metadata: ElementInstanceMetadata): ElementInstanceMetadata {
  const strippedMetadata = stripUidsFromMetadata(metadata)
  return {
    ...strippedMetadata,
    element: mapEither(simplifyJSXElementChildAttributes, strippedMetadata.element),
  }
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
  let canvasProps: UiJsxCanvasPropsWithErrorCallback

  const storeHookForTest = getStoreHook()
  let projectContents: ProjectContents = {
    [UiFilePath]: textFile(
      textFileContents(uiFileCode, parsedUIFileCode, RevisionsState.BothMatch),
      null,
      isParseSuccess(parsedUIFileCode) ? parsedUIFileCode : null,
      0,
    ),
  }
  for (const filename in codeFilesString) {
    const parsedCode = testParseCode(codeFilesString[filename])
    projectContents[filename] = textFile(
      textFileContents(codeFilesString[filename], parsedCode, RevisionsState.BothMatch),
      null,
      isParseSuccess(parsedCode) ? parsedCode : null,
      0,
    )
  }
  const updatedContents = contentsToTree(projectContents)

  const curriedRequireFn = (innerProjectContents: ProjectContentTreeRoot) =>
    getRequireFn(
      NO_OP,
      innerProjectContents,
      SampleNodeModules,
      {},
      storeHookForTest.getState().builtInDependencies,
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
      NO_OP,
      store.builtInDependencies,
    )
    return {
      ...store,
      editor: updatedEditor,
      derived: deriveState(
        updatedEditor,
        store.derived,
        'unpatched',
        unpatchedCreateRemixDerivedDataMemo,
      ),
    }
  })
  if (possibleProps == null) {
    canvasProps = {
      uiFilePath: UiFilePath,
      curriedRequireFn: curriedRequireFn,
      curriedResolveFn: dumbResolveFn(Object.keys(codeFilesString)),
      base64FileBlobs: {},
      clearErrors: clearErrors,
      hiddenInstances: [],
      displayNoneInstances: [],
      editedTextElement: null,
      mountCount: 0,
      domWalkerInvalidateCount: 0,
      canvasIsLive: false,
      shouldIncludeCanvasRootInTheSpy: false,
      linkTags: '',
      focusedElementPath: null,
      projectContents: storeHookForTest.getState().editor.projectContents,
      domWalkerAdditionalElementsToUpdate: [],
      editedText: null,
      autoFocusedPaths: storeHookForTest.getState().derived.autoFocusedPaths,
    }
  } else {
    canvasProps = {
      ...possibleProps,
      uiFilePath: UiFilePath,
      curriedRequireFn: curriedRequireFn,
      curriedResolveFn: dumbResolveFn(Object.keys(codeFilesString)),
      base64FileBlobs: {},
      clearErrors: clearErrors,
      displayNoneInstances: [],
      domWalkerInvalidateCount: 0,
      canvasIsLive: false,
      shouldIncludeCanvasRootInTheSpy: false,
      linkTags: '',
      focusedElementPath: null,
      projectContents: storeHookForTest.getState().editor.projectContents,
      domWalkerAdditionalElementsToUpdate: [],
      editedText: null,
      autoFocusedPaths: storeHookForTest.getState().derived.autoFocusedPaths,
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
      <OriginalMainEditorStateContext.Provider value={storeHookForTest}>
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
        </EditorStateContext.Provider>
      </OriginalMainEditorStateContext.Provider>,
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
      <OriginalMainEditorStateContext.Provider value={storeHookForTest}>
        <EditorStateContext.Provider value={storeHookForTest}>
          <UiJsxCanvasCtxAtom.Provider value={emptyUiJsxCanvasContextData()}>
            <UiJsxCanvas {...canvasPropsSpyDisabled} />
          </UiJsxCanvasCtxAtom.Provider>
        </EditorStateContext.Provider>
      </OriginalMainEditorStateContext.Provider>,
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
