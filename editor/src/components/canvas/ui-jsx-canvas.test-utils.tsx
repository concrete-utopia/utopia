import * as Prettier from 'prettier'
import * as React from 'react'
import { applyUIDMonkeyPatch } from '../../utils/canvas-react-utils'
applyUIDMonkeyPatch()
import * as ReactDOM from 'react-dom'
import * as ReactDOMServer from 'react-dom/server'
import * as UtopiaAPI from 'utopia-api'
import * as UUIUI from '../../uuiui'
import * as ANTD from 'antd'
import * as EmotionReact from '@emotion/react'

import { FancyError } from '../../core/shared/code-exec-utils'
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
} from '../../core/shared/project-file-types'
import { emptyImports } from '../../core/workers/common/project-file-utils'
import { testParseCode } from '../../core/workers/parser-printer/parser-printer.test-utils'
import { Utils } from '../../uuiui-deps'
import { normalizeName } from '../custom-code/custom-code-utils'
import { ConsoleLog, deriveState } from '../editor/store/editor-state'
import {
  UiJsxCanvasProps,
  UiJsxCanvasContextData,
  emptyUiJsxCanvasContextData,
  CanvasReactErrorCallback,
  UiJsxCanvasPropsWithErrorCallback,
  UiJsxCanvasContext,
  UiJsxCanvas,
} from './ui-jsx-canvas'
import { CanvasErrorBoundary } from './canvas-component-entry'
import { EditorStateContext } from '../editor/store/store-hook'
import { getStoreHook } from '../inspector/common/inspector.test-utils'
import { NO_OP } from '../../core/shared/utils'
import { directory } from '../../core/model/project-file-utils'
import { contentsToTree } from '../assets'
import { MapLike } from 'typescript'
import { getRequireFn } from '../../core/es-modules/package-manager/package-manager'

export interface PartialCanvasProps {
  offset: UiJsxCanvasProps['offset']
  scale: UiJsxCanvasProps['scale']
  hiddenInstances: UiJsxCanvasProps['hiddenInstances']
  editedTextElement: UiJsxCanvasProps['editedTextElement']
  mountCount: UiJsxCanvasProps['mountCount']
}

export const dumbResolveFn = (filenames: Array<string>) => (
  importOrigin: string,
  toImport: string,
): Either<string, string> => {
  return resolveTestFiles(filenames, importOrigin, toImport)
}

function resolveTestFiles(
  filenames: Array<string>,
  importOrigin: string,
  toImport: string,
): Either<string, string> {
  const normalizedName = normalizeName(importOrigin, toImport)
  if (filenames.includes(normalizedName)) {
    return right(normalizedName)
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

export function stripUidsFromMetadata(metadata: ElementInstanceMetadata): ElementInstanceMetadata {
  if (isRight(metadata.element)) {
    return {
      ...metadata,
      element: right(clearJSXElementUniqueIDs(metadata.element.value)),
    }
  } else {
    return metadata
  }
}

const UiFilePath: UiJsxCanvasProps['uiFilePath'] = 'test.js'
export function renderCanvasReturnResultAndError(
  possibleProps: PartialCanvasProps | null,
  uiFileCode: string,
  codeFilesString: MapLike<string>,
) {
  const spyCollector: UiJsxCanvasContextData = emptyUiJsxCanvasContextData()

  const parsedUIFileCode = testParseCode(uiFileCode)
  let errorsReported: Array<{
    editedFile: string
    error: FancyError
    errorInfo?: React.ErrorInfo
  }> = []
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
      1000,
    ),
  }
  for (const filename in codeFilesString) {
    const parsedCode = testParseCode(codeFilesString[filename])
    projectContents[filename] = textFile(
      textFileContents(codeFilesString[filename], parsedCode, RevisionsState.BothMatch),
      null,
      1000,
    )
  }
  const updatedContents = contentsToTree(projectContents)

  const baseRequireFn = getRequireFn(NO_OP, updatedContents, {}, {}, 'canvas')
  const requireFn: UiJsxCanvasProps['requireFn'] = (importOrigin: string, toImport: string) => {
    switch (toImport) {
      case 'antd':
        return ANTD
      default:
        return baseRequireFn(importOrigin, toImport)
    }
  }
  storeHookForTest.updateStore((store) => {
    const updatedEditor = {
      ...store.editor,
      canvas: {
        ...store.editor.canvas,
        openFile: {
          filename: UiFilePath,
        },
      },
      projectContents: updatedContents,
    }
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
      uiFileCode: uiFileCode,
      uiFilePath: UiFilePath,
      selectedViews: [],
      requireFn: requireFn,
      resolve: dumbResolveFn(Object.keys(codeFilesString)),
      base64FileBlobs: {},
      onDomReport: Utils.NO_OP,
      clearErrors: clearErrors,
      offset: canvasPoint({ x: 0, y: 0 }),
      scale: 1,
      hiddenInstances: [],
      editedTextElement: null,
      mountCount: 0,
      walkDOM: false,
      imports_KILLME: imports,
      canvasIsLive: false,
      shouldIncludeCanvasRootInTheSpy: false,
      clearConsoleLogs: clearConsoleLogs,
      addToConsoleLogs: addToConsoleLogs,
      linkTags: '',
      focusedElementPath: null,
      projectContents: storeHookForTest.api.getState().editor.projectContents,
      transientFilesState: storeHookForTest.api.getState().derived.canvas.transientState.filesState,
      scrollAnimation: false,
    }
  } else {
    canvasProps = {
      ...possibleProps,
      uiFileCode: uiFileCode,
      uiFilePath: UiFilePath,
      selectedViews: [],
      requireFn: requireFn,
      resolve: dumbResolveFn(Object.keys(codeFilesString)),
      base64FileBlobs: {},
      onDomReport: Utils.NO_OP,
      clearErrors: clearErrors,
      walkDOM: false,
      imports_KILLME: imports,
      canvasIsLive: false,
      shouldIncludeCanvasRootInTheSpy: false,
      clearConsoleLogs: clearConsoleLogs,
      addToConsoleLogs: addToConsoleLogs,
      linkTags: '',
      focusedElementPath: null,
      projectContents: storeHookForTest.api.getState().editor.projectContents,
      transientFilesState: storeHookForTest.api.getState().derived.canvas.transientState.filesState,
      scrollAnimation: false,
    }
  }

  const canvasPropsSpyDisabled = {
    ...canvasProps,
    spyEnabled: false,
  }

  let formattedSpyEnabled
  let errorsReportedSpyEnabled = []
  try {
    const flatFormat = ReactDOMServer.renderToStaticMarkup(
      <EditorStateContext.Provider value={storeHookForTest}>
        <UiJsxCanvasContext.Provider value={spyCollector}>
          <CanvasErrorBoundary
            fileCode={uiFileCode}
            filePath={UiFilePath}
            reportError={reportError}
            requireFn={canvasProps.requireFn}
          >
            <UiJsxCanvas {...canvasProps} />
          </CanvasErrorBoundary>
        </UiJsxCanvasContext.Provider>
      </EditorStateContext.Provider>,
    )
    formattedSpyEnabled = Prettier.format(flatFormat, { parser: 'html' })
    errorsReportedSpyEnabled = errorsReported
  } catch (e) {
    errorsReportedSpyEnabled = [e]
  }
  errorsReported = []

  let formattedSpyDisabled
  let errorsReportedSpyDisabled = []

  try {
    const flatFormatSpyDisabled = ReactDOMServer.renderToStaticMarkup(
      <EditorStateContext.Provider value={storeHookForTest}>
        <UiJsxCanvasContext.Provider value={emptyUiJsxCanvasContextData()}>
          <UiJsxCanvas {...canvasPropsSpyDisabled} />
        </UiJsxCanvasContext.Provider>
      </EditorStateContext.Provider>,
    )
    formattedSpyDisabled = Prettier.format(flatFormatSpyDisabled, { parser: 'html' })
    errorsReportedSpyDisabled = errorsReported
  } catch (e) {
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

  const metadataWithoutUIDs = Utils.objectMap(stripUidsFromMetadata, spyValues.metadata)
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
  const { errorsReportedSpyEnabled, errorsReportedSpyDisabled } = renderCanvasReturnResultAndError(
    possibleProps,
    uiFileCode,
    codeFilesString,
  )

  expect(errorsReportedSpyEnabled.length).toEqual(errorsReportedSpyDisabled.length)
  expect(errorsReportedSpyEnabled.length).toBeGreaterThan(0)
  const errorsToCheck = errorsReportedSpyEnabled.map((error) => {
    let realError = error.error != null ? error.error : error
    const stackFrame = realError.stackFrames?.[0]
    return {
      name: realError.name,
      message: realError.message,
      originalCode: stackFrame?._originalScriptCode,
      lineNumber: stackFrame?._originalLineNumber,
      columnNumber: stackFrame?._originalColumnNumber,
    }
  })
  expect(errorsToCheck).toMatchSnapshot()
}
