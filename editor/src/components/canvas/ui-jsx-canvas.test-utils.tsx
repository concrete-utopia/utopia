import * as Prettier from 'prettier'
import * as React from 'react'
import { applyUIDMonkeyPatch } from '../../utils/canvas-react-utils'
applyUIDMonkeyPatch()
import * as ReactDOM from 'react-dom'
import * as ReactDOMServer from 'react-dom/server'
import * as UtopiaAPI from 'utopia-api'
import * as UUIUI from 'uuiui'
import * as ANTD from 'antd'

import { FancyError } from '../../core/shared/code-exec-utils'
import { isRight, right } from '../../core/shared/either'
import {
  ElementInstanceMetadata,
  clearJSXElementUniqueIDs,
  TopLevelElement,
  ArbitraryJSBlock,
} from '../../core/shared/element-template'
import { canvasPoint } from '../../core/shared/math-utils'
import { RequireFn } from '../../core/shared/npm-dependency-types'
import { Imports, foldParsedTextFile } from '../../core/shared/project-file-types'
import { emptyImports } from '../../core/workers/common/project-file-utils'
import { testParseCode } from '../../core/workers/parser-printer/parser-printer.test-utils'
import { Utils } from '../../uuiui-deps'
import { normalizeName } from '../custom-code/custom-code-utils'
import { ConsoleLog } from '../editor/store/editor-state'
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

export interface PartialCanvasProps {
  offset: UiJsxCanvasProps['offset']
  scale: UiJsxCanvasProps['scale']
  hiddenInstances: UiJsxCanvasProps['hiddenInstances']
  editedTextElement: UiJsxCanvasProps['editedTextElement']
  mountCount: UiJsxCanvasProps['mountCount']
}

export const dumbRequireFn: RequireFn = (importOrigin: string, toImport: string) => {
  const normalizedName = normalizeName(importOrigin, toImport)
  switch (normalizedName) {
    case 'utopia-api':
      return UtopiaAPI
    case 'react':
      return React
    case 'react-dom':
      return ReactDOM
    case 'uuiui':
      return UUIUI
    case 'antd':
      return ANTD
    default:
      throw new Error(`Unhandled values of ${importOrigin} and ${toImport}.`)
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

export function renderCanvasReturnResultAndError(
  possibleProps: PartialCanvasProps | null,
  code: string,
) {
  const spyCollector: UiJsxCanvasContextData = emptyUiJsxCanvasContextData()

  const parsedCode = testParseCode(code)
  let errorsReported: Array<{
    editedFile: string
    error: FancyError
    errorInfo?: React.ErrorInfo
  }> = []
  const uiFilePath: UiJsxCanvasProps['uiFilePath'] = 'test.js'
  const requireFn: UiJsxCanvasProps['requireFn'] = dumbRequireFn
  const fileBlobs: UiJsxCanvasProps['fileBlobs'] = {}
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
    parsedCode,
  )
  const topLevelElements: Array<TopLevelElement> = foldParsedTextFile(
    (_) => [],
    (success) => success.topLevelElements,
    (_) => [],
    parsedCode,
  )
  const jsxFactoryFunction = foldParsedTextFile(
    (_) => null,
    (success) => success.jsxFactoryFunction,
    (_) => null,
    parsedCode,
  )
  let canvasProps: UiJsxCanvasPropsWithErrorCallback
  let consoleLogs: Array<ConsoleLog> = []

  const combinedTopLevelArbitraryBlock: ArbitraryJSBlock | null = foldParsedTextFile(
    (_) => null,
    (success) => success.combinedTopLevelArbitraryBlock,
    (_) => null,
    parsedCode,
  )

  function clearConsoleLogs(): void {
    consoleLogs = []
  }
  function addToConsoleLogs(log: ConsoleLog): void {
    consoleLogs.push(log)
  }
  if (possibleProps == null) {
    canvasProps = {
      uiFileCode: code,
      uiFilePath: uiFilePath,
      requireFn: requireFn,
      fileBlobs: fileBlobs,
      onDomReport: Utils.NO_OP,
      clearErrors: clearErrors,
      offset: canvasPoint({ x: 0, y: 0 }),
      scale: 1,
      hiddenInstances: [],
      editedTextElement: null,
      mountCount: 0,
      walkDOM: false,
      imports: imports,
      topLevelElementsIncludingScenes: topLevelElements,
      jsxFactoryFunction: jsxFactoryFunction,
      canvasIsLive: false,
      shouldIncludeCanvasRootInTheSpy: false,
      clearConsoleLogs: clearConsoleLogs,
      addToConsoleLogs: addToConsoleLogs,
      linkTags: '',
      combinedTopLevelArbitraryBlock: combinedTopLevelArbitraryBlock,
    }
  } else {
    canvasProps = {
      ...possibleProps,
      uiFileCode: code,
      uiFilePath: uiFilePath,
      requireFn: requireFn,
      fileBlobs: fileBlobs,
      onDomReport: Utils.NO_OP,
      clearErrors: clearErrors,
      walkDOM: false,
      imports: imports,
      topLevelElementsIncludingScenes: topLevelElements,
      jsxFactoryFunction: jsxFactoryFunction,
      canvasIsLive: false,
      shouldIncludeCanvasRootInTheSpy: false,
      clearConsoleLogs: clearConsoleLogs,
      addToConsoleLogs: addToConsoleLogs,
      linkTags: '',
      combinedTopLevelArbitraryBlock: combinedTopLevelArbitraryBlock,
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
      <UiJsxCanvasContext.Provider value={spyCollector}>
        <CanvasErrorBoundary
          fileCode={code}
          filePath={uiFilePath}
          reportError={reportError}
          requireFn={canvasProps.requireFn}
        >
          <UiJsxCanvas {...canvasProps} />
        </CanvasErrorBoundary>
      </UiJsxCanvasContext.Provider>,
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
      <UiJsxCanvasContext.Provider value={emptyUiJsxCanvasContextData()}>
        <UiJsxCanvas {...canvasPropsSpyDisabled} />
      </UiJsxCanvasContext.Provider>,
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
  const {
    formattedSpyEnabled,
    formattedSpyDisabled,
    errorsReportedSpyEnabled,
    errorsReportedSpyDisabled,
    spyValues,
  } = renderCanvasReturnResultAndError(possibleProps, code)
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
  const {
    formattedSpyEnabled,
    formattedSpyDisabled,
    errorsReportedSpyEnabled,
    errorsReportedSpyDisabled,
    spyValues,
  } = renderCanvasReturnResultAndError(possibleProps, code)
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
  const { errorsReportedSpyEnabled, errorsReportedSpyDisabled } = renderCanvasReturnResultAndError(
    possibleProps,
    code,
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
