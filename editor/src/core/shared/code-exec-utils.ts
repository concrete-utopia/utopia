import { strToBase64 } from '@root/encoding/base64'
import StackFrame, { ScriptLine } from '../../third-party/react-error-overlay/utils/stack-frame'
import { getSourceMapConsumer } from '../../third-party/react-error-overlay/utils/getSourceMap'
import {
  unmapBabelTranspiledCode,
  unmapUtopiaSafeFunction,
} from '../../third-party/react-error-overlay/utils/mapper'
import type { RawSourceMap } from '../workers/ts/ts-typings/RawSourceMap'
import { NO_OP } from './utils'
import { findLastIndex, take } from './array-utils'
import parseError from '../../third-party/react-error-overlay/utils/parser'

// eslint-disable-next-line @typescript-eslint/no-empty-function
export const AsyncFunction = Object.getPrototypeOf(async function () {}).constructor

let CurrentEvaluatedFileName = 'unknown'

export function setGlobalEvaluatedFileName(fileName: string) {
  CurrentEvaluatedFileName = fileName
  EvaluatedFiles.push(fileName)
}

export function getGlobalEvaluatedFileName(): string {
  return CurrentEvaluatedFileName
}

let EvaluatedFiles: Array<string> = []

export function clearListOfEvaluatedFiles() {
  EvaluatedFiles = []
}

export function getListOfEvaluatedFiles(): Array<string> {
  return [...EvaluatedFiles]
}

export interface FancyError extends Error {
  stackFrames?: StackFrame[]
}

export interface RuntimeErrorInfo {
  editedFile: string
  error: FancyError
  errorInfo: React.ErrorInfo | null
}

export type ErrorHandler = (e: Error) => void

const UTOPIA_FUNCTION_ROOT_NAME = 'SafeFunctionCurriedErrorHandler'

export const SOURCE_MAP_SEPARATOR = `;##sourceMap##`

export type SourceMapCache = { [key: string]: RawSourceMap }

export function processErrorWithSourceMap(
  fallbackCode: string | null,
  filePath: string,
  error: Error | FancyError,
  inSafeFunction: boolean,
): FancyError {
  if ((error as FancyError)?.stackFrames != null) {
    // this Error is already processed!
    return error
  }
  const sourceMapCache: SourceMapCache = {}
  const errorStack = error.stack
  if (errorStack != null) {
    try {
      const splitErrorStack = errorStack.split('\n')
      const evalEntryPoint = inSafeFunction
        ? findLastIndex((e) => e.indexOf(UTOPIA_FUNCTION_ROOT_NAME) > -1, splitErrorStack)
        : 1
      const strippedErrorStack = take(evalEntryPoint + 1, splitErrorStack)
      error.stack = strippedErrorStack
        .map((stackLine) =>
          inSafeFunction ? stackLine.replace(UTOPIA_FUNCTION_ROOT_NAME, 'eval') : stackLine,
        )
        .join('\n')
      const parsedStackFrames = parseError(error, sourceMapCache)

      const enhancedStackFrames = enhanceStackFrames(
        fallbackCode,
        filePath,
        parsedStackFrames,
        inSafeFunction,
        sourceMapCache,
      )
      ;(error as FancyError).stackFrames = enhancedStackFrames
    } catch (sourceMapError) {
      console.error('Source map handling threw an error.', sourceMapError)
    }
  }
  return error
}

export function enhanceStackFrames(
  fallbackCode: string | null,
  filePath: string,
  parsedStackFrames: Array<StackFrame>,
  inSafeFunction: boolean,
  sourceMaps: SourceMapCache,
): StackFrame[] {
  return parsedStackFrames.map((frame) => {
    const rawSourceMap = extractRawSourceMap(frame, sourceMaps)
    const scriptLinesFromSourceMap = parseSourceCodeFromRawSourceMap(rawSourceMap)
    let scriptLines: Array<ScriptLine> | null = null
    if (scriptLinesFromSourceMap == null) {
      if (fallbackCode != null) {
        const fallbackCodeLines = fallbackCode.split('\n')
        scriptLines = fallbackCodeLines.map(
          (sourceLine, index) => new ScriptLine(index + 1, sourceLine, false),
        )
      }
    } else {
      scriptLines = scriptLinesFromSourceMap
    }
    let fixedFilename: string | undefined = frame.fileName?.split(SOURCE_MAP_SEPARATOR)[0]
    if (fixedFilename === 'eval(undefined)') {
      fixedFilename = filePath
    }
    const stackFrame = new StackFrame(
      frame.functionName,
      fixedFilename,
      frame.lineNumber,
      frame.columnNumber,
      scriptLines,
      frame.functionName,
      fixedFilename,
      frame.lineNumber,
      frame.columnNumber,
      scriptLines,
    )

    const stackFrameWithoutSafeFn = inSafeFunction
      ? unmapUtopiaSafeFunction(stackFrame)
      : stackFrame

    return maybeEnhanceStackFrame(stackFrameWithoutSafeFn, rawSourceMap)
  })
}

function maybeEnhanceStackFrame(
  stackFrame: StackFrame,
  rawSourceMap: RawSourceMap | null,
): StackFrame {
  if (rawSourceMap == null) {
    return stackFrame
  } else {
    const sourceMap = getSourceMapConsumer(rawSourceMap as any)
    return unmapBabelTranspiledCode(stackFrame, sourceMap)
  }
}

function extractRawSourceMap(
  stackFrame: StackFrame,
  sourceMaps: SourceMapCache,
): RawSourceMap | null {
  const possibleSourceMapKey = stackFrame.fileName?.split(SOURCE_MAP_SEPARATOR)[1]
  if (possibleSourceMapKey == null) {
    return null
  }
  return sourceMaps[possibleSourceMapKey] ?? null
}

function parseSourceCodeFromRawSourceMap(rawSourceMap: RawSourceMap | null): ScriptLine[] | null {
  const sourceCode = rawSourceMap?.transpiledContentUtopia
  const fixedSourceCode = sourceCode?.split('\n')
  if (fixedSourceCode == null) {
    return null
  } else {
    return fixedSourceCode.map((sourceLine, index) => new ScriptLine(index + 1, sourceLine, false))
  }
}

function processErrorAndCallHandler(
  code: string,
  filePath: string,
  onError: ErrorHandler,
  error: Error,
  inSafeFunction: boolean,
): void {
  const fancyError = processErrorWithSourceMap(code, filePath, error, inSafeFunction)
  onError(fancyError)
}

export const SafeFunctionCurriedErrorHandler = {
  [UTOPIA_FUNCTION_ROOT_NAME]: function (
    async: boolean,
    cacheableContext: any,
    filePath: string,
    code: string,
    sourceMapWithoutTranspiledCode: RawSourceMap | null,
    extraParamKeys: Array<string> = [],
  ): (onError: ErrorHandler) => (...params: Array<unknown>) => unknown {
    const [contextKeys, contextValues] = Object.keys(cacheableContext).reduce(
      (working, key) => {
        working[0].push(key)
        working[1].push(cacheableContext[key])
        return working
      },
      [[] as string[], [] as any[]],
    )
    const sourceMap: RawSourceMap | null =
      sourceMapWithoutTranspiledCode != null
        ? { ...sourceMapWithoutTranspiledCode, transpiledContentUtopia: code }
        : null

    let sourceMapBase64 = strToBase64(JSON.stringify(sourceMap))

    const sourceFile = sourceMap?.sources?.[0]
    const fileName = `${UTOPIA_FUNCTION_ROOT_NAME}(${sourceFile})`

    const codeWithSourceMapAttached = `${code}

    //# sourceURL=${fileName}${SOURCE_MAP_SEPARATOR}${sourceMapBase64}${SOURCE_MAP_SEPARATOR}
    `

    const FunctionOrAsyncFunction = async ? AsyncFunction : Function
    const fn = new FunctionOrAsyncFunction(
      ...contextKeys.concat(extraParamKeys),
      codeWithSourceMapAttached,
    )
    fn.displayName = UTOPIA_FUNCTION_ROOT_NAME
    const safeFn =
      (onError: ErrorHandler) =>
      (...params: Array<unknown>) => {
        try {
          setGlobalEvaluatedFileName(sourceFile ?? 'unknown')
          const [boundThis, ...otherParams] = params
          return fn.bind(boundThis)(...contextValues, ...otherParams)
        } catch (e: any) {
          processErrorAndCallHandler(code, filePath, onError, e, true)
        }
      }
    return safeFn
  },
}[UTOPIA_FUNCTION_ROOT_NAME]

export function safeFunction(
  async: boolean,
  cacheableContext: any,
  filePath: string,
  code: string,
  sourceMapWithoutTranspiledCode: RawSourceMap | null,
  extraParamKeys: Array<string>,
  onError: ErrorHandler,
): (...params: Array<any>) => any {
  try {
    return SafeFunctionCurriedErrorHandler(
      async,
      cacheableContext,
      filePath,
      code,
      sourceMapWithoutTranspiledCode,
      extraParamKeys,
    )(onError)
  } catch (e: any) {
    processErrorAndCallHandler(code, filePath, onError, e, true)
    return NO_OP
  }
}
