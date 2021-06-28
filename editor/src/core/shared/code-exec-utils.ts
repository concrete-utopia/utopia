import { strToBase64, base64ToStr } from '@root/encoding/base64'
import StackFrame, { ScriptLine } from '../../third-party/react-error-overlay/utils/stack-frame'
import { getSourceMapConsumer } from '../../third-party/react-error-overlay/utils/getSourceMap'
import {
  unmapBabelTranspiledCode,
  unmapUtopiaSafeFunction,
} from '../../third-party/react-error-overlay/utils/mapper'
import { RawSourceMap } from '../workers/ts/ts-typings/RawSourceMap'
import { NO_OP } from './utils'
import { findLastIndex, last, take } from './array-utils'
import parseError from '../../third-party/react-error-overlay/utils/parser'

// eslint-disable-next-line @typescript-eslint/no-empty-function
export const AsyncFunction = Object.getPrototypeOf(async function () {}).constructor

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

export const SOURCE_MAP_PREFIX = `;sourceMap=`

export function processErrorWithSourceMap(
  error: Error | FancyError,
  inSafeFunction: boolean,
): FancyError {
  if ((error as FancyError)?.stackFrames != null) {
    // this Error is already processed!
    return error
  }
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
      const parsedStackFrames = parseError(error)

      const enhancedStackFrames = enhanceStackFrames(parsedStackFrames, inSafeFunction)
      ;(error as FancyError).stackFrames = enhancedStackFrames
    } catch (sourceMapError) {
      console.error('Source map handling threw an error.', sourceMapError)
    }
  }
  return error
}

export function enhanceStackFrames(
  parsedStackFrames: Array<StackFrame>,
  inSafeFunction: boolean,
): StackFrame[] {
  return parsedStackFrames.map((frame) => {
    const rawSourceMap = extractRawSourceMap(frame)
    const scriptLines = parseSourceCodeFromRawSourceMap(rawSourceMap)
    const fixedFilename = frame.fileName?.split(SOURCE_MAP_PREFIX)[0]
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

function extractRawSourceMap(stackFrame: StackFrame): RawSourceMap | null {
  const possibleSourceMapSegment = stackFrame.fileName?.split(SOURCE_MAP_PREFIX)[1] ?? null
  return possibleSourceMapSegment == null ? null : JSON.parse(base64ToStr(possibleSourceMapSegment))
}

function parseSourceCodeFromRawSourceMap(rawSourceMap: RawSourceMap | null): ScriptLine[] {
  const sourceCode = rawSourceMap?.transpiledContentUtopia
  const fixedSourceCode = sourceCode?.split('\n') ?? []
  return fixedSourceCode.map((sourceLine, index) => new ScriptLine(index + 1, sourceLine, false))
}

function processErrorAndCallHandler(
  onError: ErrorHandler,
  error: Error,
  inSafeFunction: boolean,
): void {
  const fancyError = processErrorWithSourceMap(error, inSafeFunction)
  onError(fancyError)
}

export const SafeFunctionCurriedErrorHandler = {
  [UTOPIA_FUNCTION_ROOT_NAME]: function (
    async: boolean,
    cacheableContext: any,
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

    const fileName = `${UTOPIA_FUNCTION_ROOT_NAME}(${sourceMap?.sources?.[0]})`

    const codeWithSourceMapAttached = `${code}

    //# sourceURL=${fileName}${SOURCE_MAP_PREFIX}${sourceMapBase64}
    `

    const FunctionOrAsyncFunction = async ? AsyncFunction : Function
    const fn = new FunctionOrAsyncFunction(
      ...contextKeys.concat(extraParamKeys),
      codeWithSourceMapAttached,
    )
    fn.displayName = UTOPIA_FUNCTION_ROOT_NAME
    const safeFn = (onError: ErrorHandler) => (...params: Array<unknown>) => {
      try {
        const [boundThis, ...otherParams] = params
        return fn.bind(boundThis)(...contextValues, ...otherParams)
      } catch (e) {
        processErrorAndCallHandler(onError, e, true)
      }
    }
    return safeFn
  },
}[UTOPIA_FUNCTION_ROOT_NAME]

export function SafeFunction(
  async: boolean,
  cacheableContext: any,
  code: string,
  sourceMapWithoutTranspiledCode: RawSourceMap | null,
  extraParamKeys: Array<string>,
  onError: ErrorHandler,
): (...params: Array<any>) => any {
  try {
    return SafeFunctionCurriedErrorHandler(
      async,
      cacheableContext,
      code,
      sourceMapWithoutTranspiledCode,
      extraParamKeys,
    )(onError)
  } catch (e) {
    processErrorAndCallHandler(onError, e, true)
    return NO_OP
  }
}
