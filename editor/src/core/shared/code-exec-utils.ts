import { strToBase64, base64ToStr } from '@root/encoding/base64'
import StackFrame from '../../third-party/react-error-overlay/utils/stack-frame'
import { parseUtopiaError } from '../../third-party/react-error-overlay/utils/parseUtopiaError'
import { getSourceMapConsumer } from '../../third-party/react-error-overlay/utils/getSourceMap'
import {
  unmapBabelTranspiledCode,
  unmapUtopiaSafeFunction,
} from '../../third-party/react-error-overlay/utils/mapper'
import { RawSourceMap } from '../workers/ts/ts-typings/RawSourceMap'
import { NO_OP } from './utils'
import { take } from './array-utils'
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
        ? splitErrorStack.findIndex((e) => e.indexOf(UTOPIA_FUNCTION_ROOT_NAME) > -1)
        : 1
      const strippedErrorStack = take(evalEntryPoint + 1, splitErrorStack)
      error.stack = strippedErrorStack
        .map((stackLine) =>
          inSafeFunction ? stackLine.replace(UTOPIA_FUNCTION_ROOT_NAME, 'eval') : stackLine,
        )
        .join('\n')
      const parsedStackFrames = parseError(error)

      // TODO turn ;sourceMap= into const
      const rawSourceMap: RawSourceMap | null = JSON.parse(
        base64ToStr(parsedStackFrames[0].fileName?.split(SOURCE_MAP_PREFIX)[1] ?? ''),
      )
      const sourceCode = rawSourceMap?.transpiledContentUtopia

      const fixedSourceCode = sourceCode?.split('\n') ?? []
      const stackFrames = parseUtopiaError(parsedStackFrames, fixedSourceCode)
      const stackFramesWithoutSafeFn = inSafeFunction
        ? unmapUtopiaSafeFunction(stackFrames)
        : stackFrames

      if (rawSourceMap == null) {
        ;(error as FancyError).stackFrames = stackFramesWithoutSafeFn
      } else {
        const sourceMap = getSourceMapConsumer(rawSourceMap as any)
        const enhancedStackFrames = unmapBabelTranspiledCode(stackFramesWithoutSafeFn, sourceMap)
        ;(error as FancyError).stackFrames = enhancedStackFrames
      }
    } catch (sourceMapError) {
      console.error('Source map handling threw an error.', sourceMapError)
    }
  }
  return error
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
  extraParamKeys: Array<string>,
  onError: ErrorHandler,
): (...params: Array<any>) => any {
  try {
    return SafeFunctionCurriedErrorHandler(
      async,
      cacheableContext,
      code,
      null,
      extraParamKeys,
    )(onError)
  } catch (e) {
    processErrorAndCallHandler(onError, e, true)
    return NO_OP
  }
}
