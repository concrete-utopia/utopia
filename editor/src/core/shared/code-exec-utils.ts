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

export function processErrorWithSourceMap(
  onError: ErrorHandler,
  error: Error,
  sourceCode: string,
  rawSourceMap: RawSourceMap | null,
  inSafeFunction: boolean,
): void {
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
      const fixedSourceCode = sourceCode.split('\n')
      const stackFrames = parseUtopiaError(error, fixedSourceCode)
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
  onError(error)
}

export const SafeFunctionCurriedErrorHandler = {
  [UTOPIA_FUNCTION_ROOT_NAME]: function (
    async: boolean,
    cacheableContext: any,
    code: string,
    sourceMap: RawSourceMap | null,
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

    let objJsonStr = JSON.stringify(sourceMap)
    let objJsonB64 = Buffer.from(objJsonStr).toString('base64')

    const fileName = `${UTOPIA_FUNCTION_ROOT_NAME}-${sourceMap?.sources?.[0]}`

    const codeWithSourceMapAttached = `${code}

    //# sourceURL=${fileName};sourceMap=${objJsonB64}
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
        processErrorWithSourceMap(onError, e, codeWithSourceMapAttached, sourceMap, true)
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
    processErrorWithSourceMap(onError, e, code, null, true)
    return NO_OP
  }
}
