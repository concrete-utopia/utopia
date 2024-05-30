import type {
  JSExpressionOtherJavaScript,
  ArbitraryJSBlock,
  JSXMapExpression,
} from './element-template'
import type { MapLike } from 'typescript'
import { SafeFunctionCurriedErrorHandler } from './code-exec-utils'

type JavaScriptContainer = JSExpressionOtherJavaScript | ArbitraryJSBlock

export type GetOrUpdateFunctionCache = (
  javascript: JavaScriptContainer,
) => (...args: Array<unknown>) => unknown

let functionCache: { [uniqueID: string]: (...args: Array<any>) => any } = {}

export function resetFunctionCache(): void {
  functionCache = {}
}

export function resolveParamsAndRunJsCode(
  filePath: string,
  javascriptBlock: JavaScriptContainer,
  requireResult: MapLike<any>,
  currentScope: MapLike<any>,
): any {
  const definedElsewhereInfo = resolveDefinedElsewhere(
    javascriptBlock.definedElsewhere,
    requireResult,
    currentScope,
  )
  const updatedBlock = {
    ...javascriptBlock,
    definedElsewhere: Object.keys(definedElsewhereInfo),
  }
  // NOTE: If the external dependencies of this block of code aren't present when this is first called,
  // we'll cache the block without those keys. This _shouldn't_ be an issue since we replace the unique ID
  // on a new parse, but it means we have to be careful of this when reusing js blocks in tests
  //
  // The reason for us filtering the `definedElsewhere` here is so that we can throw a ReferenceError when
  // actually executing the JS code, rather than an error that would confuse the user
  const result = getOrUpdateFunctionCache(filePath, updatedBlock, requireResult, (e) => {
    throw e
  })(currentScope['callerThis'], ...Object.values(definedElsewhereInfo))
  return result
}

function getOrUpdateFunctionCache(
  filePath: string,
  javascript: JavaScriptContainer,
  requireResult: MapLike<any>,
  handleError: (error: Error) => void,
): (...args: Array<unknown>) => unknown {
  const uidPart = javascript.uid
  const definedElsewherePart = javascript.definedElsewhere.join('_')
  const elementsWithinPart = Object.keys(javascript.elementsWithin).join('_')
  const codePart =
    javascript.type === 'ARBITRARY_JS_BLOCK' ? javascript.javascript : javascript.javascriptWithUIDs
  const cacheKey = `uid${uidPart}_de${definedElsewherePart}_ew${elementsWithinPart}_code${codePart}`
  const fromCache = functionCache[cacheKey]
  if (fromCache == null) {
    const newCachedFunction = SafeFunctionCurriedErrorHandler(
      false,
      requireResult,
      filePath,
      javascript.transpiledJavascript,
      javascript.sourceMap,
      javascript.definedElsewhere,
    )
    functionCache[cacheKey] = newCachedFunction
    return newCachedFunction(handleError)
  } else {
    return fromCache(handleError)
  }
}

function resolveDefinedElsewhere(
  definedElsewhere: Array<string>,
  requireResult: MapLike<any>,
  scope: MapLike<any>,
): { [name: string]: any } {
  let definedElsewhereInfo: { [name: string]: any } = {}

  for (const elsewhere of definedElsewhere) {
    if (scope.hasOwnProperty(elsewhere)) {
      definedElsewhereInfo[elsewhere] = scope[elsewhere]
      continue
    }

    if (requireResult.hasOwnProperty(elsewhere)) {
      definedElsewhereInfo[elsewhere] = requireResult[elsewhere]
      continue
    }

    if (elsewhere === 'console') {
      definedElsewhereInfo[elsewhere] = console
      continue
    }

    if ((global as any).hasOwnProperty(elsewhere) as boolean) {
      definedElsewhereInfo[elsewhere] = (global as any)[elsewhere]
      continue
    }
  }

  return definedElsewhereInfo
}
