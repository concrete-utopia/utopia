import {
  JSXAttributeOtherJavaScript,
  ArbitraryJSBlock,
  JSXArbitraryBlock,
} from './element-template'
import { MapLike } from 'typescript'
import { SafeFunctionCurriedErrorHandler } from './code-exec-utils'

type JavaScriptContainer = JSXAttributeOtherJavaScript | ArbitraryJSBlock | JSXArbitraryBlock

export type GetOrUpdateFunctionCache = (
  javascript: JavaScriptContainer,
) => (...args: Array<any>) => any

let functionCache: { [uniqueID: string]: (...args: Array<any>) => any } = {}

export function resetFunctionCache() {
  functionCache = {}
}

export function resolveParamsAndRunJsCode(
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
  const result = getOrUpdateFunctionCache(updatedBlock, requireResult, (e) => {
    throw e
  })(...Object.values(definedElsewhereInfo))
  return result
}

function getOrUpdateFunctionCache(
  javascript: JavaScriptContainer,
  requireResult: MapLike<any>,
  handleError: (error: Error) => void,
): (...args: Array<any>) => any {
  const fromCache = functionCache[javascript.uniqueID]
  if (fromCache == null) {
    const newCachedFunction = SafeFunctionCurriedErrorHandler(
      false,
      requireResult,
      javascript.transpiledJavascript,
      javascript.sourceMap,
      javascript.definedElsewhere,
    )
    functionCache[javascript.uniqueID] = newCachedFunction
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

  definedElsewhere.forEach((elsewhere) => {
    let possibleValue = scope[elsewhere]
    if (possibleValue != undefined || scope.hasOwnProperty(elsewhere)) {
      definedElsewhereInfo[elsewhere] = possibleValue
    }

    possibleValue = requireResult[elsewhere]
    if (possibleValue != undefined || requireResult.hasOwnProperty(elsewhere)) {
      definedElsewhereInfo[elsewhere] = possibleValue
    }

    if (elsewhere === 'console') {
      definedElsewhereInfo[elsewhere] = console
    }

    const glob: any = global as any
    possibleValue = glob[elsewhere]
    if (possibleValue != undefined || glob.hasOwnProperty(elsewhere)) {
      definedElsewhereInfo[elsewhere] = possibleValue
    }
  })

  return definedElsewhereInfo
}
