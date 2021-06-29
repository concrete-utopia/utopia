import {
  JSXAttributeOtherJavaScript,
  ArbitraryJSBlock,
  JSXArbitraryBlock,
} from './element-template'
import * as React from 'react'
import { MapLike } from 'typescript'
import { SafeFunctionCurriedErrorHandler } from './code-exec-utils'
import { ElementPath } from './project-file-types'
import * as EP from '../../core/shared/element-path'
import { UiJsxCanvasContextData } from '../../components/canvas/ui-jsx-canvas'
import { dropLast } from './array-utils'

type JavaScriptContainer = JSXAttributeOtherJavaScript | ArbitraryJSBlock | JSXArbitraryBlock

export type GetOrUpdateFunctionCache = (
  javascript: JavaScriptContainer,
) => (...args: Array<unknown>) => unknown

let functionCache: { [uniqueID: string]: (...args: Array<any>) => any } = {}

export function resetFunctionCache(): void {
  functionCache = {}
}

export function resolveParamsAndRunJsCode(
  javascriptBlock: JavaScriptContainer,
  requireResult: MapLike<any>,
  currentScope: MapLike<any>,
  metadataContext?: UiJsxCanvasContextData,
  elementPath?: ElementPath,
): any {
  let hookCounter = 0
  const MonkeyReact = {
    ...React,
    useState: (initialState: any) => {
      hookCounter += 1
      const hookId = `useState${hookCounter}`
      let stateToUse = initialState
      if (elementPath != null && metadataContext != null) {
        const componentPath = { ...elementPath, parts: dropLast(elementPath.parts) }
        if (metadataContext.current.componentStateValues[EP.toString(componentPath)] == null) {
          metadataContext.current.componentStateValues[EP.toString(componentPath)] = {}
        }
        if (
          metadataContext.current.componentStateValues[EP.toString(componentPath)][hookId] == null
        ) {
          metadataContext.current.componentStateValues[EP.toString(componentPath)][
            hookId
          ] = initialState
        } else {
          stateToUse =
            metadataContext.current.componentStateValues[EP.toString(componentPath)][hookId]
        }
      }
      return React.useState(stateToUse)
    },
  }
  const definedElsewhereInfo = resolveDefinedElsewhere(
    javascriptBlock.definedElsewhere,
    requireResult,
    currentScope,
  )
  // console.log('definedElsewhereInfo', definedElsewhereInfo, componentPath, javascriptBlock)
  definedElsewhereInfo['React'] = MonkeyReact
  const updatedBlock = {
    ...javascriptBlock,
    definedElsewhere: Object.keys(definedElsewhereInfo),
  }

  // const original = React.useState
  // if (componentPath != null) {
  //   ;(React as any)['useState'] = MonkeyReact.useState
  //   ;(React as any)['acica'] = true
  // }

  // NOTE: If the external dependencies of this block of code aren't present when this is first called,
  // we'll cache the block without those keys. This _shouldn't_ be an issue since we replace the unique ID
  // on a new parse, but it means we have to be careful of this when reusing js blocks in tests
  //
  // The reason for us filtering the `definedElsewhere` here is so that we can throw a ReferenceError when
  // actually executing the JS code, rather than an error that would confuse the user
  const result = getOrUpdateFunctionCache(updatedBlock, requireResult, (e) => {
    throw e
  })(currentScope['callerThis'], ...Object.values(definedElsewhereInfo))
  // if (componentPath != null) {
  //   ;(React as any)['useState'] = original
  // }
  return result
}

function getOrUpdateFunctionCache(
  javascript: JavaScriptContainer,
  requireResult: MapLike<any>,
  handleError: (error: Error) => void,
): (...args: Array<unknown>) => unknown {
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
    const glob: any = global as any
    let possibleValue = glob[elsewhere]
    if (possibleValue != undefined || (glob.hasOwnProperty(elsewhere) as boolean)) {
      definedElsewhereInfo[elsewhere] = possibleValue
    }

    if (elsewhere === 'console') {
      definedElsewhereInfo[elsewhere] = console
    }

    possibleValue = requireResult[elsewhere]
    if (possibleValue != undefined || requireResult.hasOwnProperty(elsewhere)) {
      definedElsewhereInfo[elsewhere] = possibleValue
    }

    possibleValue = scope[elsewhere]
    if (possibleValue != undefined || scope.hasOwnProperty(elsewhere)) {
      definedElsewhereInfo[elsewhere] = possibleValue
    }
  })

  return definedElsewhereInfo
}
