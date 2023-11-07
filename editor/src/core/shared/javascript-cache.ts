import type {
  JSExpressionOtherJavaScript,
  ArbitraryJSBlock,
  JSXMapExpression,
} from './element-template'
import type { MapLike } from 'typescript'
import { SafeFunctionCurriedErrorHandler } from './code-exec-utils'
import React from 'react'
import { atom } from 'jotai'
import type { ElementPath } from './project-file-types'
import * as EP from './element-path'
import invariant from '../../third-party/remix/invariant'

type JavaScriptContainer = JSExpressionOtherJavaScript | ArbitraryJSBlock | JSXMapExpression

export type GetOrUpdateFunctionCache = (
  javascript: JavaScriptContainer,
) => (...args: Array<unknown>) => unknown

let functionCache: { [uniqueID: string]: (...args: Array<any>) => any } = {}

export function resetFunctionCache(): void {
  functionCache = {}
}

type PropsData = Record<string, any>
type HookData = Record<string, any>

interface ComponentStateData {
  props: PropsData
  hooks: HookData
}

export interface ComponentStateDataMap {
  [pathString: string]: ComponentStateData
}

export const ComponentStateDataAtom = atom<ComponentStateDataMap>({})
type ComponentStateRecordingMode =
  | { type: 'recording' }
  | { type: 'pinned'; componentStateDataMap: ComponentStateDataMap }

export const ComponentStateRecordingModeAtom = atom<ComponentStateRecordingMode>({
  type: 'recording',
})

export type HookResultFunction = (hookId: string, result: any) => any

export const updateComponentStateData =
  (componentStateData: ComponentStateDataMap, elementPath: ElementPath): HookResultFunction =>
  (hookId, value) => {
    const elementPathString = EP.toString(elementPath)
    const entryForThisPath = componentStateData[elementPathString] ?? { props: {}, hooks: {} }
    const updatedEntries = {
      ...componentStateData,
      [elementPathString]: {
        props: entryForThisPath.props,
        hooks: { ...entryForThisPath.hooks, [hookId]: value },
      },
    }
    return updatedEntries
  }

export const getFromComponentStateData =
  (componentStateData: ComponentStateDataMap, elementPath: ElementPath): HookResultFunction =>
  (hookId) => {
    const elementPathString = EP.toString(elementPath)
    const dataForElement = componentStateData[elementPathString] ?? null
    invariant(
      dataForElement,
      `No data provided for element at path ${elementPathString} in componentStateData`,
    )
    if (!(hookId in dataForElement)) {
      throw new Error(`No data provided for hook with id ${hookId} in componentStateData`)
    }
    return dataForElement.hooks[hookId]
  }

type Callable<Args extends any[] = any[], ReturnType = any> = (...args: Args) => ReturnType

export function resolveParamsAndRunJsCode(
  filePath: string,
  javascriptBlock: JavaScriptContainer,
  requireResult: MapLike<any>,
  currentScope: MapLike<any>,
  setHookResult: HookResultFunction,
): any {
  let hookCounter = 0
  const hookOverride =
    <Hook extends Callable, StoredType>(
      name: string,
      originalHook: Hook,
      picker: (_: ReturnType<Hook>) => StoredType,
    ) =>
    (...args: unknown[]) => {
      hookCounter += 1
      const hookId = `${name}-${hookCounter}`
      const useStateResult = originalHook(args)
      const valueFromRecording = setHookResult(hookId, picker(useStateResult))

      return valueFromRecording
    }

  const useStateOverridden = hookOverride('useState', React.useState, (a) => a[0])

  const MonkeyReact = {
    ...React,
    useState: useStateOverridden,
  }

  const definedElsewhereInfo = resolveDefinedElsewhere(
    javascriptBlock.definedElsewhere,
    requireResult,
    currentScope,
  )
  definedElsewhereInfo['React'] = MonkeyReact
  definedElsewhereInfo['useState'] = useStateOverridden

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
  const codePart = javascript.javascript
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

    if (elsewhere === 'window') {
      // By spreading the `window.location` we can prevent redirection via e.g. `window.location.href = '...'`
      definedElsewhereInfo[elsewhere] = {
        ...window,
        location: {
          ...window.location,
        },
      }
      continue
    }

    if ((global as any).hasOwnProperty(elsewhere) as boolean) {
      definedElsewhereInfo[elsewhere] = (global as any)[elsewhere]
      continue
    }
  }

  return definedElsewhereInfo
}
