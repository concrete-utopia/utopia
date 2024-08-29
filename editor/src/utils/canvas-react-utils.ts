import React from 'react'
import { omitWithPredicate } from '../core/shared/object-utils'
import type { MapLike } from 'typescript'
import { firstLetterIsLowerCase } from '../core/shared/string-utils'
import { isIntrinsicHTMLElementString } from '../core/shared/element-template'
import { UtopiaKeys, UTOPIA_UID_KEY, UTOPIA_PATH_KEY } from '../core/model/utopia-constants'
import { v4 } from 'uuid'
import { isFeatureEnabled } from './feature-switches'
import { PERFORMANCE_MARKS_ALLOWED } from '../common/env-vars'
import { memoize } from '../core/shared/memoize'

const realCreateElement = React.createElement

const fragmentSymbol = Symbol.for('react.fragment')
const providerSymbol = Symbol.for('react.provider')
const contextSymbol = Symbol.for('react.context')

let uidMonkeyPatchApplied: boolean = false

export function applyUIDMonkeyPatch(): void {
  if (!uidMonkeyPatchApplied) {
    uidMonkeyPatchApplied = true
    ;(React as any).createElement = patchedCreateReactElement
    ;(React as any).monkeyPatched = true
  }
}

export function getDisplayName(type: any): string {
  // taken from https://github.com/facebook/react/blob/7e405d458d6481fb1c04dfca6afab0651e6f67cd/packages/react/src/ReactElement.js#L415
  if (type == null) {
    return 'null'
  } else if (typeof type === 'function') {
    return type.displayName ?? type.name ?? 'Unknown'
  } else if (typeof type === 'symbol') {
    return type.toString()
  } else if (typeof type === 'string') {
    return type
  } else if (typeof type === 'object') {
    // Typically React context, memo, or forwardRef
    const wrapper = type.$$typeof == null ? 'Unknown object' : getDisplayName(type.$$typeof)
    const wrappedType = type.type ?? type.render
    const wrapped = wrappedType == null ? '' : `(${getDisplayName(wrappedType)})`

    return `${wrapper}${wrapped}`
  } else {
    return 'Unknown'
  }
}

function isFragment(type: any): boolean {
  return type === React.Fragment || type.$$typeof === fragmentSymbol
}

function fragmentOrProviderOrContext(type: any): boolean {
  return isFragment(type) || type?.$$typeof == providerSymbol || type?.$$typeof == contextSymbol
}

function keyShouldBeExcluded(key: string): boolean {
  return UtopiaKeys.includes(key)
}

export function filterDataProps(props: MapLike<any>): MapLike<any> {
  return omitWithPredicate(props, (key) => typeof key === 'string' && keyShouldBeExcluded(key))
}

export function makeCanvasElementPropsSafe(props: any): any {
  return {
    skipDeepFreeze: true,
    ...props,
  }
}

function shouldIncludeDataUID(type: any): boolean {
  if (typeof type === 'string') {
    const elementIsIntrinsic = firstLetterIsLowerCase(type)
    const elementIsBaseHTML = isIntrinsicHTMLElementString(type)
    // Looks like an intrinsic element (div/span/etc), but isn't a recognised
    // React intrinsic HTML element.
    if (elementIsIntrinsic && !elementIsBaseHTML) {
      return false
    }
  }
  return true
}

function attachDataUidToRoot(
  originalResponse: React.ReactElement | null | undefined,
  dataUid: string | null,
  path: string | null,
): React.ReactElement | null
function attachDataUidToRoot(
  originalResponse: Array<React.ReactElement | null>,
  dataUid: string | null,
  path: string | null,
): Array<React.ReactElement | null>
function attachDataUidToRoot(
  originalResponse: React.ReactElement | Array<React.ReactElement | null> | null | undefined,
  dataUid: string | null,
  path: string | null,
): React.ReactElement | Array<React.ReactElement | null> | null {
  if (originalResponse == null || dataUid == null) {
    return originalResponse as any
  } else if (Array.isArray(originalResponse)) {
    // the response was an array of elements
    return originalResponse.map((element) => attachDataUidToRoot(element, dataUid, path))
  } else if (!React.isValidElement(originalResponse as any)) {
    return originalResponse
  } else {
    if (shouldIncludeDataUID(originalResponse.type)) {
      return React.cloneElement(originalResponse, {
        [UTOPIA_UID_KEY]: originalResponse.props[UTOPIA_UID_KEY] ?? dataUid,
        [UTOPIA_PATH_KEY]: originalResponse.props[UTOPIA_PATH_KEY] ?? path,
      })
    } else {
      return originalResponse
    }
  }
}

const mangleFunctionType = memoize(
  (type: unknown): React.FunctionComponent<React.PropsWithChildren<unknown>> => {
    const mangledFunctionName = `UtopiaSpiedFunctionComponent(${getDisplayName(type)})`

    const mangledFunction = {
      [mangledFunctionName]: (p: any, context?: any) => {
        const MeasureRenderTimes =
          isFeatureEnabled('Debug – Performance Marks (Slow)') && PERFORMANCE_MARKS_ALLOWED
        const uuid = MeasureRenderTimes ? v4() : ''
        if (MeasureRenderTimes) {
          performance.mark(`render_start_${uuid}`)
        }
        let originalTypeResponse = (
          type as React.FunctionComponent<React.PropsWithChildren<unknown>>
        )(p, context)
        const res = attachDataUidToRoot(
          originalTypeResponse,
          (p as any)?.[UTOPIA_UID_KEY],
          (p as any)?.[UTOPIA_PATH_KEY],
        )
        if (MeasureRenderTimes) {
          performance.mark(`render_end_${uuid}`)
          performance.measure(
            `Render Component ${getDisplayName(type)}`,
            `render_start_${uuid}`,
            `render_end_${uuid}`,
          )
        }
        return res
      },
    }[mangledFunctionName]!
    ;(mangledFunction as any).theOriginalType = type
    ;(mangledFunction as any).contextTypes = (type as any).contextTypes
    ;(mangledFunction as any).childContextTypes = (type as any).childContextTypes
    ;(mangledFunction as any).displayName = `UtopiaSpiedFunctionComponent(${getDisplayName(type)})`
    return mangledFunction
  },
  {
    maxSize: 10000,
  },
)

const mangleClassType = memoize(
  (type: any) => {
    const originalRender = type.prototype.render
    // mutation
    type.prototype.render = function monkeyRender() {
      const MeasureRenderTimes =
        isFeatureEnabled('Debug – Performance Marks (Slow)') && PERFORMANCE_MARKS_ALLOWED
      const uuid = MeasureRenderTimes ? v4() : ''
      if (MeasureRenderTimes) {
        performance.mark(`render_start_${uuid}`)
      }
      let originalTypeResponse = originalRender.bind(this)()
      const res = attachDataUidToRoot(
        originalTypeResponse,
        (this.props as any)?.[UTOPIA_UID_KEY],
        (this.props as any)?.[UTOPIA_PATH_KEY],
      )
      if (MeasureRenderTimes) {
        performance.mark(`render_end_${uuid}`)
        performance.measure(
          `Render ComponentClass ${getDisplayName(type)}`,
          `render_start_${uuid}`,
          `render_end_${uuid}`,
        )
      }
      return res
    }
    ;(type as any).theOriginalType = type
    ;(type as any).displayName = `UtopiaSpiedClass(${getDisplayName(type)})`
    return type
  },
  {
    maxSize: 10000,
  },
)

const mangleExoticType = memoize(
  (
    type: React.ComponentType<React.PropsWithChildren<unknown>>,
  ): React.FunctionComponent<React.PropsWithChildren<unknown>> => {
    function updateChild(
      child: React.ReactElement | null,
      dataUid: string | null,
      path: string | null,
      fragmentParentProps: any,
    ) {
      if (child == null || !shouldIncludeDataUID(child.type)) {
        return child
      }
      const existingChildUID = child.props?.[UTOPIA_UID_KEY]
      const existingChildPath = child.props?.[UTOPIA_PATH_KEY]
      const childUID = existingChildUID ?? dataUid
      const mangledChildPath = existingChildPath ?? path
      if ((!React.isValidElement(child) as boolean) || child == null) {
        return child
      } else {
        // Setup the result.
        let additionalProps: any = {}
        let shouldClone: boolean = false
        // For any properties that are not `key` or `children`, push those onto the children of the fragment.
        // This has been done to handle some naughty behaviour seen where a library (in this case headlessui)
        // sees our wrapper component and doesn't think it's a fragment (even though it's a wrapper around one) and
        // then passes properties to the fragment instead of the children which is what it intends to do.
        if (isFragment(type)) {
          for (const parentPropsKey in fragmentParentProps) {
            if (parentPropsKey !== 'key' && parentPropsKey !== 'children') {
              additionalProps[parentPropsKey] = fragmentParentProps[parentPropsKey]
              shouldClone = true
            }
          }
        }

        if (childUID != null) {
          additionalProps[UTOPIA_UID_KEY] = childUID
          additionalProps[UTOPIA_PATH_KEY] = mangledChildPath
          shouldClone = true
        }

        if (shouldClone) {
          return React.cloneElement(child, additionalProps)
        } else {
          return child
        }
      }
    }
    /**
     * Fragment-like components need to be special cased because we know they return with a root component
     * that will not end up in the DOM, but is also not subject to further reconciliation.
     *
     * For this reason, the usual approach of `mangleFunctionType` where we alter the root element's props
     * is not effective, those props will just go into the abyss.
     *
     * Instead of that we render these fragment-like components, and mangle with their children
     */
    const wrapperComponent = (p: any, context?: any) => {
      // Capture this early so that it's possible to prevent an early return.
      let isFragmentWithNonFragmentProps: boolean = false
      if (isFragment(type)) {
        for (const parentPropsKey in p) {
          if (parentPropsKey !== 'key' && parentPropsKey !== 'children') {
            isFragmentWithNonFragmentProps = true
            break
          }
        }
      }
      const uid = p?.[UTOPIA_UID_KEY]
      const path = p?.[UTOPIA_PATH_KEY]
      if (uid == null && !isFragmentWithNonFragmentProps) {
        // early return for the cases where there's no data-uid
        return realCreateElement(type, p)
      } else if (p?.children == null || typeof p.children === 'string') {
        return realCreateElement(type, p)
      } else {
        let children: any
        if (typeof p?.children === 'function') {
          // mangle the function so that what it returns has the data uid
          const originalFunction = p.children
          children = function (...params: any[]) {
            const originalResponse = originalFunction(...params)
            return attachDataUidToRoot(originalResponse, uid, path)
          }
        } else {
          const uidToPass = uid
          const pathToPass = path

          if (Array.isArray(p?.children)) {
            children = React.Children.map(p?.children, (child) =>
              updateChild(child, uidToPass, pathToPass, p),
            )
          } else {
            children = updateChild(p.children, uidToPass, pathToPass, p)
          }
        }
        let mangledProps = {
          ...p,
          children: children,
        }

        if (isFragment(type)) {
          // Fragments cannot take any properties other than `key` or `children`,
          // so we create a props object that only has those two properties.
          let newMangledProps: any = {}
          if (mangledProps.key !== undefined) {
            newMangledProps.key = mangledProps.key
          }
          if (mangledProps.children !== undefined) {
            newMangledProps.children = mangledProps.children
          }
          mangledProps = newMangledProps
        } else {
          delete mangledProps[UTOPIA_UID_KEY]
          delete mangledProps[UTOPIA_PATH_KEY]
        }
        return realCreateElement(type as any, mangledProps)
      }
    }
    ;(wrapperComponent as any).theOriginalType = type
    ;(wrapperComponent as any).contextTypes = (type as any).contextTypes
    ;(wrapperComponent as any).childContextTypes = (type as any).childContextTypes
    ;(wrapperComponent as any).displayName = `UtopiaSpiedExoticType(${getDisplayName(type)})`
    return wrapperComponent
  },
  {
    maxSize: 10000,
  },
)

function isClassComponent(component: any) {
  // this is copied from stack overflow https://stackoverflow.com/a/41658173
  return typeof component === 'function' && component?.prototype?.isReactComponent != null
}

export function patchedCreateReactElement(type: any, props: any, ...children: any): any {
  if (isClassComponent(type)) {
    const mangledClass = mangleClassType(type)
    return realCreateElement(mangledClass, props, ...children)
  } else if (typeof type === 'function') {
    // if the type is function and it is NOT a class component, we deduce it is a function component
    const mangledType: React.FunctionComponent<React.PropsWithChildren<unknown>> =
      mangleFunctionType(type)
    return realCreateElement(mangledType, props, ...children)
  } else if (fragmentOrProviderOrContext(type)) {
    // fragment-like components, the list is not exhaustive, we might need to extend it later
    return realCreateElement(mangleExoticType(type), props, ...children)
  } else {
    let updatedProps = props
    if (!shouldIncludeDataUID(type)) {
      updatedProps = filterDataProps(updatedProps)
    }
    return realCreateElement(type, updatedProps, ...children)
  }
}

export function isHooksErrorMessage(message: string): boolean {
  return (
    message === 'Rendered more hooks than during the previous render.' ||
    message ===
      'Rendered fewer hooks than expected. This may be caused by an accidental early return statement.' ||
    message === 'Should have a queue. This is likely a bug in React. Please file an issue.'
  )
}
