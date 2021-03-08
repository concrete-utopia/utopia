import * as React from 'react'
import Utils from './utils'
import { keepDeepReferenceEqualityIfPossible } from './react-performance'
import { omitWithPredicate } from '../core/shared/object-utils'
import { MapLike } from 'typescript'
import { firstLetterIsLowerCase } from '../core/shared/string-utils'
import { isIntrinsicHTMLElementString } from '../core/shared/element-template'
import { UtopiaKeys, UTOPIA_UID_KEY, UTOPIA_UID_PARENTS_KEY } from '../core/model/utopia-constants'
import { v4 } from 'uuid'
import { PRODUCTION_ENV } from '../common/env-vars'

const realCreateElement = React.createElement

const fragmentSymbol = Symbol.for('react.fragment')
const providerSymbol = Symbol.for('react.provider')
const contextSymbol = Symbol.for('react.context')

let uidMonkeyPatchApplied: boolean = false

export function applyUIDMonkeyPatch(): void {
  if (!uidMonkeyPatchApplied) {
    uidMonkeyPatchApplied = true
    ;(React as any).createElement = patchedCreateReactElement
  }
}

function getDisplayName(type: any) {
  // taken from https://github.com/facebook/react/blob/7e405d458d6481fb1c04dfca6afab0651e6f67cd/packages/react/src/ReactElement.js#L415
  if (typeof type === 'function') {
    return type.displayName || type.name || 'Unknown'
  } else if (typeof type === 'symbol') {
    return type.toString()
  } else if (typeof type === 'string') {
    return type
  } else {
    return 'Unknown'
  }
}

function fragmentOrProviderOrContext(type: any): boolean {
  return (
    type == React.Fragment ||
    type?.$$typeof == fragmentSymbol ||
    type?.$$typeof == providerSymbol ||
    type?.$$typeof == contextSymbol
  )
}

function keyShouldBeExcluded(key: string): boolean {
  return UtopiaKeys.includes(key)
}

export function filterDataProps(props: MapLike<any>): MapLike<any> {
  return omitWithPredicate(props, (key) => typeof key === 'string' && keyShouldBeExcluded(key))
}

export function makeCanvasElementPropsSafe(props: any): any {
  function removeUnsafeValues(innerProps: any, visited: any[]): any {
    if (React.isValidElement(innerProps)) {
      return 'REACT_ELEMENT'
    } else {
      switch (typeof innerProps) {
        case 'object': {
          if (Array.isArray(innerProps)) {
            return innerProps.map((prop) => {
              return removeUnsafeValues(prop, visited)
            })
          } else if (innerProps != null) {
            visited.push(innerProps)
            return Utils.objectMap((value, key) => {
              if (typeof value === 'object') {
                if (visited.includes(value)) {
                  return null
                } else {
                  return removeUnsafeValues(value, visited)
                }
              } else {
                return removeUnsafeValues(value, visited)
              }
            }, innerProps)
          }
          break
        }
        case 'function': {
          return 'FUNCTION'
        }
        default:
          return innerProps
      }
    }
  }

  let working: any
  if (typeof props === 'object' && !Array.isArray(props)) {
    working = { ...props }
    delete working['children']
  } else {
    working = props
  }

  return keepDeepReferenceEqualityIfPossible(props, {
    skipDeepFreeze: true,
    ...removeUnsafeValues(working, []),
  })
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
): React.ReactElement | null
function attachDataUidToRoot(
  originalResponse: Array<React.ReactElement | null>,
  dataUid: string | null,
): Array<React.ReactElement | null>
function attachDataUidToRoot(
  originalResponse: React.ReactElement | Array<React.ReactElement | null> | null | undefined,
  dataUid: string | null,
): React.ReactElement | Array<React.ReactElement | null> | null {
  if (originalResponse == null || dataUid == null) {
    return originalResponse as any
  } else if (Array.isArray(originalResponse)) {
    // the response was an array of elements
    return originalResponse.map((element) => attachDataUidToRoot(element, dataUid))
  } else if (!React.isValidElement(originalResponse as any)) {
    return originalResponse
  } else if (originalResponse.props[UTOPIA_UID_KEY] != null) {
    return originalResponse
  } else {
    if (shouldIncludeDataUID(originalResponse.type)) {
      return React.cloneElement(originalResponse, { [UTOPIA_UID_KEY]: dataUid })
    } else {
      return React.cloneElement(originalResponse)
    }
  }
}

const MeasureRenderTimes = !PRODUCTION_ENV && typeof window.performance.mark === 'function'

const mangleFunctionType = Utils.memoize(
  (type: unknown): React.FunctionComponent => {
    const uuid = MeasureRenderTimes ? v4() : ''
    const mangledFunction = (p: any, context?: any) => {
      if (MeasureRenderTimes) {
        performance.mark(`render_start_${uuid}`)
      }
      let originalTypeResponse = (type as React.FunctionComponent)(p, context)
      const res = attachDataUidToRoot(originalTypeResponse, (p as any)?.[UTOPIA_UID_KEY])
      if (MeasureRenderTimes) {
        performance.mark(`render_end_${uuid}`)
        performance.measure(
          `Render Component ${getDisplayName(type)}`,
          `render_start_${uuid}`,
          `render_end_${uuid}`,
        )
      }
      return res
    }
    ;(mangledFunction as any).theOriginalType = type
    ;(mangledFunction as any).contextTypes = (type as any).contextTypes
    ;(mangledFunction as any).childContextTypes = (type as any).childContextTypes
    ;(mangledFunction as any).displayName = `UtopiaSpiedExoticType(${getDisplayName(type)})`
    return mangledFunction
  },
  {
    maxSize: 10000,
  },
)

const mangleClassType = Utils.memoize(
  (type: any) => {
    const uuid = MeasureRenderTimes ? v4() : ''
    const originalRender = type.prototype.render
    // mutation
    type.prototype.render = function monkeyRender() {
      if (MeasureRenderTimes) {
        performance.mark(`render_start_${uuid}`)
      }
      let originalTypeResponse = originalRender.bind(this)()
      const res = attachDataUidToRoot(originalTypeResponse, (this.props as any)?.['data-uid'])
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

const mangleExoticType = Utils.memoize(
  (type: React.ComponentType): React.FunctionComponent => {
    function updateChild(
      child: React.ReactElement,
      dataUid: string | null,
      parentUid: string | null,
    ) {
      const existingChildUID = child.props?.[UTOPIA_UID_KEY]
      const existingParentIDs = child.props?.[UTOPIA_UID_PARENTS_KEY]
      if ((!React.isValidElement(child) as any) || child == null) {
        return child
      } else {
        let pathParts: Array<string> = []
        // Added to here in reverse order, attempting to rebuild
        // what the path _should_ be.
        if (typeof existingChildUID === 'string') {
          pathParts.push(existingChildUID)
        }
        if (typeof dataUid === 'string') {
          pathParts.push(dataUid)
        }
        if (typeof parentUid === 'string') {
          pathParts.push(parentUid)
        }
        if (typeof existingParentIDs === 'string') {
          pathParts.push(existingParentIDs)
        }
        let [childUID, ...parentParts] = pathParts

        // Setup the result.
        let additionalProps: any = {}
        let shouldClone: boolean = false

        if (childUID != null) {
          additionalProps[UTOPIA_UID_KEY] = childUID
          shouldClone = true
        }
        if (parentParts != null && parentParts.length > 0) {
          additionalProps[UTOPIA_UID_PARENTS_KEY] = parentParts.reverse().join('/')
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
      if (p?.[UTOPIA_UID_KEY] == null) {
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
            return attachDataUidToRoot(originalResponse, p?.[UTOPIA_UID_KEY])
          }
        } else if (Array.isArray(p?.children)) {
          children = React.Children.map(p?.children, (child) =>
            updateChild(child, p?.[UTOPIA_UID_KEY], p?.[UTOPIA_UID_PARENTS_KEY]),
          )
        } else {
          children = updateChild(p.children, p?.[UTOPIA_UID_KEY], p?.[UTOPIA_UID_PARENTS_KEY])
        }
        let mangledProps = {
          ...p,
          children: children,
        }

        delete mangledProps[UTOPIA_UID_KEY]
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

function patchedCreateReactElement(type: any, props: any, ...children: any): any {
  if (isClassComponent(type)) {
    const mangledClass = mangleClassType(type)
    return realCreateElement(mangledClass, props, ...children)
  } else if (typeof type === 'function') {
    // if the type is function and it is NOT a class component, we deduce it is a function component
    const mangledType: React.FunctionComponent = mangleFunctionType(type)
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
