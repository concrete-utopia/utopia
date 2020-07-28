import * as React from 'react'
import Utils from './utils'
import { keepDeepReferenceEqualityIfPossible } from './react-performance'
import { UTOPIA_ORIGINAL_ID_KEY } from '../core/model/element-metadata-utils'

const realCreateElement = React.createElement

let uidMonkeyPatchApplied: boolean = false

export function applyUIDMonkeyPatch(): void {
  if (!uidMonkeyPatchApplied) {
    uidMonkeyPatchApplied = true
    ;(React as any).createElement = patchedCreateReactElement
  }
}

export function makeCanvasElementPropsSafe(props: any): any {
  function removeUnsafeValues(innerProps: any, visited: any[]): any {
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
  } else {
    const finalElement = React.cloneElement(originalResponse, { 'data-uid': dataUid })
    return finalElement
  }
}

const mangleFunctionType = Utils.memoize(
  (type: unknown): React.FunctionComponent => {
    const mangledFunction = (p: any, context?: any) => {
      let originalTypeResponse = (type as React.FunctionComponent)(p, context)
      const res = attachDataUidToRoot(originalTypeResponse, (p as any)?.['data-uid'])
      return res
    }
    ;(mangledFunction as any).theOriginalType = type
    ;(mangledFunction as any).contextTypes = (type as any).contextTypes
    ;(mangledFunction as any).childContextTypes = (type as any).childContextTypes
    return mangledFunction
  },
  {
    maxSize: 10000,
  },
)

const mangleClassType = Utils.memoize(
  (type: any) => {
    const originalRender = type.prototype.render
    // mutation
    type.prototype.render = function monkeyRender() {
      let originalTypeResponse = originalRender.bind(this)()
      return attachDataUidToRoot(originalTypeResponse, (this.props as any)?.['data-uid'])
    }
    ;(type as any).theOriginalType = type
    return type
  },
  {
    maxSize: 10000,
  },
)

const mangleExoticType = Utils.memoize(
  (type: React.ComponentType): React.FunctionComponent => {
    function updateChild(child: React.ReactElement, dataUid: string | null) {
      if (
        (!React.isValidElement(child) as any) ||
        child == null ||
        child.props?.['data-uid'] != null
      ) {
        return child
      } else {
        return React.cloneElement(child, dataUid != null ? { 'data-uid': dataUid } : {})
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
      if (p?.['data-uid'] == null) {
        // early return for the cases where there's no data-uid
        return realCreateElement(type, p)
      }
      if (p?.children == null || typeof p.children === 'string') {
        return realCreateElement(type, p)
      } else {
        let children: any
        if (typeof p?.children === 'function') {
          // mangle the function so that what it returns has the data uid
          const originalFunction = p.children
          children = function (...params: any[]) {
            const originalResponse = originalFunction(...params)
            return attachDataUidToRoot(originalResponse, p?.['data-uid'])
          }
        } else if (!Array.isArray(p?.children)) {
          children = updateChild(p.children, p?.['data-uid'])
        } else {
          children = React.Children.map(p?.children, (child) => updateChild(child, p?.['data-uid']))
        }
        let mangledProps = {
          ...p,
          children: children,
        }

        delete mangledProps['data-uid']
        return realCreateElement(type as any, mangledProps)
      }
    }
    ;(wrapperComponent as any).theOriginalType = type
    ;(wrapperComponent as any).contextTypes = (type as any).contextTypes
    ;(wrapperComponent as any).childContextTypes = (type as any).childContextTypes
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
  } else if (
    type == React.Fragment ||
    type?.$$typeof == Symbol.for('react.fragment') ||
    type?.$$typeof == Symbol.for('react.provider') ||
    type?.$$typeof == Symbol.for('react.context')
  ) {
    // fragment-like components, the list is not exhaustive, we might need to extend it later
    return realCreateElement(mangleExoticType(type), props, ...children)
  }
  return realCreateElement(type, props, ...children)
}
