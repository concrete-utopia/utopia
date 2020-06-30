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
    return originalResponse as null
  } else if (Array.isArray(originalResponse)) {
    // the response was an array of elements
    return originalResponse.map((element) => attachDataUidToRoot(element, dataUid))
  } else {
    const finalElement = React.cloneElement(originalResponse, { 'data-uid': dataUid })
    return finalElement
  }
}

const mangleFunctionType = Utils.memoize(
  (type: unknown): React.FunctionComponent => {
    return (p) => {
      let originalTypeResponse = (type as React.FunctionComponent)(p)
      const res = attachDataUidToRoot(originalTypeResponse, (p as any)['data-uid'])
      return res
    }
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
      return attachDataUidToRoot(originalTypeResponse, (this.props as any)['data-uid'])
    }
    return type
  },
  {
    maxSize: 10000,
  },
)

const mangleExoticType = Utils.memoize(
  (type: React.ComponentType): React.FunctionComponent => {
    const wrapperComponent = (p: any) => {
      if (p['data-uid'] == null) {
        // early return for the cases where there's no data-uid
        return realCreateElement(type, p)
      }
      if (p.children == null || typeof p.children === 'string') {
        return realCreateElement(type, p)
      } else if (typeof p.children === 'function') {
        // mangle the function so that what it returns has the data uid
        const originalFunction = p.children
        const newProps = {
          ...p,
          children: function (...params: any[]) {
            const originalResponse = originalFunction(...params)
            return attachDataUidToRoot(originalResponse, p['data-uid'])
          },
        }

        return realCreateElement(type, newProps)
      } else {
        const mangledChildren = React.Children.map(p.children, (child) => {
          if (
            (!React.isValidElement(child) as any) ||
            child == null ||
            child.props?.['data-uid'] != null
          ) {
            return child
          } else {
            return React.cloneElement(
              child,
              p['data-uid'] != null ? { 'data-uid': p['data-uid'] } : {},
            )
          }
        })
        const mangledProps = {
          ...p,
          children: mangledChildren,
        }
        return realCreateElement(type as any, mangledProps)
      }
    }
    ;(wrapperComponent as any).theOriginalType = type
    return wrapperComponent
  },
  {
    maxSize: 10000,
  },
)

function patchedCreateReactElement(type: any, props: any, ...children: any): any {
  if (props?.['key'] !== 'monkey-oh-monkey-please-leave-me-be') {
    if (typeof type?.prototype?.isReactComponent === 'object') {
      const mangledClass = mangleClassType(type)
      ;(mangledClass as any).theOriginalType = type
      return realCreateElement(mangledClass, props, ...children)
    } else if (typeof type === 'function') {
      const mangledType: React.FunctionComponent = mangleFunctionType(type)
      ;(mangledType as any).theOriginalType = type
      return realCreateElement(mangledType, props, ...children)
    } else if (
      type == React.Fragment ||
      type?.$$typeof == Symbol.for('react.fragment') ||
      type?.$$typeof == Symbol.for('react.provider') ||
      type?.$$typeof == Symbol.for('react.context')
    ) {
      return realCreateElement(mangleExoticType(type), props, ...children)
    }
  }
  return realCreateElement(type, props, ...children)
}
