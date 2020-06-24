import * as React from 'react'
import Utils from './utils'
import { keepDeepReferenceEqualityIfPossible } from './react-performance'
import { UTOPIA_ORIGINAL_ID_KEY } from '../core/model/element-metadata-utils'

const realCreateElement = React.createElement

function enhanceProps(
  element: any,
  dataUIDToPass: any,
  originalUIDFromProps: string | null,
  isClassInstance: boolean,
): any {
  if (element == null || dataUIDToPass == null) {
    return element
  } else {
    if (isClassInstance) {
      const currentRender = element.render
      function newRender() {
        const originalRenderResult = currentRender.bind(element)()
        const updated = updateWithUID(
          originalRenderResult,
          dataUIDToPass,
          originalUIDFromProps,
          false,
        )
        return updated
      }
      Object.defineProperty(element, 'render', {
        value: newRender,
      })
      return element
    } else {
      let propsToReturn = {
        ...element,
        props: {
          ...element.props,
          ['data-uid']: dataUIDToPass,
        },
      }
      if (originalUIDFromProps != null) {
        propsToReturn.props[UTOPIA_ORIGINAL_ID_KEY] = originalUIDFromProps
      }
      return propsToReturn
    }
  }
}

function updateWithUID(
  result: any,
  dataUIDFromProps: string,
  originalUIDFromProps: string | null,
  isClassInstance: boolean,
): any {
  const dataUIDFromResult = Utils.path(['props', 'data-uid'], result)
  const relevantUID = Utils.defaultIfNull(dataUIDFromResult, dataUIDFromProps)
  if (result.type === React.Fragment) {
    if (result.key === 'monkey-oh-monkey-please-leave-me-be') {
      // Special case when we render a Canvas component, and we don't want the below code to mangle with us!
      // The Canvas component is a HOC that wraps its children in a Fragment. The issue is that the code below
      // modifies all of the children (the Scenes) to have the uid of the canvas. The Scenes will in turn put that data-uid prop
      // on the Component root elements, overwriting the real data-uid, preventing the DOM-walker from successfully working
      // The ideal solution would be better Fragments support, but for right now this hack seems to be cheap and non-invasive
      // May the lord have mercy on our souls
      return result
    }
    // the returned root element is a fragment, so instead of changing its props,
    // lets map through its first children and mingle those props
    const modifiedResult = {
      ...result,
      props: {
        ...result.props,
        children: React.Children.map(result.props.children, (child) =>
          enhanceProps(child, relevantUID, originalUIDFromProps, isClassInstance),
        ),
      },
    }
    return modifiedResult
  } else if (Array.isArray(result)) {
    // I'm honestly not sure why we're seeing arrays here rather than fragments, but here we are...
    return result.map((child) =>
      enhanceProps(child, relevantUID, originalUIDFromProps, isClassInstance),
    )
  } else {
    const modifiedResult = enhanceProps(result, relevantUID, originalUIDFromProps, isClassInstance)

    return modifiedResult
  }
}

interface ClassMonkeyFunctionProps {
  type: any
  dataUIDFromProps: string
  originalUIDFromProps: string | null
}

interface FunctionMonkeyFunctionProps {
  type: React.FunctionComponent<any>
  dataUIDFromProps: string
  originalUIDFromProps: string | null
}

function getFunctionMonkeyFunction(monkeyProps: FunctionMonkeyFunctionProps): (props: any) => any {
  function monkeyFunction(props: any): any {
    // if this props has data-uid, let's put it on the root returned element too
    const result = monkeyProps.type(props)
    return updateWithUID(
      result,
      monkeyProps.dataUIDFromProps,
      monkeyProps.originalUIDFromProps,
      false,
    )
  }
  ;(monkeyFunction as any).displayName = `monkeyPatched(${monkeyProps.type.displayName})`

  return monkeyFunction
}

function getClassMonkeyFunction(monkeyProps: ClassMonkeyFunctionProps): (props: any) => any {
  function monkeyFunction(props: any): any {
    // if this props has data-uid, let's put it on the root returned element too
    const result = new monkeyProps.type(props)
    const returnResult = updateWithUID(
      result,
      monkeyProps.dataUIDFromProps,
      monkeyProps.originalUIDFromProps,
      true,
    )
    return returnResult
  }

  monkeyFunction.prototype = React.Component.prototype
  // we copy over the properties on the original class
  // this copies over Class.contextType, Class.propTypes, Class.defaultProps among others
  Object.assign(monkeyFunction, monkeyProps.type)
  return monkeyFunction
}

const memoEqualityCheck = (first: any, second: any): boolean => {
  return (
    first.type === second.type &&
    first.dataUIDFromProps === second.dataUIDFromProps &&
    first.originalUIDFromProps === second.originalUIDFromProps
  )
}

const memoizedGetClassMonkeyFunction = Utils.memoize(getClassMonkeyFunction, {
  maxSize: 10000,
  equals: memoEqualityCheck,
})

const memoizedGetFunctionMonkeyFunction = Utils.memoize(getFunctionMonkeyFunction, {
  maxSize: 10000,
  equals: memoEqualityCheck,
})

// Behold the lens of infinite nightmares through which our destruction shall be wrought.
function monkeyPatchReactType(
  type: any,
  dataUIDFromProps: string,
  originalUIDFromProps: string | null,
): any {
  if (type.prototype != null && typeof type.prototype.isReactComponent === 'object') {
    return memoizedGetClassMonkeyFunction({
      type: type,
      dataUIDFromProps: dataUIDFromProps,
      originalUIDFromProps: originalUIDFromProps,
    })
  } else if (typeof type === 'function') {
    return memoizedGetFunctionMonkeyFunction({
      type: type,
      dataUIDFromProps: dataUIDFromProps,
      originalUIDFromProps: originalUIDFromProps,
    })
  } else {
    return type
  }
}

function monkeyCreateElement(type: any, props: any, ...children: any): any {
  const dataUIDFromProps = Utils.propOrNull('data-uid', props)
  const originalUIDFromProps = Utils.propOrNull(UTOPIA_ORIGINAL_ID_KEY, props)
  if (dataUIDFromProps == null || typeof dataUIDFromProps !== 'string') {
    // Avoid doing anything with this if there's no data-uid prop.
    return realCreateElement(type, props, ...children)
  } else {
    const creResult: any = realCreateElement(type, props, ...children)

    const modifiedType = monkeyPatchReactType(
      creResult.type,
      dataUIDFromProps,
      originalUIDFromProps,
    )

    const final = {
      ...creResult,
      type: modifiedType,
    }
    return final
  }
}

let uidMonkeyPatchApplied: boolean = false

export function applyUIDMonkeyPatch(): void {
  if (!uidMonkeyPatchApplied) {
    uidMonkeyPatchApplied = true
    ;(React as any).createElement = monkeyCreateElement
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
