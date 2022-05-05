import React from 'react'
import Utils from './utils'
import { omitWithPredicate } from '../core/shared/object-utils'
import { MapLike } from 'typescript'
import { firstLetterIsLowerCase } from '../core/shared/string-utils'
import { isIntrinsicHTMLElementString } from '../core/shared/element-template'
import { UtopiaKeys, UTOPIA_UID_KEY, UTOPIA_PATH_KEY } from '../core/model/utopia-constants'
import { v4 } from 'uuid'
import { isFeatureEnabled } from './feature-switches'
import { PERFORMANCE_MARKS_ALLOWED } from '../common/env-vars'
import { ElementSeparator, SceneSeparator } from '../core/shared/element-path'

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

function getDisplayName(type: any): string {
  // taken from https://github.com/facebook/react/blob/7e405d458d6481fb1c04dfca6afab0651e6f67cd/packages/react/src/ReactElement.js#L415
  if (typeof type === 'function') {
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

function trimLastSeparatorFromPath(path: string): string {
  return path.endsWith(SceneSeparator) || path.endsWith(ElementSeparator) ? path.slice(0, -1) : path
}

function appendRootUIDToPath(path: string | null, rootUID: string | null): string | undefined {
  const splitUid = rootUID ? rootUID.split(' ')?.[0] : undefined
  if (path == null) {
    return splitUid ?? undefined
  } else if (!splitUid) {
    return path
  } else {
    const trimmedPath = trimLastSeparatorFromPath(path)
    return `${trimmedPath}${SceneSeparator}${splitUid ?? ''}`
  }
}

function appendChildUIDToPath(path: string | null, childUID: string | null): string | undefined {
  const splitUid = childUID ? childUID.split(' ')?.[0] : undefined // might not be needed

  if (path == null) {
    return childUID ?? undefined
  } else if (!splitUid) {
    return path
  } else {
    const trimmedPath = trimLastSeparatorFromPath(path)
    return `${trimmedPath}${ElementSeparator}${splitUid ?? ''}`
  }
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
        [UTOPIA_PATH_KEY]: appendRootUIDToPath(path, originalResponse.props[UTOPIA_UID_KEY]),
      })
    } else {
      return originalResponse
    }
  }
}

function attachPathToChild(
  originalResponse: React.ReactElement | null | undefined,
  path: string | null,
): React.ReactElement | null
function attachPathToChild(
  originalResponse: Array<React.ReactElement | null>,
  path: string | null,
): Array<React.ReactElement | null>
function attachPathToChild(
  originalResponse: React.ReactElement | Array<React.ReactElement | null> | null | undefined,
  path: string | null,
): React.ReactElement | Array<React.ReactElement | null> | null {
  if (originalResponse == null || path == null) {
    return originalResponse as any
  } else if (Array.isArray(originalResponse)) {
    // the response was an array of elements
    return originalResponse.map((element) => attachPathToChild(element, path))
  } else if (!React.isValidElement(originalResponse as any)) {
    return originalResponse
  } else if (originalResponse.props?.[UTOPIA_PATH_KEY] != null) {
    // Prevent the path being overwritten
    return originalResponse
  } else {
    if (shouldIncludeDataUID(originalResponse.type)) {
      return React.cloneElement(originalResponse, {
        [UTOPIA_PATH_KEY]: appendChildUIDToPath(path, originalResponse.props[UTOPIA_UID_KEY]),
      })
    } else {
      return originalResponse
    }
  }
}

function attachPathToChildElement(child: React.ReactElement | null, path: string | null) {
  if (child == null || !shouldIncludeDataUID(child.type)) {
    return child
  }
  if ((!React.isValidElement(child) as boolean) || child == null) {
    return child
  } else if (child.props?.[UTOPIA_PATH_KEY] != null) {
    // Prevent the path being overwritten
    return child
  } else {
    // Setup the result.
    let additionalProps: any = {}
    let shouldClone: boolean = false

    const childPath = appendChildUIDToPath(path, child.props?.[UTOPIA_UID_KEY])

    if (childPath != null) {
      additionalProps[UTOPIA_PATH_KEY] = childPath
      shouldClone = true
    }

    if (shouldClone) {
      return React.cloneElement(child, additionalProps)
    } else {
      return child
    }
  }
}

function attachPathToChildren(children: any, path: string | null): any {
  if (typeof children === 'function') {
    const originalFunction = children
    return function (...params: any[]) {
      const originalResponse = originalFunction(...params)
      return attachPathToChild(originalResponse, path)
    }
  } else {
    if (Array.isArray(children)) {
      return React.Children.map(children, (child) => attachPathToChildElement(child, path))
    } else {
      return attachPathToChildElement(children, path)
    }
  }
}

function attachPathToChildrenOfElement(
  originalTypeResponse: React.ReactElement | null,
  children: any,
  path: string | null,
): any {
  if (originalTypeResponse == null) {
    return originalTypeResponse
  }

  const updatedChildren = attachPathToChildren(children, path)

  if (updatedChildren == null) {
    return originalTypeResponse
  } else if (Array.isArray(updatedChildren)) {
    return React.cloneElement(originalTypeResponse, undefined, ...updatedChildren)
  } else {
    return React.cloneElement(originalTypeResponse, undefined, updatedChildren)
  }
}

const mangleFunctionType = Utils.memoize(
  (type: unknown): React.FunctionComponent => {
    const mangledFunctionName = `UtopiaSpiedFunctionComponent(${getDisplayName(type)})`

    const mangledFunction = {
      [mangledFunctionName]: (p: any, context?: any) => {
        const MeasureRenderTimes =
          isFeatureEnabled('Debug mode – Performance Marks') && PERFORMANCE_MARKS_ALLOWED
        const uuid = MeasureRenderTimes ? v4() : ''
        if (MeasureRenderTimes) {
          performance.mark(`render_start_${uuid}`)
        }

        let originalTypeResponse = (type as React.FunctionComponent)(p, context)

        const path = p?.[UTOPIA_PATH_KEY] ?? p?.[UTOPIA_UID_KEY]
        const withUpdatedChildren = attachPathToChildrenOfElement(
          originalTypeResponse,
          p?.children,
          path,
        )

        const res = attachDataUidToRoot(withUpdatedChildren, (p as any)?.[UTOPIA_UID_KEY], path)
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
    }[mangledFunctionName]
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

const mangleClassType = Utils.memoize(
  (type: any) => {
    const originalRender = type.prototype.render
    // mutation
    type.prototype.render = function monkeyRender() {
      const MeasureRenderTimes =
        isFeatureEnabled('Debug mode – Performance Marks') && PERFORMANCE_MARKS_ALLOWED
      const uuid = MeasureRenderTimes ? v4() : ''
      if (MeasureRenderTimes) {
        performance.mark(`render_start_${uuid}`)
      }

      let originalTypeResponse = originalRender.bind(this)()

      const path = this.props?.[UTOPIA_PATH_KEY] ?? this.props?.[UTOPIA_UID_KEY]
      const withUpdatedChildren = attachPathToChildrenOfElement(
        originalTypeResponse,
        this.props?.children,
        path,
      )

      const res = attachDataUidToRoot(
        withUpdatedChildren,
        (this.props as any)?.[UTOPIA_UID_KEY],
        path,
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

function pathIsRoot(path: string | null): boolean {
  if (path == null) {
    return false
  } else if (path.endsWith(SceneSeparator)) {
    return true
  } else {
    return !path.includes(SceneSeparator) && !path.includes(ElementSeparator)
  }
}

function updateChildOfExotic(
  child: React.ReactElement | null,
  dataUid: string | null,
  path: string | null,
) {
  if (child == null || !shouldIncludeDataUID(child.type)) {
    return child
  }
  const existingChildUID = child.props?.[UTOPIA_UID_KEY]
  const childUID = existingChildUID ?? dataUid

  if ((!React.isValidElement(child) as boolean) || child == null) {
    return child
  } else {
    // Setup the result.
    let additionalProps: any = {}
    let shouldClone: boolean = false

    // Because the parent of this won't exist in the rendered DOM, we need to capture whether the parent's
    // path was that of a root element, or a child element, and transfer that relationship to this element
    const isRootElement = pathIsRoot(path)
    const appendUIDToPath = isRootElement ? appendRootUIDToPath : appendChildUIDToPath

    const childPath = appendUIDToPath(path, child.props?.[UTOPIA_UID_KEY])
    if (childPath != null && child.props?.[UTOPIA_PATH_KEY] == null) {
      additionalProps[UTOPIA_PATH_KEY] = childPath
      shouldClone = true
    }

    if (childUID != null) {
      additionalProps[UTOPIA_UID_KEY] = childUID
      shouldClone = true
    }

    if (shouldClone) {
      return React.cloneElement(child, additionalProps)
    } else {
      return child
    }
  }
}

const mangleExoticType = Utils.memoize(
  (type: React.ComponentType): React.FunctionComponent => {
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
      const uid = p?.[UTOPIA_UID_KEY]
      const path = p?.[UTOPIA_PATH_KEY] ?? (p as any)?.[UTOPIA_UID_KEY]
      if (p?.children == null || typeof p.children === 'string') {
        return realCreateElement(type, p)
      } else {
        let children: any

        let mangledProps = {
          ...p,
        }

        delete mangledProps[UTOPIA_UID_KEY]
        delete mangledProps[UTOPIA_PATH_KEY]
        let originalTypeResponse = realCreateElement(type, { ...mangledProps })

        if (typeof p?.children === 'function') {
          // mangle the function so that what it returns has the data uid
          const originalFunction = p.children
          children = function (...params: any[]) {
            const originalResponse = originalFunction(...params)
            return attachDataUidToRoot(originalResponse, uid, path)
          }
        } else {
          const uidToPass = uid

          if (Array.isArray(p?.children)) {
            children = React.Children.map(p?.children, (child) =>
              updateChildOfExotic(child, uidToPass, path),
            )
          } else {
            children = updateChildOfExotic(p.children, uidToPass, path)
          }
        }

        if (children == null) {
          return originalTypeResponse
        } else if (Array.isArray(children)) {
          return React.cloneElement(originalTypeResponse, undefined, ...children)
        } else {
          return React.cloneElement(originalTypeResponse, undefined, children)
        }
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

const mangleIntrinsicType = Utils.memoize(
  (type: string): React.FunctionComponent => {
    const wrapperComponent = (p: any, context?: any) => {
      let updatedProps = p

      if (!shouldIncludeDataUID(type)) {
        updatedProps = filterDataProps(updatedProps)
      }

      let originalTypeResponse = realCreateElement(type, { ...updatedProps })

      const path = updatedProps?.[UTOPIA_PATH_KEY] ?? updatedProps?.[UTOPIA_UID_KEY]

      const withUpdatedChildren = attachPathToChildrenOfElement(
        originalTypeResponse,
        updatedProps?.children,
        path,
      )

      return withUpdatedChildren
    }

    return wrapperComponent
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
    const mangledType = mangleExoticType(type)
    return realCreateElement(mangledType, props, ...children)
  } else if (typeof type === 'string') {
    const mangledType = mangleIntrinsicType(type)
    return realCreateElement(mangledType, props, ...children)
  } else {
    // Are there other types we're missing here?
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
