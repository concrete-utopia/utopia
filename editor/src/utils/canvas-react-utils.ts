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
const memoSymbol = Symbol.for('react.memo')
const forwardRefSymbol = Symbol.for('react.forward_ref')

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
    type?.$$typeof == contextSymbol ||
    type?.$$typeof == memoSymbol
    // || type?.$$typeof == forwardRefSymbol
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
  if (path == null) {
    return rootUID ?? undefined
  } else if (!rootUID) {
    return path
  } else if (path.endsWith(rootUID)) {
    return path
  } else {
    const trimmedPath = trimLastSeparatorFromPath(path)
    return `${trimmedPath}${SceneSeparator}${rootUID}`
  }
}

function appendChildUIDToPath(
  path: string | null,
  childUID: string | null,
  usePathIfNoUID: boolean,
): string | undefined {
  if (path == null) {
    return childUID ?? undefined
  } else if (childUID == null) {
    // This special case exists for exotics, since they'll act purely as a pass-through
    // for getting a path and/or UID onto their children, but won't have either themselves
    return usePathIfNoUID ? path : undefined
  } else if (path.endsWith(childUID)) {
    return path
  } else {
    const trimmedPath = trimLastSeparatorFromPath(path)
    return `${trimmedPath}${ElementSeparator}${childUID}`
  }
}

function maybeAttachPathToChildrenOfIntrinsic(
  originalResponse: React.ReactElement,
  path: string | null | undefined,
): any {
  if (typeof originalResponse.type === 'string' && path != null) {
    // Since we can't create a patched component for rendering intrinsic elements, _but_ we know all of their children,
    // we must recursively pass the path down any chains of intrinsic descendants
    return attachPathToChildren(originalResponse.props.children, path)
  } else {
    return null
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
      const rootPath = appendRootUIDToPath(path, originalResponse.props[UTOPIA_UID_KEY])
      let updatedProps: any = {
        [UTOPIA_UID_KEY]: originalResponse.props[UTOPIA_UID_KEY] ?? dataUid,
        [UTOPIA_PATH_KEY]: rootPath,
      }

      const maybeUpdatedGrandchildren = maybeAttachPathToChildrenOfIntrinsic(
        originalResponse,
        rootPath,
      )
      if (maybeUpdatedGrandchildren != null) {
        updatedProps.children = maybeUpdatedGrandchildren
      }

      return React.cloneElement(originalResponse, updatedProps)
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
  } else if (!shouldIncludeDataUID(originalResponse.type)) {
    return originalResponse
  } else {
    const existingPath = originalResponse.props?.[UTOPIA_PATH_KEY]
    const childPath = appendChildUIDToPath(
      path,
      originalResponse.props[UTOPIA_UID_KEY],
      fragmentOrProviderOrContext(originalResponse.type),
    )
    const shouldSetPath = existingPath == null || childPath?.endsWith(existingPath)
    if (shouldSetPath) {
      let updatedProps: any = {
        [UTOPIA_PATH_KEY]: childPath,
      }

      const maybeUpdatedGrandchildren = maybeAttachPathToChildrenOfIntrinsic(
        originalResponse,
        childPath,
      )
      if (maybeUpdatedGrandchildren != null) {
        updatedProps.children = maybeUpdatedGrandchildren
      }

      return React.cloneElement(originalResponse, updatedProps)
    } else {
      return originalResponse
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
      return React.Children.map(children, (child) => attachPathToChild(child, path))
    } else {
      return attachPathToChild(children, path)
    }
  }
}

const mangleFunctionType = Utils.memoize(
  (type: unknown): React.FunctionComponent<React.PropsWithChildren<unknown>> => {
    const mangledFunctionName = `UtopiaSpiedFunctionComponent(${getDisplayName(type)})`

    const mangledFunction = {
      [mangledFunctionName]: (p: any, context?: any) => {
        const MeasureRenderTimes =
          isFeatureEnabled('Debug mode – Performance Marks') && PERFORMANCE_MARKS_ALLOWED
        const uuid = MeasureRenderTimes ? v4() : ''
        if (MeasureRenderTimes) {
          performance.mark(`render_start_${uuid}`)
        }

        const path = p?.[UTOPIA_PATH_KEY] ?? p?.[UTOPIA_UID_KEY]
        const updatedChildren = attachPathToChildren(p?.children, path)

        let updatedProps = { ...p }
        if (updatedChildren != null) {
          updatedProps.children = updatedChildren
        }

        const withUpdatedChildren = (
          type as React.FunctionComponent<React.PropsWithChildren<unknown>>
        )(updatedProps, context)

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

      const path = this.props?.[UTOPIA_PATH_KEY] ?? this.props?.[UTOPIA_UID_KEY]
      const updatedChildren = attachPathToChildren(this.props?.children, path)
      if (updatedChildren != null) {
        // FIXME We really should not be replacing this.props, but I can't think of a better
        // way to achieve this
        this.props = { ...this.props, children: updatedChildren }
      }

      const withUpdatedChildren = originalRender.bind(this)()

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
    const childPath = isRootElement
      ? appendRootUIDToPath(path, child.props?.[UTOPIA_UID_KEY])
      : appendChildUIDToPath(path, child.props?.[UTOPIA_UID_KEY], true)
    if (childPath != null && child.props?.[UTOPIA_PATH_KEY] == null) {
      additionalProps[UTOPIA_PATH_KEY] = childPath
      shouldClone = true
    }

    if (childUID != null) {
      additionalProps[UTOPIA_UID_KEY] = childUID
      shouldClone = true
    }

    const maybeUpdatedGrandchildren = maybeAttachPathToChildrenOfIntrinsic(child, childPath)
    if (maybeUpdatedGrandchildren != null) {
      additionalProps.children = maybeUpdatedGrandchildren
    }

    if (shouldClone) {
      return React.cloneElement(child, additionalProps)
    } else {
      return child
    }
  }
}

const mangleExoticType = Utils.memoize(
  (type: React.ComponentType): React.FunctionComponent<React.PropsWithChildren<unknown>> => {
    /**
     * Fragment-like components need to be special cased because we know they return with a root component
     * that will not end up in the DOM, but is also not subject to further reconciliation.
     *
     * For this reason, the usual approach of `mangleFunctionType` where we alter the root element's props
     * is not effective, those props will just go into the abyss.
     *
     * Instead of that we render these fragment-like components, and mangle with their children
     */
    let mangledType: any = type

    if ((type as React.MemoExoticComponent<any>).type != null) {
      // React.memo uses a field `type` to hold the actual component
      const innerType = (type as React.MemoExoticComponent<any>).type
      mangledType = {
        ...type,
        type: mangleElementType(innerType),
      }
    }

    if ((type as any).render != null) {
      // React.forwardRef uses a field `render` to hold the actual component
      // FIXME This isn't quite right. Something else is needed to handle forwardRef components...
      const innerRender = (type as any).render
      mangledType = {
        ...type,
        render: mangleElementType(innerRender),
      }
    }

    const wrapperComponent = (p: any, context?: any) => {
      const uid = p?.[UTOPIA_UID_KEY]
      const path = p?.[UTOPIA_PATH_KEY] ?? (p as any)?.[UTOPIA_UID_KEY]

      let mangledProps = {
        ...p,
      }

      if (mangledType === type) {
        delete mangledProps[UTOPIA_UID_KEY]
        delete mangledProps[UTOPIA_PATH_KEY]
      }

      if (p?.children == null || typeof p.children === 'string') {
        return realCreateElement(mangledType, mangledProps)
      } else {
        let children: any

        if (typeof p?.children === 'function') {
          // mangle the function so that what it returns has the data uid
          const originalFunction = p.children
          children = function (...params: any[]) {
            const originalResponse = originalFunction(...params)
            return updateChildOfExotic(originalResponse, uid, path)
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

        if (children != null) {
          mangledProps.children = children
        }

        return realCreateElement(mangledType, { ...mangledProps })
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

function mangleElementType(type: any): any {
  if (isClassComponent(type)) {
    return mangleClassType(type)
  } else if (typeof type === 'function') {
    // if the type is function and it is NOT a class component, we deduce it is a function component
    return mangleFunctionType(type)
  } else if (fragmentOrProviderOrContext(type)) {
    // fragment-like components, the list is not exhaustive, we might need to extend it later
    return mangleExoticType(type)
  } else {
    // Are there other types we're missing here?
    return type
  }
}

export function patchedCreateReactElement(type: any, props: any, ...children: any): any {
  // createElement runs from the inside out, meaning it will run for child elements before parents, as
  // opposed to the actual rendering of the created elements, which will happen in the correct order.
  // Because of that, in order to pass down paths and UIDs we mangle the component definitions so that
  // we can pass them down at render time.
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
    const mangledType = mangleExoticType(type)
    return realCreateElement(mangledType, props, ...children)
  } else if (typeof type === 'string') {
    // We cannot create a mangled type for this as that would break libraries like ReactDND that rely
    // on the type remaining intrinsic, so we have to add the paths upfront during the createElement
    // call, rather than as part of the rendering of the element
    let updatedProps = { ...props }

    if (!shouldIncludeDataUID(type)) {
      updatedProps = filterDataProps(updatedProps)
    }

    const path = props?.[UTOPIA_PATH_KEY] ?? props?.[UTOPIA_UID_KEY]
    const updatedChildren = attachPathToChildren(children, path) ?? children

    return realCreateElement(type, updatedProps, ...updatedChildren)
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

export const PatchedReact: typeof React = {
  ...React,
  createElement: patchedCreateReactElement,
}
