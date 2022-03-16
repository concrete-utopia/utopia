import React from 'react'
import Utils from './utils'
import { keepDeepReferenceEqualityIfPossible } from './react-performance'
import { omitWithPredicate } from '../core/shared/object-utils'
import { MapLike } from 'typescript'
import { firstLetterIsLowerCase } from '../core/shared/string-utils'
import { isIntrinsicHTMLElementString } from '../core/shared/element-template'
import {
  UtopiaKeys,
  UTOPIA_UIDS_KEY,
  UTOPIA_PATHS_KEY,
  UTOPIA_UID_PARENTS_KEY,
  UTOPIA_PATHS_2_KEY,
} from '../core/model/utopia-constants'
import { v4 } from 'uuid'
import { appendToUidString } from '../core/shared/uid-utils'
import { isFeatureEnabled } from './feature-switches'
import { PERFORMANCE_MARKS_ALLOWED } from '../common/env-vars'

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

function appendRootUIDToPath(path: string | null, rootUID: string | null): string | undefined {
  if (rootUID == null) {
    return undefined
  } else if (path == null) {
    return rootUID
  } else {
    return `${path}:${rootUID}`
  }
}

function appendChildUIDToPath(path: string | null, childUID: string | null): string | undefined {
  if (childUID == null) {
    return undefined
  } else if (path == null) {
    return childUID
  } else {
    return `${path}/${childUID}`
  }
}

function attachDataUidToRoot(
  originalResponse: React.ReactElement | null | undefined,
  dataUids: string | null,
  paths: string | null,
  path2: string | null,
): React.ReactElement | null
function attachDataUidToRoot(
  originalResponse: Array<React.ReactElement | null>,
  dataUids: string | null,
  paths: string | null,
  path2: string | null,
): Array<React.ReactElement | null>
function attachDataUidToRoot(
  originalResponse: React.ReactElement | Array<React.ReactElement | null> | null | undefined,
  dataUids: string | null,
  paths: string | null,
  path2: string | null,
): React.ReactElement | Array<React.ReactElement | null> | null {
  if (originalResponse == null || dataUids == null) {
    return originalResponse as any
  } else if (Array.isArray(originalResponse)) {
    // the response was an array of elements
    return originalResponse.map((element) => attachDataUidToRoot(element, dataUids, paths, path2))
  } else if (!React.isValidElement(originalResponse as any)) {
    return originalResponse
  } else {
    if (shouldIncludeDataUID(originalResponse.type)) {
      return React.cloneElement(originalResponse, {
        [UTOPIA_UIDS_KEY]: appendToUidString(originalResponse.props[UTOPIA_UIDS_KEY], dataUids),
        [UTOPIA_PATHS_KEY]: appendToUidString(originalResponse.props[UTOPIA_PATHS_KEY], paths), // This is the line that adds all paths to the element
        [UTOPIA_PATHS_2_KEY]: appendRootUIDToPath(path2, originalResponse.props[UTOPIA_UIDS_KEY]),
      })
    } else {
      return originalResponse
    }
  }
}

function attachPath2ToChild(
  originalResponse: React.ReactElement | null | undefined,
  path2: string | null,
): React.ReactElement | null
function attachPath2ToChild(
  originalResponse: Array<React.ReactElement | null>,
  path2: string | null,
): Array<React.ReactElement | null>
function attachPath2ToChild(
  originalResponse: React.ReactElement | Array<React.ReactElement | null> | null | undefined,
  path2: string | null,
): React.ReactElement | Array<React.ReactElement | null> | null {
  if (originalResponse == null || path2 == null) {
    return originalResponse as any
  } else if (Array.isArray(originalResponse)) {
    // the response was an array of elements
    return originalResponse.map((element) => attachPath2ToChild(element, path2))
  } else if (!React.isValidElement(originalResponse as any)) {
    return originalResponse
  } else {
    if (shouldIncludeDataUID(originalResponse.type)) {
      return React.cloneElement(originalResponse, {
        [UTOPIA_PATHS_2_KEY]: appendChildUIDToPath(path2, originalResponse.props[UTOPIA_UIDS_KEY]),
      })
    } else {
      return originalResponse
    }
  }
}

function attachPath2ToChildElement(child: React.ReactElement | null, path2: string | null) {
  if (child == null || !shouldIncludeDataUID(child.type)) {
    return child
  }
  if ((!React.isValidElement(child) as boolean) || child == null) {
    return child
  } else {
    // Setup the result.
    let additionalProps: any = {}
    let shouldClone: boolean = false

    const childPath2 = appendChildUIDToPath(path2, child.props?.[UTOPIA_UIDS_KEY])
    if (childPath2 != null) {
      additionalProps[UTOPIA_PATHS_2_KEY] = childPath2
      shouldClone = true
    }

    if (shouldClone) {
      return React.cloneElement(child, additionalProps)
    } else {
      return child
    }
  }
}

function attachPath2ToChildren(children: any, path2: string | null): any {
  if (typeof children === 'function') {
    const originalFunction = children
    return function (...params: any[]) {
      const originalResponse = originalFunction(...params)
      return attachPath2ToChild(originalResponse, path2)
    }
  } else {
    if (Array.isArray(children)) {
      return React.Children.map(children, (child) => attachPath2ToChildElement(child, path2))
    } else {
      return attachPath2ToChildElement(children, path2)
    }
  }
}

function attachPath2ToChildrenOfElement(
  originalTypeResponse: React.ReactElement | null,
  children: any,
  path2: string | null,
): any {
  if (originalTypeResponse == null) {
    return originalTypeResponse
  }

  const updatedChildren = attachPath2ToChildren(children, path2)

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

        const path2 = p?.[UTOPIA_PATHS_2_KEY] ?? p?.[UTOPIA_UIDS_KEY]
        const withUpdatedChildren = attachPath2ToChildrenOfElement(
          originalTypeResponse,
          p?.children,
          path2,
        )

        const res = attachDataUidToRoot(
          withUpdatedChildren,
          (p as any)?.[UTOPIA_UIDS_KEY],
          (p as any)?.[UTOPIA_PATHS_KEY],
          path2,
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

      const path2 = this.props?.[UTOPIA_PATHS_2_KEY] ?? this.props?.[UTOPIA_UIDS_KEY]
      const withUpdatedChildren = attachPath2ToChildrenOfElement(
        originalTypeResponse,
        this.props?.children,
        path2,
      )

      const res = attachDataUidToRoot(
        withUpdatedChildren,
        (this.props as any)?.[UTOPIA_UIDS_KEY],
        (this.props as any)?.[UTOPIA_PATHS_KEY],
        path2,
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

const mangleExoticType = Utils.memoize(
  (type: React.ComponentType): React.FunctionComponent => {
    function updateChild(
      child: React.ReactElement | null,
      dataUids: string | null,
      paths: string | null,
    ) {
      if (child == null || !shouldIncludeDataUID(child.type)) {
        return child
      }
      const existingChildUIDs = child.props?.[UTOPIA_UIDS_KEY]
      const existingChildPaths = child.props?.[UTOPIA_PATHS_KEY]
      const appendedUIDString = appendToUidString(existingChildUIDs, dataUids)
      const appendedPathsString = appendToUidString(existingChildPaths, paths)
      if ((!React.isValidElement(child) as boolean) || child == null) {
        return child
      } else {
        // Setup the result.
        let additionalProps: any = {}
        let shouldClone: boolean = false

        if (appendedUIDString != null) {
          additionalProps[UTOPIA_UIDS_KEY] = appendedUIDString
          additionalProps[UTOPIA_PATHS_KEY] = appendedPathsString
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
      const uids = p?.[UTOPIA_UIDS_KEY]
      const paths = p?.[UTOPIA_PATHS_KEY]
      const path2 = (p as any)?.[UTOPIA_PATHS_2_KEY]
      if (uids == null) {
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
            return attachDataUidToRoot(originalResponse, uids, paths, path2)
          }
        } else {
          const uidsToPass = uids
          const pathsToPass = paths

          if (Array.isArray(p?.children)) {
            children = React.Children.map(p?.children, (child) =>
              updateChild(child, uidsToPass, pathsToPass),
            )
          } else {
            children = updateChild(p.children, uidsToPass, pathsToPass)
          }
        }
        let mangledProps = {
          ...p,
          children: children,
        }

        delete mangledProps[UTOPIA_UIDS_KEY]
        delete mangledProps[UTOPIA_PATHS_KEY]
        delete mangledProps[UTOPIA_PATHS_2_KEY]
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

const mangleIntrinsicType = Utils.memoize(
  (type: string): React.FunctionComponent => {
    const wrapperComponent = (p: any, context?: any) => {
      console.log(`Creating a ${type}`)

      let updatedProps = p
      if (!shouldIncludeDataUID(type)) {
        updatedProps = filterDataProps(updatedProps)
      }

      const path2 = updatedProps?.[UTOPIA_PATHS_2_KEY] ?? updatedProps?.[UTOPIA_UIDS_KEY]
      const updatedChildren = attachPath2ToChildren(updatedProps?.children, path2)

      return realCreateElement(type, { ...updatedProps, children: updatedChildren })
    }

    return wrapperComponent
  },
)

function isClassComponent(component: any) {
  // this is copied from stack overflow https://stackoverflow.com/a/41658173
  return typeof component === 'function' && component?.prototype?.isReactComponent != null
}

// Remaining TODO
// - [ ] Update mangleExoticType
//   - [ ] Add a test case
// - [ ] Update mangleIntrinsicType
//   - [ ] Add a test case
// - [ ] Remove existing creation of `data-paths` and replace it completely with this new method
// - [ ] Tidy up the types
// - [ ] Check if we need all paths on an element, or just the deepest path

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
