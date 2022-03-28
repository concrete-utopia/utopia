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
import { ElementSeparator, SceneSeparator } from '../core/shared/element-path'

const realCreateElement = React.createElement

const fragmentSymbol = Symbol.for('react.fragment')
const providerSymbol = Symbol.for('react.provider')
const contextSymbol = Symbol.for('react.context')
const forwardRefSymbol = Symbol.for('react.forward_ref')

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
  console.log({ 'typeOF~~~': type?.$$typeof })
  // console.log(type)
  console.log({ type: type })
  // console.log(typeof type)

  return (
    type == React.Fragment ||
    type?.$$typeof == fragmentSymbol ||
    type?.$$typeof == providerSymbol ||
    type?.$$typeof == contextSymbol ||
    type?.$$typeof == forwardRefSymbol
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
  } else {
    const trimmedPath = trimLastSeparatorFromPath(path)
    return `${trimmedPath}${SceneSeparator}${rootUID ?? ''}`
  }
}

function appendChildUIDToPath(path: string | null, childUID: string | null): string | undefined {
  if (path == null) {
    return childUID ?? undefined
  } else {
    const trimmedPath = trimLastSeparatorFromPath(path)
    return `${trimmedPath}${ElementSeparator}${childUID ?? ''}`
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
  } else if (originalResponse.props?.[UTOPIA_PATHS_2_KEY] != null) {
    // Prevent the path being overwritten
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
    console.log('~attachPath2ToChildElement 1')
    return child
  }
  if ((!React.isValidElement(child) as boolean) || child == null) {
    console.log('~attachPath2ToChildElement 2')
    return child
  } else if (child.props?.[UTOPIA_PATHS_2_KEY] != null) {
    console.log('~attachPath2ToChildElement 3')
    // Prevent the path being overwritten
    return child
  } else {
    console.log('~attachPath2ToChildElement 4')
    // Setup the result.
    let additionalProps: any = {}
    let shouldClone: boolean = false

    // ^V here, got to be here

    console.log('trouble?')

    const childPath2 = appendChildUIDToPath(path2, child.props?.[UTOPIA_UIDS_KEY])
    console.log({ childPath2 })
    if (childPath2 != null) {
      additionalProps[UTOPIA_PATHS_2_KEY] = childPath2
      shouldClone = true
    }

    console.log({ child, additionalProps, childPath2 })

    if (shouldClone) {
      const result1 = React.cloneElement(child, additionalProps)
      console.log({ result1 })
      return result1
    } else {
      return child
    }
  }
}

function attachPath2ToChildren(children: any, path2: string | null): any {
  if (typeof children === 'function') {
    console.log('attachPath2ToChildren TYPE', typeof children)
    const originalFunction = children
    return function (...params: any[]) {
      const originalResponse = originalFunction(...params)
      return attachPath2ToChild(originalResponse, path2)
    }
  } else {
    console.log('attachPath2ToChildren ELSE', typeof children)
    if (Array.isArray(children)) {
      return React.Children.map(children, (child) => attachPath2ToChildElement(child, path2))
      // hits here?
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
    console.log('1')
    return originalTypeResponse
  } else if (Array.isArray(updatedChildren)) {
    console.log('2', path2)
    console.log('updatedChildren', updatedChildren)
    const result2 = React.cloneElement(originalTypeResponse, undefined, ...updatedChildren)
    if (result2?.props?.children) {
      result2.props.children.forEach((child: any) => {
        console.log({ result2prop: child.props })
      })
    }
    return result2
    // ^ here is where the children render
  } else {
    console.log('3')
    return React.cloneElement(originalTypeResponse, undefined, updatedChildren)
  }
}

const mangleFunctionType = Utils.memoize(
  (type: unknown): React.FunctionComponent => {
    const mangledFunctionName = `UtopiaSpiedFunctionComponent(${getDisplayName(type)})`

    const mangledFunction = {
      [mangledFunctionName]: (p: any, context?: any) => {
        console.log('inside mangledFunction')
        console.log({ 'mangledFunction uid': p?.[UTOPIA_UIDS_KEY] || 'Nupkis' })
        const MeasureRenderTimes =
          isFeatureEnabled('Debug mode – Performance Marks') && PERFORMANCE_MARKS_ALLOWED
        const uuid = MeasureRenderTimes ? v4() : ''
        if (MeasureRenderTimes) {
          performance.mark(`render_start_${uuid}`)
        }
        // console.log({ uid: 'ZERO?' })

        // re-calls patchedCreateReactElement all over again on element, as intrinsic?
        let originalTypeResponse = (type as React.FunctionComponent)(p, context)

        // console.log({ uid: 'ANYTHING?' })
        console.log({ 'mangledFunction uid': p?.[UTOPIA_UIDS_KEY] })
        console.log('about to run attachPath2ToChildrenOfElement FUNCTION')
        const path2 = p?.[UTOPIA_PATHS_2_KEY] ?? p?.[UTOPIA_UIDS_KEY]
        const withUpdatedChildren = attachPath2ToChildrenOfElement(
          originalTypeResponse,
          p?.children,
          path2,
        )
        // ^^ adds paths to children, before iterating over children to render them
        console.log('DONE attachPath2ToChildrenOfElement')

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
        console.log('DONE mangledFunction for ', p?.[UTOPIA_UIDS_KEY])

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

function updateChildOfExotic(
  child: React.ReactElement | null,
  dataUids: string | null,
  paths: string | null,
  path2: string | null,
) {
  console.log('~~updateChildOfExotic')

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

    // Because the parent of this won't exist in the rendered DOM, we need to capture whether the parent's
    // path was that of a root element, or a child element, and transfer that relationship to this element
    const isRootElement = path2 != null && path2.endsWith(SceneSeparator)
    const appendUIDToPath = isRootElement ? appendRootUIDToPath : appendChildUIDToPath

    const childPath2 = appendUIDToPath(path2, child.props?.[UTOPIA_UIDS_KEY])
    if (childPath2 != null && child.props?.[UTOPIA_PATHS_2_KEY] == null) {
      additionalProps[UTOPIA_PATHS_2_KEY] = childPath2
      shouldClone = true
    }

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
      console.log('~~mangleExoticType')
      const uids = p?.[UTOPIA_UIDS_KEY]
      const paths = p?.[UTOPIA_PATHS_KEY]
      const path2 = p?.[UTOPIA_PATHS_2_KEY] ?? (p as any)?.[UTOPIA_UIDS_KEY]
      if (p?.children == null || typeof p.children === 'string') {
        return realCreateElement(type, p)
      } else {
        let children: any

        let mangledProps = {
          ...p,
        }

        delete mangledProps[UTOPIA_UIDS_KEY]
        delete mangledProps[UTOPIA_PATHS_KEY]
        delete mangledProps[UTOPIA_PATHS_2_KEY]
        let originalTypeResponse = realCreateElement(type, { ...mangledProps })

        if (typeof p?.children === 'function') {
          console.log('~~hereABC')
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
            console.log('~~here00')
            children = React.Children.map(p?.children, (child) =>
              updateChildOfExotic(child, uidsToPass, pathsToPass, path2),
            )
          } else {
            console.log('~~here11')
            children = updateChildOfExotic(p.children, uidsToPass, pathsToPass, path2)
          }
        }

        if (children == null) {
          console.log('~~here888')
          return originalTypeResponse
        } else if (Array.isArray(children)) {
          console.log('~~here999')
          return React.cloneElement(originalTypeResponse, undefined, ...children)
        } else {
          console.log('~~hereXYZ')
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
      console.log('inside mangleIntrinsicType')
      let updatedProps = p

      // console.log({ updatedProps })
      console.log({ shouldIncludeDataUID: shouldIncludeDataUID(type) })
      console.log({ 'mangleIntrinsicType-shouldIncludeDataUID': shouldIncludeDataUID(type) })
      if (!shouldIncludeDataUID(type)) {
        updatedProps = filterDataProps(updatedProps)
      }
      console.log('mangleIntrinsicType uid', p?.[UTOPIA_UIDS_KEY])
      console.log('p', p)
      console.log('type p', typeof p)
      let originalTypeResponse = realCreateElement(type, { ...updatedProps })

      const path2 = updatedProps?.[UTOPIA_PATHS_2_KEY] ?? updatedProps?.[UTOPIA_UIDS_KEY]
      console.log({ uid: p?.[UTOPIA_UIDS_KEY] })

      console.log('about to run attachPath2ToChildrenOfElement INTRINSIC')
      const withUpdatedChildren = attachPath2ToChildrenOfElement(
        originalTypeResponse,
        updatedProps?.children,
        path2,
      )
      console.log('DONE attachPath2ToChildrenOfElement')

      console.log('~~path2~~', path2)
      console.log('DONE mangleIntrinsicType')
      // console.log('about to run on children')

      return withUpdatedChildren
    }

    return wrapperComponent
  },
)

function isClassComponent(component: any) {
  // this is copied from stack overflow https://stackoverflow.com/a/41658173
  return typeof component === 'function' && component?.prototype?.isReactComponent != null
}

// Remaining TODO
// - [x] BUG Parent child paths should not include inner paths of component
// - [x] Update mangleExoticType
//   - [ ] Add test cases
// - [x] Update mangleIntrinsicType
//   - [x] Add a test case
// - [ ] Remove existing creation of `data-paths` and replace it completely with this new method
// - [ ] Tidy up the types
// - [ ] Check if we need all paths on an element, or just the deepest path

const logType = (type: any) => {
  console.log('logType', type)
  return true
}

function patchedCreateReactElement(type: any, props: any, ...children: any): any {
  console.log('patchedCreateReactElement PROPS', props)
  if (isClassComponent(type)) {
    console.log('mangled Class')
    const mangledClass = mangleClassType(type)
    return realCreateElement(mangledClass, props, ...children)
  } else if (logType(type) && typeof type === 'function') {
    console.log('mangle FunctionType')

    // if the type is function and it is NOT a class component, we deduce it is a function component
    const mangledType: React.FunctionComponent = mangleFunctionType(type)
    return realCreateElement(mangledType, props, ...children)
  } else if (logType(type) && fragmentOrProviderOrContext(type)) {
    console.log('mangle ExoticType')

    // fragment-like components, the list is not exhaustive, we might need to extend it later
    const mangledType = mangleExoticType(type)
    return realCreateElement(mangledType, props, ...children)
  } else if (logType(type) && typeof type === 'string') {
    console.log('mangle IntrinsicType')
    // console.log('typeof type', typeof type)
    // console.log('VVV')

    const mangledType = mangleIntrinsicType(type)
    return realCreateElement(mangledType, props, ...children)
  } else if (logType(type)) {
    console.log('ELSE for patchedCreateReactElement')

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
