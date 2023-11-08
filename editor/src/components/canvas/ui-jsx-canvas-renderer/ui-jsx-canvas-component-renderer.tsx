import React, { useState } from 'react'
import type { MapLike } from 'typescript'
import type { PropertyControls } from 'utopia-api/core'
import type { JSXElementChild, UtopiaJSXComponent } from '../../../core/shared/element-template'
import {
  isUtopiaJSXComponent,
  isSVGElement,
  isJSXElement,
} from '../../../core/shared/element-template'
import { optionalMap } from '../../../core/shared/optional-utils'
import type { DomWalkerInvalidatePathsCtxData, UiJsxCanvasContextData } from '../ui-jsx-canvas'
import {
  DomWalkerInvalidatePathsCtxAtom,
  UiJsxCanvasCtxAtom,
  ElementsToRerenderGLOBAL,
} from '../ui-jsx-canvas'
import type { MutableUtopiaCtxRefData } from './ui-jsx-canvas-contexts'
import { RerenderUtopiaCtxAtom, SceneLevelUtopiaCtxAtom } from './ui-jsx-canvas-contexts'
import { applyPropsParamToPassedProps } from './ui-jsx-canvas-props-utils'
import { runBlockUpdatingScope } from './ui-jsx-canvas-scope-utils'
import * as EP from '../../../core/shared/element-path'
import {
  createLookupRender,
  renderCoreElement,
  utopiaCanvasJSXLookup,
} from './ui-jsx-canvas-element-renderer-utils'
import type { ElementPath } from '../../../core/shared/project-file-types'
import { UTOPIA_INSTANCE_PATH, UTOPIA_PATH_KEY } from '../../../core/model/utopia-constants'
import { getPathsFromString, getUtopiaID } from '../../../core/shared/uid-utils'
import { useGetTopLevelElementsAndImports } from './ui-jsx-canvas-top-level-elements'
import { useGetCodeAndHighlightBounds } from './ui-jsx-canvas-execution-scope'
import { usePubSubAtomReadOnly } from '../../../core/shared/atom-with-pub-sub'
import { JSX_CANVAS_LOOKUP_FUNCTION_NAME } from '../../../core/shared/dom-utils'
import type { HookResultContext } from '../../../core/shared/javascript-cache'
import {
  ComponentStateDataAtom,
  updateComponentStateData,
  ComponentStateRecordingModeAtom,
} from '../../../core/shared/javascript-cache'
import { useAtom, useSetAtom } from 'jotai'
import { NO_OP } from '../../../core/shared/utils'
import { useForceUpdate } from '../../editor/hook-utils'

export type ComponentRendererComponent = React.ComponentType<
  React.PropsWithChildren<{
    [UTOPIA_INSTANCE_PATH]: ElementPath
    [UTOPIA_PATH_KEY]?: string
  }>
> & {
  topLevelElementName: string | null
  propertyControls?: PropertyControls
  utopiaType: 'UTOPIA_COMPONENT_RENDERER_COMPONENT'
}

export function isComponentRendererComponent(
  component:
    | ComponentRendererComponent
    | React.ComponentType<React.PropsWithChildren<unknown>>
    | null
    | undefined,
): component is ComponentRendererComponent {
  return (
    component != null &&
    typeof component === 'function' &&
    (component as ComponentRendererComponent).utopiaType === 'UTOPIA_COMPONENT_RENDERER_COMPONENT'
  )
}

function tryToGetInstancePath(
  maybePath: ElementPath | null,
  pathsString: string | null,
): ElementPath | null {
  const paths = getPathsFromString(pathsString)
  if (EP.isElementPath(maybePath)) {
    return maybePath
  } else if (paths.length > 0) {
    return paths[0]
  } else {
    return null
  }
}

const HookValueReff: {
  current: {
    [elementPathString: string]: {
      fiberReference: any
      hookValues: Array<any>
      forceUpdate: () => void
    }
  }
} = { current: {} }

export function getHookValueRef() {
  return { ...HookValueReff.current }
}

export function restoreHookValues(snapshot: typeof HookValueReff.current) {
  Object.values(snapshot).forEach(({ fiberReference, hookValues, forceUpdate }) => {
    forEachHook((hook, index) => {
      if (hookValues[index] === 'do-not-restore-state') {
        return
      }
      hook.memoizedState = hookValues[index]
      hook.baseState = hookValues[index]
      hook.queue.lastRenderedState = hookValues[index]
    }, findHooksAfterSentinel(fiberReference.memoizedState))

    fiberReference.memoizedProps = { ...fiberReference.memoizedProps }

    forceUpdate()
  })
}

export function createComponentRendererComponent(params: {
  topLevelElementName: string | null
  filePath: string
  mutableContextRef: React.MutableRefObject<MutableUtopiaCtxRefData>
}): ComponentRendererComponent {
  const Component = (realPassedPropsIncludingUtopiaSpecialStuff: any) => {
    const {
      [UTOPIA_INSTANCE_PATH]: instancePathAny, // TODO types?
      [UTOPIA_PATH_KEY]: pathsString, // TODO types?
      ...realPassedProps
    } = realPassedPropsIncludingUtopiaSpecialStuff

    const mutableContext = params.mutableContextRef.current[params.filePath].mutableContext

    const instancePath: ElementPath | null = tryToGetInstancePath(instancePathAny, pathsString)

    const componentIdHookGuardValue = `utopia-component-renderer-component-${optionalMap(
      EP.toString,
      instancePath,
    )}`
    React.useState(componentIdHookGuardValue)

    const forceUpdate = useForceUpdate()

    function shouldUpdate() {
      return (
        ElementsToRerenderGLOBAL.current === 'rerender-all-elements' ||
        ElementsToRerenderGLOBAL.current.some((er) => {
          return (
            (instancePath != null &&
              (EP.pathsEqual(er, instancePath) || EP.isParentComponentOf(instancePath, er))) ||
            isElementInChildrenPropTree(EP.toString(er), realPassedProps)
          )
        })
      )
    }

    const rerenderUtopiaContext = usePubSubAtomReadOnly(RerenderUtopiaCtxAtom, shouldUpdate)

    const { topLevelElements, imports } = useGetTopLevelElementsAndImports(
      params.filePath,
      shouldUpdate,
    )
    const { code, highlightBounds } = useGetCodeAndHighlightBounds(params.filePath, shouldUpdate)

    const utopiaJsxComponent: UtopiaJSXComponent | null =
      topLevelElements.find((elem): elem is UtopiaJSXComponent => {
        return isUtopiaJSXComponent(elem) && elem.name === params.topLevelElementName
      }) ?? null

    const shouldIncludeCanvasRootInTheSpy = rerenderUtopiaContext.shouldIncludeCanvasRootInTheSpy

    const hiddenInstances = rerenderUtopiaContext.hiddenInstances
    const displayNoneInstances = rerenderUtopiaContext.displayNoneInstances
    const sceneContext = usePubSubAtomReadOnly(SceneLevelUtopiaCtxAtom, shouldUpdate)

    let metadataContext: UiJsxCanvasContextData = usePubSubAtomReadOnly(
      UiJsxCanvasCtxAtom,
      shouldUpdate,
    )
    const updateInvalidatedPaths: DomWalkerInvalidatePathsCtxData = usePubSubAtomReadOnly(
      DomWalkerInvalidatePathsCtxAtom,
      shouldUpdate,
    )

    if (utopiaJsxComponent == null) {
      // If this element cannot be found, we want to purposefully cause a 'ReferenceError' to notify the user.
      throw new ReferenceError(`${params.topLevelElementName} is not defined`)
    }

    const setHookValues = useSetAtom(ComponentStateDataAtom)
    const [hookValuesMode] = useAtom(ComponentStateRecordingModeAtom)

    const appliedProps = optionalMap(
      (param) =>
        applyPropsParamToPassedProps(
          params.filePath,
          mutableContext.rootScope,
          mutableContext.requireResult,
          realPassedProps,
          param,
          { type: 'transparent' },
        ),
      utopiaJsxComponent.param,
    ) ?? { props: realPassedProps }

    let scope: MapLike<any> = {
      ...mutableContext.rootScope,
      ...appliedProps,
    }

    let codeError: Error | null = null

    // Protect against infinite recursion by taking the view that anything
    // beyond a particular depth is going infinite or is likely
    // to be out of control otherwise.
    if (instancePath != null && EP.depth(instancePath) > 100) {
      throw new Error(`Element hierarchy is too deep, potentially has become infinite.`)
    }

    const rootElementPath = optionalMap(
      (path) => EP.appendNewElementPath(path, getUtopiaID(utopiaJsxComponent.rootElement)),
      instancePath,
    )

    // either this updateInvalidatedPaths or the one in SpyWrapper is probably redundant
    if (shouldUpdate()) {
      updateInvalidatedPaths((invalidPaths) => {
        // Do not add `svg` elements that are the root element of a component.
        // As they will not be cleared by the DOM walker as they are not instances
        // of HTMLElement.
        const isSVGJSXElement =
          isJSXElement(utopiaJsxComponent.rootElement) &&
          isSVGElement(utopiaJsxComponent.rootElement.name)
        if (rootElementPath != null && !isSVGJSXElement) {
          return invalidPaths.add(EP.toString(rootElementPath))
        } else {
          return invalidPaths
        }
      })
    }

    React.useEffect(() => {
      if (instancePath == null) {
        return
      }
      const elementPathString = EP.toString(instancePath)
      /**
       * TODO move this to a dedicated helper and share code with dom-walker
       */
      /**
       * if a elementToFocusOn path points to a component instance, such as App/card-instance, the DOM will
       * only contain an element with the path App/card-instance:card-root. To be able to quickly find the "rootest" element
       * that belongs to a path, we use the ^= prefix search in querySelector.
       * The assumption is that querySelector will return the "topmost" DOM-element with the matching prefix,
       * which is the same as the "rootest" element we are looking for
       */
      const foundElement = document.querySelector(
        `[${UTOPIA_PATH_KEY}^="${EP.toString(instancePath)}"]`,
      ) as HTMLElement | null

      if (foundElement == null) {
        return // give up?
      }

      const foundFiber = (() => {
        const fiberName = Object.keys(foundElement).find((key) => key.startsWith(`__reactFiber$`))
        if (fiberName == null) {
          return null
        }
        return (foundElement as any)[fiberName]
      })()

      function walkUpUntilMatchingFiber(maybeMatchingFiber: any) {
        if (maybeMatchingFiber == null) {
          return null // we reached the root of a React tree
        }
        if (maybeMatchingFiber.memoizedState?.baseState === componentIdHookGuardValue) {
          return maybeMatchingFiber
        }
        // otherwise walk up, rinse, repeat
        return walkUpUntilMatchingFiber(maybeMatchingFiber.return)
      }

      const matchingFiber = walkUpUntilMatchingFiber(foundFiber)

      const firstHookAfterSentinel = findHooksAfterSentinel(matchingFiber.memoizedState)

      HookValueReff.current[elementPathString] = {
        fiberReference: matchingFiber,
        hookValues: [],
        forceUpdate: forceUpdate,
      }

      forEachHook((hook, index) => {
        const stateToStore =
          hook.queue == null ? 'do-not-restore-state' : hook.queue.lastRenderedState
        HookValueReff.current[elementPathString].hookValues.push(stateToStore)
      }, firstHookAfterSentinel)
    })

    React.useState('sentinel')

    if (utopiaJsxComponent.arbitraryJSBlock != null) {
      const hookResultContext: HookResultContext =
        rootElementPath === null
          ? { type: 'transparent' }
          : {
              type: 'active',
              mode: hookValuesMode,
              elementPath: rootElementPath,
              setHookResult: (hookId, value) =>
                setHookValues((currentValue) =>
                  updateComponentStateData(currentValue, rootElementPath)(hookId, value),
                ),
            }

      const lookupRenderer = createLookupRender(
        rootElementPath,
        scope,
        realPassedProps,
        mutableContext.requireResult,
        hiddenInstances,
        displayNoneInstances,
        mutableContext.fileBlobs,
        sceneContext.validPaths,
        undefined,
        metadataContext,
        updateInvalidatedPaths,
        hookResultContext,
        mutableContext.jsxFactoryFunctionName,
        shouldIncludeCanvasRootInTheSpy,
        params.filePath,
        imports,
        code,
        highlightBounds,
        rerenderUtopiaContext.editedText,
        null,
      )

      scope[JSX_CANVAS_LOOKUP_FUNCTION_NAME] = utopiaCanvasJSXLookup(
        utopiaJsxComponent.arbitraryJSBlock.elementsWithin,
        scope,
        lookupRenderer,
      )

      runBlockUpdatingScope(
        params.filePath,
        mutableContext.requireResult,
        utopiaJsxComponent.arbitraryJSBlock,
        scope,
        hookResultContext,
      )
    }

    function buildComponentRenderResult(element: JSXElementChild): React.ReactElement {
      const ownElementPath = optionalMap(
        (path) => EP.appendNewElementPath(path, getUtopiaID(element)),
        instancePath,
      )

      const hookResultContext: HookResultContext =
        ownElementPath === null
          ? { type: 'transparent' }
          : {
              type: 'active',
              mode: hookValuesMode,
              elementPath: ownElementPath,
              setHookResult: (hookId, value) =>
                setHookValues((currentValue) =>
                  updateComponentStateData(currentValue, ownElementPath)(hookId, value),
                ),
            }

      const renderedCoreElement = renderCoreElement(
        element,
        ownElementPath,
        mutableContext.rootScope,
        scope,
        realPassedProps,
        mutableContext.requireResult,
        hiddenInstances,
        displayNoneInstances,
        mutableContext.fileBlobs,
        sceneContext.validPaths,
        realPassedProps['data-uid'],
        undefined,
        metadataContext,
        updateInvalidatedPaths,
        hookResultContext,
        mutableContext.jsxFactoryFunctionName,
        codeError,
        shouldIncludeCanvasRootInTheSpy,
        params.filePath,
        imports,
        code,
        highlightBounds,
        rerenderUtopiaContext.editedText,
      )

      if (typeof renderedCoreElement === 'string' || typeof renderedCoreElement === 'number') {
        return <>{renderedCoreElement}</>
      } else {
        return renderedCoreElement
      }
    }
    React.useState('uto-sentinel')

    const buildResult = React.useRef<React.ReactElement | null>(
      buildComponentRenderResult(utopiaJsxComponent.rootElement),
    )
    if (shouldUpdate()) {
      buildResult.current = buildComponentRenderResult(utopiaJsxComponent.rootElement)
    }
    return buildResult.current
  }
  Component.displayName = `ComponentRenderer(${params.topLevelElementName})`
  Component.topLevelElementName = params.topLevelElementName
  Component.utopiaType = 'UTOPIA_COMPONENT_RENDERER_COMPONENT' as const
  return Component
}

// Checks if the element with the given elementPath is rendered in the props.children subtree
// LIMITATION: this function only checks props.children, so if the given element is rendered, but from a
// different prop, isElementInChildrenPropTree will return false
// If we will support renderProps, this should be updated to check other props which receive react elements
function isElementInChildrenPropTree(elementPath: string, props: any): boolean {
  const childrenArr = React.Children.toArray(props.children).filter(React.isValidElement)

  if (childrenArr.length === 0) {
    return false
  }
  const elementIsChild = childrenArr.some((c) => (c.props as any)[UTOPIA_PATH_KEY] === elementPath)
  if (elementIsChild) {
    return true
  } else {
    return childrenArr.some((c) => isElementInChildrenPropTree(elementPath, c.props))
  }
}

function forEachHook(callback: (hook: any, hookIndex: number) => void, firstHook: any) {
  if (firstHook == null) {
    return
  }
  let workingHook = firstHook
  let hookIndex = 0
  while (workingHook.memoizedState !== 'uto-sentinel') {
    callback(workingHook, hookIndex)
    hookIndex++
    workingHook = workingHook.next
  }
}

function findHooksAfterSentinel(maybeSentinel: any) {
  if (maybeSentinel == null) {
    return null
  }

  if (maybeSentinel.memoizedState === 'sentinel') {
    if (maybeSentinel.next?.memoizedState === 'uto-sentinel') {
      return null
    }
    return maybeSentinel.next
  }
  return findHooksAfterSentinel(maybeSentinel.next)
}
