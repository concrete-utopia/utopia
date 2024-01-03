import React from 'react'
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
import { useContextSelector } from 'use-context-selector'
import type { ElementPath } from '../../../core/shared/project-file-types'
import { UTOPIA_INSTANCE_PATH, UTOPIA_PATH_KEY } from '../../../core/model/utopia-constants'
import { getPathsFromString, getUtopiaID } from '../../../core/shared/uid-utils'
import { useGetTopLevelElementsAndImports } from './ui-jsx-canvas-top-level-elements'
import { useGetCodeAndHighlightBounds } from './ui-jsx-canvas-execution-scope'
import { usePubSubAtomReadOnly } from '../../../core/shared/atom-with-pub-sub'
import { JSX_CANVAS_LOOKUP_FUNCTION_NAME } from '../../../core/shared/dom-utils'
import { isFeatureEnabled } from '../../../utils/feature-switches'

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

    const appliedProps = optionalMap(
      (param) =>
        applyPropsParamToPassedProps(
          params.filePath,
          mutableContext.rootScope,
          mutableContext.requireResult,
          realPassedProps,
          param,
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

    let definedWithinWithValues: MapLike<any> = {}

    if (utopiaJsxComponent.arbitraryJSBlock != null) {
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

      definedWithinWithValues = runBlockUpdatingScope(
        params.filePath,
        mutableContext.requireResult,
        utopiaJsxComponent.arbitraryJSBlock,
        scope,
      )
    }

    function buildComponentRenderResult(element: JSXElementChild): React.ReactElement {
      const ownElementPath = optionalMap(
        (path) => EP.appendNewElementPath(path, getUtopiaID(element)),
        instancePath,
      )

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
        mutableContext.jsxFactoryFunctionName,
        codeError,
        shouldIncludeCanvasRootInTheSpy,
        params.filePath,
        imports,
        code,
        highlightBounds,
        rerenderUtopiaContext.editedText,
        definedWithinWithValues,
        false,
      )

      if (typeof renderedCoreElement === 'string' || typeof renderedCoreElement === 'number') {
        return <>{renderedCoreElement}</>
      } else {
        return renderedCoreElement
      }
    }

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
