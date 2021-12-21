import React from 'react'
import { MapLike } from 'typescript'
import { PropertyControls } from 'utopia-api'
import { getUtopiaID } from '../../../core/model/element-template-utils'
import {
  JSXElementChild,
  isJSXFragment,
  isUtopiaJSXComponent,
  UtopiaJSXComponent,
} from '../../../core/shared/element-template'
import { optionalMap } from '../../../core/shared/optional-utils'
import {
  DomWalkerInvalidatePathsCtxAtom,
  DomWalkerInvalidatePathsCtxData,
  UiJsxCanvasCtxAtom,
  UiJsxCanvasContextData,
} from '../ui-jsx-canvas'
import {
  MutableUtopiaCtxRefData,
  RerenderUtopiaCtxAtom,
  SceneLevelUtopiaCtxAtom,
  UtopiaProjectCtxAtom,
} from './ui-jsx-canvas-contexts'
import { applyPropsParamToPassedProps } from './ui-jsx-canvas-props-utils'
import { runBlockUpdatingScope } from './ui-jsx-canvas-scope-utils'
import * as EP from '../../../core/shared/element-path'
import {
  createLookupRender,
  renderCoreElement,
  utopiaCanvasJSXLookup,
} from './ui-jsx-canvas-element-renderer-utils'
import { useContextSelector } from 'use-context-selector'
import { ElementPath } from '../../../core/shared/project-file-types'
import { UTOPIA_INSTANCE_PATH, UTOPIA_PATHS_KEY } from '../../../core/model/utopia-constants'
import { getPathsFromString } from '../../../core/shared/uid-utils'
import { useGetTopLevelElementsAndImports } from './ui-jsx-canvas-top-level-elements'
import { useGetCodeAndHighlightBounds } from './ui-jsx-canvas-execution-scope'
import { usePubSubAtomReadOnly } from '../../../core/shared/atom-with-pub-sub'
import { JSX_CANVAS_LOOKUP_FUNCTION_NAME } from '../../../core/shared/dom-utils'

export type ComponentRendererComponent = React.ComponentType<{
  [UTOPIA_INSTANCE_PATH]: ElementPath
  [UTOPIA_PATHS_KEY]?: string
}> & {
  topLevelElementName: string | null
  propertyControls?: PropertyControls
  utopiaType: 'UTOPIA_COMPONENT_RENDERER_COMPONENT'
}

export function isComponentRendererComponent(
  component: ComponentRendererComponent | React.ComponentType | null | undefined,
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

function useIsActiveCanvasDrag() {
  return usePubSubAtomReadOnly(UtopiaProjectCtxAtom).transientFilesState != null
}

export function createComponentRendererComponent(params: {
  topLevelElementName: string | null
  filePath: string
  mutableContextRef: React.MutableRefObject<MutableUtopiaCtxRefData>
}): ComponentRendererComponent {
  const Component = (realPassedPropsIncludingUtopiaSpecialStuff: any) => {
    const {
      [UTOPIA_INSTANCE_PATH]: instancePathAny, // TODO types?
      [UTOPIA_PATHS_KEY]: pathsString, // TODO types?
      ...realPassedProps
    } = realPassedPropsIncludingUtopiaSpecialStuff

    const mutableContext = params.mutableContextRef.current[params.filePath].mutableContext

    const { topLevelElements, imports } = useGetTopLevelElementsAndImports(params.filePath)
    const { code, highlightBounds } = useGetCodeAndHighlightBounds(params.filePath)

    const utopiaJsxComponent: UtopiaJSXComponent | null =
      topLevelElements.find((elem): elem is UtopiaJSXComponent => {
        return isUtopiaJSXComponent(elem) && elem.name === params.topLevelElementName
      }) ?? null

    const rerenderUtopiaContext = usePubSubAtomReadOnly(RerenderUtopiaCtxAtom)
    const shouldIncludeCanvasRootInTheSpy = rerenderUtopiaContext.shouldIncludeCanvasRootInTheSpy

    const hiddenInstances = rerenderUtopiaContext.hiddenInstances
    const sceneContext = usePubSubAtomReadOnly(SceneLevelUtopiaCtxAtom)

    let metadataContext: UiJsxCanvasContextData = usePubSubAtomReadOnly(UiJsxCanvasCtxAtom)
    const updateInvalidatedPaths: DomWalkerInvalidatePathsCtxData = usePubSubAtomReadOnly(
      DomWalkerInvalidatePathsCtxAtom,
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

    const instancePath: ElementPath | null = tryToGetInstancePath(instancePathAny, pathsString)

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

    const activeCanvasDrag = useIsActiveCanvasDrag()

    const previousJsxComponent = React.useRef<JSXElementChild>()
    const memoizedComponentResult = React.useRef<React.ReactElement>()

    if (utopiaJsxComponent.arbitraryJSBlock != null) {
      const lookupRenderer = createLookupRender(
        rootElementPath,
        scope,
        realPassedProps,
        mutableContext.requireResult,
        hiddenInstances,
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
      )
    }

    function buildComponentRenderResult(element: JSXElementChild): React.ReactElement {
      if (isJSXFragment(element)) {
        return <>{element.children.map(buildComponentRenderResult)}</>
      } else {
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
        )

        if (typeof renderedCoreElement === 'string' || typeof renderedCoreElement === 'number') {
          return <>{renderedCoreElement}</>
        } else {
          return renderedCoreElement
        }
      }
    }

    if (
      activeCanvasDrag &&
      memoizedComponentResult.current != null &&
      previousJsxComponent.current != null &&
      previousJsxComponent.current === utopiaJsxComponent.rootElement
    ) {
      return memoizedComponentResult.current
    } else {
      previousJsxComponent.current = utopiaJsxComponent.rootElement
      memoizedComponentResult.current = buildComponentRenderResult(utopiaJsxComponent.rootElement)
      return memoizedComponentResult.current
    }
  }
  Component.displayName = `ComponentRenderer(${params.topLevelElementName})`
  Component.topLevelElementName = params.topLevelElementName
  Component.utopiaType = 'UTOPIA_COMPONENT_RENDERER_COMPONENT' as const
  return Component
}

function useMaybeRerender(rerender: () => React.ReactElement): React.ReactElement {
  const cachedResultRef = React.useRef<React.ReactElement>()

  const useCache = false

  if (useCache && cachedResultRef.current != null) {
    return cachedResultRef.current
  } else {
    const result = rerender()
    cachedResultRef.current = result
    return cachedResultRef.current
  }
}
