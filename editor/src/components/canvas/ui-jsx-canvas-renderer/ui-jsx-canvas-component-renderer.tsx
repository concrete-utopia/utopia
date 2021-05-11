import * as React from 'react'
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
import { UiJsxCanvasContext, UiJsxCanvasContextData } from '../ui-jsx-canvas'
import {
  MutableUtopiaContextProps,
  RerenderUtopiaContext,
  SceneLevelUtopiaContext,
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
import { JSX_CANVAS_LOOKUP_FUNCTION_NAME } from '../../../core/workers/parser-printer/parser-printer-utils'
import { useGetTopLevelElements } from './ui-jsx-canvas-top-level-elements'
import { getPathsFromString } from '../../../core/shared/uid-utils'
import { useKeepReferenceEqualityIfPossible } from '../../../utils/react-performance'

export type ComponentRendererComponent = React.ComponentType<{
  [UTOPIA_INSTANCE_PATH]: ElementPath
  [UTOPIA_PATHS_KEY]?: string
}> & {
  topLevelElementName: string
  propertyControls?: PropertyControls
}

export function isComponentRendererComponent(
  component: ComponentRendererComponent | React.ComponentType | null | undefined,
): component is ComponentRendererComponent {
  return (
    component != null &&
    typeof component === 'function' &&
    (component as ComponentRendererComponent).topLevelElementName != null
  )
}

function tryToGetInstancePath(
  topLevelElementName: string,
  maybePath: ElementPath | null,
  pathsString: string | null,
): ElementPath {
  const paths = getPathsFromString(pathsString)
  if (EP.isElementPath(maybePath)) {
    return maybePath
  } else if (paths.length > 0) {
    return paths[0]
  } else {
    throw new Error(`Utopia Error: Instance Path is not provided for ${topLevelElementName}.`)
  }
}

function useGetTopLevelComponent(
  filePath: string,
  topLevelElementName: string,
): UtopiaJSXComponent | null {
  const topLevelElements = useGetTopLevelElements(filePath)

  const utopiaJsxComponent: UtopiaJSXComponent | null =
    topLevelElements.find((elem): elem is UtopiaJSXComponent => {
      return isUtopiaJSXComponent(elem) && elem.name === topLevelElementName
    }) ?? null

  return useKeepReferenceEqualityIfPossible(utopiaJsxComponent)
}

function useCachedAgainstComponentAndProps<T>(
  utopiaJsxComponent: UtopiaJSXComponent | null,
  props: any,
  fn: () => T,
): T {
  return React.useMemo(fn, [utopiaJsxComponent, props])
}

export function createComponentRendererComponent(params: {
  topLevelElementName: string
  filePath: string
  mutableContextRef: React.MutableRefObject<MutableUtopiaContextProps>
}): ComponentRendererComponent {
  const Component = (realPassedPropsIncludingUtopiaSpecialStuff: any) => {
    const utopiaJsxComponent = useGetTopLevelComponent(params.filePath, params.topLevelElementName)

    const {
      [UTOPIA_INSTANCE_PATH]: instancePathAny, // TODO types?
      [UTOPIA_PATHS_KEY]: pathsString, // TODO types?
      ...realPassedProps
    } = realPassedPropsIncludingUtopiaSpecialStuff

    const instancePath: ElementPath = tryToGetInstancePath(
      params.topLevelElementName,
      instancePathAny,
      pathsString,
    )

    const rootScope = useCachedAgainstComponentAndProps(
      utopiaJsxComponent,
      realPassedPropsIncludingUtopiaSpecialStuff,
      () => params.mutableContextRef.current[params.filePath].mutableContext.rootScope,
    )

    const requireResult = useCachedAgainstComponentAndProps(
      utopiaJsxComponent,
      realPassedPropsIncludingUtopiaSpecialStuff,
      () => params.mutableContextRef.current[params.filePath].mutableContext.requireResult,
    )

    const fileBlobs = useCachedAgainstComponentAndProps(
      utopiaJsxComponent,
      realPassedPropsIncludingUtopiaSpecialStuff,
      () => params.mutableContextRef.current[params.filePath].mutableContext.fileBlobs,
    )

    const jsxFactoryFunctionName = useCachedAgainstComponentAndProps(
      utopiaJsxComponent,
      realPassedPropsIncludingUtopiaSpecialStuff,
      () => params.mutableContextRef.current[params.filePath].mutableContext.jsxFactoryFunctionName,
    )

    const shouldIncludeCanvasRootInTheSpy = useContextSelector(
      RerenderUtopiaContext,
      (c) => c.shouldIncludeCanvasRootInTheSpy,
    )
    const hiddenInstances = useContextSelector(RerenderUtopiaContext, (c) => c.hiddenInstances)
    const sceneContext = React.useContext(SceneLevelUtopiaContext)

    let metadataContext: UiJsxCanvasContextData = React.useContext(UiJsxCanvasContext)

    if (utopiaJsxComponent == null) {
      // If this element cannot be found, we want to purposefully cause a 'ReferenceError' to notify the user.
      throw new ReferenceError(`${params.topLevelElementName} is not defined`)
    }

    const appliedProps = useKeepReferenceEqualityIfPossible(
      optionalMap(
        (param) => applyPropsParamToPassedProps(rootScope, requireResult, realPassedProps, param),
        utopiaJsxComponent.param,
      ) ?? { props: realPassedProps },
    )

    let codeError: Error | null = null

    const elementScope: MapLike<any> = useCachedAgainstComponentAndProps(
      utopiaJsxComponent,
      realPassedPropsIncludingUtopiaSpecialStuff,
      () => {
        let workingScope: MapLike<any> = {
          ...rootScope,
          ...appliedProps,
        }

        const rootElementPath = EP.appendNewElementPath(
          instancePath,
          getUtopiaID(utopiaJsxComponent.rootElement),
        )

        if (utopiaJsxComponent.arbitraryJSBlock != null) {
          const lookupRenderer = createLookupRender(
            rootElementPath,
            workingScope,
            realPassedProps,
            requireResult,
            hiddenInstances,
            fileBlobs,
            sceneContext.validPaths,
            undefined,
            metadataContext,
            jsxFactoryFunctionName,
            shouldIncludeCanvasRootInTheSpy,
            params.filePath,
          )

          workingScope[JSX_CANVAS_LOOKUP_FUNCTION_NAME] = utopiaCanvasJSXLookup(
            utopiaJsxComponent.arbitraryJSBlock.elementsWithin,
            workingScope,
            lookupRenderer,
          )

          runBlockUpdatingScope(requireResult, utopiaJsxComponent.arbitraryJSBlock, workingScope)
        }

        return workingScope
      },
    )

    function buildComponentRenderResult(element: JSXElementChild): React.ReactElement {
      if (isJSXFragment(element)) {
        return <>{element.children.map(buildComponentRenderResult)}</>
      } else {
        const ownElementPath = EP.appendNewElementPath(instancePath, getUtopiaID(element))

        return renderCoreElement(
          element,
          ownElementPath,
          rootScope,
          elementScope,
          realPassedProps,
          requireResult,
          hiddenInstances,
          fileBlobs,
          sceneContext.validPaths,
          realPassedProps['data-uid'],
          undefined,
          metadataContext,
          jsxFactoryFunctionName,
          codeError,
          shouldIncludeCanvasRootInTheSpy,
          params.filePath,
        )
      }
    }

    return buildComponentRenderResult(utopiaJsxComponent.rootElement)
  }
  Component.displayName = `ComponentRenderer(${params.topLevelElementName})`
  Component.topLevelElementName = params.topLevelElementName
  return Component
}
