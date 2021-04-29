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
import { forceNotNull, optionalMap } from '../../../core/shared/optional-utils'
import { UiJsxCanvasContext, UiJsxCanvasContextData } from '../ui-jsx-canvas'
import {
  MutableUtopiaContextProps,
  RerenderUtopiaContext,
  SceneLevelUtopiaContext,
} from './ui-jsx-canvas-contexts'
import { applyPropsParamToPassedProps } from './ui-jsx-canvas-props-utils'
import { runBlockUpdatingScope } from './ui-jsx-canvas-scope-utils'
import * as TP from '../../../core/shared/template-path'
import {
  createLookupRender,
  renderCoreElement,
  utopiaCanvasJSXLookup,
} from './ui-jsx-canvas-element-renderer-utils'
import { useContextSelector } from 'use-context-selector'
import { InstancePath, TemplatePath } from '../../../core/shared/project-file-types'
import { UTOPIA_INSTANCE_PATH, UTOPIA_PATHS_KEY } from '../../../core/model/utopia-constants'
import { JSX_CANVAS_LOOKUP_FUNCTION_NAME } from '../../../core/workers/parser-printer/parser-printer-utils'
import { useEditorState } from '../../editor/store/store-hook'
import { getFileForName } from '../../editor/store/editor-state'
import { mapDropNulls } from '../../../core/shared/array-utils'
import {
  getParseSuccessOrTransientForFilePath,
  useGetTopLevelElements,
} from './ui-jsx-canvas-top-level-elements'
import { getPathsFromString } from '../../../core/shared/uid-utils'

export type ComponentRendererComponent = React.ComponentType<{
  [UTOPIA_INSTANCE_PATH]: TemplatePath
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
  instancePath: InstancePath | null,
  pathsString: string | null,
): InstancePath {
  const paths = getPathsFromString(pathsString)
  if (TP.isInstancePath(instancePath)) {
    return instancePath
  } else if (paths.length > 0) {
    return paths[0]
  } else {
    throw new Error(`Utopia Error: Instance Path is not provided for ${topLevelElementName}.`)
  }
}

export function createComponentRendererComponent(params: {
  topLevelElementName: string
  filePath: string
  mutableContextRef: React.MutableRefObject<MutableUtopiaContextProps>
}): ComponentRendererComponent {
  const Component = (realPassedPropsIncludingUtopiaSpecialStuff: any) => {
    const {
      [UTOPIA_INSTANCE_PATH]: instancePathAny, // TODO types?
      [UTOPIA_PATHS_KEY]: pathsString, // TODO types?
      ...realPassedProps
    } = realPassedPropsIncludingUtopiaSpecialStuff

    const instancePath: InstancePath = tryToGetInstancePath(
      params.topLevelElementName,
      instancePathAny,
      pathsString,
    )

    const mutableContext = params.mutableContextRef.current[params.filePath].mutableContext

    const topLevelElements = useGetTopLevelElements(params.filePath)

    const utopiaJsxComponent: UtopiaJSXComponent | null =
      topLevelElements.find((elem): elem is UtopiaJSXComponent => {
        return isUtopiaJSXComponent(elem) && elem.name === params.topLevelElementName
      }) ?? null

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

    const appliedProps = optionalMap(
      (param) =>
        applyPropsParamToPassedProps(
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

    const rootTemplatePath = TP.appendNewElementPath(
      instancePath,
      getUtopiaID(utopiaJsxComponent.rootElement),
    )

    if (utopiaJsxComponent.arbitraryJSBlock != null) {
      const lookupRenderer = createLookupRender(
        rootTemplatePath,
        scope,
        realPassedProps,
        mutableContext.requireResult,
        hiddenInstances,
        mutableContext.fileBlobs,
        sceneContext.validPaths,
        undefined,
        metadataContext,
        mutableContext.jsxFactoryFunctionName,
        shouldIncludeCanvasRootInTheSpy,
        params.filePath,
      )

      scope[JSX_CANVAS_LOOKUP_FUNCTION_NAME] = utopiaCanvasJSXLookup(
        utopiaJsxComponent.arbitraryJSBlock.elementsWithin,
        scope,
        lookupRenderer,
      )

      runBlockUpdatingScope(
        mutableContext.requireResult,
        utopiaJsxComponent.arbitraryJSBlock,
        scope,
      )
    }

    function buildComponentRenderResult(element: JSXElementChild): React.ReactElement {
      if (isJSXFragment(element)) {
        return <>{element.children.map(buildComponentRenderResult)}</>
      } else {
        const ownTemplatePath = TP.appendNewElementPath(instancePath, getUtopiaID(element))

        return renderCoreElement(
          element,
          ownTemplatePath,
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
          mutableContext.jsxFactoryFunctionName,
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
