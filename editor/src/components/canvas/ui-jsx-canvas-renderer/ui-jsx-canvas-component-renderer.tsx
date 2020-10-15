import * as React from 'react'
import { MapLike } from 'typescript'
import { PropertyControls } from 'utopia-api'
import { getUtopiaID } from '../../../core/model/element-template-utils'
import { JSXElementChild, isJSXFragment } from '../../../core/shared/element-template'
import { optionalMap } from '../../../core/shared/optional-utils'
import { UiJsxCanvasContext, UiJsxCanvasContextData } from '../ui-jsx-canvas'
import {
  MutableUtopiaContext,
  RerenderUtopiaContext,
  SceneLevelUtopiaContext,
} from './ui-jsx-canvas-contexts'
import { applyPropsParamToPassedProps } from './ui-jsx-canvas-props-utils'
import { runBlockUpdatingScope } from './ui-jsx-canvas-scope-utils'
import * as TP from '../../../core/shared/template-path'
import { renderCoreElement } from './ui-jsx-canvas-element-renderer-utils'

export type ComponentRendererComponent = React.ComponentType<any> & {
  topLevelElementName: string
  propertyControls?: PropertyControls
}

export function isComponentRendererComponent(
  component: ComponentRendererComponent | React.ComponentType | null,
): component is ComponentRendererComponent {
  return (
    component != null &&
    typeof component === 'function' &&
    (component as ComponentRendererComponent).topLevelElementName != null
  )
}

export function createComponentRendererComponent(params: {
  topLevelElementName: string
}): ComponentRendererComponent {
  const Component = (realPassedProps: any) => {
    const { current: mutableContext } = React.useContext(MutableUtopiaContext)
    const rerenderUtopiaContext = React.useContext(RerenderUtopiaContext)
    const sceneContext = React.useContext(SceneLevelUtopiaContext)

    let metadataContext: UiJsxCanvasContextData = React.useContext(UiJsxCanvasContext)

    const utopiaJsxComponent = rerenderUtopiaContext.topLevelElements.get(
      params.topLevelElementName,
    )

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

    const scenePath = sceneContext.scenePath
    let codeError: Error | null = null

    if (utopiaJsxComponent.arbitraryJSBlock != null) {
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
        // so. this template path is ONLY correct if this component is used as a Scene Root.
        // if this component is used as an instance inside some other component, this template path will be garbage.
        // but! worry not, because in cases this is an instance, we are not running the DOM-walker and we discard the spy results
        // so it is not an issue that we have a false template path
        const ownTemplatePath = TP.instancePath(scenePath, [getUtopiaID(element)])

        return renderCoreElement(
          element,
          ownTemplatePath,
          mutableContext.rootScope,
          scope,
          realPassedProps,
          mutableContext.requireResult,
          rerenderUtopiaContext.hiddenInstances,
          mutableContext.fileBlobs,
          sceneContext.validPaths,
          realPassedProps['data-uid'],
          undefined,
          metadataContext,
          mutableContext.jsxFactoryFunctionName,
          codeError,
          rerenderUtopiaContext.shouldIncludeCanvasRootInTheSpy,
        )
      }
    }

    return buildComponentRenderResult(utopiaJsxComponent.rootElement)
  }
  Component.displayName = `ComponentRenderer(${params.topLevelElementName})`
  Component.topLevelElementName = params.topLevelElementName
  return Component
}
