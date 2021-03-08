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
import { useContextSelector } from 'use-context-selector'
import { UTOPIA_TEMPLATE_PATH_KEY, UTOPIA_UID_KEY } from '../../../core/model/utopia-constants'

export type ComponentRendererComponent = React.ComponentType<any> & {
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

export function createComponentRendererComponent(params: {
  topLevelElementName: string
}): ComponentRendererComponent {
  const Component = (realPassedProps: any) => {
    const { current: mutableContext } = React.useContext(MutableUtopiaContext)
    const utopiaJsxComponent = useContextSelector(RerenderUtopiaContext, (c) =>
      c.topLevelElements.get(params.topLevelElementName),
    )
    const shouldIncludeCanvasRootInTheSpy = useContextSelector(
      RerenderUtopiaContext,
      (c) => c.shouldIncludeCanvasRootInTheSpy,
    )
    const hiddenInstances = useContextSelector(RerenderUtopiaContext, (c) => c.hiddenInstances)
    const temporarySceneTemplatePath = useContextSelector(
      RerenderUtopiaContext,
      (c) => c.temporarySceneTemplatePath,
    )
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

    // ONLY correct if this component is used as a Scene Root.
    const contextScenePath = sceneContext.scenePath
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
        const realTemplatePath = realPassedProps[UTOPIA_TEMPLATE_PATH_KEY]
        const uid = realPassedProps[UTOPIA_UID_KEY] || getUtopiaID(element)
        const ownTemplatePath = realTemplatePath ?? TP.instancePath(contextScenePath, [uid])

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
          temporarySceneTemplatePath,
        )
      }
    }

    return buildComponentRenderResult(utopiaJsxComponent.rootElement)
  }
  Component.displayName = `ComponentRenderer(${params.topLevelElementName})`
  Component.topLevelElementName = params.topLevelElementName
  return Component
}
