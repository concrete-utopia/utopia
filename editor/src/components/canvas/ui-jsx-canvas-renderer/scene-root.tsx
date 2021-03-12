import * as React from 'react'
import { View } from 'utopia-api'
import { useContextSelector } from 'use-context-selector'
import * as fastDeepEquals from 'fast-deep-equal'
import { getValidTemplatePaths } from '../../../core/model/element-template-utils'
import { left } from '../../../core/shared/either'
import {
  emptySpecialSizeMeasurements,
  emptyComputedStyle,
  JSXElement,
  emptyAttributeMetadatada,
} from '../../../core/shared/element-template'
import { InstancePath, ScenePath } from '../../../core/shared/project-file-types'
import { colorTheme, UtopiaStyles } from '../../../uuiui'
import { UiJsxCanvasContextData, UiJsxCanvasContext } from '../ui-jsx-canvas'
import {
  ComponentRendererComponent,
  isComponentRendererComponent,
} from './ui-jsx-canvas-component-renderer'
import {
  MutableUtopiaContext,
  ParentLevelUtopiaContext,
  RerenderUtopiaContext,
  SceneLevelUtopiaContext,
} from './ui-jsx-canvas-contexts'
import { renderComponentUsingJsxFactoryFunction } from './ui-jsx-canvas-element-renderer-utils'
import * as TP from '../../../core/shared/template-path'
import * as PP from '../../../core/shared/property-path'
import { betterReactMemo } from '../../../uuiui-deps'
import { jsxAttributesToProps } from '../../../core/shared/jsx-attributes'
import { getUtopiaIDFromJSXElement } from '../../../core/shared/uid-utils'
import utils from '../../../utils/utils'
import { PathForResizeContent } from '../../../core/model/scene-utils'

interface SceneProps {
  component?: React.ComponentType | null
  props?: any
  style?: React.CSSProperties
  'data-uid'?: string
  'data-label'?: string
}

function useRunSpy(
  scenePath: ScenePath,
  templatePath: InstancePath,
  componentName: string | null,
  props: SceneProps,
): void {
  const shouldIncludeCanvasRootInTheSpy = useContextSelector(
    RerenderUtopiaContext,
    (c) => c.shouldIncludeCanvasRootInTheSpy,
  )
  let metadataContext: UiJsxCanvasContextData = React.useContext(UiJsxCanvasContext)

  const resizesContent = Boolean(utils.path(PP.getElements(PathForResizeContent), props) ?? false)

  metadataContext.current.spyValues.scenes[TP.toString(scenePath)] = {
    scenePath: scenePath,
    templatePath: templatePath,
    component: componentName,
    sceneResizesContent: resizesContent,
    globalFrame: null,
    label: props['data-label'],
    style: props.style ?? {},
  }
  if (shouldIncludeCanvasRootInTheSpy) {
    metadataContext.current.spyValues.metadata[TP.toComponentId(templatePath)] = {
      element: left('Scene'),
      templatePath: templatePath,
      props: {},
      globalFrame: null,
      localFrame: null,
      children: [],
      componentInstance: false,
      specialSizeMeasurements: emptySpecialSizeMeasurements,
      computedStyle: emptyComputedStyle,
      attributeMetadatada: emptyAttributeMetadatada,
    }
  }
}

function getTopLevelElementName(
  componentRenderer: ComponentRendererComponent | React.ComponentType | null | undefined,
): string | null {
  if (isComponentRendererComponent(componentRenderer)) {
    return componentRenderer.topLevelElementName
  } else {
    return null
  }
}

function useGetValidTemplatePaths(
  topLevelElementName: string | null,
  scenePath: ScenePath,
): Array<InstancePath> {
  const utopiaJsxComponent = useContextSelector(RerenderUtopiaContext, (c) =>
    c.topLevelElements.get(topLevelElementName ?? ''),
  )
  if (utopiaJsxComponent != null) {
    return getValidTemplatePaths(utopiaJsxComponent, scenePath)
  }
  return []
}

interface SceneRootRendererProps {
  sceneElement: JSXElement
  style?: React.CSSProperties
}

export const SceneRootRenderer = betterReactMemo(
  'SceneRootRenderer',
  (props: SceneRootRendererProps) => {
    const mutableUtopiaContext = React.useContext(MutableUtopiaContext).current
    const inScope = mutableUtopiaContext.rootScope
    const requireResult = mutableUtopiaContext.requireResult
    const canvasIsLive = useContextSelector(RerenderUtopiaContext, (c) => c.canvasIsLive)
    const parentPath = useContextSelector(ParentLevelUtopiaContext, (c) => c.templatePath)
    const uid = getUtopiaIDFromJSXElement(props.sceneElement)

    if (parentPath == null) {
      throw new Error(`Utopia Error: no parent template path provided for Scene (uid: ${uid})`)
    }

    const sceneProps: SceneProps = React.useMemo(
      () => jsxAttributesToProps(inScope, props.sceneElement.props, requireResult),
      [inScope, props.sceneElement.props, requireResult],
    )
    const templatePath = React.useMemo(() => TP.appendToPath(parentPath, uid), [parentPath, uid])
    const scenePath = TP.scenePathForElementAtInstancePath(templatePath)

    const topLevelElementName = getTopLevelElementName(sceneProps.component)

    const validPaths = useGetValidTemplatePaths(topLevelElementName, scenePath)

    useRunSpy(scenePath, templatePath, topLevelElementName, sceneProps)

    const rootElement =
      sceneProps.component == null
        ? null
        : renderComponentUsingJsxFactoryFunction(
            inScope,
            mutableUtopiaContext.jsxFactoryFunctionName,
            sceneProps.component,
            sceneProps.props,
            undefined,
          )

    const sceneStyle: React.CSSProperties = {
      position: 'relative',
      backgroundColor: colorTheme.emphasizedBackground.value,
      boxShadow: canvasIsLive
        ? UtopiaStyles.scene.live.boxShadow
        : UtopiaStyles.scene.editing.boxShadow,
      ...props.style,
      ...sceneProps.style,
    }

    return (
      <SceneLevelUtopiaContext.Provider value={{ validPaths: validPaths, scenePath: scenePath }}>
        <View
          data-utopia-scene-id={TP.toString(scenePath)}
          data-utopia-valid-paths={validPaths.map(TP.toString).join(' ')}
          style={sceneStyle}
        >
          {rootElement}
        </View>
      </SceneLevelUtopiaContext.Provider>
    )
  },
  (prevProps, nextProps) => {
    // TODO BALAZS remove me, this is only needed until I fix the pragma
    return (
      prevProps.sceneElement === nextProps.sceneElement &&
      fastDeepEquals(prevProps.style, nextProps.style)
    )
  },
  true,
)
