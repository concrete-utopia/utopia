import * as React from 'react'
import { MapLike } from 'typescript'
import { EmptyScenePathForStoryboard } from '../../../core/model/scene-utils'
import { right } from '../../../core/shared/either'
import {
  ElementInstanceMetadata,
  emptyAttributeMetadatada,
  emptyComputedStyle,
  emptySpecialSizeMeasurements,
  getJSXElementNameNoPathName,
  JSXElement,
} from '../../../core/shared/element-template'
import { InstancePath, ScenePath, TemplatePath } from '../../../core/shared/project-file-types'
import { makeCanvasElementPropsSafe } from '../../../utils/canvas-react-utils'
import { UiJsxCanvasContextData } from '../ui-jsx-canvas'
import * as TP from '../../../core/shared/template-path'
import { renderComponentUsingJsxFactoryFunction } from './ui-jsx-canvas-element-renderer-utils'
import { getTopLevelElementName, useGetValidTemplatePaths } from './scene-root'
import { UTOPIA_UID_KEY } from '../../../core/model/utopia-constants'
import { getUtopiaID } from '../../../core/model/element-template-utils'
import { ComponentRendererComponent } from './ui-jsx-canvas-component-renderer'

export function buildSpyWrappedElement(
  jsx: JSXElement,
  finalProps: any,
  templatePath: InstancePath,
  metadataContext: UiJsxCanvasContextData,
  childrenTemplatePaths: Array<InstancePath>,
  childrenElements: Array<React.ReactNode>,
  Element: any,
  inScope: MapLike<any>,
  jsxFactoryFunctionName: string | null,
  shouldIncludeCanvasRootInTheSpy: boolean,
  focusedElementTemplatePath: TemplatePath | null,
  validPathsForCurrentScene: Array<InstancePath>,
): React.ReactElement {
  let props = {
    ...finalProps,
    key: TP.toComponentId(templatePath),
  }

  if (TP.pathsEqual(templatePath, focusedElementTemplatePath)) {
    // Replace the instance's UID with the definition's
    const originalComponent = inScope[jsx.name.baseVariable]
    const originalUID = (originalComponent as ComponentRendererComponent)?.originalUID
    props[UTOPIA_UID_KEY] = originalUID ?? getUtopiaID(jsx)
  }

  const childrenElementsOrNull = childrenElements.length > 0 ? childrenElements : null
  const spyCallback = (reportedProps: any) => {
    const instanceMetadata: ElementInstanceMetadata = {
      element: right(jsx),
      templatePath: templatePath,
      props: makeCanvasElementPropsSafe(reportedProps),
      globalFrame: null,
      localFrame: null,
      children: childrenTemplatePaths,
      componentInstance: false,
      specialSizeMeasurements: emptySpecialSizeMeasurements, // This is not the nicest, but the results from the DOM walker will override this anyways
      computedStyle: emptyComputedStyle,
      attributeMetadatada: emptyAttributeMetadatada,
    }
    const isChildOfRootScene = TP.pathsEqual(
      TP.scenePathForPath(templatePath),
      EmptyScenePathForStoryboard,
    )
    if (!isChildOfRootScene || shouldIncludeCanvasRootInTheSpy) {
      metadataContext.current.spyValues.metadata[TP.toComponentId(templatePath)] = instanceMetadata
    }
  }

  let scenePath = null
  const topLevelElementName = (childrenElements[0] as React.ReactElement)?.props?.elementToRender
    ?.topLevelElementName

  if (TP.pathsEqual(TP.parentPath(focusedElementTemplatePath), templatePath)) {
    scenePath = TP.scenePath([
      ...TP.scenePathForPath(templatePath).sceneElementPath,
      ...TP.elementPathForPath(templatePath),
    ])
  }

  const spyWrapperProps: SpyWrapperProps = {
    elementToRender: Element,
    spyCallback: spyCallback,
    inScope: inScope,
    jsxFactoryFunctionName: jsxFactoryFunctionName,
    scenePath: scenePath,
    topLevelElementName: topLevelElementName,
    validPathsForCurrentScene: validPathsForCurrentScene,
  }

  return renderComponentUsingJsxFactoryFunction(
    inScope,
    jsxFactoryFunctionName,
    SpyWrapper,
    {
      ...props,
      ...spyWrapperProps,
    },
    childrenElementsOrNull,
  )
}

interface SpyWrapperProps {
  spyCallback: (finalProps: any) => void
  elementToRender: React.ComponentType<any>
  inScope: MapLike<any>
  jsxFactoryFunctionName: string | null
  topLevelElementName: string | null
  scenePath: ScenePath | null
  validPathsForCurrentScene: Array<InstancePath>
}
const SpyWrapper: React.FunctionComponent<SpyWrapperProps> = (props) => {
  const {
    spyCallback,
    elementToRender: ElementToRender,
    inScope,
    jsxFactoryFunctionName,
    topLevelElementName,
    scenePath,
    validPathsForCurrentScene,
    ...passThroughProps
  } = props
  const validPaths = useGetValidTemplatePaths(topLevelElementName, scenePath ?? TP.scenePath([]))

  const cumulativeValidPathsAsString = [...validPathsForCurrentScene, ...validPaths]
    .map(TP.toString)
    .join(' ')

  spyCallback(passThroughProps)
  const result = renderComponentUsingJsxFactoryFunction(
    inScope,
    jsxFactoryFunctionName,
    ElementToRender,
    passThroughProps,
  )
  if (scenePath == null) {
    return result
  } else {
    // this element is promoted to be a temporary Scene
    return React.cloneElement(result, {
      'data-utopia-valid-paths': cumulativeValidPathsAsString,
      'data-utopia-scene-id': TP.elementPathToString(scenePath?.sceneElementPath),
    })
  }
}
SpyWrapper.displayName = 'SpyWapper'
