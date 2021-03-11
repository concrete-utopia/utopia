import * as React from 'react'
import { MapLike } from 'typescript'
import { EmptyScenePathForStoryboard } from '../../../core/model/scene-utils'
import { right } from '../../../core/shared/either'
import {
  ElementInstanceMetadata,
  emptyAttributeMetadatada,
  emptyComputedStyle,
  emptySpecialSizeMeasurements,
  JSXElement,
} from '../../../core/shared/element-template'
import { InstancePath } from '../../../core/shared/project-file-types'
import { makeCanvasElementPropsSafe } from '../../../utils/canvas-react-utils'
import { UiJsxCanvasContextData } from '../ui-jsx-canvas'
import * as TP from '../../../core/shared/template-path'
import { renderComponentUsingJsxFactoryFunction } from './ui-jsx-canvas-element-renderer-utils'

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
): React.ReactElement {
  const props = {
    ...finalProps,
    key: TP.toComponentId(templatePath),
  }
  const childrenElementsOrNull = childrenElements.length > 0 ? childrenElements : null
  const spyCallback = (reportedProps: any) => {
    /** This is not so nice, but the way to know if something is an emotion component is
     * that it adds some extra properties to the Element itself, like __emotion_base,
     * TODO move this out of metadata once we have syledcomponent editing
     */
    const isEmotionComponent = Element['__emotion_base'] != null
    const instanceMetadata: ElementInstanceMetadata = {
      element: right(jsx),
      templatePath: templatePath,
      props: makeCanvasElementPropsSafe(reportedProps),
      globalFrame: null,
      localFrame: null,
      children: childrenTemplatePaths,
      componentInstance: false,
      isEmotionComponent: isEmotionComponent,
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
  const spyWrapperProps: SpyWrapperProps = {
    elementToRender: Element,
    spyCallback: spyCallback,
    inScope: inScope,
    jsxFactoryFunctionName: jsxFactoryFunctionName,
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
}
const SpyWrapper: React.FunctionComponent<SpyWrapperProps> = (props) => {
  const {
    spyCallback,
    elementToRender: ElementToRender,
    inScope,
    jsxFactoryFunctionName,
    ...passThroughProps
  } = props
  spyCallback(passThroughProps)
  return renderComponentUsingJsxFactoryFunction(
    inScope,
    jsxFactoryFunctionName,
    ElementToRender,
    passThroughProps,
  )
}
SpyWrapper.displayName = 'SpyWapper'
