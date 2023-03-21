import React from 'react'
import { MapLike } from 'typescript'
import { Either, foldEither, left, right } from '../../../core/shared/either'
import {
  ElementInstanceMetadata,
  emptyAttributeMetadatada,
  emptyComputedStyle,
  emptySpecialSizeMeasurements,
  JSXElementLike,
  isJSXElement,
  ChildOrAttribute,
  JSXElementChild,
  childOrBlockIsChild,
  isJSXConditionalExpression,
  JSXConditionalExpression,
  ConditionValue,
} from '../../../core/shared/element-template'
import { ElementPath, Imports } from '../../../core/shared/project-file-types'
import { makeCanvasElementPropsSafe } from '../../../utils/canvas-react-utils'
import type { DomWalkerInvalidatePathsCtxData, UiJsxCanvasContextData } from '../ui-jsx-canvas'
import * as EP from '../../../core/shared/element-path'
import { renderComponentUsingJsxFactoryFunction } from './ui-jsx-canvas-element-renderer-utils'
import { importInfoFromImportDetails } from '../../../core/model/project-file-utils'
import { jsxSimpleAttributeToValue } from '../../../core/shared/jsx-attributes'

export function addFakeSpyEntry(
  metadataContext: UiJsxCanvasContextData,
  elementPath: ElementPath,
  elementOrAttribute: ChildOrAttribute,
  filePath: string,
  imports: Imports,
  conditionValue: ConditionValue,
): void {
  let element: Either<string, JSXElementChild>
  if (childOrBlockIsChild(elementOrAttribute)) {
    element = right(elementOrAttribute)
  } else {
    const simpleAttributeValue = jsxSimpleAttributeToValue(elementOrAttribute)
    element = left(
      foldEither(
        () => '(unknown)',
        (value) => {
          if (value === null) {
            return 'null'
          } else if (value === undefined) {
            return 'undefined'
          } else {
            return value.toString()
          }
        },
        simpleAttributeValue,
      ),
    )
  }
  const instanceMetadata: ElementInstanceMetadata = {
    element: element,
    elementPath: elementPath,
    globalFrame: null,
    localFrame: null,
    componentInstance: false,
    isEmotionOrStyledComponent: false,
    specialSizeMeasurements: emptySpecialSizeMeasurements,
    computedStyle: emptyComputedStyle,
    attributeMetadatada: emptyAttributeMetadatada,
    label: null,
    importInfo: foldEither(
      () => {
        return null
      },
      (e) => {
        if (isJSXElement(e)) {
          return importInfoFromImportDetails(e.name, imports, filePath)
        } else {
          return null
        }
      },
      element,
    ),
    conditionValue: conditionValue,
  }
  const elementPathString = EP.toComponentId(elementPath)
  metadataContext.current.spyValues.metadata[elementPathString] = instanceMetadata
}

export function buildSpyWrappedElement(
  jsx: JSXElementLike,
  finalProps: any,
  elementPath: ElementPath,
  metadataContext: UiJsxCanvasContextData,
  updateInvalidatedPaths: DomWalkerInvalidatePathsCtxData,
  childrenElements: Array<React.ReactChild>,
  Element: any,
  inScope: MapLike<any>,
  jsxFactoryFunctionName: string | null,
  shouldIncludeCanvasRootInTheSpy: boolean,
  imports: Imports,
  filePath: string,
): React.ReactElement {
  const props = {
    ...finalProps,
    key: EP.toComponentId(elementPath),
  }
  const spyCallback = (reportedProps: any) => {
    /** This is not so nice, but the way to know if something is an emotion component is
     * that it adds some extra properties to the Element itself, like __emotion_base,
     * TODO move this out of metadata once we have syledcomponent editing
     */
    const isEmotionComponent = Element['__emotion_base'] != null
    const isStyledComponent = Element['styledComponentId'] != null
    const instanceMetadata: ElementInstanceMetadata = {
      element: right(jsx),
      elementPath: elementPath,
      globalFrame: null,
      localFrame: null,
      componentInstance: false,
      isEmotionOrStyledComponent: isEmotionComponent || isStyledComponent,
      specialSizeMeasurements: emptySpecialSizeMeasurements, // This is not the nicest, but the results from the DOM walker will override this anyways
      computedStyle: emptyComputedStyle,
      attributeMetadatada: emptyAttributeMetadatada,
      label: null,
      importInfo: isJSXElement(jsx)
        ? importInfoFromImportDetails(jsx.name, imports, filePath)
        : null,
      conditionValue: 'not-a-conditional',
    }
    if (!EP.isStoryboardPath(elementPath) || shouldIncludeCanvasRootInTheSpy) {
      const elementPathString = EP.toComponentId(elementPath)
      // TODO right now we don't actually invalidate the path, just let the dom-walker know it should walk again
      updateInvalidatedPaths((current) => current)
      metadataContext.current.spyValues.metadata[elementPathString] = instanceMetadata
      metadataContext.current.spyValues.allElementProps[elementPathString] =
        makeCanvasElementPropsSafe(reportedProps)
    }
  }
  const spyWrapperProps: SpyWrapperProps = {
    elementToRender: Element,
    spyCallback: spyCallback,
    inScope: inScope,
    jsxFactoryFunctionName: jsxFactoryFunctionName,
    $$utopiaElementPath: elementPath,
  }
  return renderComponentUsingJsxFactoryFunction(
    inScope,
    jsxFactoryFunctionName,
    SpyWrapper,
    {
      ...props,
      ...spyWrapperProps,
    },
    ...childrenElements,
  )
}

interface SpyWrapperProps {
  spyCallback: (finalProps: any) => void
  elementToRender: React.ComponentType<React.PropsWithChildren<any>>
  inScope: MapLike<any>
  jsxFactoryFunctionName: string | null
  $$utopiaElementPath: ElementPath
}
const SpyWrapper = React.forwardRef<any, SpyWrapperProps>((props, ref) => {
  const {
    spyCallback,
    elementToRender: ElementToRender,
    inScope,
    jsxFactoryFunctionName,
    $$utopiaElementPath,
    ...passThroughPropsFromProps
  } = props
  const passThroughProps = {
    ...passThroughPropsFromProps,
    ...(ref == undefined ? {} : { ref: ref }),
  }
  spyCallback(passThroughProps)
  return renderComponentUsingJsxFactoryFunction(
    inScope,
    jsxFactoryFunctionName,
    ElementToRender,
    passThroughProps,
  )
})
SpyWrapper.displayName = 'SpyWrapper'
