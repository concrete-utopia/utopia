import React from 'react'
import type { MapLike } from 'typescript'
import type { Either } from '../../../core/shared/either'
import { foldEither, right } from '../../../core/shared/either'
import type {
  ElementInstanceMetadata,
  JSXElementChild,
  JSXConditionalExpression,
  ConditionValue,
  EarlyReturn,
  StylePluginConfig,
} from '../../../core/shared/element-template'
import {
  emptyAttributeMetadata,
  emptyComputedStyle,
  emptySpecialSizeMeasurements,
  isJSXElement,
} from '../../../core/shared/element-template'
import type { ElementPath, Imports } from '../../../core/shared/project-file-types'
import { makeCanvasElementPropsSafe } from '../../../utils/canvas-react-utils'
import type {
  DomWalkerInvalidatePathsCtxData,
  UiJsxCanvasContextData,
  VariableData,
} from '../ui-jsx-canvas'
import * as EP from '../../../core/shared/element-path'
import { renderComponentUsingJsxFactoryFunction } from './ui-jsx-canvas-element-renderer-utils'
import { importInfoFromImportDetails } from '../../../core/model/project-file-utils'
import { getUtopiaID } from '../../../core/shared/uid-utils'
import { isComponentInstance } from '../../../core/model/element-template-utils'

// Should the condition value of conditional expression change (which maybe be done by overriding it),
// then the values we have accumulated in the spy metadata may need to be cleaned up.
export function clearOpposingConditionalSpyValues(
  metadataContext: UiJsxCanvasContextData,
  conditional: JSXConditionalExpression,
  conditionValue: boolean,
  elementPath: ElementPath,
): void {
  const opposingElement = conditionValue ? conditional.whenFalse : conditional.whenTrue
  const pathToOpposing = EP.appendToPath(elementPath, getUtopiaID(opposingElement))
  const opposingPathString = EP.toString(pathToOpposing)
  const metadata = metadataContext.current.spyValues.metadata
  // Search for this and should we find it, only then should we attempt to clear the metadata.
  // As walking all the keys is somewhat expensive.
  if (opposingPathString in metadata) {
    for (const metadataKey of Object.keys(metadata)) {
      const metadataEntry = metadata[metadataKey]
      const metadataPath = metadataEntry.elementPath
      // This is one of the descendants or the value of the opposing clause
      // of the conditional.
      if (EP.isDescendantOfOrEqualTo(metadataPath, pathToOpposing)) {
        delete metadata[metadataKey]
      }
    }
  }
}

export function addFakeSpyEntry(
  validPaths: Set<string>,
  metadataContext: UiJsxCanvasContextData,
  elementPath: ElementPath,
  elementOrAttribute: JSXElementChild,
  filePath: string,
  imports: Imports,
  conditionValue: ConditionValue,
  earlyReturn: EarlyReturn | null,
  stylePluginConfig: StylePluginConfig,
  prop: string | null,
): void {
  // Ensure that entries are not created which aren't included in `validPaths`,
  // so that ghost like entries are not created.
  if (validPaths.has(EP.toString(EP.makeLastPartOfPathStatic(elementPath)))) {
    const element: Either<string, JSXElementChild> = right(elementOrAttribute)
    const instanceMetadata: ElementInstanceMetadata = {
      element: element,
      elementPath: elementPath,
      globalFrame: null,
      nonRoundedGlobalFrame: null,
      componentInstance: isComponentInstance(elementOrAttribute),
      isEmotionOrStyledComponent: false,
      specialSizeMeasurements: emptySpecialSizeMeasurements,
      computedStyle: emptyComputedStyle,
      attributeMetadata: emptyAttributeMetadata,
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
      textContent: null,
      earlyReturn: earlyReturn,
      assignedToProp: prop,
      stylePluginConfig: stylePluginConfig,
    }
    const elementPathString = EP.toComponentId(elementPath)
    metadataContext.current.spyValues.metadata[elementPathString] = instanceMetadata
  }
}

export function buildSpyWrappedElement(
  jsx: JSXElementChild,
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
  variablesInScope: VariableData,
  isTextEditor: 'text-editor' | 'real-element' = 'real-element',
  assignedToProp: string | null,
  stylePluginConfig: StylePluginConfig,
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
    // when the spied element is an active text editor, we need the props of the edited element
    // (stored in passthroughProps), and not the props of the text editor itself
    const realReportedProps =
      isTextEditor === 'text-editor' ? reportedProps.passthroughProps : reportedProps

    const instanceMetadata: ElementInstanceMetadata = {
      element: right(jsx),
      elementPath: elementPath,
      globalFrame: null,
      nonRoundedGlobalFrame: null,
      componentInstance: isComponentInstance(jsx),
      isEmotionOrStyledComponent: isEmotionComponent || isStyledComponent,
      specialSizeMeasurements: emptySpecialSizeMeasurements, // This is not the nicest, but the results from the DOM walker will override this anyways
      computedStyle: emptyComputedStyle,
      attributeMetadata: emptyAttributeMetadata,
      label: null,
      importInfo: isJSXElement(jsx)
        ? importInfoFromImportDetails(jsx.name, imports, filePath)
        : null,
      conditionValue: 'not-a-conditional',
      textContent: null,
      earlyReturn: null,
      assignedToProp: assignedToProp,
      stylePluginConfig: stylePluginConfig,
    }
    if (!EP.isStoryboardPath(elementPath) || shouldIncludeCanvasRootInTheSpy) {
      const elementPathString = EP.toComponentId(elementPath)
      // TODO right now we don't actually invalidate the path, just let the dom-walker know it should walk again
      updateInvalidatedPaths((current) => current)
      metadataContext.current.spyValues.metadata[elementPathString] = instanceMetadata
      metadataContext.current.spyValues.allElementProps[elementPathString] =
        makeCanvasElementPropsSafe(realReportedProps)
      metadataContext.current.spyValues.variablesInScope[elementPathString] = variablesInScope
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
