import type { MapLike } from 'typescript'
import type {
  Param,
  JSExpressionOtherJavaScript,
  BoundParam,
  JSExpressionMapOrOtherJavascript,
} from '../../../core/shared/element-template'
import {
  isRegularParam,
  isDestructuredObject,
  isOmittedParam,
} from '../../../core/shared/element-template'
import { AnyMap, jsxAttributeToValue } from '../../../core/shared/jsx-attributes'
import type {
  ElementPath,
  HighlightBoundsForUids,
  Imports,
} from '../../../core/shared/project-file-types'
import type { UIFileBase64Blobs } from '../../editor/store/editor-state'
import type {
  DomWalkerInvalidatePathsCtxData,
  UiJsxCanvasContextData,
  VariableData,
} from '../ui-jsx-canvas'

export function applyPropsParamToPassedProps(
  filePath: string,
  inScope: MapLike<any>,
  requireResult: MapLike<any>,
  passedProps: MapLike<unknown>,
  propsParam: Param,
  elementPath: ElementPath | null,
  rootScope: MapLike<any>,
  parentComponentInputProps: MapLike<any>,
  hiddenInstances: Array<ElementPath>,
  displayNoneInstances: Array<ElementPath>,
  fileBlobs: UIFileBase64Blobs,
  validPaths: Set<string>,
  uid: string | undefined,
  reactChildren: React.ReactNode | undefined,
  metadataContext: UiJsxCanvasContextData,
  updateInvalidatedPaths: DomWalkerInvalidatePathsCtxData,
  jsxFactoryFunctionName: string | null,
  codeError: Error | null,
  shouldIncludeCanvasRootInTheSpy: boolean,
  imports: Imports,
  code: string,
  highlightBounds: HighlightBoundsForUids | null,
  editedText: ElementPath | null,
  variablesInScope: VariableData,
): MapLike<unknown> {
  let output: MapLike<unknown> = {}

  function getParamValue(
    value: unknown,
    defaultExpression: JSExpressionMapOrOtherJavascript | null,
  ): unknown {
    if (value === undefined && defaultExpression != null) {
      return jsxAttributeToValue(
        inScope,
        defaultExpression,
        elementPath,
        {
          filePath,
          requireResult,
          rootScope,
          parentComponentInputProps,
          hiddenInstances,
          displayNoneInstances,
          fileBlobs,
          validPaths,

          reactChildren,
          metadataContext,
          updateInvalidatedPaths,
          jsxFactoryFunctionName,
          shouldIncludeCanvasRootInTheSpy,
          imports,
          code,
          highlightBounds,
          editedText,
          variablesInScope,
        },
        uid,
        codeError,
      )
    } else {
      return value
    }
  }

  function applyBoundParamToOutput(value: unknown, boundParam: BoundParam): void {
    if (isRegularParam(boundParam)) {
      const { paramName } = boundParam
      output[paramName] = getParamValue(value, boundParam.defaultExpression)
    } else if (isDestructuredObject(boundParam)) {
      if (typeof value === 'object' && !Array.isArray(value) && value !== null) {
        const valueAsRecord: Record<string, unknown> = { ...value }
        let remainingValues = { ...value } as Record<string, unknown>
        for (const part of boundParam.parts) {
          const { propertyName, param } = part
          if (propertyName == null) {
            const { dotDotDotToken: spread, boundParam: innerBoundParam } = param
            if (isRegularParam(innerBoundParam)) {
              // e.g. `{ prop }` or `{ ...remainingProps }`
              const { paramName } = innerBoundParam
              if (spread) {
                output[paramName] = remainingValues
                remainingValues = {}
              } else {
                output[paramName] = getParamValue(
                  valueAsRecord[paramName],
                  innerBoundParam.defaultExpression,
                )
                delete remainingValues[paramName]
              }
            }
          } else {
            // e.g. `{ prop: renamedProp }` or `{ prop: { /* further destructuring */ } }`
            // Can't spread if we have a property name
            const innerValue = valueAsRecord[propertyName]
            applyBoundParamToOutput(innerValue, param.boundParam)
            delete remainingValues[propertyName]
            // No other cases are legal
            // TODO Should we throw? Users will already have a lint error
          }
        }
      }
      // TODO Throw, but what?
    } else {
      if (Array.isArray(value)) {
        let remainingValues = [...value]
        boundParam.parts.forEach((param) => {
          if (isOmittedParam(param)) {
            remainingValues.shift()
          } else {
            const { dotDotDotToken: spread, boundParam: innerBoundParam } = param
            if (isRegularParam(innerBoundParam)) {
              const { paramName } = innerBoundParam
              if (spread) {
                output[paramName] = remainingValues
                remainingValues = []
              } else {
                output[paramName] = getParamValue(
                  remainingValues.shift(),
                  innerBoundParam.defaultExpression,
                )
              }
            } else {
              const nextValue = remainingValues.shift()
              applyBoundParamToOutput(nextValue, innerBoundParam)
            }
          }
        })
      }
      // TODO Throw, but what?
    }
  }

  applyBoundParamToOutput(passedProps, propsParam.boundParam)
  return output
}
