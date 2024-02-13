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

export function applyPropsParamToPassedProps(
  filePath: string,
  inScope: MapLike<any>,
  requireResult: MapLike<any>,
  passedProps: MapLike<unknown>,
  propsParam: Param,
): MapLike<unknown> {
  let output: MapLike<unknown> = {}

  function getParamValue(
    value: unknown,
    defaultExpression: JSExpressionMapOrOtherJavascript | null,
  ): unknown {
    if (value === undefined && defaultExpression != null) {
      return jsxAttributeToValue(filePath, inScope, requireResult, defaultExpression)
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
