import React from 'react'
import { RegularControlDescription } from 'utopia-api'
import { parseStringValidateAsColor } from '../../../../core/property-controls/property-controls-parser'
import { isLeft } from '../../../../core/shared/either'
import { mapValues } from '../../../../core/shared/object-utils'

function isNumberArray(arr: Array<unknown>): arr is Array<number> {
  return arr.every((v) => typeof v === 'number')
}

function inferControlTypeBasedOnValueInner(
  stackSize: number,
  propValue: any,
  propName?: string,
): RegularControlDescription {
  if (stackSize > 100) {
    // Prevent this blowing out on recursive structures
    return {
      type: 'ignore',
    }
  }

  switch (typeof propValue) {
    case 'number':
      return {
        type: 'number',
        title: propName,
      }
    case 'string': {
      const parsedAsColor = parseStringValidateAsColor(propValue)
      const controlType = isLeft(parsedAsColor) ? 'string' : 'color'
      return {
        type: controlType,
        title: propName,
      }
    }
    case 'boolean': {
      return {
        type: 'boolean',
        title: propName,
      }
    }
    case 'function': {
      return {
        type: 'rawjs',
        title: propName,
      }
    }
    case 'object': {
      if (propValue == null || propName === 'style') {
        return {
          type: 'ignore',
        }
      } else if (React.isValidElement(propValue)) {
        if (propName === 'children') {
          return {
            type: 'ignore',
          }
        } else {
          return {
            type: 'rawjs',
            title: propName,
          }
        }
      } else if (Array.isArray(propValue)) {
        if (propValue.length === 2 && isNumberArray(propValue)) {
          // First check for vectors or matrices
          return {
            type: 'vector2',
            title: propName,
          }
        } else if (propValue.length === 3 && isNumberArray(propValue)) {
          return {
            type: 'vector3',
            title: propName,
          }
        } else if (propValue.length === 4 && isNumberArray(propValue)) {
          return {
            type: 'vector4',
            title: propName,
          }
        } else if (
          propValue.length === 4 &&
          isNumberArray(propValue.slice(0, 3)) &&
          typeof propValue[3] === 'string'
        ) {
          return {
            type: 'euler',
            title: propName,
          }
        } else if (propValue.length === 9 && isNumberArray(propValue)) {
          return {
            type: 'matrix3',
            title: propName,
          }
        } else if (propValue.length === 16 && isNumberArray(propValue)) {
          return {
            type: 'matrix4',
            title: propName,
          }
        } else if (propValue.length > 0) {
          // Otherwise we go with a regular array control
          return {
            type: 'array',
            title: propName,
            propertyControl: inferControlTypeBasedOnValueInner(stackSize + 1, propValue[0]),
          }
        } else {
          // We can't infer the underlying control type for empty arrays, so our hands are tied here
          return {
            type: 'ignore',
          }
        }
      } else {
        const controlsForKeys = mapValues(
          (v: unknown, key: string) => inferControlTypeBasedOnValueInner(stackSize + 1, v, key),
          propValue,
        )

        return {
          type: 'object',
          title: propName,
          object: controlsForKeys,
        }
      }
    }
    default:
      return {
        type: 'ignore',
      }
  }
}

export function inferControlTypeBasedOnValue(
  propValue: any,
  propName?: string,
): RegularControlDescription {
  return inferControlTypeBasedOnValueInner(0, propValue, propName)
}
