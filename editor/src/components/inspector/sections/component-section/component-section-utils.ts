import React from 'react'
import type { RegularControlDescription } from '../../../custom-code/internal-property-controls'
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
      control: 'none',
    }
  }

  if (React.isValidElement(propValue)) {
    return {
      control: 'jsx',
      label: propName,
      preferredChildComponents: [],
    }
  }

  switch (typeof propValue) {
    case 'number':
      return {
        control: 'number-input',
        label: propName,
      }
    case 'string': {
      const parsedAsColor = parseStringValidateAsColor(propValue)
      const controlType = isLeft(parsedAsColor) ? 'string-input' : 'color'
      return {
        control: controlType,
        label: propName,
      }
    }
    case 'boolean': {
      return {
        control: 'checkbox',
        label: propName,
      }
    }
    case 'function': {
      return {
        control: 'expression-input',
        label: propName,
      }
    }
    case 'object': {
      if (propValue == null || propName === 'style') {
        return {
          control: 'none',
        }
      } else if (React.isValidElement(propValue)) {
        if (propName === 'children') {
          return {
            control: 'none',
          }
        } else {
          return {
            control: 'expression-input',
            label: propName,
          }
        }
      } else if (Array.isArray(propValue)) {
        if (propValue.length === 2 && isNumberArray(propValue)) {
          // First check for vectors or matrices
          return {
            control: 'vector2',
            label: propName,
          }
        } else if (propValue.length === 3 && isNumberArray(propValue)) {
          return {
            control: 'vector3',
            label: propName,
          }
        } else if (propValue.length === 4 && isNumberArray(propValue)) {
          return {
            control: 'vector4',
            label: propName,
          }
        } else if (
          propValue.length === 4 &&
          isNumberArray(propValue.slice(0, 3)) &&
          typeof propValue[3] === 'string'
        ) {
          return {
            control: 'euler',
            label: propName,
          }
        } else if (propValue.length === 9 && isNumberArray(propValue)) {
          return {
            control: 'matrix3',
            label: propName,
          }
        } else if (propValue.length === 16 && isNumberArray(propValue)) {
          return {
            control: 'matrix4',
            label: propName,
          }
        } else if (propValue.length > 0) {
          // It's either an array or a tuple control, so let's check if the values' controls all match
          const propertyControls = propValue.map((v) =>
            inferControlTypeBasedOnValueInner(stackSize + 1, v),
          )
          if (propertyControls.every((c) => c.control === propertyControls[0].control)) {
            // Assume a regular array control without drilling into inner arrays or objects
            return {
              control: 'array',
              label: propName,
              propertyControl: propertyControls[0],
            }
          } else {
            // Different typed values means we have to use a tuple control
            return {
              control: 'tuple',
              label: propName,
              propertyControls: propertyControls,
            }
          }
        } else {
          // We can't infer the underlying control type for empty arrays, so our hands are tied here
          return {
            control: 'none',
          }
        }
      } else {
        const controlsForKeys = mapValues(
          (v: unknown, key: string) => inferControlTypeBasedOnValueInner(stackSize + 1, v, key),
          propValue,
        )

        return {
          control: 'object',
          label: propName,
          object: controlsForKeys,
        }
      }
    }
    default:
      return {
        control: 'none',
      }
  }
}

export function inferControlTypeBasedOnValue(
  propValue: any,
  propName?: string,
): RegularControlDescription {
  return inferControlTypeBasedOnValueInner(0, propValue, propName)
}
