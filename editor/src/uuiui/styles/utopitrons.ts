import { css } from '@emotion/react'
import type { CSSPropertiesWithMultiValues } from '@emotion/serialize'
import { FunctionInterpolation } from '@emotion/serialize'
import type { UtopiColor } from './utopi-color-helpers'
import type { Property } from 'csstype'

type CSSObject = CSSPropertiesWithMultiValues

export function fill(utopiColor: UtopiColor): CSSObject {
  return {
    fill: utopiColor.value,
  }
}

export function stroke(utopiColor: UtopiColor): CSSObject {
  return {
    stroke: utopiColor.value,
  }
}

/**
 * CSS background-color property
 * @param utopiColor the UtopiColor of the background
 * @param color2 (optional) if present, the background will be a gradient
 */
export function background(
  utopiColor: UtopiColor,
  color2?: UtopiColor,
  deg: number = 0,
): CSSObject {
  if (color2 != null) {
    return {
      backgroundImage: `linear-gradient(${deg}deg, ${utopiColor.value} 0%, ${color2.value} 100%)`,
    }
  } else {
    return {
      backgroundColor: utopiColor.value,
    }
  }
}

export function width(value: CSSObject['width']): CSSObject {
  return {
    width: value,
  }
}

export function height(value: CSSObject['height']): CSSObject {
  return {
    height: value,
  }
}

/* Padding */

export function padding(value: number): CSSObject {
  return {
    paddingTop: value,
  }
}

export function paddingTop(value: number): CSSObject {
  return {
    paddingTop: value,
  }
}

export function paddingBottom(value: number): CSSObject {
  return {
    paddingBottom: value,
  }
}

export function paddingLeft(value: number): CSSObject {
  return {
    paddingLeft: value,
  }
}

export function paddingRight(value: number): CSSObject {
  return {
    paddingRight: value,
  }
}

export function paddingVertical(value: number): CSSObject {
  return {
    paddingTop: value,
    paddingBottom: value,
  }
}

export function paddingHorizontal(value: number): CSSObject {
  return {
    paddingLeft: value,
    paddingRight: value,
  }
}

/* Margin */

export function margin(value: number): CSSObject {
  return {
    marginTop: value,
  }
}

export function marginTop(value: number): CSSObject {
  return {
    marginTop: value,
  }
}

export function marginBottom(value: number): CSSObject {
  return {
    marginBottom: value,
  }
}

export function marginLeft(value: number): CSSObject {
  return {
    marginLeft: value,
  }
}

export function marginRight(value: number): CSSObject {
  return {
    marginRight: value,
  }
}

export function marginVertical(value: number): CSSObject {
  return {
    marginTop: value,
    marginBottom: value,
  }
}

export function marginHorizontal(value: number): CSSObject {
  return {
    marginLeft: value,
    marginRight: value,
  }
}

export function borderRadius(value: number): CSSObject {
  return {
    borderRadius: value,
  }
}

/* Border */

export function border(utopiColor: UtopiColor, lineWidth: number = 1): CSSObject {
  return {
    borderColor: utopiColor.value,
    borderWidth: lineWidth,
    borderStyle: 'solid',
  }
}

export function borderLeft(utopiColor: UtopiColor, lineWidth: number = 1): CSSObject {
  return {
    borderLeftColor: utopiColor.value,
    borderLeftWidth: lineWidth,
    borderLeftStyle: 'solid',
  }
}

export function borderRight(utopiColor: UtopiColor, lineWidth: number = 1): CSSObject {
  return {
    borderRightColor: utopiColor.value,
    borderRightWidth: lineWidth,
    borderRightStyle: 'solid',
  }
}

export function borderBottom(utopiColor: UtopiColor, lineWidth: number = 1): CSSObject {
  return {
    borderBottomColor: utopiColor.value,
    borderBottomWidth: lineWidth,
    borderBottomStyle: 'solid',
  }
}

export function borderTop(utopiColor: UtopiColor, lineWidth: number = 1): CSSObject {
  return {
    borderTopColor: utopiColor.value,
    borderTopWidth: lineWidth,
    borderTopStyle: 'solid',
  }
}

export function color(value: UtopiColor): CSSObject {
  return {
    color: value.value,
  }
}

/* Font Weight */
export function fontWeight(value: number | 'normal' | 'bold'): CSSObject {
  return {
    fontWeight: value,
  }
}
export const normal = fontWeight('normal')
export const bold = fontWeight('bold')

/* Font Size */
export function fontSize(value: number): CSSObject {
  return {
    fontSize: value,
  }
}

export function opacity(value: number): CSSObject {
  return {
    opacity: value,
  }
}

export function display(value: CSSObject['display']): CSSObject {
  return {
    display: value,
  }
}

export function items(
  value: 'start' | 'flex-start' | 'end' | 'flex-end' | 'center' | 'baseline' | 'stretch',
): CSSObject {
  let valueToUse = value
  if (value === 'start') {
    valueToUse = 'flex-start'
  }

  if (value === 'end') {
    valueToUse = 'flex-end'
  }

  return {
    alignItems: valueToUse,
  }
}

export function justifyContent(value: Property.JustifyContent): CSSObject {
  return {
    justifyContent: value,
  }
}

export function textDecorationLine(
  value: 'none' | 'underline' | 'overline' | 'line-through' | 'blink',
): CSSObject {
  return {
    textDecoration: value,
  }
}

export const flexColumn: CSSObject = {
  flexDirection: 'column',
}

export const focusOutline = (utopiColor: UtopiColor) =>
  css({
    outline: 'none',
    '&:focus': {
      outline: `1px solid ${utopiColor.value}`,
    },
  })

interface DisabledOpacityProps {
  /**
   * set to true if you want H1 to be in disabled style.
   * shorthand syntax:
   * `<H1 disabled> ... </H1>`
   */
  disabled?: boolean
}

export const disabledOpacityStyle = (props: DisabledOpacityProps) =>
  css({
    opacity: props.disabled === true ? 0.5 : 1,
    pointerEvents: props.disabled === true ? 'none' : 'initial',
  })
