import React from 'react'
import { CSSNumber, printCSSNumber } from '../../../inspector/common/css-utils'

const FontSize = 12
const Padding = 4

interface PaddingValueLabelProps {
  scale: number
  color: string
  value: CSSNumber
}

export const CSSNumberLabel = (props: PaddingValueLabelProps): JSX.Element => {
  const { scale, color, value } = props
  const fontSize = FontSize / scale
  const padding = Padding / scale
  return (
    <div
      style={{
        fontSize: fontSize,
        padding: padding,
        backgroundColor: color,
        color: 'white',
      }}
    >
      {printCSSNumber(value, null)}
    </div>
  )
}

interface PillHandleProps {
  width: number
  height: number
  color: string
  borderWidth: number
}

export const PillHandle = (props: PillHandleProps): JSX.Element => {
  const { width, height, color, borderWidth } = props
  return (
    <div
      style={{
        boxSizing: 'border-box',
        width: width,
        height: height,
        backgroundColor: color,
        border: `${borderWidth}px solid rgba(255, 255, 255)`,
      }}
    />
  )
}
