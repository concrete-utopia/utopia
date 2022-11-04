import React from 'react'
import { CSSProperties } from 'twind'
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

export const StripedBackgroundCSS = (
  stripeColor: string,
  scale: number,
): { backgroundImage: string; backgroundSize: string } => ({
  backgroundImage: `linear-gradient(135deg, ${stripeColor} 12.5%, rgba(255,255,255,0) 12.5%, rgba(255,255,255,0) 50%, ${stripeColor} 50%, ${stripeColor} 62%, rgba(255,255,255,0) 62%, rgba(255,255,255,0) 100%)`,
  backgroundSize: `${20 / scale}px ${20 / scale}px`,
})

export type Timeout = ReturnType<typeof setTimeout>

export function useHoverWithDelay(
  delay: number,
  update: (hovered: boolean) => void,
): [React.MouseEventHandler, React.MouseEventHandler] {
  const fadeInTimeout = React.useRef<Timeout | null>(null)

  const onHoverEnd = () => {
    if (fadeInTimeout.current) {
      clearTimeout(fadeInTimeout.current)
    }
    fadeInTimeout.current = null
    update(false)
  }

  const onHoverStart = () => {
    fadeInTimeout.current = setTimeout(() => update(true), delay)
  }

  return [onHoverStart, onHoverEnd]
}
