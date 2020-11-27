import * as React from 'react'
import { IcnColor, IcnProps } from './icn'
import { betterReactMemo } from '../utils/react-performance'
import { Icons } from './icons'

interface WarningIconProps {
  color?: IcnColor
  tooltipText?: string
  style?: React.CSSProperties
}

export const WarningIcon = betterReactMemo('Warning Icon', (props: WarningIconProps) => {
  const icnProps = getWarningIconProps(props.tooltipText, props.color, props.style)
  return <Icons.WarningTriangle {...icnProps} />
})

export function getWarningIconProps(
  tooltipText: string | undefined,
  color: IcnColor = 'orange',
  style: React.CSSProperties | undefined = undefined,
): IcnProps {
  const tooltipPlacement = tooltipText == null ? undefined : 'right'
  return {
    type: 'warningtriangle',
    color: color,
    tooltipText: tooltipText,
    tooltipPlacement: tooltipPlacement,
    style: style,
  }
}
