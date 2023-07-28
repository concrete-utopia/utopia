import React from 'react'
import type { IcnColor, IcnProps } from './icn'
import { Icons } from './icons'

interface WarningIconProps {
  color?: IcnColor
  tooltipText?: string
  style?: React.CSSProperties
  testId?: string
}

export const WarningIcon = React.memo((props: WarningIconProps) => {
  const icnProps = getWarningIconProps(props.tooltipText, props.color, props.style)
  return <Icons.WarningTriangle {...icnProps} testId={props.testId} />
})

export function getWarningIconProps(
  tooltipText: string | undefined,
  color: IcnColor = 'warning',
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
