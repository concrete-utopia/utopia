import React from 'react'
import type { UtopiaComponentProps } from './common'
import { addEventHandlersToDivProps } from './common'

export interface EllipseProps
  extends React.DetailedHTMLProps<React.HTMLAttributes<HTMLDivElement>, HTMLDivElement>,
    UtopiaComponentProps {}

export const Ellipse: React.FunctionComponent<EllipseProps> = (props: EllipseProps) => {
  let { 'data-uid': dataUid, 'data-label': dataLabel, ...divProps } = props
  const propsWithEventHandlers = addEventHandlersToDivProps(divProps)

  return (
    <div
      {...propsWithEventHandlers}
      data-uid={dataUid}
      data-label={dataLabel}
      data-utopia-do-not-traverse={true}
      style={{
        borderRadius: '50%',
        ...divProps.style,
      }}
    />
  )
}
Ellipse.displayName = 'Ellipse'
