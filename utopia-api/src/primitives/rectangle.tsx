import React from 'react'
import { UtopiaComponentProps, addEventHandlersToDivProps } from './common'

export interface RectangleProps
  extends React.DetailedHTMLProps<React.HTMLAttributes<HTMLDivElement>, HTMLDivElement>,
    UtopiaComponentProps {}

export const Rectangle: React.FunctionComponent<RectangleProps> = (props: RectangleProps) => {
  let { 'data-uid': dataUid, 'data-label': dataLabel, ...divProps } = props

  const propsWithEventHandlers = addEventHandlersToDivProps(divProps)
  const propsWithoutChildren = { ...propsWithEventHandlers, children: undefined }

  return <div {...propsWithoutChildren} data-uid={dataUid} data-label={dataLabel} />
}
Rectangle.displayName = 'Rectangle'
