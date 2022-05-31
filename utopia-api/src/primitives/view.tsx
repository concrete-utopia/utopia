import { Interpolation, Theme } from '@emotion/react'
import React from 'react'
import { UtopiaComponentProps, addEventHandlersToDivProps } from './common'

export interface ViewProps
  extends React.DetailedHTMLProps<React.HTMLAttributes<HTMLDivElement>, HTMLDivElement>,
    UtopiaComponentProps {
  css?: Interpolation<Theme>
}

export const View: React.FunctionComponent<ViewProps> = (props: ViewProps) => {
  let { 'data-uid': dataUid, 'data-label': dataLabel, ...divProps } = props
  // We're removing the data-uid prop here as the monkey patch will deal with it
  const propsWithEventHandlers = addEventHandlersToDivProps(divProps)

  return (
    <div {...propsWithEventHandlers} data-label={dataLabel}>
      {props.children}
    </div>
  )
}
View.displayName = 'View'
