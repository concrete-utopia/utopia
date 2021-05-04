import { Interpolation, Theme } from '@emotion/react'
import * as React from 'react'
import { UtopiaComponentProps, addEventHandlersToDivProps } from './common'

export interface ViewProps
  extends React.DetailedHTMLProps<React.HTMLAttributes<HTMLDivElement>, HTMLDivElement>,
    UtopiaComponentProps {
  css?: Interpolation<Theme>
}

export const View: React.FunctionComponent<ViewProps> = (props: ViewProps) => {
  let { 'data-uid': dataUid, 'data-label': dataLabel, ...divProps } = props
  const propsWithEventHandlers = addEventHandlersToDivProps(divProps)

  return (
    <div {...propsWithEventHandlers} data-uid={dataUid} data-label={dataLabel}>
      {props.children}
    </div>
  )
}
View.displayName = 'View'
