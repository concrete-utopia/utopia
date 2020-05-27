import * as React from 'react'
import {
  UtopiaComponentProps,
  calculateResizeableStyle,
  addEventHandlersToDivProps,
} from './common'

export interface ResizeableProps
  extends React.DetailedHTMLProps<React.HTMLAttributes<HTMLDivElement>, HTMLDivElement>,
    UtopiaComponentProps {
  wrappedComponent: React.JSXElementConstructor<any>
}

export const Resizeable: React.FunctionComponent<ResizeableProps> = (props: ResizeableProps) => {
  let {
    layout: passedLayout,
    'data-uid': dataUid,
    'data-label': dataLabel,
    wrappedComponent: WrappedComponent,
    ...divProps
  } = props

  const ownStyle = calculateResizeableStyle(props)
  const propsWithEventHandlers = addEventHandlersToDivProps(divProps)

  return (
    <div
      data-uid={dataUid}
      data-label={dataLabel}
      style={{
        ...ownStyle,
      }}
    >
      <WrappedComponent {...propsWithEventHandlers}>{props.children}</WrappedComponent>
    </div>
  )
}
Resizeable.displayName = 'Resizeable'
