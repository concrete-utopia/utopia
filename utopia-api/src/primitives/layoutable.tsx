import * as React from 'react'
import { UtopiaComponentProps, calculateOwnStyleProp, addEventHandlersToDivProps } from './common'

export interface LayoutableProps
  extends React.DetailedHTMLProps<React.HTMLAttributes<HTMLDivElement>, HTMLDivElement>,
    UtopiaComponentProps {
  wrappedComponent: React.JSXElementConstructor<any>
}

export const Layoutable: React.FunctionComponent<LayoutableProps> = (props: LayoutableProps) => {
  let {
    layout: passedLayout,
    'data-uid': dataUid,
    'data-label': dataLabel,
    wrappedComponent: WrappedComponent,
    ...divProps
  } = props

  const ownStyle = calculateOwnStyleProp(props, props.children)
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
Layoutable.displayName = 'Layoutable'
