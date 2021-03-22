import * as React from 'react'
import {
  UtopiaComponentProps,
  calculatePositionableStyle,
  addEventHandlersToDivProps,
} from './common'

export interface PositionableProps
  extends React.DetailedHTMLProps<React.HTMLAttributes<HTMLDivElement>, HTMLDivElement>,
    UtopiaComponentProps {
  wrappedComponent: React.JSXElementConstructor<any>
}

// TODO Delete the entire Positionable component and remove it from the UI
const wrappedComponentProp = (props: any) => {
  return <div />
}

export const Positionable: React.FunctionComponent<PositionableProps> = (
  props: PositionableProps,
) => {
  let {
    layout: passedLayout,
    'data-uid': dataUid,
    'data-label': dataLabel,
    wrappedComponent: WrappedComponent,
    ...divProps
  } = props

  const ownStyle = calculatePositionableStyle(props)
  const propsWithEventHandlers = addEventHandlersToDivProps(divProps)

  return (
    <div
      data-uid={dataUid}
      data-label={dataLabel}
      style={{
        ...ownStyle,
      }}
    >
      <WrappedComponent wrappedComponent={wrappedComponentProp} {...propsWithEventHandlers}>
        {props.children}
      </WrappedComponent>
    </div>
  )
}
Positionable.displayName = 'Positionable'
