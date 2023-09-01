import React from 'react'
import { View } from './view'

export interface RemixContainerProps {
  style?: React.CSSProperties
  'data-label'?: string
  'data-uid'?: string
}

export const RemixContainer = React.memo((props: React.PropsWithChildren<RemixContainerProps>) => {
  let style: React.CSSProperties = {
    overflow: 'hidden',
  }
  if (props.style != null) {
    style = {
      ...style,
      ...props.style,
    }
  }
  const adjustedProps: React.PropsWithChildren<RemixContainerProps> = {
    ...props,
    style: style,
  }
  return <View {...adjustedProps}>{props.children}</View>
})
