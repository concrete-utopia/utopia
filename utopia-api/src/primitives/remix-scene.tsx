import React from 'react'
import { View } from './view'

export interface RemixSceneProps {
  id?: string
  style?: React.CSSProperties
  'data-label'?: string
  'data-uid'?: string
}

export const RemixScene = React.memo((props: React.PropsWithChildren<RemixSceneProps>) => {
  let style: React.CSSProperties = {
    overflow: 'hidden',
  }
  if (props.style != null) {
    style = {
      ...style,
      ...props.style,
    }
  }
  const adjustedProps: React.PropsWithChildren<RemixSceneProps> = {
    ...props,
    style: style,
  }
  return <View {...adjustedProps}>{props.children}</View>
})
