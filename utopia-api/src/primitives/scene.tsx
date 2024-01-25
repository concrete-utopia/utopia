import React from 'react'
import { View } from './view'

export interface SceneProps {
  id?: string
  commentId?: string
  style?: React.CSSProperties
  'data-label'?: string
  'data-uid'?: string
}

export const Scene = React.memo((props: React.PropsWithChildren<SceneProps>) => {
  let style: React.CSSProperties = {
    overflow: 'hidden',
  }
  if (props.style != null) {
    style = {
      ...style,
      ...props.style,
    }
  }
  const adjustedProps: React.PropsWithChildren<SceneProps> = {
    ...props,
    style: style,
  }
  delete adjustedProps['commentId']

  return <View {...adjustedProps}>{props.children}</View>
})
