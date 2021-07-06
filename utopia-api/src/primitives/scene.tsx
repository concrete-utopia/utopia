import React from 'react'
import { View } from './view'

export interface SceneProps {
  style?: React.CSSProperties
  'data-label'?: string
  'data-uid'?: string
}

export const Scene = React.memo((props: React.PropsWithChildren<SceneProps>) => {
  return <View {...props}>{props.children}</View>
})
