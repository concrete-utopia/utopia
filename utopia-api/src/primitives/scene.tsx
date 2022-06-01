import React from 'react'

export interface SceneProps {
  style?: React.CSSProperties
  'data-label'?: string
  'data-uid'?: string
}

export const Scene = React.memo((props: React.PropsWithChildren<SceneProps>) => {
  return <div {...props}>{props.children}</div>
})
