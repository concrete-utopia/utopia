import React from 'react'
import { UtopiaRemixRootComponent } from '../utopia-remix-root-component'

export interface RemixAppContainerProps {
  style?: React.CSSProperties
  'data-label'?: string
  'data-uid'?: string
}

export const RemixAppContainer = React.memo((props: RemixAppContainerProps) => {
  let style: React.CSSProperties = {
    overflow: 'hidden',
  }
  if (props.style != null) {
    style = {
      ...style,
      ...props.style,
    }
  }
  const adjustedProps: React.PropsWithChildren<RemixAppContainerProps> = {
    ...props,
    style: style,
  }
  return (
    <div {...adjustedProps}>
      <UtopiaRemixRootComponent />
    </div>
  )
})
