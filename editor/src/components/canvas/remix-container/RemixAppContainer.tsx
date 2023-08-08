import React from 'react'
import { UtopiaRemixRootComponent } from '../UtopiaRemixRootComponent'

export interface RemixAppContainer {
  style?: React.CSSProperties
  'data-label'?: string
  'data-uid'?: string
}

export const RemixAppContainer = React.memo((props: RemixAppContainer) => {
  let style: React.CSSProperties = {
    overflow: 'hidden',
  }
  if (props.style != null) {
    style = {
      ...style,
      ...props.style,
    }
  }
  const adjustedProps: React.PropsWithChildren<RemixAppContainer> = {
    ...props,
    style: style,
  }
  return (
    <div {...adjustedProps}>
      <UtopiaRemixRootComponent />
    </div>
  )
})
