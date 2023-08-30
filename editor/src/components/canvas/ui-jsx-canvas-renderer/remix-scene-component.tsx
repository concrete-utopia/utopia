import React from 'react'
import { UtopiaRemixRootComponent } from '../remix/utopia-remix-root-component'

export const REMIX_SCENE_TESTID = 'remix-scene'

export interface RemixSceneProps {
  style?: React.CSSProperties
}

export const RemixSceneComponent = React.memo((props: React.PropsWithChildren<RemixSceneProps>) => {
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

  if (props.style != null) {
    style = {
      ...style,
      ...props.style,
    }
  }

  return (
    <div data-testid={REMIX_SCENE_TESTID} {...adjustedProps}>
      <UtopiaRemixRootComponent />
    </div>
  )
})
