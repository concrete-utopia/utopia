import React from 'react'
import { UtopiaRemixRootComponent } from '../remix/utopia-remix-root-component'

export interface RemixContainerProps {
  style?: React.CSSProperties
}

export const RemixContainerComponent = React.memo(
  (props: React.PropsWithChildren<RemixContainerProps>) => {
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

    if (props.style != null) {
      style = {
        ...style,
        ...props.style,
      }
    }

    return (
      <div {...adjustedProps}>
        <UtopiaRemixRootComponent />
      </div>
    )
  },
)
