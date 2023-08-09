import React from 'react'
import { RemixAppContainer } from '../remix/remix-container/remix-app-container'
export interface RemixContainerProps {
  style?: React.CSSProperties
  'data-label'?: string
  'data-uid'?: string
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
    return <RemixAppContainer {...adjustedProps} />
  },
)
