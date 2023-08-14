import React from 'react'
import { UtopiaRemixRootComponent } from '../utopia-remix-root-component'
import * as EP from '../../../../core/shared/element-path'

export interface RemixAppContainerProps {
  style?: React.CSSProperties
  'data-label'?: string
  'data-uid'?: string
  'data-path': string
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

  const path = EP.fromString(props['data-path'])

  return (
    <div {...adjustedProps}>
      <UtopiaRemixRootComponent data-path={path} />
    </div>
  )
})
