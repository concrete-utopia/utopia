import React from 'react'
import { UTOPIA_PATH_KEY } from '../../../core/model/utopia-constants'
import * as EP from '../../../core/shared/element-path'
import { UtopiaRemixRootComponent } from '../remix/utopia-remix-root-component'
export interface RemixContainerProps {
  style?: React.CSSProperties
  'data-label'?: string
  'data-uid'?: string
  [UTOPIA_PATH_KEY]: string
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

    const path = EP.fromString(props[UTOPIA_PATH_KEY])

    return (
      <div {...adjustedProps}>
        <UtopiaRemixRootComponent data-path={path} />
      </div>
    )
  },
)
