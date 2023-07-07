import React from 'react'
import type { IcnProps } from '../../../uuiui'
import { Icn } from '../../../uuiui'
import { useComponentIcon } from '../layout-element-icons'
import type { NavigatorEntry } from '../../../components/editor/store/editor-state'

interface ComponentPreviewProps {
  navigatorEntry: NavigatorEntry
  color: IcnProps['color']
}

export const ComponentPreview: React.FunctionComponent<
  React.PropsWithChildren<ComponentPreviewProps>
> = React.memo((props) => {
  const iconProps = useComponentIcon(props.navigatorEntry)

  if (iconProps == null) {
    return null
  } else {
    return (
      <div
        style={{
          width: 18,
          height: 18,
          display: 'flex',
          justifyItems: 'center',
          alignItems: 'center',
          position: 'relative',
          paddingTop: 1,
          marginLeft: 2,
        }}
      >
        <Icn {...iconProps} color={props.color} />
      </div>
    )
  }
})
