import React from 'react'
import { ElementPath } from '../../../core/shared/project-file-types'
import { IcnProps, Icn } from '../../../uuiui'
import * as EP from '../../../core/shared/element-path'
import { useComponentIcon } from '../layout-element-icons'

interface ComponentPreviewProps {
  path: ElementPath
  color: IcnProps['color']
}

export const ComponentPreview: React.FunctionComponent<
  React.PropsWithChildren<ComponentPreviewProps>
> = React.memo((props) => {
  const iconProps = useComponentIcon(props.path)

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
          marginLeft: 4,
        }}
      >
        <Icn {...iconProps} color={props.color} />
      </div>
    )
  }
})
