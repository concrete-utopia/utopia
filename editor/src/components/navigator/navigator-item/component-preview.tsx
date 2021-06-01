import * as React from 'react'
import { ElementPath } from '../../../core/shared/project-file-types'
import { IcnProps, Icn } from '../../../uuiui'
import { betterReactMemo } from '../../../uuiui-deps'
import * as EP from '../../../core/shared/element-path'
import { useComponentIcon } from '../layout-element-icons'

interface ComponentPreviewProps {
  path: ElementPath
  color: IcnProps['color']
}

export const ComponentPreview: React.FunctionComponent<ComponentPreviewProps> = betterReactMemo(
  'ComponentPreview',
  (props) => {
    const iconProps = useComponentIcon(props.path)

    if (iconProps == null) {
      return null
    } else {
      return (
        <div
          className='w20 h20 flex justify-center items-center relative'
          style={{
            paddingLeft: 8,
          }}
        >
          <Icn {...iconProps} color={props.color} />
        </div>
      )
    }
  },
)
