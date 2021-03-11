import * as React from 'react'
import { TemplatePath } from '../../../core/shared/project-file-types'
import { IcnProps, Icn } from '../../../uuiui'
import { betterReactMemo } from '../../../uuiui-deps'
import * as TP from '../../../core/shared/template-path'
import { useComponentIcon } from '../layout-element-icons'

interface ItemPreviewProps {
  path: TemplatePath
  color: IcnProps['color']
}

export const ItemPreview: React.FunctionComponent<ItemPreviewProps> = betterReactMemo(
  'ItemPreview',
  (props) => {
    // preview depends on three things:
    // 1 - role
    // 2 - if it's empty or not
    // 3 - if it's a component or not
    // 4 - if it's a placeholder or not
    // 5 if it's generated or not
    const iconProps = useComponentIcon(props.path)

    if (iconProps == null || TP.isScenePath(props.path)) {
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
