import * as React from 'react'
import { TemplatePath } from '../../../core/shared/project-file-types'
import { IcnProps, Icn, colorTheme } from '../../../uuiui'
import { betterReactMemo } from '../../../uuiui-deps'
import { WarningIcon } from '../../../uuiui/warning-icon'
import { useLayoutOrElementIcon } from '../layout-element-icons'

interface LayoutIconProps {
  path: TemplatePath
  color: IcnProps['color']
  warningText: string | null
}

const borderColorForIconColor = (color: IcnProps['color']): string | undefined => {
  if (color === 'orange') {
    return colorTheme.navigatorComponentIconBorder.shade(30).value
  } else {
    return color
  }
}

export const LayoutIcon: React.FunctionComponent<LayoutIconProps> = betterReactMemo(
  'LayoutIcon',
  (props) => {
    const { iconProps, hasWidthOrHeight } = useLayoutOrElementIcon(props.path)

    if (props.warningText != null) {
      return <WarningIcon tooltipText={props.warningText} />
    } else {
      return (
        <div
          style={{
            width: 18,
            height: 18,
            display: 'flex',
            alignItems: 'center',
            justifyContent: 'center',
            border: hasWidthOrHeight
              ? `1px dotted ${borderColorForIconColor(props.color)}`
              : undefined,
            borderRadius: hasWidthOrHeight ? 3 : 0,
          }}
        >
          <Icn {...iconProps} color={props.color} />
        </div>
      )
    }
  },
)
