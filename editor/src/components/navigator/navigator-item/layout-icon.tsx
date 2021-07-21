import * as React from 'react'
import { ElementPath } from '../../../core/shared/project-file-types'
import { IcnProps, Icn, useColorTheme } from '../../../uuiui'
import { betterReactMemo } from '../../../uuiui-deps'
import { WarningIcon } from '../../../uuiui/warning-icon'
import { useLayoutOrElementIcon } from '../layout-element-icons'

interface LayoutIconProps {
  path: ElementPath
  color: IcnProps['color']
  warningText: string | null
}

const borderColorForIconColor = (color: IcnProps['color'], colorTheme: any): string | undefined => {
  if (color === 'warning') {
    return colorTheme.navigatorComponentIconBorder.value
  } else {
    return color
  }
}

export const LayoutIcon: React.FunctionComponent<LayoutIconProps> = betterReactMemo(
  'LayoutIcon',
  (props) => {
    const colorTheme = useColorTheme()
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
            justifyItems: 'center',
            position: 'relative',
            marginLeft: 8,
            border: hasWidthOrHeight
              ? `1px solid ${borderColorForIconColor(props.color, colorTheme)}`
              : undefined,
            borderRadius: hasWidthOrHeight ? 5 : 0,
          }}
        >
          <Icn {...iconProps} color={props.color} />
        </div>
      )
    }
  },
)
