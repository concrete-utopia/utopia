import { ElementPath } from '../../../core/shared/project-file-types'
import React from 'react'
import { IcnProps, Icn, useColorTheme } from '../../../uuiui'
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

export const LayoutIcon: React.FunctionComponent<LayoutIconProps> = React.memo((props) => {
  const colorTheme = useColorTheme()
  const { iconProps, isPositionAbsolute } = useLayoutOrElementIcon(props.path)

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
      }}
    >
      {isPositionAbsolute ? (
        <div
          style={{
            position: 'absolute',
            left: -5,
            top: 3,
            color: '#ff00ff',
            fontSize: 11,
            fontWeight: 600,
          }}
        >
          *
        </div>
      ) : null}

      {props.warningText != null ? (
        <WarningIcon tooltipText={props.warningText} />
      ) : (
        <Icn {...iconProps} color={props.color} />
      )}
    </div>
  )
})
