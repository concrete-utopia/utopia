import { ElementPath } from '../../../core/shared/project-file-types'
import React from 'react'
import type { IcnProps } from '../../../uuiui'
import { Icn, useColorTheme } from '../../../uuiui'
import { WarningIcon } from '../../../uuiui/warning-icon'
import { useLayoutOrElementIcon } from '../layout-element-icons'
import type { NavigatorEntry } from '../../../components/editor/store/editor-state'

interface LayoutIconProps {
  navigatorEntry: NavigatorEntry
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

export const LayoutIcon: React.FunctionComponent<React.PropsWithChildren<LayoutIconProps>> =
  React.memo((props) => {
    const colorTheme = useColorTheme()
    const { iconProps, isPositionAbsolute } = useLayoutOrElementIcon(props.navigatorEntry)

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
          transform: 'scale(.8)',
        }}
      >
        {isPositionAbsolute ? (
          <div
            style={{
              position: 'absolute',
              left: -8,
              top: 2,
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
          <Icn
            {...iconProps}
            color={props.color}
            style={{
              opacity: 'var(--iconOpacity)',
            }}
          />
        )}
      </div>
    )
  })
