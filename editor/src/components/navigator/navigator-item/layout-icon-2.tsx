import { ElementPath } from '../../../core/shared/project-file-types'
import React from 'react'
import { IcnProps, Icn, useColorTheme } from '../../../uuiui'
import { WarningIcon } from '../../../uuiui/warning-icon'
import {
  useComponentIcon,
  useLayoutOrElementIcon,
  useLayoutOrElementIcon2,
} from '../layout-element-icons'
import {
  NavigatorEntry,
  getJSXComponentsAndImportsForPathFromState,
} from '../../../components/editor/store/editor-state'
import { useEditorState, Substores } from '../../../components/editor/store/store-hook'

interface LayoutIconProps {
  navigatorEntry: NavigatorEntry
  color: IcnProps['color']
  warningText: string | null
  isFocusedComponent: boolean
}

export const LayoutIcon2: React.FunctionComponent<React.PropsWithChildren<LayoutIconProps>> =
  React.memo((props) => {
    const colorTheme = useColorTheme()
    const rootComponent = useEditorState(
      Substores.fullStore,
      (store) => {
        return getJSXComponentsAndImportsForPathFromState(
          props.navigatorEntry.elementPath,
          store.editor,
          store.derived,
        ).components
      },
      'rootComponent',
    )
    const { iconProps, isPositionAbsolute } = useLayoutOrElementIcon2(
      props.navigatorEntry,
      rootComponent,
      props.isFocusedComponent,
    )

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
