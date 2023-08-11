/** @jsxRuntime classic */
/** @jsx jsx */
import { jsx } from '@emotion/react'
import React from 'react'
import styled from '@emotion/styled'
import { FlexRow, SimpleFlexRow } from './widgets/layout/flex-row'
import { Button } from './button'
import { Icons } from './icons'

import { useColorTheme, UtopiaTheme } from './styles/theme'
import { CSSObject } from '@emotion/serialize'
import { defaultIfNull } from '../core/shared/optional-utils'
import { NO_OP } from '../core/shared/utils'
import { UIGridRow } from '../components/inspector/widgets/ui-grid-row'

interface MenuTabProps {
  selected?: boolean
  hasErrorMessages?: boolean
  label: React.ReactElement | string
  onClose?: () => void
  onClick?: () => void
  onMouseDown?: () => void
  className?: string
}

export const MenuTab: React.FunctionComponent<React.PropsWithChildren<MenuTabProps>> = React.memo(
  (props) => {
    const colorTheme = useColorTheme()

    const selected = defaultIfNull<boolean>(false, props.selected)
    const hasErrorMessages = defaultIfNull<boolean>(false, props.hasErrorMessages)
    const label = defaultIfNull<React.ReactElement | string>('', props.label)

    const baseStyle = {
      padding: '4px 6px',
      transition: 'all .05s ease-in-out',
      '&:hover': {
        backgroundColor: colorTheme.tabHoveredBackground.value,
        boxShadow: `inset 0px -2px 0px 0px ${colorTheme.dynamicBlue.value}`,
      },
      cursor: 'pointer',
      flexGrow: 1,
      justifyContent: 'center',
    }

    const selectionHandlingStyle = {
      boxShadow: selected ? `inset 0px -2px 0px 0px ${colorTheme.dynamicBlue.value}` : undefined,
      color: hasErrorMessages
        ? colorTheme.errorForeground.value
        : colorTheme.tabSelectedForeground.value,
      fontWeight: selected ? 500 : undefined,
    }

    return (
      <SimpleFlexRow
        onClick={props.onClick}
        onMouseDown={props.onMouseDown}
        css={{
          ...baseStyle,
          ...selectionHandlingStyle,
        }}
        className={props.className}
      >
        <div css={{ flexGrow: 1, textAlign: 'center' }}>{label}</div>
      </SimpleFlexRow>
    )
  },
)
