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
  testId?: string
}

export const MenuTab: React.FunctionComponent<React.PropsWithChildren<MenuTabProps>> = React.memo(
  (props) => {
    const colorTheme = useColorTheme()

    const selected = defaultIfNull<boolean>(false, props.selected)
    const hasErrorMessages = defaultIfNull<boolean>(false, props.hasErrorMessages)
    const label = defaultIfNull<React.ReactElement | string>('', props.label)

    const baseStyle = {
      padding: '4px 8px',
      transition: 'all .05s ease-in-out',
      cursor: 'pointer',

      justifyContent: 'center',
      fontWeight: 500,
      '&:hover': {
        opacity: 1,
      },
    }

    const selectionHandlingStyle = {
      color: hasErrorMessages
        ? colorTheme.errorForeground.value
        : colorTheme.tabSelectedForeground.value,
      opacity: selected ? 1 : 0.4,
    }
    return (
      <SimpleFlexRow
        data-testid={props.testId}
        onClick={props.onClick}
        onMouseDown={props.onMouseDown}
        css={{
          ...baseStyle,
          ...selectionHandlingStyle,
          width: 'fit-content',
        }}
        className={props.className}
      >
        <div>{label}</div>
      </SimpleFlexRow>
    )
  },
)
