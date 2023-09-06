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

interface TabComponentProps {
  modified?: boolean
  showModifiedIndicator?: boolean
  showCloseIndicator?: boolean
  selected?: boolean
  hasErrorMessages?: boolean
  icon?: React.ReactElement | string
  label: React.ReactElement | string
  onClose?: () => void
  onClick?: () => void
  onDoubleClick?: () => void
  onMouseDown?: () => void
  className?: string
}

export const TabComponent: React.FunctionComponent<React.PropsWithChildren<TabComponentProps>> =
  React.memo((props) => {
    const colorTheme = useColorTheme()
    const [tabIsHovered, setTabIsHovered] = React.useState(false)
    const [indicatorIsHovered, setIndicatorIsHovered] = React.useState(false)

    const modified = defaultIfNull<boolean>(false, props.showModifiedIndicator)
    const showModifiedIndicator = defaultIfNull<boolean>(true, props.showModifiedIndicator)
    const showCloseIndicator = defaultIfNull<boolean>(true, props.showCloseIndicator)
    const selected = defaultIfNull<boolean>(false, props.selected)
    const hasErrorMessages = defaultIfNull<boolean>(false, props.hasErrorMessages)
    const label = defaultIfNull<React.ReactElement | string>('', props.label)
    const icon = defaultIfNull<React.ReactElement | string>('', props.icon)
    const onClose = defaultIfNull(NO_OP, props.onClose)

    const baseStyle = {
      paddingLeft: 4,
      paddingRight: 4,
      transition: 'all .05s ease-in-out',
      '&:hover': {
        backgroundColor: colorTheme.tabHoveredBackground.value,
      },
      cursor: 'pointer',
    }

    const selectionHandlingStyle = {
      color: hasErrorMessages
        ? colorTheme.errorForeground.value
        : colorTheme.tabSelectedForeground.value,

      fontWeight: selected ? 500 : undefined,
    }

    const modifiedIndicator = showModifiedIndicator ? <Icons.CircleSmall /> : null
    const closeIndicator = showCloseIndicator ? <Icons.CrossSmall /> : null
    const closeIndicatorHovered = showCloseIndicator ? <Icons.CrossInTranslucentCircle /> : null

    const tabUnhoveredIndicator = modified ? modifiedIndicator : selected ? closeIndicator : null
    const tabHoveredIndicator = indicatorIsHovered ? closeIndicatorHovered : closeIndicator

    const setTabHoveredTrue = React.useCallback(() => setTabIsHovered(true), [setTabIsHovered])
    const setTabHoveredFalse = React.useCallback(() => setTabIsHovered(false), [setTabIsHovered])

    const setIndicatorHoveredTrue = React.useCallback(
      () => setIndicatorIsHovered(true),
      [setIndicatorIsHovered],
    )
    const setIndicatorHoveredFalse = React.useCallback(
      () => setIndicatorIsHovered(false),
      [setIndicatorIsHovered],
    )

    const close: React.MouseEventHandler<HTMLDivElement> = React.useCallback(
      (e) => {
        onClose()
        e.stopPropagation()
      },
      [onClose],
    )

    return (
      <SimpleFlexRow
        onMouseEnter={setTabHoveredTrue}
        onMouseLeave={setTabHoveredFalse}
        onClick={props.onClick}
        onDoubleClick={props.onDoubleClick}
        onMouseDown={props.onMouseDown}
        style={{
          ...baseStyle,
          ...selectionHandlingStyle,
        }}
        className={props.className}
      >
        <SimpleFlexRow style={{ flexGrow: 1, marginRight: 32 }}>
          <SimpleFlexRow style={{ marginLeft: 4 }}>{icon}</SimpleFlexRow>
          <SimpleFlexRow style={{ marginLeft: 8 }}>{label}</SimpleFlexRow>
        </SimpleFlexRow>
        <Button
          css={{
            label: 'indicatorContainer',
            width: 18,
            height: 18,
            '&:active': {
              transform: 'scale(.92)',
            },
          }}
          onMouseEnter={setIndicatorHoveredTrue}
          onMouseLeave={setIndicatorHoveredFalse}
          onClick={close}
        >
          {showCloseIndicator ? (tabIsHovered ? tabHoveredIndicator : tabUnhoveredIndicator) : null}
        </Button>
      </SimpleFlexRow>
    )
  })
