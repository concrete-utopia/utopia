/** @jsx jsx */
import { jsx } from '@emotion/core'
import * as React from 'react'
import { UtopiaStyles, FlexRow, SimpleFlexRow, SimpleFlexColumn, UtopiaTheme } from 'uuiui'

/**
 *  - _Error_: High visibility
 *  - _Warning_: High visisibility
 *  - _Primary_: Medium high visibility. Use for CTAs, nudges
 *  - _Success_: Medium visibility. Use for async or higher-failure-probability positive outcomes
 *  - _Notice_: Medium visibility. Use for synchronous info, confirmation, or to stand out from background
 *  - _Info_: Low visibility. Use to display information that can blend into background
 *
 * */
export type NoticeLevel = 'ERROR' | 'WARNING' | 'PRIMARY' | 'SUCCESS' | 'NOTICE' | 'INFO'
export interface Notice {
  message: React.ReactChild
  persistent?: boolean
  level?: NoticeLevel
}

export function notice(
  message: React.ReactChild,
  persistent: boolean = false,
  level: NoticeLevel = 'INFO',
): Notice {
  return { message: message, persistent: persistent, level: level }
}

interface NoticeProps extends Notice {
  style?: React.CSSProperties
  onClick?: () => void
}

export const getStylesForLevel = (level: NoticeLevel): React.CSSProperties => {
  let resultingStyle = UtopiaStyles.darkNoticeStyles.info

  if (level === 'WARNING') {
    resultingStyle = UtopiaStyles.darkNoticeStyles.warning
  } else if (level === 'ERROR') {
    resultingStyle = UtopiaStyles.darkNoticeStyles.error
  } else if (level === 'SUCCESS') {
    resultingStyle = UtopiaStyles.darkNoticeStyles.success
  } else if (level === 'PRIMARY') {
    resultingStyle = UtopiaStyles.darkNoticeStyles.primary
  }
  return resultingStyle
}

/**
 * Show information in a stack of toasts at the bottom of the editor
 *
 * **Layout**: use as flex child with fixed height
 * **Level**: see NoticeLevel jsdoc
 */
export const Toast: React.FunctionComponent<NoticeProps> = (props) => (
  <div
    key={'toast-item'}
    // className={'fadeout'}
    style={{
      ...getStylesForLevel(props.level || 'INFO'),
      opacity: 1,
      borderRadius: 3,
      overflowWrap: 'break-word',
      wordWrap: 'break-word',
      hyphens: 'auto',
      whiteSpace: 'normal',
      maxWidth: 400,
      width: 400,
      padding: 12,
      fontWeight: 500,
      letterSpacing: 0.2,
      margin: '5px',
    }}
  >
    {props.message}
  </div>
)

/**
 * Show information atop the editor
 *
 * **Layout**: use as flex child with fixed height
 * **Level**: see NoticeLevel jsdoc
 */
export const NotificationBar: React.FunctionComponent<NoticeProps> = (props) => (
  <SimpleFlexRow
    style={{
      flexGrow: 0,
      minHeight: UtopiaTheme.layout.rowHeight.small,
      fontWeight: 600,
      letterSpacing: 0.2,
      alignItems: 'center',
      justifyContent: 'center',
      ...props.style,
      ...getStylesForLevel(props.level || 'INFO'),
    }}
    onClick={props.onClick}
  >
    {props.message}&nbsp;{props.children}
  </SimpleFlexRow>
)

/**
 * Displays information in color-coded box eg in the inspector or navigator
 *
 * **Layout**: takes full width, sizes itself to height
 * **Level**: see NoticeLevel jsdoc
 */
export const InfoBox: React.FunctionComponent<NoticeProps> = (props) => (
  <SimpleFlexColumn
    style={{
      padding: 8,
      alignItems: 'stretch',
      flexGrow: 1,
      ...getStylesForLevel(props.level || 'INFO'),
    }}
  >
    <SimpleFlexRow style={{ fontWeight: 600 }}>{props.message}</SimpleFlexRow>
    <SimpleFlexRow>{props.children}</SimpleFlexRow>
  </SimpleFlexColumn>
)
