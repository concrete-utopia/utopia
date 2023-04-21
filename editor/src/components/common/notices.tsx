/** @jsxRuntime classic */
/** @jsx jsx */
import { jsx } from '@emotion/react'
import * as EditorActions from '../editor/actions/action-creators'
import React from 'react'
import { UtopiaStyles, SimpleFlexRow, UtopiaTheme, SimpleFlexColumn } from '../../uuiui'
import { Notice, NoticeLevel } from './notice'
import { useDispatch } from '../editor/store/dispatch-context'
import { assertNever } from '../../core/shared/utils'

interface NoticeProps extends Notice {
  style?: React.CSSProperties
  onClick?: () => void
}

export const getStylesForLevel = (level: NoticeLevel): React.CSSProperties => {
  let resultingStyle = UtopiaStyles.noticeStyles.info

  switch (level) {
    case 'WARNING':
      return UtopiaStyles.noticeStyles.warning
    case 'ERROR':
      return UtopiaStyles.noticeStyles.error
    case 'SUCCESS':
      return UtopiaStyles.noticeStyles.success
    case 'PRIMARY':
      return UtopiaStyles.noticeStyles.primary
    case 'INFO':
      return UtopiaStyles.noticeStyles.info
    case 'NOTICE':
      return UtopiaStyles.noticeStyles.notice
    default:
      assertNever(level)
  }
}

export const getPrefixForLevel = (level: NoticeLevel): string => {
  switch (level) {
    case 'WARNING':
      return '﹗'
    case 'ERROR':
      return '⚠️'
    case 'SUCCESS':
      return '✓'
    default:
      return ''
  }
}

const ToastTimeout = 5500

/**
 * Show information in a stack of toasts at the bottom of the editor
 *
 * **Layout**: use as flex child with fixed height
 * **Level**: see NoticeLevel jsdoc
 */
export const Toast: React.FunctionComponent<React.PropsWithChildren<NoticeProps>> = (props) => {
  const dispatch = useDispatch()
  const deleteToast = React.useCallback(() => {
    dispatch([EditorActions.removeToast(props.id)])
  }, [dispatch, props.id])

  React.useEffect(() => {
    // timeout type conflicts between node and browser – but this will work for both, trust me
    const timeoutId: any = props.persistent ? null : setTimeout(deleteToast, ToastTimeout)
    return function cleanup() {
      clearTimeout(timeoutId)
    }
  }, [props.persistent, deleteToast])

  return (
    <div
      key={'toast-item'}
      style={{
        background: '#111',
        ...getStylesForLevel(props.level ?? 'INFO'),
        borderRadius: 6,
        boxShadow:
          '0px 0px .5px rgba(0, 0, 0, .12), 0px 10px 16px rgba(0, 0, 0, .12), 0px 2px 5px rgba(0, 0, 0, .15), 0px 2px 14px rgba(0, 0, 0, .15), 0px 0px 0px 0.5px rgba(0, 0, 0, .2)',
        color: 'white',
        width: 290,
        minHeight: 27,
        fontSize: 12,
        fontWeight: 400,
        letterSpacing: 0.2,
        fontFamily: 'utopian-Inter',
        overflow: 'hidden',
        overflowWrap: 'break-word',
        wordWrap: 'break-word',
        hyphens: 'auto',
        whiteSpace: 'normal',
        margin: '5px',
        display: 'flex',
        alignItems: 'stretch',
      }}
    >
      <div
        style={{ flexGrow: 1, padding: 8, display: 'flex', alignItems: 'center' }}
        id='toast-message'
      >
        {getPrefixForLevel(props.level)}&nbsp;
        {props.message}
      </div>
      {props.persistent ? (
        <div
          css={{
            display: 'flex',
            flex: '0 0 24px',
            alignItems: 'center',
            justifyContent: 'center',
            fontSize: 14,
            cursor: 'pointer',
            opacity: 0.3,
            '&:hover': {
              opacity: 1,
            },
          }}
          onClick={deleteToast}
          id='toast-button'
        >
          ×
        </div>
      ) : null}
    </div>
  )
}

interface NotificationBarProps {
  message: React.ReactChild
  level?: NoticeLevel
  style?: React.CSSProperties
  onClick?: () => void
}
/**
 * Show information atop the editor
 *
 * **Layout**: use as flex child with fixed height
 * **Level**: see NoticeLevel jsdoc
 */
export const NotificationBar: React.FunctionComponent<
  React.PropsWithChildren<NotificationBarProps>
> = (props) => (
  <SimpleFlexRow
    style={{
      flexGrow: 0,
      minHeight: 32,
      fontWeight: 600,
      letterSpacing: 0.2,
      alignItems: 'center',
      justifyContent: 'center',
      ...props.style,
      ...getStylesForLevel(props.level ?? 'INFO'),
    }}
    onClick={props.onClick}
  >
    {props.message}&nbsp;{props.children}
  </SimpleFlexRow>
)

interface InfoBoxProps {
  message: React.ReactChild
  level?: NoticeLevel
  style?: React.CSSProperties
  onClick?: () => void
}
/**
 * Displays information in color-coded box eg in the inspector or navigator
 *
 * **Layout**: takes full width, sizes itself to height
 * **Level**: see NoticeLevel jsdoc
 */
export const InfoBox: React.FunctionComponent<React.PropsWithChildren<InfoBoxProps>> = (props) => (
  <SimpleFlexColumn
    style={{
      padding: 8,
      alignItems: 'stretch',
      flexGrow: 1,
      ...getStylesForLevel(props.level ?? 'INFO'),
    }}
  >
    <SimpleFlexRow style={{ fontWeight: 600 }}>{props.message}</SimpleFlexRow>
    <SimpleFlexRow>{props.children}</SimpleFlexRow>
  </SimpleFlexColumn>
)
