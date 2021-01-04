/** @jsx jsx */
import { jsx } from '@emotion/react'
import * as EditorActions from '../editor/actions/action-creators'
import * as React from 'react'
import { UtopiaStyles, SimpleFlexRow, UtopiaTheme, SimpleFlexColumn } from '../../uuiui'
import { useEditorState } from '../editor/store/store-hook'
import { Notice, NoticeLevel } from './notice'

interface NoticeProps extends Notice {
  style?: React.CSSProperties
  onClick?: () => void
}

export const getStylesForLevel = (level: NoticeLevel): React.CSSProperties => {
  let resultingStyle = UtopiaStyles.noticeStyles.info

  if (level === 'WARNING') {
    resultingStyle = UtopiaStyles.noticeStyles.warning
  } else if (level === 'ERROR') {
    resultingStyle = UtopiaStyles.noticeStyles.error
  } else if (level === 'SUCCESS') {
    resultingStyle = UtopiaStyles.noticeStyles.success
  } else if (level === 'PRIMARY') {
    resultingStyle = UtopiaStyles.noticeStyles.primary
  }
  return resultingStyle
}

/**
 * Show information in a stack of toasts at the bottom of the editor
 *
 * **Layout**: use as flex child with fixed height
 * **Level**: see NoticeLevel jsdoc
 */
export const Toast: React.FunctionComponent<NoticeProps> = (props) => {
  const dispatch = useEditorState((store) => store.dispatch, 'Toast dispatch')
  const deleteNotice = React.useCallback(() => {
    dispatch([EditorActions.removeToast(props.id)])
  }, [dispatch, props.id])

  return (
    <div
      key={'toast-item'}
      style={{
        ...getStylesForLevel(props.level || 'INFO'),
        boxShadow: UtopiaStyles.shadowStyles.medium.boxShadow,
        borderRadius: 3,
        width: 270,
        minHeight: 27,
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
        style={{ flexGrow: 1, fontWeight: 500, padding: 8, display: 'flex', alignItems: 'center' }}
        id='toast-message'
      >
        {props.message}
      </div>

      <div
        css={{
          backgroundColor: 'hsl(0,0%,0%,3%)',
          display: 'flex',
          flex: '0 0 24px',
          alignItems: 'center',
          justifyContent: 'center',
          fontSize: 14,
          cursor: 'pointer',
          '&:hover': {
            backgroundColor: 'hsl(0,0%,0%,5%)',
          },
          '&:active': {
            backgroundColor: 'hsl(0,0%,0%,6%)',
          },
        }}
        onClick={deleteNotice}
        id='toast-button'
      >
        ×
      </div>
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
export const NotificationBar: React.FunctionComponent<NotificationBarProps> = (props) => (
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
export const InfoBox: React.FunctionComponent<InfoBoxProps> = (props) => (
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
