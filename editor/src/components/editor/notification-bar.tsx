import React from 'react'
import { auth0Url } from '../../common/env-vars'
import { useEditorState } from './store/store-hook'

import { NotificationBar } from '../common/notices'

export const BrowserInfoBar = React.memo(() => {
  return (
    <NotificationBar level='INFO' message={`Utopia works best and fastest in Chrome right now`} />
  )
})

const EditorOfflineBar = React.memo(() => {
  return (
    <NotificationBar
      level='ERROR'
      message={`Utopia is offline, and will reconnect automatically.`}
    />
  )
})

export const LoginStatusBar = React.memo(() => {
  const loginState = useEditorState((store) => store.userState.loginState, 'LoginStatusBar')
  const saveError = useEditorState(
    (store) => store.editor.saveError,
    'EditorComponentInner saveError',
  )

  const onClickLoginNewTab = React.useCallback(() => {
    window.open(auth0Url('auto-close'), '_blank')
  }, [])

  if (saveError) {
    return <EditorOfflineBar />
  }

  switch (loginState.type) {
    case 'LOGIN_NOT_YET_KNOWN':
      return null
    case 'LOGGED_IN':
      return null
    case 'OFFLINE_STATE':
      return <EditorOfflineBar />
    case 'NOT_LOGGED_IN':
      return null
    case 'LOGIN_LOST':
      return (
        <NotificationBar
          level='ERROR'
          message={'You have been logged out. Click here to log in again and save your changes.'}
          onClick={onClickLoginNewTab}
          style={{ cursor: 'pointer' }}
        />
      )
    case 'COOKIES_OR_LOCALFORAGE_UNAVAILABLE':
      return (
        <NotificationBar
          level='ERROR'
          message={
            'Cookies or IndexedDB are currently disabled in this browser. Please enable those and then refresh this window.'
          }
        />
      )
    default:
      const _exhaustiveCheck: never = loginState
      throw new Error(`Unhandled login state ${loginState}`)
  }
})
LoginStatusBar.displayName = 'LoginStatusBar'
