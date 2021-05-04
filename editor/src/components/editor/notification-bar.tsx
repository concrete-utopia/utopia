import * as React from 'react'
import { LoginState, isLoggedIn } from './action-types'
import { auth0Url } from '../../common/env-vars'
import { setRedirectUrl } from '../../common/persistence'
import { useEditorState } from './store/store-hook'

import { NotificationBar } from '../common/notices'
import { NoticeLevel } from '../common/notice'
import { betterReactMemo } from '../../uuiui-deps'

export const BrowserInfoBar = betterReactMemo('EditorOfflineBar', () => {
  return (
    <NotificationBar level='INFO' message={`Utopia works best and fastest in Chrome right now`} />
  )
})

export const EditorOfflineBar = betterReactMemo('EditorOfflineBar', () => {
  return (
    <NotificationBar
      level='ERROR'
      message={`Utopia is offline, and will reconnect automatically.`}
    />
  )
})

export const LoginStatusBar = betterReactMemo('LoginStatusBar', () => {
  const loginState = useEditorState((store) => store.userState.loginState, 'LoginStatusBar')

  const onClickLoginSameTab = React.useCallback(() => {
    setRedirectUrl(window.top.location.pathname).then(() => window.top.location.replace(auth0Url))
  }, [])

  const onClickLoginNewTab = React.useCallback(() => {
    window.open(auth0Url, '_blank')
  }, [])

  switch (loginState.type) {
    case 'LOGGED_IN':
      return null
    case 'NOT_LOGGED_IN':
      return (
        <NotificationBar
          level='PRIMARY'
          message={'Welcome to Utopia. Click here to sign in and save your projects.'}
          onClick={onClickLoginSameTab}
          style={{ cursor: 'pointer' }}
        />
      )
    case 'LOGIN_LOST':
      return (
        <NotificationBar
          level='ERROR'
          message={'You have been logged out. Click here to log in again and save your changes.'}
          onClick={onClickLoginNewTab}
          style={{ cursor: 'pointer' }}
        />
      )
    default:
      const _exhaustiveCheck: never = loginState
      throw new Error(`Unhandled login state ${loginState}`)
  }
})
LoginStatusBar.displayName = 'LoginStatusBar'
