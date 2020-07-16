import * as React from 'react'
import { LoginState, isLoggedIn } from './action-types'
import { auth0Url } from '../../core/shared/detect-env'
import { setRedirectUrl } from '../../common/persistence'
import { colorTheme, SimpleFlexRow, UtopiaStyles, UtopiaTheme } from 'uuiui'
import { useEditorState } from './store/store-hook'
import { betterReactMemo } from 'uuiui-deps'

import { NoticeLevel, NotificationBar } from '../common/notices'

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
  const { loginState, projectId } = useEditorState((store) => {
    return {
      loginState: store.loginState,
      projectId: store.editor.id,
    }
  })

  const onClickCallback = React.useCallback(() => {
    const redirectUrlSuffix = projectId == null ? '' : `/${projectId}`
    const redirectUrl = `/project${redirectUrlSuffix}`
    setRedirectUrl(redirectUrl).then(() => window.top.location.replace(auth0Url))
  }, [projectId])

  if (isLoggedIn(loginState)) {
    return null
  } else {
    return (
      <NotificationBar
        level='PRIMARY'
        message={'Welcome to Utopia. Click here to sign in and save your projects.'}
        onClick={onClickCallback}
        style={{ cursor: 'pointer' }}
      />
    )
  }
})
LoginStatusBar.displayName = 'LoginStatusBar'
