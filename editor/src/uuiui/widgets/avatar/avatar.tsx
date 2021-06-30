import * as React from 'react'
import { betterReactMemo, LoginState, User } from '../../../uuiui-deps'

import { useColorTheme } from '../../styles/theme'

interface AvatarProps {
  userPicture: string | null
  isLoggedIn: boolean
  size?: number
}

export const Avatar = betterReactMemo('Avatar', (props: AvatarProps) => {
  const colorTheme = useColorTheme()
  const size: string = (props.size ?? '24') + 'px'

  /* Make the user wish they'd never logged in with an avatar-providing service. */
  /* Change these in   avatars.sketch and export from there */
  const utopinoIndex = Math.round(Math.random() * 13)
  const anonyminoIndex = Math.round(Math.random() * 10)

  const fallbackLoggedOutImageURL =
    'url(/editor/avatars/anonymino' + anonyminoIndex.toString() + '.png)'
  const fallbackLoggedInImageURL = 'url(/editor/avatars/utopino' + utopinoIndex.toString() + '.png)'

  const imageURL =
    props.userPicture != null
      ? `url(${props.userPicture})`
      : props.isLoggedIn
      ? fallbackLoggedInImageURL
      : fallbackLoggedOutImageURL

  const backgroundStyle = {
    backgroundImage: imageURL,
    backgroundPosition: 'center',
    backgroundSize: 'cover',
    backgroundRepeat: 'no-repeat',
  }

  return (
    <div
      className='user-avatar'
      style={{
        ...backgroundStyle,
        justifyContent: 'center',
        overflow: 'hidden',
        width: size,
        height: size,
        borderRadius: size,
        backgroundColor: colorTheme.emphasizedBackground.value,
      }}
    />
  )
})
