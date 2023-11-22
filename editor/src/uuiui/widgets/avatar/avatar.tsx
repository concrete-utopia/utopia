import React from 'react'
import { LoginState, User } from '../../../uuiui-deps'

import { useColorTheme } from '../../styles/theme'
import { Icn } from '../../../../src/uuiui/icn'

interface AvatarProps {
  userPicture: string | null
  isLoggedIn: boolean
  size?: number
  isOwner: boolean
}

export const Avatar = React.memo((props: AvatarProps) => {
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
        overflow: 'visible',
        width: size,
        height: size,
        borderRadius: size,
        backgroundColor: colorTheme.emphasizedBackground.value,
      }}
    >
      {props.isOwner ? (
        <Icn
          category='semantic'
          type={'star'}
          width={14}
          height={14}
          color='main'
          style={{ position: 'relative', bottom: -13, left: 13, zIndex: 10 }}
        />
      ) : null}
    </div>
  )
})
