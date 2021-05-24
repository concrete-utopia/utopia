import * as React from 'react'
import { LoginState, User } from '../../../uuiui-deps'

import { colorTheme } from '../../styles/theme'

interface AvatarProps {
  userPicture: string | null
  isLoggedIn: boolean
  size?: number
}

export class Avatar extends React.Component<AvatarProps> {
  size: string = (this.props.size ? this.props.size : '24') + 'px'

  /* Make the user wish they'd never logged in with an avatar-providing service. */
  /* Change these in   avatars.sketch and export from there */
  utopinoIndex = Math.round(Math.random() * 13)
  anonyminoIndex = Math.round(Math.random() * 10)

  fallbackLoggedOutImageURL =
    'url(/editor/avatars/anonymino' + this.anonyminoIndex.toString() + '.png)'
  fallbackLoggedInImageURL = 'url(/editor/avatars/utopino' + this.utopinoIndex.toString() + '.png)'

  render() {
    const imageURL =
      this.props.userPicture != null
        ? `url(${this.props.userPicture})`
        : this.props.isLoggedIn
        ? this.fallbackLoggedInImageURL
        : this.fallbackLoggedOutImageURL

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
          width: this.size,
          height: this.size,
          borderRadius: this.size,
          backgroundColor: colorTheme.emphasizedBackground.value,
        }}
      />
    )
  }
}
