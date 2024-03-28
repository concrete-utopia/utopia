import classNames from 'classnames'
import type { CSSProperties } from 'react'
import React from 'react'
import { useIsDarkMode } from '../hooks/useIsDarkMode'
import { sprinkles } from '../styles/sprinkles.css'
import { when } from '../util/react-conditionals'
import { multiplayerInitialsFromName } from '../util/strings'
import { useCDNLink } from '../util/cdnLink'

export const UserAvatar = React.memo(
  ({
    name,
    picture,
    starBadge,
    size,
    style,
    className,
  }: {
    name: string
    picture: string | null
    starBadge?: boolean
    size: number
    style?: CSSProperties
    className?: string
  }) => {
    const cdnLink = useCDNLink()

    const isDarkMode = useIsDarkMode()
    const starIcon = React.useMemo(() => {
      return isDarkMode ? 'star-white-14x14@2x.png' : 'star-black-14x14@2x.png'
    }, [isDarkMode])

    return (
      <div
        className={classNames(sprinkles({ backgroundColor: 'secondary' }), className)}
        style={{
          width: size,
          height: size,
          backgroundImage: `url("${picture ?? ''}")`,
          backgroundSize: `${size}px`,
          backgroundRepeat: 'no-repeat',
          position: 'relative',
          display: 'flex',
          justifyContent: 'center',
          alignItems: 'center',
          fontSize: '.9em',
          fontWeight: 700,
          color: isDarkMode ? 'white' : 'black',
          ...style,
        }}
      >
        {when(
          starBadge === true,
          <img
            src={cdnLink(starIcon)}
            style={{
              width: 13,
              height: 13,
              position: 'absolute',
              bottom: -3,
              left: -4,
            }}
          />,
        )}
        {when(picture == null || picture === '', multiplayerInitialsFromName(name))}
      </div>
    )
  },
)
UserAvatar.displayName = 'UserAvatar'
