import { ClientSideSuspense } from '@liveblocks/react'
import React from 'react'
import { useOthers, useSelf, useStatus } from '../../liveblocks.config'
import { getUserPicture, isLoggedIn } from '../common/user'
import type { MultiplayerColor } from '../core/shared/multiplayer'
import {
  isDefaultAuth0AvatarURL,
  multiplayerColorFromIndex,
  multiplayerInitialsFromName,
  normalizeMultiplayerName,
  normalizeOthersList,
} from '../core/shared/multiplayer'
import { Avatar, Tooltip, useColorTheme } from '../uuiui'
import { Substores, useEditorState } from './editor/store/store-hook'
import { when } from '../utils/react-conditionals'

const MAX_VISIBLE_OTHER_PLAYERS = 4

export const UserBar = React.memo(() => {
  const loginState = useEditorState(
    Substores.userState,
    (store) => store.userState.loginState,
    'UserBar loginState',
  )

  const roomStatus = useStatus()

  if (!isLoggedIn(loginState)) {
    return null
  } else if (roomStatus !== 'connected') {
    return <SinglePlayerUserBar />
  } else {
    return (
      <ClientSideSuspense fallback={<div />}>{() => <MultiplayerUserBar />}</ClientSideSuspense>
    )
  }
})
UserBar.displayName = 'UserBar'

export const SinglePlayerUserBar = React.memo(() => {
  const userPicture = useEditorState(
    Substores.userState,
    (store) => getUserPicture(store.userState.loginState),
    'SinglePlayerUserBar userPicture',
  )
  return (
    <a href='/projects' target='_blank'>
      <Avatar userPicture={userPicture} isLoggedIn={true} />
    </a>
  )
})
SinglePlayerUserBar.displayName = 'SinglePlayerUserBar'

const MultiplayerUserBar = React.memo(() => {
  const colorTheme = useColorTheme()

  const self = useSelf()
  const myName = normalizeMultiplayerName(self.presence.name)

  const others = useOthers((list) =>
    normalizeOthersList(self.id, list).map((other) => ({
      id: other.id,
      name: other.presence.name,
      colorIndex: other.presence.colorIndex,
      picture: other.presence.picture,
    })),
  )

  const visibleOthers = React.useMemo(() => {
    return others.slice(0, MAX_VISIBLE_OTHER_PLAYERS)
  }, [others])
  const hiddenOthers = React.useMemo(() => {
    return others.slice(MAX_VISIBLE_OTHER_PLAYERS)
  }, [others])

  if (self.presence.name == null) {
    // it may still be loading, so fallback until it sorts itself out
    return <SinglePlayerUserBar />
  }

  return (
    <div
      style={{
        display: 'flex',
        alignItems: 'center',
        gap: 4,
      }}
    >
      {when(
        visibleOthers.length > 0,
        <div
          style={{
            display: 'flex',
            alignItems: 'center',
            gap: 4,
            background: colorTheme.bg3.value,
            borderRadius: '20px',
            justifyContent: 'center',
            padding: '4px',
          }}
        >
          {visibleOthers.map((other) => {
            if (other == null) {
              return null
            }
            const name = normalizeMultiplayerName(other.name)
            return (
              <MultiplayerAvatar
                key={`avatar-${other.id}`}
                name={multiplayerInitialsFromName(name)}
                tooltip={name}
                color={multiplayerColorFromIndex(other.colorIndex)}
                picture={other.picture}
                border={true}
                coloredTooltip={true}
              />
            )
          })}
          {when(
            hiddenOthers.length > 0,
            <MultiplayerAvatar
              name={`+${hiddenOthers.length}`}
              tooltip={hiddenOthers.map((c) => normalizeMultiplayerName(c.name)).join(', ')}
              color={{
                background: colorTheme.fg8.value,
                foreground: colorTheme.fg0.value,
              }}
              picture={null}
              border={false}
            />,
          )}
        </div>,
      )}
      <a href='/projects' target='_blank'>
        <MultiplayerAvatar
          name={multiplayerInitialsFromName(myName)}
          tooltip={`${myName} (you)`}
          color={{ background: colorTheme.bg3.value, foreground: colorTheme.fg1.value }}
          picture={self.presence.picture}
          border={false}
        />
      </a>
    </div>
  )
})
MultiplayerUserBar.displayName = 'MultiplayerUserBar'

const MultiplayerAvatar = React.memo(
  (props: {
    name: string
    tooltip: string
    color: MultiplayerColor
    coloredTooltip?: boolean
    picture: string | null
    border: boolean
  }) => {
    const picture = React.useMemo(() => {
      return isDefaultAuth0AvatarURL(props.picture) ? null : props.picture
    }, [props.picture])
    return (
      <Tooltip
        title={props.tooltip}
        placement='bottom'
        backgroundColor={props.coloredTooltip ? props.color.background : undefined}
        textColor={props.coloredTooltip ? props.color.foreground : undefined}
      >
        <div
          style={{
            width: 24,
            height: 24,
            backgroundColor: props.color.background,
            color: props.color.foreground,
            display: 'flex',
            alignItems: 'center',
            justifyContent: 'center',
            borderRadius: '100%',
            fontSize: 9,
            fontWeight: 700,
            cursor: 'pointer',
          }}
        >
          {when(picture == null, props.name)}
          {when(
            picture != null,
            // Using an img tag instead of using it as backgroundColor above because of potential 403s
            <img
              style={{
                width: props.border ? 22 : '100%',
                height: props.border ? 22 : '100%',
                borderRadius: '100%',
              }}
              src={props.picture ?? ''}
              referrerPolicy='no-referrer'
            />,
          )}
        </div>
      </Tooltip>
    )
  },
)
MultiplayerAvatar.displayName = 'MultiplayerAvatar'
