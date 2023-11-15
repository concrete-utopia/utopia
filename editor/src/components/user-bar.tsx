import { ClientSideSuspense } from '@liveblocks/react'
import React from 'react'
import { useOthers, useSelf, useStatus } from '../../liveblocks.config'
import { getUserPicture, isLoggedIn } from '../common/user'
import type { MultiplayerColor } from '../core/shared/multiplayer'
import {
  multiplayerColorFromIndex,
  multiplayerInitialsFromName,
  normalizeMultiplayerName,
  normalizeOthersList,
} from '../core/shared/multiplayer'
import { Avatar, Tooltip, useColorTheme } from '../uuiui'
import { Substores, useEditorState } from './editor/store/store-hook'

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
  const myName = React.useMemo(
    () => normalizeMultiplayerName(self.presence.name),
    [self.presence.name],
  )
  const others = useOthers((list) =>
    normalizeOthersList(self.id, list).map((other) => ({
      id: other.id,
      name: other.presence.name,
      colorIndex: other.presence.colorIndex,
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
      {visibleOthers.length > 0 && (
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
              />
            )
          })}
          {hiddenOthers.length > 0 && (
            <MultiplayerAvatar
              name={`+${hiddenOthers.length}`}
              tooltip={hiddenOthers.map((c) => normalizeMultiplayerName(c.name)).join(', ')}
              color={{
                background: colorTheme.fg8.value,
                foreground: colorTheme.fg0.value,
              }}
            />
          )}
        </div>
      )}
      <MultiplayerAvatar
        name={multiplayerInitialsFromName(myName)}
        tooltip={`${myName} (you)`}
        color={{ background: colorTheme.bg3.value, foreground: colorTheme.fg1.value }}
      />
    </div>
  )
})
MultiplayerUserBar.displayName = 'MultiplayerUserBar'

const MultiplayerAvatar = React.memo(
  (props: { name: string; tooltip: string; color: MultiplayerColor; border?: string }) => {
    return (
      <Tooltip title={props.tooltip} placement='bottom'>
        <div
          style={{
            width: 24,
            height: 24,
            background: props.color.background,
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
          {props.name}
        </div>
      </Tooltip>
    )
  },
)
MultiplayerAvatar.displayName = 'MultiplayerAvatar'
