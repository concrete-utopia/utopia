import React from 'react'
import { useOthers, useStatus, useStorage } from '../../liveblocks.config'
import { getUserPicture, isLoggedIn } from '../common/user'
import type { MultiplayerColor } from '../core/shared/multiplayer'
import {
  canFollowTarget,
  isDefaultAuth0AvatarURL,
  multiplayerColorFromIndex,
  multiplayerInitialsFromName,
  normalizeMultiplayerName,
  normalizeOthersList,
} from '../core/shared/multiplayer'
import { Avatar, Tooltip, useColorTheme } from '../uuiui'
import { Substores, useEditorState } from './editor/store/store-hook'
import { when } from '../utils/react-conditionals'
import { MultiplayerWrapper } from '../utils/multiplayer-wrapper'
import { useDispatch } from './editor/store/dispatch-context'
import { showToast, switchEditorMode } from './editor/actions/action-creators'
import type { EditorAction } from './editor/action-types'
import { EditorModes, isFollowMode } from './editor/editor-modes'
import { getCollaborator, useMyUserAndPresence } from '../core/commenting/comment-hooks'
import { notice } from './common/notice'

const MAX_VISIBLE_OTHER_PLAYERS = 4

export const cannotFollowToastId = 'cannot-follow-toast-id'

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
      <MultiplayerWrapper errorFallback={null} suspenseFallback={null}>
        <MultiplayerUserBar />
      </MultiplayerWrapper>
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
  const dispatch = useDispatch()
  const colorTheme = useColorTheme()
  const collabs = useStorage((store) => store.collaborators)

  const { user: myUser } = useMyUserAndPresence()
  const myName = React.useMemo(() => normalizeMultiplayerName(myUser.name), [myUser])

  const others = useOthers((list) =>
    normalizeOthersList(myUser.id, list).map((other) => {
      return {
        ...getCollaborator(collabs, other),
        following: other.presence.following,
      }
    }),
  )

  const visibleOthers = React.useMemo(() => {
    return others.slice(0, MAX_VISIBLE_OTHER_PLAYERS)
  }, [others])
  const hiddenOthers = React.useMemo(() => {
    return others.slice(MAX_VISIBLE_OTHER_PLAYERS)
  }, [others])

  const mode = useEditorState(
    Substores.restOfEditor,
    (store) => store.editor.mode,
    'MultiplayerUserBar mode',
  )

  const toggleFollowing = React.useCallback(
    (targetId: string) => () => {
      let actions: EditorAction[] = []
      if (
        !canFollowTarget(
          myUser.id,
          targetId,
          others.map((o) => o),
        )
      ) {
        actions.push(
          showToast(
            notice(
              'Cannot follow this player at the moment.',
              'WARNING',
              false,
              cannotFollowToastId,
            ),
          ),
        )
      } else {
        const newMode =
          isFollowMode(mode) && mode.playerId === targetId
            ? EditorModes.selectMode(null, false, 'none')
            : EditorModes.followMode(targetId)
        actions.push(switchEditorMode(newMode))
      }
      dispatch(actions)
    },
    [dispatch, mode, myUser, others],
  )

  if (myUser.name == null) {
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
                picture={other.avatar}
                border={true}
                coloredTooltip={true}
                onClick={toggleFollowing(other.id)}
                active={isFollowMode(mode) && mode.playerId === other.id}
                follower={other.following === myUser.id}
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
            />,
          )}
        </div>,
      )}
      <a href='/projects' target='_blank'>
        <MultiplayerAvatar
          name={multiplayerInitialsFromName(myName)}
          tooltip={`${myName} (you)`}
          color={{ background: colorTheme.bg3.value, foreground: colorTheme.fg1.value }}
          picture={myUser.avatar}
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
    picture?: string | null
    border?: boolean
    onClick?: () => void
    active?: boolean
    size?: number
    follower?: boolean
  }) => {
    const picture = React.useMemo(() => {
      return isDefaultAuth0AvatarURL(props.picture ?? null) ? null : props.picture
    }, [props.picture])

    const colorTheme = useColorTheme()
    return (
      <Tooltip
        title={`${props.tooltip}${props.follower === true ? ' (following you)' : ''}`}
        placement='bottom'
        backgroundColor={props.coloredTooltip ? props.color.background : undefined}
        textColor={props.coloredTooltip ? props.color.foreground : undefined}
      >
        <div
          style={{
            width: props.size ?? 24,
            height: props.size ?? 24,
            backgroundColor: props.color.background,
            color: props.color.foreground,
            display: 'flex',
            alignItems: 'center',
            justifyContent: 'center',
            borderRadius: '100%',
            border: `3px solid ${props.active === true ? colorTheme.primary.value : 'transparent'}`,
            fontSize: 9,
            fontWeight: 700,
            cursor: 'pointer',
            boxShadow:
              props.active === true ? `0px 0px 15px ${colorTheme.primary.value}` : undefined,
            position: 'relative',
          }}
          onClick={props.onClick}
        >
          <AvatarPicture
            url={picture}
            size={props.border === true ? 22 : undefined}
            initials={props.name}
          />
          {when(
            props.follower === true,
            <div
              style={{
                position: 'absolute',
                top: -6,
                right: -6,
                borderRadius: '100%',
                backgroundColor: colorTheme.primary.value,
                color: colorTheme.white.value,
                width: 10,
                height: 10,
                display: 'flex',
                alignItems: 'center',
                justifyContent: 'center',
              }}
            />,
          )}
        </div>
      </Tooltip>
    )
  },
)
MultiplayerAvatar.displayName = 'MultiplayerAvatar'

interface AvatarPictureProps {
  url: string | null | undefined
  initials: string
  size?: number
}

export const AvatarPicture = React.memo((props: AvatarPictureProps) => {
  const url = React.useMemo(() => {
    return isDefaultAuth0AvatarURL(props.url ?? null) ? null : props.url
  }, [props.url])

  const { initials, size } = props

  const [pictureNotFound, setPictureNotFound] = React.useState(false)

  React.useEffect(() => {
    setPictureNotFound(false)
  }, [url])

  const onPictureError = React.useCallback(() => {
    console.warn('cannot get picture', url)
    setPictureNotFound(true)
  }, [url])

  if (url == null || pictureNotFound) {
    return <span>{initials}</span>
  }
  return (
    <img
      style={{
        width: size ?? '100%',
        height: size ?? '100%',
        borderRadius: '100%',
      }}
      src={url}
      referrerPolicy='no-referrer'
      onError={onPictureError}
    />
  )
})
AvatarPicture.displayName = 'AvatarPicture'
