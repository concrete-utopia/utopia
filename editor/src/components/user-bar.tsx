import type { CSSProperties } from 'react'
import React from 'react'
import { useOthers, useStatus, useStorage } from '../../liveblocks.config'
import { getUserPicture, isLoggedIn, isOfflineState } from '../common/user'
import { getCollaborator, useMyUserAndPresence } from '../core/commenting/comment-hooks'
import type { MultiplayerColor } from '../core/shared/multiplayer'
import {
  canFollowTarget,
  isDefaultAuth0AvatarURL,
  multiplayerColorFromIndex,
  multiplayerInitialsFromName,
  normalizeMultiplayerName,
  normalizeOthersList,
} from '../core/shared/multiplayer'
import { MultiplayerWrapper } from '../utils/multiplayer-wrapper'
import { when } from '../utils/react-conditionals'
import { Avatar, FlexRow, Icn, Tooltip, color, colorTheme } from '../uuiui'
import { notice } from './common/notice'
import type { EditorAction } from './editor/action-types'
import { showToast, switchEditorMode } from './editor/actions/action-creators'
import { EditorModes, isFollowMode } from './editor/editor-modes'
import { useDispatch } from './editor/store/dispatch-context'
import { Substores, useEditorState } from './editor/store/store-hook'

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
  }
  return (
    <FlexRow style={{ gap: 4 }}>
      {when(
        roomStatus === 'connected',
        <MultiplayerWrapper errorFallback={null} suspenseFallback={null}>
          <MultiplayerUserBar />
        </MultiplayerWrapper>,
      )}
      <SinglePlayerUserBar />
    </FlexRow>
  )
})
UserBar.displayName = 'UserBar'

export const SinglePlayerUserBar = React.memo(() => {
  const userPicture = useEditorState(
    Substores.userState,
    (store) => getUserPicture(store.userState.loginState),
    'SinglePlayerUserBar userPicture',
  )
  const amIOwner = useEditorState(
    Substores.projectServerState,
    (store) => store.projectServerState.isMyProject === 'yes',
    'SinglePlayerUserBar amIOwner',
  )
  return (
    <a href='/projects' target='_blank'>
      <div
        style={{
          width: 24,
          height: 24,
          position: 'relative',
        }}
      >
        <Avatar userPicture={userPicture} isLoggedIn={true} />
        {amIOwner ? <OwnerBadge /> : null}
      </div>
    </a>
  )
})
SinglePlayerUserBar.displayName = 'SinglePlayerUserBar'

const MultiplayerUserBar = React.memo(() => {
  const dispatch = useDispatch()
  const { user: myUser } = useMyUserAndPresence()
  const collabs = useStorage((store) => store.collaborators)

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

  const offlineOthers = Object.values(collabs).filter((collab) => {
    return !others.some((other) => other.id === collab.id)
  })

  const mode = useEditorState(
    Substores.restOfEditor,
    (store) => store.editor.mode,
    'MultiplayerUserBar mode',
  )

  const ownerId = useEditorState(
    Substores.projectServerState,
    (store) => store.projectServerState.ownerId,
    'MultiplayerUserBar ownerId',
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
        justifyContent: 'center',
        gap: 4,
      }}
    >
      {visibleOthers.map((other) => {
        if (other == null) {
          return null
        }
        const name = normalizeMultiplayerName(other.name)
        const isOwner = ownerId === other.id
        return (
          <MultiplayerAvatarWithTooltip
            key={`avatar-${other.id}`}
            name={multiplayerInitialsFromName(name)}
            tooltip={name}
            color={multiplayerColorFromIndex(other.colorIndex)}
            picture={other.avatar}
            coloredTooltip={true}
            onClick={toggleFollowing(other.id)}
            isBeingFollowed={isFollowMode(mode) && mode.playerId === other.id}
            follower={other.following === myUser.id}
            isOwner={isOwner}
          />
        )
      })}
      {when(
        hiddenOthers.length > 0,
        <MultiplayerAvatarWithTooltip
          name={`+${hiddenOthers.length}`}
          tooltip={hiddenOthers.map((c) => normalizeMultiplayerName(c.name)).join(', ')}
          color={{
            background: colorTheme.fg8.value,
            foreground: colorTheme.fg0.value,
          }}
          picture={null}
        />,
      )}
      {when(
        offlineOthers.length > 0,
        offlineOthers.map((other) => {
          if (other == null) {
            return null
          }
          const name = normalizeMultiplayerName(other.name)
          const isOwner = ownerId === other.id
          return (
            <MultiplayerAvatarWithTooltip
              key={`avatar-${other.id}`}
              name={multiplayerInitialsFromName(name)}
              tooltip={name}
              color={multiplayerColorFromIndex(other.colorIndex)}
              picture={other.avatar}
              coloredTooltip={true}
              isOwner={isOwner}
              isOffline={true}
            />
          )
        }),
      )}
    </div>
  )
})
MultiplayerUserBar.displayName = 'MultiplayerUserBar'

const MultiplayerAvatarWithTooltip = React.memo(
  ({
    tooltip,
    coloredTooltip,
    ...otherProps
  }: { tooltip: string; coloredTooltip?: boolean } & MultiplayerAvatarProps) => {
    return (
      <Tooltip
        title={`${tooltip}${otherProps.follower === true ? ' (following you)' : ''}`}
        placement='bottom'
        backgroundColor={coloredTooltip ? otherProps.color.background : undefined}
        textColor={coloredTooltip ? otherProps.color.foreground : undefined}
      >
        <MultiplayerAvatar {...otherProps} />
      </Tooltip>
    )
  },
)
MultiplayerAvatarWithTooltip.displayName = 'MultiplayerAvatarWithTooltip'

export type MultiplayerAvatarProps = {
  name: string
  color: MultiplayerColor
  coloredTooltip?: boolean
  picture?: string | null
  onClick?: () => void
  isBeingFollowed?: boolean
  size?: number
  follower?: boolean
  isOwner?: boolean
  style?: CSSProperties
  isOffline?: boolean
}

export const MultiplayerAvatar = React.memo((props: MultiplayerAvatarProps) => {
  const picture = React.useMemo(() => {
    return isDefaultAuth0AvatarURL(props.picture ?? null) ? null : props.picture
  }, [props.picture])
  return (
    <div
      style={{
        width: props.size ?? 24,
        height: props.size ?? 24,
        backgroundColor: props.isOffline ? colorTheme.bg4.value : props.color.background,
        color: props.isOffline ? colorTheme.fg2.value : props.color.foreground,
        display: 'flex',
        alignItems: 'center',
        justifyContent: 'center',
        borderRadius: '100%',
        fontSize: 9,
        fontWeight: 700,
        cursor: props.onClick != null ? 'pointer' : 'default',
        position: 'relative',
        outline: `.3px solid ${colorTheme.bg1.value}`,
        boxShadow:
          props.isBeingFollowed === true
            ? `0px 0px 8px ${colorTheme.dynamicBlue.value}`
            : undefined,
        ...props.style,
      }}
      onClick={props.onClick}
    >
      <AvatarPicture url={picture} size={24} initials={props.name} isOffline={props.isOffline} />
      {props.isOwner ? <OwnerBadge /> : null}
      {props.follower ? <FollowerBadge /> : null}
    </div>
  )
})
MultiplayerAvatar.displayName = 'MultiplayerAvatar'

const FollowerBadge = React.memo(() => {
  return (
    <div
      style={{
        position: 'absolute',
        top: -1,
        left: -1,
        borderRadius: '100%',
        backgroundColor: colorTheme.primary.value,
        color: colorTheme.white.value,
        width: 8,
        height: 8,
        display: 'flex',
        alignItems: 'center',
        justifyContent: 'center',
        border: `1px solid ${colorTheme.bg1.value}`,
      }}
    />
  )
})
FollowerBadge.displayName = 'FollowerBadge'

const OwnerBadge = React.memo(() => {
  return (
    <Icn
      category='semantic'
      type={'star'}
      width={14}
      height={14}
      color='main'
      style={{ position: 'absolute', zIndex: 1, bottom: -1, left: -2 }}
    />
  )
})

OwnerBadge.displayName = 'OwnerBadge'

interface AvatarPictureProps {
  url: string | null | undefined
  initials: string
  size?: number
  isOffline?: boolean
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
        filter: props.isOffline ? 'grayscale(1)' : undefined,
      }}
      src={url}
      referrerPolicy='no-referrer'
      onError={onPictureError}
      draggable={false}
    />
  )
})
AvatarPicture.displayName = 'AvatarPicture'
