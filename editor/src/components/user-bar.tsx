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
import {
  Avatar,
  FlexRow,
  Icn,
  Tooltip,
  UtopiaStyles,
  color,
  colorTheme,
  useColorTheme,
} from '../uuiui'
import { Substores, useEditorState } from './editor/store/store-hook'
import { when } from '../utils/react-conditionals'
import { MultiplayerWrapper } from '../utils/multiplayer-wrapper'
import { useDispatch } from './editor/store/dispatch-context'
import { showToast, switchEditorMode } from './editor/actions/action-creators'
import type { EditorAction } from './editor/action-types'
import { EditorModes, isFollowMode } from './editor/editor-modes'
import { getCollaborator, useMyUserAndPresence } from '../core/commenting/comment-hooks'
import { notice } from './common/notice'
import { MenuProvider } from './context-menu-wrapper'

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
      {roomStatus === 'connected' && (
        <MultiplayerWrapper errorFallback={null} suspenseFallback={null}>
          <MultiplayerUserBar />
        </MultiplayerWrapper>
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
      {/* <FlexRow
        style={{
          width: 72,
          background: colorTheme.primary.value,
          borderRadius: '24px',
          color: 'white',
          // fontWeight: 600,
          gap: 5,
          border: `2px solid ${colorTheme.primary.value}`,
          boxShadow: UtopiaStyles.shadowStyles.low.boxShadow,
        }}
      >
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
        Share
      </FlexRow> */}

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
          <MultiplayerAvatar
            key={`avatar-${other.id}`}
            name={multiplayerInitialsFromName(name)}
            tooltip={name}
            color={multiplayerColorFromIndex(other.colorIndex)}
            picture={other.avatar}
            border={true}
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
    isBeingFollowed?: boolean
    size?: number
    follower?: boolean
    isOwner?: boolean
  }) => {
    const picture = React.useMemo(() => {
      return isDefaultAuth0AvatarURL(props.picture ?? null) ? null : props.picture
    }, [props.picture])

    return (
      <Tooltip
        title={`${props.tooltip}${props.follower === true ? ' (following you)' : ''}`}
        placement='bottom'
        backgroundColor={props.coloredTooltip ? props.color.background : undefined}
        textColor={props.coloredTooltip ? props.color.foreground : undefined}
      >
        <div
          style={{
            width: 24,
            height: 24,
            color: props.color.foreground,
            display: 'flex',
            alignItems: 'center',
            justifyContent: 'center',
            borderRadius: '100%',
            fontSize: 9,
            fontWeight: 700,
            cursor: 'pointer',
            position: 'relative',
            outline: props.follower ? `${colorTheme.primary.value} solid 1px ` : undefined,
            outlineOffset: props.follower ? -2 : undefined,
            // boxShadow: props.follower ? `0 0 1px ${colorTheme.primary.value}` : undefined,
          }}
          onClick={props.onClick}
        >
          <AvatarPicture url={picture} initials={props.name} size={24} />
          {props.isOwner ? <OwnerBadge2 /> : null}
          {/* {props.follower ? <FollowerBadge2 /> : null} */}
          {props.isBeingFollowed ? <FolloweeBadge /> : null}
        </div>
      </Tooltip>
    )
  },
)
MultiplayerAvatar.displayName = 'MultiplayerAvatar'

const FollowerBadge = React.memo(() => {
  return (
    <div>
      <div
        style={{
          position: 'absolute',
          bottom: -1,
          left: 0,
          borderRadius: '100%',
          // backgroundColor: colorTheme.primary.value,
          background: 'white',
          color: colorTheme.white.value,
          width: 8,
          height: 12,
          display: 'flex',
          alignItems: 'center',
          justifyContent: 'center',
          border: `1px solid ${colorTheme.black.value}`,
          zIndex: 1,
        }}
      >
        <div
          style={{
            width: 5,
            height: 6,
            background: 'black',
            borderRadius: '100%',
            position: 'absolute',
            bottom: 0,
            left: 0,
            border: `1px solid ${colorTheme.primary.value}`,
          }}
        ></div>
      </div>
      <div
        style={{
          position: 'absolute',
          bottom: -1,
          left: 7,
          borderRadius: '100%',
          backgroundColor: 'white',
          color: colorTheme.white.value,
          width: 8,
          height: 12,
          display: 'flex',
          alignItems: 'center',
          justifyContent: 'center',
          border: `1px solid ${colorTheme.black.value}`,
          zIndex: 1,
        }}
      >
        <div
          style={{
            width: 5,
            height: 6,
            background: 'black',
            borderRadius: '100%',
            position: 'absolute',
            bottom: 0,
            left: 0,
            border: `1px solid ${colorTheme.primary.value}`,
          }}
        ></div>
      </div>
    </div>
  )
})
FollowerBadge.displayName = 'FollowerBadge'

const FolloweeBadge = React.memo(() => {
  return (
    <div
      style={{
        position: 'absolute',
        bottom: -4,
        left: 1,
        backgroundColor: colorTheme.primary.value,
        width: 22,
        height: 2,
        borderRadius: 5,
      }}
    />
  )
})
FollowerBadge.displayName = 'FollowerBadge'

const FollowerBadge2 = React.memo(() => {
  return (
    <div
      style={{
        height: 8,
        width: 8,
        background: colorTheme.primary.value,
        borderRadius: '100%',
        border: `1px solid ${colorTheme.bg1.value}`,
        position: 'absolute',
        left: -1,
        top: 0,
      }}
    />
  )
})
FollowerBadge.displayName = 'FollowerBadge'

const OwnerBadge = React.memo(() => {
  return (
    <Icn
      category='semantic'
      type={'crown'}
      width={22}
      height={22}
      color='main'
      style={{ position: 'absolute', zIndex: 1, top: -10, left: 1 }}
    />
  )
})

OwnerBadge.displayName = 'OwnerBadge'

const OwnerBadge2 = React.memo(() => {
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
      draggable={false}
    />
  )
})
AvatarPicture.displayName = 'AvatarPicture'
