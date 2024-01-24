/** @jsxRuntime classic */
/** @jsx jsx */
import React from 'react'
import { jsx } from '@emotion/react'
import type { CSSProperties } from 'react'
import { useOthers, useStatus, useStorage } from '../../liveblocks.config'
import { getUserPicture, isLoggedIn } from '../common/user'
import {
  getCollaborator,
  getConnectionById,
  useConnections,
  useGetMyConnection,
  useMyUserAndPresence,
} from '../core/commenting/comment-hooks'
import type { FollowTarget, MultiplayerColor } from '../core/shared/multiplayer'
import {
  canFollowTarget,
  excludeMyConnection,
  followTarget,
  isDefaultAuth0AvatarURL,
  multiplayerColorFromIndex,
  multiplayerInitialsFromName,
  normalizeMultiplayerName,
} from '../core/shared/multiplayer'
import { MultiplayerWrapper } from '../utils/multiplayer-wrapper'
import { unless, when } from '../utils/react-conditionals'
import { Avatar, FlexRow, Icn, Tooltip, colorTheme } from '../uuiui'
import { notice } from './common/notice'
import type { EditorAction } from './editor/action-types'
import { showToast, switchEditorMode } from './editor/actions/action-creators'
import { EditorModes, isFollowMode } from './editor/editor-modes'
import { useDispatch } from './editor/store/dispatch-context'
import { Substores, useEditorState } from './editor/store/store-hook'
import { useIsMyProject } from './editor/store/collaborative-editing'
import { motion } from 'framer-motion'
import { useIsBeingFollowed, useSortMultiplayerUsers } from '../core/shared/multiplayer-hooks'

const MAX_VISIBLE_OTHER_PLAYERS = 4

const AvatarSize = 20

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
      {unless(roomStatus === 'connected', <SinglePlayerUserBar />)}
    </FlexRow>
  )
})
UserBar.displayName = 'UserBar'

const SinglePlayerUserBar = React.memo(() => {
  const dispatch = useDispatch()

  const url = window.location.href
  const handleCopyToClipboard = React.useCallback(async () => {
    try {
      await window.navigator.clipboard.writeText(url)
      dispatch([showToast(notice('Project link copied to clipboard!', 'NOTICE', false))])
    } catch (error) {
      console.error('Error copying to clipboard:', error)
    }
  }, [dispatch, url])

  const userPicture = useEditorState(
    Substores.userState,
    (store) => getUserPicture(store.userState.loginState),
    'SinglePlayerUserBar userPicture',
  )
  const isMyProject = useIsMyProject()

  return (
    <FlexRow
      onClick={handleCopyToClipboard}
      css={{
        background: colorTheme.primary30.value,
        borderRadius: 24,
        height: 24,
        padding: 2,
        border: `1px solid ${colorTheme.transparent.value}`,
        transition: 'all .1s ease-in-out',
        '&:hover': {
          background: colorTheme.primary25.value,
        },
        '&:active': {
          border: `1px solid ${colorTheme.primary30.value}`,
        },
      }}
    >
      <Avatar
        userPicture={userPicture}
        isLoggedIn={true}
        size={AvatarSize}
        style={{ outline: 'undefined' }}
      />
      {isMyProject ? <OwnerBadge /> : null}
      <div style={{ padding: '0 8px 0 5px', fontWeight: 500 }}>Share</div>
    </FlexRow>
  )
})
SinglePlayerUserBar.displayName = 'SinglePlayerUserBar'

const MultiplayerUserBar = React.memo(() => {
  const dispatch = useDispatch()

  const url = window.location.href
  const handleCopyToClipboard = React.useCallback(async () => {
    try {
      let actions: EditorAction[] = []
      actions.push(showToast(notice('Project link copied to clipboard!', 'NOTICE', false)))
      await window.navigator.clipboard.writeText(url)
      dispatch(actions)
    } catch (error) {
      console.error('Error copying to clipboard:', error)
    }
  }, [dispatch, url])

  const collabs = useStorage((store) => store.collaborators)

  const connections = useConnections()

  const { user: myUser, presence: myPresence } = useMyUserAndPresence()
  const sortAvatars = useSortMultiplayerUsers()
  const isBeingFollowed = useIsBeingFollowed()

  const others = useOthers((list) => {
    return excludeMyConnection(myPresence.id, myPresence.connectionId, list).map((other) => {
      return {
        ...getCollaborator(collabs, other),
        following: other.presence.following,
        colorIndex:
          getConnectionById(connections, other.id, other.connectionId)?.colorIndex ?? null,
        connectionId: other.connectionId,
        connectedAt: connections?.[other.id]?.[other.connectionId]?.startedAt ?? 0,
      }
    })
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

  const sortedOthers = React.useMemo(() => {
    return others.sort(sortAvatars)
  }, [others, sortAvatars])

  const visibleOthers = React.useMemo(() => {
    return sortedOthers.slice(0, MAX_VISIBLE_OTHER_PLAYERS)
  }, [sortedOthers])
  const hiddenOthers = React.useMemo(() => {
    return sortedOthers.slice(MAX_VISIBLE_OTHER_PLAYERS)
  }, [sortedOthers])

  const offlineOthers = Object.values(collabs).filter((collab) => {
    return collab.id !== myUser.id && !sortedOthers.some((other) => other.id === collab.id)
  })

  const amIOwner = React.useMemo(() => {
    return ownerId === myUser.id
  }, [ownerId, myUser])

  const toggleFollowing = React.useCallback(
    (target: FollowTarget) => () => {
      let actions: EditorAction[] = []
      const canFollow = canFollowTarget(
        followTarget(myUser.id, myPresence.connectionId),
        target,
        sortedOthers,
      )
      if (!canFollow) {
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
          isFollowMode(mode) &&
          mode.playerId === target.playerId &&
          mode.connectionId === target.connectionId
            ? EditorModes.selectMode(null, false, 'none')
            : EditorModes.followMode(target.playerId, target.connectionId)
        actions.push(switchEditorMode(newMode))
      }
      dispatch(actions)
    },
    [dispatch, mode, sortedOthers, myUser, myPresence],
  )

  if (myUser.name == null) {
    // it may still be loading, so fallback until it sorts itself out
    return <SinglePlayerUserBar />
  }

  return (
    <motion.div
      layoutRoot={true}
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
        const key = `avatar-${other.id}-${other.connectionId}`
        return (
          <motion.div key={key} layout={'position'}>
            <MultiplayerAvatar
              name={multiplayerInitialsFromName(name)}
              tooltip={{ text: name, colored: true }}
              color={multiplayerColorFromIndex(other.colorIndex)}
              picture={other.avatar}
              onClick={toggleFollowing(followTarget(other.id, other.connectionId))}
              isBeingFollowed={isBeingFollowed(other.id, other.connectionId)}
              follower={other.following === myUser.id}
              isOwner={isOwner}
              size={AvatarSize}
            />
          </motion.div>
        )
      })}
      {when(
        hiddenOthers.length > 0,
        <MultiplayerAvatar
          name={`+${hiddenOthers.length}`}
          tooltip={{
            text: hiddenOthers.map((c) => normalizeMultiplayerName(c.name)).join(', '),
            colored: false,
          }}
          color={{
            background: colorTheme.fg8.value,
            foreground: colorTheme.fg0.value,
          }}
          picture={null}
          size={AvatarSize}
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
          const key = `avatar-${other.id}-offline`
          return (
            <motion.div key={key} layout={'position'}>
              <MultiplayerAvatar
                name={multiplayerInitialsFromName(name)}
                tooltip={{ text: name, colored: false }}
                picture={other.avatar}
                isOwner={isOwner}
                isOffline={true}
                size={AvatarSize}
              />
            </motion.div>
          )
        }),
      )}
      <FlexRow
        onClick={handleCopyToClipboard}
        css={{
          background: colorTheme.primary30.value,
          borderRadius: 24,
          height: 24,
          padding: 2,
          border: `1px solid ${colorTheme.transparent.value}`,
          transition: 'all .1s ease-in-out',
          '&:hover': {
            background: colorTheme.primary25.value,
          },
          '&:active': {
            border: `1px solid ${colorTheme.primary30.value}`,
          },
        }}
      >
        <MultiplayerAvatar
          name={multiplayerInitialsFromName(myUser.name)}
          color={multiplayerColorFromIndex(
            getConnectionById(connections, myUser.id, myPresence.connectionId)?.colorIndex ?? null,
          )}
          picture={myUser.avatar}
          isOwner={amIOwner}
          size={AvatarSize}
          style={{ outline: 'undefined' }}
        />
        <div style={{ padding: '0 8px 0 5px', fontWeight: 500 }}>Share</div>
      </FlexRow>
    </motion.div>
  )
})
MultiplayerUserBar.displayName = 'MultiplayerUserBar'

export type MultiplayerAvatarProps = {
  name: string
  color?: MultiplayerColor
  picture?: string | null
  onClick?: () => void
  isBeingFollowed?: boolean
  size?: number
  follower?: boolean
  isOwner?: boolean
  style?: CSSProperties
  tooltip?: { text: string; colored: boolean }
  isOffline?: boolean
}

export const MultiplayerAvatar = React.memo((props: MultiplayerAvatarProps) => {
  const picture = React.useMemo(() => {
    return isDefaultAuth0AvatarURL(props.picture ?? null) ? null : props.picture
  }, [props.picture])

  const tooltipText = <strong>{props.tooltip?.text}</strong>
  const tooltipSubtext =
    props.follower === true ? ' following you' : props.isOffline ? ' offline' : ''

  const tooltipWithLineBreak = (
    <div>
      {tooltipText}
      {<br />}
      {tooltipSubtext}
    </div>
  )

  return (
    <Tooltip
      disabled={props.tooltip == null}
      title={tooltipWithLineBreak}
      placement='bottom'
      backgroundColor={
        props.tooltip?.colored === true && props.color != null ? props.color.background : undefined
      }
      textColor={
        props.tooltip?.colored === true && props.color != null ? props.color.foreground : undefined
      }
    >
      <div
        style={{
          width: props.size ?? 25.5,
          height: props.size ?? 25.5,
          backgroundColor:
            props.isOffline || props.color == null ? colorTheme.bg4.value : props.color.background,
          color:
            props.isOffline || props.color == null ? colorTheme.fg2.value : props.color.foreground,
          display: 'flex',
          alignItems: 'center',
          justifyContent: 'center',
          borderRadius: '100%',
          fontSize: 9,
          fontWeight: 700,
          cursor: props.onClick != null ? 'pointer' : 'inherit',
          position: 'relative',
          outline:
            props.isBeingFollowed === true
              ? `1px solid ${colorTheme.bg1.value}`
              : `1px solid ${colorTheme.transparent.value}`,
          boxShadow:
            props.isBeingFollowed === true && props.color != null
              ? `0px 0px 0px 2.5px ${props.color.background}`
              : `0px 0px 0px 2.5px ${colorTheme.transparent.value}`,
          ...props.style,
        }}
        onClick={props.onClick}
      >
        <AvatarPicture
          url={picture}
          size={props.size ?? 25.5}
          initials={props.name}
          isOffline={props.isOffline}
        />
        {props.isOwner ? <OwnerBadge isOffline={props.isOffline} /> : null}
      </div>
    </Tooltip>
  )
})
MultiplayerAvatar.displayName = 'MultiplayerAvatar'

interface OwnerBadge {
  isOffline?: boolean
}

const OwnerBadge = React.memo((props: OwnerBadge) => {
  return (
    <Icn
      category='semantic'
      type={'star'}
      width={14}
      height={14}
      color='main'
      style={{
        position: 'absolute',
        zIndex: 1,
        bottom: -4,
        left: -4,
        filter: props.isOffline ? 'grayscale(1)' : 'undefined',
      }}
    />
  )
})

OwnerBadge.displayName = 'OwnerBadge'

interface AvatarPictureProps {
  url: string | null | undefined
  initials: string
  size?: number
  isOffline?: boolean
  resolved?: boolean
}

const AvatarPicture = React.memo((props: AvatarPictureProps) => {
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
        filter: props.isOffline || props.resolved ? 'grayscale(1)' : undefined,
        opacity: props.isOffline ? 0.6 : 'undefined',
        pointerEvents: 'none',
      }}
      src={url}
      referrerPolicy='no-referrer'
      onError={onPictureError}
      draggable={false}
    />
  )
})
AvatarPicture.displayName = 'AvatarPicture'
