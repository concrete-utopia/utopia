import React from 'react'
import { useOthers, useSelf, useStatus, useStorage } from '../../liveblocks.config'
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
import { unless, when } from '../utils/react-conditionals'
import { MultiplayerWrapper } from '../utils/multiplayer-wrapper'
import { useDispatch } from './editor/store/dispatch-context'
import { showToast, switchEditorMode } from './editor/actions/action-creators'
import type { EditorAction } from './editor/action-types'
import { EditorModes, isFollowMode } from './editor/editor-modes'
import { notice } from './common/notice'
import { useMyUserAndPresence } from '../core/commenting/comment-hooks'

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

  const { user: myUser } = useMyUserAndPresence()
  const myName = normalizeMultiplayerName(myUser.name)

  const others = useOthers((list) =>
    normalizeOthersList(myUser.id, list).map((other) => ({
      id: other.id,
      name: myUser.name,
      colorIndex: myUser.colorIndex,
      picture: myUser.avatar,
      following: other.presence.following,
    })),
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
                picture={other.picture}
                border={true}
                coloredTooltip={true}
                onClick={toggleFollowing(other.id)}
                active={isFollowMode(mode) && mode.playerId === other.id}
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
  }) => {
    const picture = React.useMemo(() => {
      return isDefaultAuth0AvatarURL(props.picture ?? null) ? null : props.picture
    }, [props.picture])

    const [pictureNotFound, setPictureNotFound] = React.useState(false)

    React.useEffect(() => {
      setPictureNotFound(false)
    }, [picture])

    const onPictureError = React.useCallback(() => {
      console.warn('cannot get picture', props.picture)
      setPictureNotFound(true)
    }, [props.picture])

    const showPicture = React.useMemo(() => {
      return picture != null && !pictureNotFound
    }, [picture, pictureNotFound])

    const colorTheme = useColorTheme()
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
            border: `3px solid ${props.active === true ? colorTheme.primary.value : 'transparent'}`,
            fontSize: 9,
            fontWeight: 700,
            cursor: 'pointer',
            boxShadow:
              props.active === true ? `0px 0px 15px ${colorTheme.primary.value}` : undefined,
          }}
          onClick={props.onClick}
        >
          {unless(showPicture, props.name)}
          {when(
            showPicture,
            // Using an img tag instead of using it as backgroundColor above because of potential 403s
            <img
              style={{
                width: props.border === true ? 22 : '100%',
                height: props.border === true ? 22 : '100%',
                borderRadius: '100%',
              }}
              src={picture ?? ''}
              referrerPolicy='no-referrer'
              onError={onPictureError}
            />,
          )}
        </div>
      </Tooltip>
    )
  },
)
MultiplayerAvatar.displayName = 'MultiplayerAvatar'
