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
import { unless, when } from '../utils/react-conditionals'
import { useDispatch } from './editor/store/dispatch-context'
import { switchEditorMode, updateMultiplayerState } from './editor/actions/action-creators'
import { EditorModes } from './editor/editor-modes'
import type { EditorAction } from './editor/action-types'

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
      picture: other.presence.picture, // TODO remove this once able to resolve users
    })),
  )

  const visibleOthers = React.useMemo(() => {
    return others.slice(0, MAX_VISIBLE_OTHER_PLAYERS)
  }, [others])
  const hiddenOthers = React.useMemo(() => {
    return others.slice(MAX_VISIBLE_OTHER_PLAYERS)
  }, [others])

  const dispatch = useDispatch()

  const following = useEditorState(
    Substores.restOfEditor,
    (store) => store.editor.multiplayer.following,
    'MultiplayerUserBar following',
  )
  const followMode = useEditorState(
    Substores.restOfEditor,
    (store) => store.editor.multiplayer.followMode,
    'MultiplayerUserBar followMode',
  )

  const toggleFollowing = React.useCallback(
    (id: string) => () => {
      let actions: EditorAction[] = [
        updateMultiplayerState({
          following: id === following ? null : id,
          followMode: null,
        }),
      ]
      if (followMode != null) {
        actions.push(switchEditorMode(EditorModes.selectMode(null, false, 'none'), undefined, true))
      }
      dispatch(actions)
    },
    [dispatch, following, followMode],
  )

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
                onClick={toggleFollowing(other.id)}
                active={following === other.id}
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
          picture={self.presence.picture}
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
