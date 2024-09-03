/** @jsxRuntime classic */
/** @jsx jsx */
import React from 'react'
import { Fragment } from 'react'
import { jsx } from '@emotion/react'
import type { User } from '@liveblocks/client'
import { AnimatePresence, motion } from 'framer-motion'
import { useAtom, useSetAtom } from 'jotai'
import type { Presence, PresenceActiveFrame, UserMeta } from '../../../liveblocks.config'
import {
  useOthers,
  useOthersListener,
  useRoom,
  useSelf,
  useUpdateMyPresence,
} from '../../../liveblocks.config'
import {
  getCollaborator,
  getConnectionById,
  useCanComment,
  useConnections,
  useCollaborators,
  getCollaboratorById,
  useAddMyselfToCollaborators_DEPRECATED,
} from '../../core/commenting/comment-hooks'
import { MetadataUtils } from '../../core/model/element-metadata-utils'
import { mapDropNulls } from '../../core/shared/array-utils'
import * as EP from '../../core/shared/element-path'
import type { CanvasPoint } from '../../core/shared/math-utils'
import {
  canvasPoint,
  pointsEqual,
  scaleRect,
  windowPoint,
  zeroRectIfNullOrInfinity,
} from '../../core/shared/math-utils'
import type { MultiplayerColor } from '../../core/shared/multiplayer'
import {
  excludeMyConnection,
  isPlayerOnAnotherRemixRoute,
  multiplayerColorFromIndex,
  normalizeOthersList,
  useIsOnSameRemixRoute,
} from '../../core/shared/multiplayer'
import { assertNever } from '../../core/shared/utils'
import { Button, FlexRow, UtopiaStyles, colorTheme } from '../../uuiui'
import { notice } from '../common/notice'
import type { EditorAction } from '../editor/action-types'
import { isLoggedIn } from '../editor/action-types'
import { showToast, switchEditorMode } from '../editor/actions/action-creators'
import { EditorModes, isCommentMode, isFollowMode } from '../editor/editor-modes'
import { useDispatch } from '../editor/store/dispatch-context'
import {
  Substores,
  useEditorState,
  useRefEditorState,
  useSelectorWithCallback,
} from '../editor/store/store-hook'
import CanvasActions from './canvas-actions'
import { activeFrameActionToString } from './commands/set-active-frames-command'
import { canvasPointToWindowPoint, windowToCanvasCoordinates } from './dom-lookup'
import { ActiveRemixSceneAtom, RemixNavigationAtom } from './remix/utopia-remix-root-component'
import {
  useStoreConnection,
  useMyUserId,
  useRemixPresence,
  useMonitorConnection,
  useLoadCollaborators,
} from '../../core/shared/multiplayer-hooks'
import { CanvasOffsetWrapper } from './controls/canvas-offset-wrapper'
import { when } from '../../utils/react-conditionals'
import { CommentIndicators } from './controls/comment-indicator'
import { CommentPopup } from './controls/comment-popup'
import { getIdOfScene, getSceneUnderPoint } from './controls/comment-mode/comment-mode-hooks'
import { optionalMap } from '../../core/shared/optional-utils'
import { useKeepReferenceEqualityIfPossible } from '../../utils/react-performance'

export const OtherUserPointer = (props: any) => {
  return (
    <svg width='12' height='17' viewBox='0 0 12 17' fill='none' xmlns='http://www.w3.org/2000/svg'>
      <path
        id='Default'
        d='M0.0397339 0.0756531V16.5757L4.16473 11.0757H11.0397L0.0397339 0.0756531Z'
        fill={props.color}
        stroke='white'
      />
    </svg>
  )
}

export const MultiplayerPresence = React.memo(() => {
  const dispatch = useDispatch()

  const room = useRoom()
  const updateMyPresence = useUpdateMyPresence()
  const liveBlocksUserId = useSelf((self) => self.id)
  const liveBlocksConnectionId = useSelf((self) => self.connectionId)

  const loginState = useEditorState(
    Substores.userState,
    (store) => store.userState.loginState,
    'MultiplayerPresence loginState',
  )
  const canvasScale = useEditorState(
    Substores.canvasOffset,
    (store) => store.editor.canvas.scale,
    'MultiplayerPresence canvasScale',
  )
  const canvasOffset = useEditorState(
    Substores.canvasOffset,
    (store) => store.editor.canvas.roundedCanvasOffset,
    'MultiplayerPresence canvasOffset',
  )
  const mode = useEditorState(
    Substores.restOfEditor,
    (store) => store.editor.mode,
    'MultiplayerPresence mode',
  )

  useAddMyselfToCollaborators_DEPRECATED()
  useLoadCollaborators()

  useStoreConnection()
  useMonitorConnection()

  const remixPresence = useRemixPresence()

  React.useEffect(() => {
    if (!isLoggedIn(loginState)) {
      return
    }
    updateMyPresence({
      canvasScale: canvasScale,
      canvasOffset: canvasOffset,
      following: isFollowMode(mode) ? mode.playerId : null,
      remix: remixPresence,
    })
  }, [canvasScale, canvasOffset, updateMyPresence, loginState, mode, remixPresence])

  React.useEffect(() => {
    // when the mouse moves over the canvas, update the presence cursor
    function onMouseMove(e: MouseEvent) {
      updateMyPresence({
        cursor: windowPoint({ x: e.clientX, y: e.clientY }),
      })
    }
    window.addEventListener('mousemove', onMouseMove)
    return function () {
      window.removeEventListener('mousemove', onMouseMove)
    }
  }, [updateMyPresence])

  const [roomId, setRoomId] = React.useState<string>(room.id)

  React.useEffect(() => {
    // when the room changes, reset
    // (unfortunately there's not a subscription event for room id changes :()
    if (roomId !== room.id) {
      setRoomId(room.id)
      queueMicrotask(() => {
        dispatch([switchEditorMode(EditorModes.selectMode(null, false, 'none'))])
      })
    }
  }, [room.id, roomId, dispatch])

  const canComment = useCanComment()

  if (!isLoggedIn(loginState)) {
    return null
  }

  return (
    <Fragment>
      <FollowingOverlay
        liveBlocksUserId={liveBlocksUserId}
        liveBlocksConnectionId={liveBlocksConnectionId}
      />
      <MultiplayerShadows />
      {when(canComment, <CommentIndicators />)}
      <MultiplayerCursors
        liveBlocksUserId={liveBlocksUserId}
        liveBlocksConnectionId={liveBlocksConnectionId}
      />
      {when(canComment && isCommentMode(mode) && mode.comment != null, <CommentPopup />)}
    </Fragment>
  )
})
MultiplayerPresence.displayName = 'MultiplayerPresence'

interface MultiplayerCursorsProps {
  liveBlocksUserId: string
  liveBlocksConnectionId: number
}

const MultiplayerCursors = React.memo((props: MultiplayerCursorsProps) => {
  const { liveBlocksUserId, liveBlocksConnectionId } = props
  const collabs = useCollaborators()
  const others = useOthers((list) => {
    const presences = excludeMyConnection(liveBlocksUserId, liveBlocksConnectionId, list)
    return presences.map((p) => ({
      presenceInfo: p,
      userInfo: getCollaborator(collabs, p),
    }))
  })

  const connections = useConnections()

  return (
    <div
      style={{
        position: 'fixed',
        top: 0,
        left: 0,
        pointerEvents: 'none',
      }}
    >
      {others.map((other) => {
        if (
          other.presenceInfo.presence.cursor == null ||
          other.presenceInfo.presence.canvasOffset == null ||
          other.presenceInfo.presence.canvasScale == null
        ) {
          return null
        }
        const position = windowToCanvasCoordinates(
          other.presenceInfo.presence.canvasScale,
          other.presenceInfo.presence.canvasOffset,
          other.presenceInfo.presence.cursor,
        ).canvasPositionRounded
        return (
          <MultiplayerCursor
            key={`cursor-${other.presenceInfo.id}`}
            name={other.userInfo.name ?? null}
            colorIndex={
              getConnectionById(connections, other.userInfo.id, other.presenceInfo.connectionId)
                ?.colorIndex ?? null
            }
            position={position}
            userId={other.userInfo.id}
          />
        )
      })}
    </div>
  )
})
MultiplayerCursors.displayName = 'MultiplayerCursors'

function useGhostPointerState(position: CanvasPoint, userId: string) {
  const [shouldShowGhostPointer, setShouldShowGhostPointer] = React.useState<boolean>(false)

  const scenesRef = useRefEditorState((store) =>
    MetadataUtils.getScenesMetadata(store.editor.jsxMetadata),
  )

  const isOnSameRemixRoute = useIsOnSameRemixRoute()

  const timeoutHandle = React.useRef<ReturnType<typeof setTimeout> | null>(null)
  React.useEffect(() => {
    timeoutHandle.current = setTimeout(() => {
      timeoutHandle.current = null
      const instance =
        // making a new canvasPoint here so that the memo array contains only primitive types
        getSceneUnderPoint(canvasPoint({ x: position.x, y: position.y }), scenesRef.current)
      const remixSceneId = optionalMap(getIdOfScene, instance)

      if (instance == null || remixSceneId == null) {
        setShouldShowGhostPointer(false)
      } else {
        setShouldShowGhostPointer(
          !isOnSameRemixRoute({ otherUserId: userId, remixSceneId: remixSceneId }),
        )
      }
    }, 1000)

    return () => {
      if (timeoutHandle.current != null) {
        clearTimeout(timeoutHandle.current)
      }
    }
  }, [isOnSameRemixRoute, position.x, position.y, scenesRef, userId])

  return shouldShowGhostPointer
}

const MultiplayerCursor = React.memo(
  ({
    name,
    colorIndex,
    position,
    userId,
  }: {
    name: string | null
    colorIndex: number | null
    position: CanvasPoint
    userId: string
  }) => {
    const canvasScale = useEditorState(
      Substores.canvasOffset,
      (store) => store.editor.canvas.scale,
      'MultiplayerCursor canvasScale',
    )
    const color = multiplayerColorFromIndex(colorIndex)

    const shouldShowGhostPointer = useGhostPointerState(position, userId)

    return (
      <CanvasOffsetWrapper setScaleToo={true}>
        <motion.div
          initial={position}
          animate={position}
          transition={{
            type: 'spring',
            damping: 30,
            mass: 0.8,
            stiffness: 350,
          }}
          style={{
            scale: canvasScale <= 1 ? 1 / canvasScale : 1,
            opacity: shouldShowGhostPointer ? 0.3 : 1,
          }}
        >
          <div
            style={{
              filter: 'drop-shadow(1px 2px 3px rgb(0 0 0 / 0.3))',
              transform: 'translate(0px, 0px)',
              zoom: canvasScale > 1 ? 1 / canvasScale : 1,
            }}
          >
            <OtherUserPointer color={color.background} />
          </div>
          <div
            style={{
              color: color.foreground,
              backgroundColor: color.background,
              padding: '0 4px',
              borderRadius: 2,
              boxShadow: UtopiaStyles.shadowStyles.mid.boxShadow,
              fontWeight: 'bold',
              fontSize: 9,
              position: 'absolute',
              left: 6,
              top: 15,
              zoom: canvasScale > 1 ? 1 / canvasScale : 1,
            }}
          >
            {name}
          </div>
        </motion.div>
      </CanvasOffsetWrapper>
    )
  },
)
MultiplayerCursor.displayName = 'MultiplayerCursor'

const remixRouteChangedToastId = 'follow-changed-scene'

interface FollowingOverlayProps {
  liveBlocksUserId: string
  liveBlocksConnectionId: number
}

const FollowingOverlay = React.memo((props: FollowingOverlayProps) => {
  const { liveBlocksUserId, liveBlocksConnectionId } = props
  const dispatch = useDispatch()

  const room = useRoom()

  const mode = useEditorState(
    Substores.restOfEditor,
    (store) => store.editor.mode,
    'FollowingOverlay mode',
  )
  const canvasScale = useEditorState(
    Substores.canvasOffset,
    (store) => store.editor.canvas.scale,
    'FollowingOverlay canvasScale',
  )
  const canvasOffset = useEditorState(
    Substores.canvasOffset,
    (store) => store.editor.canvas.roundedCanvasOffset,
    'FollowingOverlay canvasOffset',
  )

  const isFollowTarget = React.useCallback(
    (other: User<Presence, UserMeta>): boolean => {
      return (
        isFollowMode(mode) && other.id === mode.playerId && other.connectionId === mode.connectionId
      )
    },
    [mode],
  )

  const resetFollowed = React.useCallback(() => {
    dispatch([switchEditorMode(EditorModes.selectMode(null, false, 'none'))])
  }, [dispatch])

  const collabs = useCollaborators()

  const followed = React.useMemo(() => {
    return room.getOthers().find(isFollowTarget) ?? null
  }, [room, isFollowTarget])

  const followedUser = React.useMemo(() => {
    return followed != null ? getCollaboratorById(collabs, followed.id) : null
  }, [followed, collabs])

  const remixPresence = useRemixPresence()

  const setActiveRemixScene = useSetAtom(ActiveRemixSceneAtom)
  const [remixNavigationState] = useAtom(RemixNavigationAtom)

  const updateCanvasFromOtherPresence = React.useCallback(
    (presence: Presence) => {
      let actions: EditorAction[] = []
      if (presence.canvasScale != null && presence.canvasScale !== canvasScale) {
        actions.push(CanvasActions.zoom(presence.canvasScale, null))
      }
      if (presence.canvasOffset != null && !pointsEqual(presence.canvasOffset, canvasOffset)) {
        actions.push(CanvasActions.positionCanvas(presence.canvasOffset))
      }
      if (presence.remix != null && isPlayerOnAnotherRemixRoute(remixPresence, presence.remix)) {
        const sceneState = remixNavigationState[presence.remix.scene]
        if (sceneState != null && presence.remix.locationRoute != null) {
          setActiveRemixScene(EP.fromString(presence.remix.scene))
          void sceneState.navigate(presence.remix.locationRoute)
          actions.push(
            showToast(
              notice(
                `The route has been changed to ${presence.remix.locationRoute}`,
                'PRIMARY',
                false,
                remixRouteChangedToastId,
              ),
            ),
          )
        }
      }
      if (actions.length > 0) {
        dispatch(actions)
      }
    },
    [dispatch, canvasScale, canvasOffset, setActiveRemixScene, remixPresence, remixNavigationState],
  )

  useOthersListener((event) => {
    if (isFollowMode(mode)) {
      switch (event.type) {
        case 'enter':
        case 'update':
          if (isFollowTarget(event.user)) {
            updateCanvasFromOtherPresence(event.user.presence)
          }
          break
        case 'leave':
          if (isFollowTarget(event.user)) {
            resetFollowed()
          }
          break
        case 'reset':
          resetFollowed()
          break
        default:
          assertNever(event)
      }
    }
  })

  const stopFollowing = React.useCallback(() => {
    dispatch([switchEditorMode(EditorModes.selectMode(null, false, 'none'))])
  }, [dispatch])

  const connections = useConnections()

  const followedUserColor: MultiplayerColor = React.useMemo(() => {
    if (followed == null) {
      return multiplayerColorFromIndex(null)
    } else {
      return multiplayerColorFromIndex(
        getConnectionById(connections, followed.id, followed.connectionId)?.colorIndex ?? null,
      )
    }
  }, [connections, followed])

  const others = useKeepReferenceEqualityIfPossible(
    useOthers((list) =>
      list
        .filter((entry) => entry.connectionId !== liveBlocksConnectionId)
        .map((other) => {
          return {
            ...getCollaborator(collabs, other),
            following: other.presence.following,
            connectionId: other.connectionId,
            connectedAt: connections?.[other.id]?.[other.connectionId]?.startedAt ?? 0,
          }
        }),
    ),
  )

  const myFollowers = React.useMemo(() => {
    return others.filter((other) => other.following === liveBlocksUserId)
  }, [liveBlocksUserId, others])
  const followers = myFollowers.length
  const hasFollowers = followers > 0

  return (
    <AnimatePresence>
      {when(
        followedUser != null || hasFollowers,
        <motion.div
          style={{
            position: 'fixed',
            top: 0,
            left: 0,
            bottom: 0,
            right: 0,
            background: 'transparent',
            display: 'flex',
            flexDirection: 'column',
            alignItems: 'center',
            justifyContent: 'flex-end',
            gap: 5,
            paddingBottom: 14,
            cursor: 'default',
            border: hasFollowers ? `undefined` : `4px solid ${followedUserColor.background}`,
          }}
          initial={{ opacity: 0 }}
          animate={{ opacity: 1 }}
          exit={{ opacity: 0 }}
          transition={{ duration: 0.1 }}
        >
          {when(
            followers > 0,
            <motion.div
              style={{
                backgroundColor: colorTheme.primary.value,
                color: colorTheme.white.value,
                padding: '4px 12px',
                borderRadius: 100,
                boxShadow: UtopiaStyles.shadowStyles.mid.boxShadow,
                display: 'flex',
                alignItems: 'center',
              }}
            >
              {followers === 1 ? (
                <FlexRow style={{ height: 22, alignItems: 'center', justifyContent: 'center' }}>
                  1 person is following you
                </FlexRow>
              ) : (
                <FlexRow style={{ height: 22, alignItems: 'center', justifyContent: 'center' }}>
                  {followers} people are following you
                </FlexRow>
              )}
            </motion.div>,
          )}
          {when(
            followedUser != null,
            <motion.div
              style={{
                backgroundColor: followedUserColor.background,
                color: followedUserColor.foreground,
                padding: '4px 4px 4px 12px',
                borderRadius: 100,
                boxShadow: UtopiaStyles.shadowStyles.mid.boxShadow,
                display: 'flex',
                alignItems: 'center',
                gap: 5,
              }}
            >
              <div>You're following {followedUser?.name}</div>
              <Button
                highlight
                spotlight
                onClick={stopFollowing}
                css={{
                  padding: '4px 10px',
                  borderRadius: 100,
                  cursor: 'pointer',
                  backgroundColor: '#00000025',
                  '&:hover': {
                    backgroundColor: '#00000015',
                  },
                }}
              >
                Stop following
              </Button>
            </motion.div>,
          )}
        </motion.div>,
      )}
    </AnimatePresence>
  )
})
FollowingOverlay.displayName = 'FollowingOverlay'

const MultiplayerShadows = React.memo(() => {
  const myUserId = useMyUserId()
  const updateMyPresence = useUpdateMyPresence()

  const collabs = useCollaborators()
  const others = useOthers((list) => {
    if (myUserId == null) {
      return []
    }
    const presences = normalizeOthersList(myUserId, list)
    return presences.map((p) => ({
      presenceInfo: p,
      userInfo: getCollaboratorById(collabs, p.id),
    }))
  })

  const connections = useConnections()

  const shadows = React.useMemo(() => {
    return others.flatMap(
      (other) =>
        other.presenceInfo.presence.activeFrames?.map((activeFrame) => ({
          activeFrame: activeFrame,
          colorIndex:
            other.userInfo != null
              ? getConnectionById(connections, other.userInfo.id, other.presenceInfo.connectionId)
                  ?.colorIndex ?? null
              : null,
        })) ?? [],
    )
  }, [connections, others])

  const myActiveFrames = useEditorState(
    Substores.restOfEditor,
    (store) => store.editor.activeFrames,
    'MultiplayerShadows activeFrames',
  )

  const canvasScale = useEditorState(
    Substores.canvasOffset,
    (store) => store.editor.canvas.scale,
    'MultiplayerShadows canvasScale',
  )
  const canvasOffset = useEditorState(
    Substores.canvasOffset,
    (store) => store.editor.canvas.roundedCanvasOffset,
    'MultiplayerShadows canvasOffset',
  )

  const editorRef = useRefEditorState((store) => ({
    jsxMetadata: store.editor.jsxMetadata,
  }))

  useSelectorWithCallback(
    Substores.canvas,
    (store) => store.editor.canvas.interactionSession?.interactionData,
    (interactionData) => {
      if (interactionData?.type === 'DRAG') {
        updateMyPresence({
          activeFrames: mapDropNulls(({ target, action, source }): PresenceActiveFrame | null => {
            const { jsxMetadata } = editorRef.current
            switch (target.type) {
              case 'ACTIVE_FRAME_TARGET_RECT':
                return { frame: target.rect, action, source }
              case 'ACTIVE_FRAME_TARGET_PATH':
                const frame = MetadataUtils.getFrameInCanvasCoords(target.path, jsxMetadata)
                return { frame: zeroRectIfNullOrInfinity(frame), action, source }
              default:
                assertNever(target)
            }
          }, myActiveFrames),
        })
      } else {
        updateMyPresence({ activeFrames: [] })
      }
    },
    'MultiplayerShadows update presence shadows',
  )

  return (
    <Fragment>
      {shadows.map((shadow, index) => {
        const { frame, action, source } = shadow.activeFrame
        const color = multiplayerColorFromIndex(shadow.colorIndex)

        const framePosition = canvasPointToWindowPoint(frame, canvasScale, canvasOffset)
        const scaledFrame = scaleRect(frame, canvasScale)

        const sourcePosition = canvasPointToWindowPoint(source, canvasScale, canvasOffset)
        const scaledSource = scaleRect(source, canvasScale)

        return (
          <React.Fragment key={`shadow-${index}`}>
            <div
              style={{
                position: 'fixed',
                top: sourcePosition.y,
                left: sourcePosition.x,
                width: scaledSource.width,
                height: scaledSource.height,
                pointerEvents: 'none',
                border: `1px dashed ${color.background}`,
                opacity: 0.5,
              }}
            />
            <motion.div
              initial={{
                x: framePosition.x,
                y: framePosition.y,
                width: scaledFrame.width,
                height: scaledFrame.height,
              }}
              animate={{
                x: framePosition.x,
                y: framePosition.y,
                width: scaledFrame.width,
                height: scaledFrame.height,
              }}
              transition={{
                type: 'spring',
                damping: 30,
                mass: 0.8,
                stiffness: 350,
              }}
              style={{
                position: 'fixed',
                pointerEvents: 'none',
                background: `${color.background}44`,
                display: 'flex',
                alignItems: 'center',
                justifyContent: 'center',
                fontSize: 9,
                color: color.background,
                border: `1px dashed ${color.background}`,
              }}
            >
              {activeFrameActionToString(action)}
            </motion.div>
          </React.Fragment>
        )
      })}
    </Fragment>
  )
})
MultiplayerShadows.displayName = 'MultiplayerShadows'
