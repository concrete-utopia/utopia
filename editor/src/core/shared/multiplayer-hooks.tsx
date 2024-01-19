import { useAtom } from 'jotai'
import React from 'react'
import {
  ActiveRemixSceneAtom,
  RemixNavigationAtom,
} from '../../components/canvas/remix/utopia-remix-root-component'
import type { RemixPresence } from './multiplayer'
import * as EP from './element-path'
import { Substores, useEditorState } from '../../components/editor/store/store-hook'
import { isLoggedIn } from '../../common/user'
import { isFollowMode } from '../../components/editor/editor-modes'
import type { Connections } from '../../../liveblocks.config'
import { useStorage, useMutation } from '../../../liveblocks.config'
import { LiveObject } from '@liveblocks/client'

const ConnectionHeartbeatInterval = 10 * 1000 // ms
const ConnectionExpiredTime = 60 * 1000 // 1 minute

export function useRemixPresence(): RemixPresence | null {
  const [activeRemixScene] = useAtom(ActiveRemixSceneAtom)
  const [remixNavigationState] = useAtom(RemixNavigationAtom)

  const remixPresence = React.useMemo((): RemixPresence | null => {
    if (EP.isEmptyPath(activeRemixScene)) {
      return null
    }
    const scene = EP.toString(activeRemixScene)
    const locationRoute = remixNavigationState[scene]?.location.pathname ?? null
    return {
      scene: scene,
      locationRoute: locationRoute,
    }
  }, [activeRemixScene, remixNavigationState])

  return remixPresence
}

export function useMyUserId(): string | null {
  const myUserId = useEditorState(
    Substores.userState,
    (store) =>
      isLoggedIn(store.userState.loginState) ? store.userState.loginState.user.userId : null,
    'useMyUserId myUserId',
  )
  return myUserId
}

export function useIsBeingFollowed() {
  const mode = useEditorState(
    Substores.restOfEditor,
    (store) => store.editor.mode,
    'useIsBeingFollowed mode',
  )

  const isBeingFollowed = React.useCallback(
    (connectionId: number) => {
      return isFollowMode(mode) && mode.connectionId === connectionId
    },
    [mode],
  )

  return isBeingFollowed
}

export interface SortableUser {
  connectionId: number
}

export function useSortMultiplayerUsers() {
  const isBeingFollowed = useIsBeingFollowed()
  return React.useCallback(
    (a: SortableUser, b: SortableUser) => {
      if (isBeingFollowed(a.connectionId)) {
        return -1
      }
      if (isBeingFollowed(b.connectionId)) {
        return 1
      }
      if (a.connectionId < b.connectionId) {
        return -1
      }
      if (b.connectionId < a.connectionId) {
        return 1
      }
      return 0
    },
    [isBeingFollowed],
  )
}

export function useMyConnections(): Connections {
  const myUserId = useMyUserId()
  const conns = useStorage((store) => store.connections)
  if (myUserId == null) {
    return {}
  }
  return conns[myUserId] ?? {}
}

export function useAddConnection() {
  const loginState = useEditorState(
    Substores.userState,
    (store) => store.userState.loginState,
    'useAddConnection loginState',
  )

  const updateLastSeen = useMutation(
    ({ storage, self }) => {
      if (!isLoggedIn(loginState)) {
        return
      }
      const selfConns: Connections = storage.get('connections').get(self.id) ?? {}
      if (selfConns[self.connectionId] != null) {
        const now = Date.now()
        selfConns[self.connectionId].lastSeen = now
        storage.get('connections').update({ [self.id]: selfConns })
      }
    },
    [loginState],
  )

  // cleanup inactive connections, if found
  const cleanupInactive = useMutation(
    ({ storage }) => {
      if (!isLoggedIn(loginState)) {
        return
      }

      // get all conns by player ids
      const allConns = storage.get('connections').toObject()

      let changed = false
      const now = Date.now()

      // for each player id...
      for (const id of Object.keys(allConns)) {
        const playerConns = allConns[id]
        // iterate its connections...
        for (const connIdKey of Object.keys(playerConns)) {
          const connId = parseInt(connIdKey)
          // if the connection is expired, delete it
          const expired = now - playerConns[connId].lastSeen > ConnectionHeartbeatInterval * 2
          if (expired) {
            delete playerConns[connId]
            changed = true
          }
        }
        allConns[id] = playerConns
      }

      // if any connections were deleted, update the connections
      if (changed) {
        storage.set('connections', new LiveObject(allConns))
      }
    },
    [loginState],
  )

  const update = useMutation(
    ({ storage, self }) => {
      if (!isLoggedIn(loginState)) {
        return
      }
      const selfConns: Connections = storage.get('connections').get(self.id) ?? {}
      if (selfConns[self.connectionId] == null) {
        const now = Date.now()
        selfConns[self.connectionId] = {
          startedAt: now,
          lastSeen: now,
        }
        storage.get('connections').update({ [self.id]: selfConns })

        window.setInterval(updateLastSeen, ConnectionHeartbeatInterval)
        window.setInterval(cleanupInactive, ConnectionExpiredTime)
      }
    },
    [loginState, updateLastSeen],
  )

  const connections = useStorage((store) => store.connections)

  React.useEffect(() => {
    if (connections == null) {
      return
    }
    update()
  }, [connections, update])
}
