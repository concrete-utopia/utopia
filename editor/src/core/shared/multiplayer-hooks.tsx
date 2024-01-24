import { LiveObject } from '@liveblocks/client'
import { useAtom } from 'jotai'
import React from 'react'
import type { ConnectionInfo } from '../../../liveblocks.config'
import { useMutation, useStorage } from '../../../liveblocks.config'
import { isLoggedIn } from '../../common/user'
import {
  ActiveRemixSceneAtom,
  RemixNavigationAtom,
} from '../../components/canvas/remix/utopia-remix-root-component'
import { isFollowMode } from '../../components/editor/editor-modes'
import { Substores, useEditorState } from '../../components/editor/store/store-hook'
import * as EP from './element-path'
import type { RemixPresence } from './multiplayer'
import { PRODUCTION_ENV } from '../../common/env-vars'
import { isFeatureEnabled } from '../../utils/feature-switches'

/**
 * How often to perform heartbeat bumps on connections.
 * 10s on production, 5s on local/test.
 */
const ConnectionHeartbeatInterval = PRODUCTION_ENV ? 10 * 1000 : 5 * 1000

/**
 * How often to perform heartbeat bumps on connections.
 * 1 minute on production, 10s on local/test.
 */
const ConnectionExpiredTime = PRODUCTION_ENV ? 60 * 1000 : 10 * 1000

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

export function useIsLoggedIn(): boolean {
  return useEditorState(
    Substores.restOfStore,
    (store) => isLoggedIn(store.userState.loginState),
    'useIsLoggedIn',
  )
}

export function useIsBeingFollowed() {
  const mode = useEditorState(
    Substores.restOfEditor,
    (store) => store.editor.mode,
    'useIsBeingFollowed mode',
  )

  const isBeingFollowed = React.useCallback(
    (playerId: string, connectionId: number) => {
      return isFollowMode(mode) && mode.connectionId === connectionId && mode.playerId === playerId
    },
    [mode],
  )

  return isBeingFollowed
}

export interface SortableUser {
  id: string
  connectionId: number
}

export function useSortMultiplayerUsers() {
  const isBeingFollowed = useIsBeingFollowed()
  return React.useCallback(
    (a: SortableUser, b: SortableUser) => {
      if (isBeingFollowed(a.id, a.connectionId)) {
        return -1
      }
      if (isBeingFollowed(b.id, b.connectionId)) {
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

export function useMyConnections(): ConnectionInfo[] {
  const myUserId = useMyUserId()
  const conns = useStorage((store) => store.connections)
  if (myUserId == null) {
    return []
  }
  return conns[myUserId] ?? []
}

/**
 * Store the current user's connection in the room storage.
 */
export function useStoreConnection() {
  const loginState = useEditorState(
    Substores.userState,
    (store) => store.userState.loginState,
    'useAddConnection loginState',
  )

  const storeConnection = useMutation(
    ({ storage, self }) => {
      if (!isLoggedIn(loginState)) {
        return
      }

      const selfConns: ConnectionInfo[] = storage.get('connections').get(self.id) ?? []

      // if the current connection is not stored yet...
      if (!selfConns.some((c) => c.id === self.connectionId)) {
        // ...store it...
        const now = Date.now()
        selfConns.push({
          id: self.connectionId,
          startedAt: now,
          lastSeen: now,
        })
        storage.get('connections').update({ [self.id]: selfConns })
      }
    },
    [loginState],
  )

  const connections = useStorage((store) => store.connections)

  React.useEffect(() => {
    if (connections == null) {
      return
    }
    storeConnection()
  }, [connections, storeConnection])
}

/**
 * Update the stored connection's heartbeat field and cleanup stale connections.
 */
export function useMonitorConnection() {
  const debug = isFeatureEnabled('Debug – Connections')
  const loginState = useEditorState(
    Substores.userState,
    (store) => store.userState.loginState,
    'useMonitorConnection loginState',
  )

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
      for (const playerId of Object.keys(allConns)) {
        // iterate its connections...
        allConns[playerId] = allConns[playerId].filter((conn) => {
          // ...and if a connection is expired, remove it
          const expired = now - conn.lastSeen > ConnectionExpiredTime
          changed ||= expired
          if (debug) {
            console.info(`connection ${playerId}-${conn.id} expired=${expired}`)
          }
          return !expired
        })
      }

      // if any connections were deleted, update the connections.
      if (changed) {
        if (debug) {
          console.info('cleaning up expired connections')
        }
        storage.set('connections', new LiveObject(allConns))
      }
    },
    [loginState],
  )

  const updateLastSeen = useMutation(
    ({ storage, self }) => {
      if (!isLoggedIn(loginState)) {
        return
      }

      const selfConns: ConnectionInfo[] = storage.get('connections').get(self.id) ?? []
      // if the current connection is stored, bump its lastSeen field
      const doUpdate = selfConns.some((c) => c.id === self.connectionId)
      if (doUpdate) {
        if (debug) {
          console.info(`heartbeat ${self.id}-${self.connectionId}`)
        }
        const now = Date.now()
        const updatedConns = selfConns.map((c) =>
          c.id === self.connectionId ? { ...c, lastSeen: now } : c,
        )
        storage.get('connections').update({ [self.id]: updatedConns })
      }
    },
    [loginState],
  )

  React.useEffect(() => {
    // ...and start the heartbeat monitoring.
    const updateLastSeenInterval = window.setInterval(updateLastSeen, ConnectionHeartbeatInterval)
    const cleanupInactiveInterval = window.setInterval(cleanupInactive, ConnectionHeartbeatInterval)
    return function () {
      window.clearInterval(updateLastSeenInterval)
      window.clearInterval(cleanupInactiveInterval)
    }
  }, [updateLastSeen, cleanupInactive])
}
