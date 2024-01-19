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
