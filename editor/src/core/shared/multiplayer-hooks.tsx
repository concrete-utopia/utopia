import { useAtom } from 'jotai'
import React from 'react'
import {
  ActiveRemixSceneAtom,
  RemixNavigationAtom,
} from '../../components/canvas/remix/utopia-remix-root-component'
import type { RemixPresence } from './multiplayer'
import * as EP from './element-path'

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
