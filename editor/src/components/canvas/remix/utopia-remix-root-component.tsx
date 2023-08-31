import React from 'react'

import { RouterProvider, createMemoryRouter } from 'react-router'
import type { UNSAFE_RouteModules as RouteModules } from '@remix-run/react'

import { UNSAFE_RemixContext as RemixContext } from '@remix-run/react'

import { Substores, useEditorState } from '../../editor/store/store-hook'
import type { ElementPath } from '../../../core/shared/project-file-types'
import { UTOPIA_PATH_KEY } from '../../../core/model/utopia-constants'
import type { UiJsxCanvasContextData } from '../ui-jsx-canvas'
import { ForceRerunDOMWalkerGLOBAL_SPIKE_KILLME, UiJsxCanvasCtxAtom } from '../ui-jsx-canvas'
import { forceNotNull, optionalMap } from '../../../core/shared/optional-utils'
import { AlwaysFalse, usePubSubAtomReadOnly } from '../../../core/shared/atom-with-pub-sub'
import { PathPropHOC } from './remix-utils'
import * as EP from '../../../core/shared/element-path'
import { appendTwoPaths } from '../canvas-utils'
import { atom, useSetAtom } from 'jotai'

interface RemixNavigationContext {
  forward: () => void
  back: () => void
  home: () => void
  pathname: string
}

export const RemixNavigationAtom = atom<RemixNavigationContext | null>(null)

export interface UtopiaRemixRootComponentProps {
  [UTOPIA_PATH_KEY]: ElementPath
}

export const UtopiaRemixRootComponent = React.memo((props: UtopiaRemixRootComponentProps) => {
  const remixDerivedDataRef = useEditorState(
    Substores.derived,
    (store) => store.derived.remixData,
    'UtopiaRemixRootComponent remixData',
  )

  const basePath = props[UTOPIA_PATH_KEY]

  const routeModules = React.useMemo(() => {
    if (remixDerivedDataRef == null) {
      return null
    }

    const routeModulesResult: RouteModules = {}
    for (const [key, value] of Object.entries(remixDerivedDataRef.routeModules)) {
      const relativePath =
        remixDerivedDataRef.routeModulesToRelativePaths[value.filePath].relativePath

      routeModulesResult[key] = {
        ...value,
        default: PathPropHOC(value.default, EP.toString(appendTwoPaths(basePath, relativePath))),
      }
    }

    return routeModulesResult
  }, [basePath, remixDerivedDataRef])

  const router = React.useMemo(
    () => optionalMap(createMemoryRouter, remixDerivedDataRef?.routes),
    [remixDerivedDataRef],
  )

  let uiJsxCanvasContext: UiJsxCanvasContextData = forceNotNull(
    `Missing UiJsxCanvasCtxAtom provider`,
    usePubSubAtomReadOnly(UiJsxCanvasCtxAtom, AlwaysFalse),
  )

  const setNavigationAtom = useSetAtom(RemixNavigationAtom)

  React.useLayoutEffect(() => {
    if (router != null) {
      setNavigationAtom({
        forward: () => void router.navigate(1),
        back: () => void router.navigate(-1),
        home: () => void router.navigate('/'),
        pathname: router.state.location.pathname,
      })
    }
  })

  React.useLayoutEffect(() => {
    return router?.subscribe((n) => {
      uiJsxCanvasContext.current.spyValues.metadata = {}
      ForceRerunDOMWalkerGLOBAL_SPIKE_KILLME.current = true
      setNavigationAtom({
        forward: () => void router.navigate(1),
        back: () => void router.navigate(-1),
        home: () => void router.navigate('/'),
        pathname: n.location.pathname,
      })
    })
  }, [router, setNavigationAtom, uiJsxCanvasContext])

  if (remixDerivedDataRef == null || router == null || routeModules == null) {
    return null
  }

  const { assetsManifest, futureConfig } = remixDerivedDataRef

  return (
    <RemixContext.Provider
      value={{
        manifest: assetsManifest,
        routeModules: routeModules,
        future: futureConfig,
      }}
    >
      {/* <UtopiaRemixRootErrorBoundary location={location}> */}
      <RouterProvider
        router={router}
        fallbackElement={null}
        future={{ v7_startTransition: true }}
      />
      {/* </UtopiaRemixRootErrorBoundary> */}
    </RemixContext.Provider>
  )
})
