import React from 'react'

import { RouterProvider, createMemoryRouter } from 'react-router'
import type { Location, UNSAFE_RouteModules as RouteModules } from '@remix-run/react'

import { UNSAFE_RemixContext as RemixContext } from '@remix-run/react'

import { Substores, useEditorState, useRefEditorState } from '../../editor/store/store-hook'
import type { ElementPath } from '../../../core/shared/project-file-types'
import { UTOPIA_PATH_KEY } from '../../../core/model/utopia-constants'
import type { UiJsxCanvasContextData } from '../ui-jsx-canvas'
import { UiJsxCanvasCtxAtom } from '../ui-jsx-canvas'
import { forceNotNull, optionalMap } from '../../../core/shared/optional-utils'
import { AlwaysFalse, usePubSubAtomReadOnly } from '../../../core/shared/atom-with-pub-sub'
import { PathPropHOC, getDefaultExportNameAndUidFromFile } from './remix-utils'
import * as EP from '../../../core/shared/element-path'
import { appendTwoPaths } from '../canvas-utils'
import { atom, useSetAtom } from 'jotai'

type RouterType = ReturnType<typeof createMemoryRouter>

interface RemixNavigationContext {
  forward: () => void
  back: () => void
  home: () => void
  location: Location
}

interface RemixNavigationAtomData {
  [pathString: string]: RemixNavigationContext
}

export const ActiveRemixSceneAtom = atom<ElementPath>(EP.emptyElementPath)
export const RemixNavigationAtom = atom<RemixNavigationAtomData>({})

export interface UtopiaRemixRootComponentProps {
  [UTOPIA_PATH_KEY]: ElementPath
}

export const UtopiaRemixRootComponent = React.memo((props: UtopiaRemixRootComponentProps) => {
  const remixDerivedDataRef = useRefEditorState((store) => store.derived.remixData)
  const projectContentsRef = useRefEditorState((store) => store.editor.projectContents)

  const routes = useEditorState(
    Substores.derived,
    (store) => store.derived.remixData?.routes ?? [],
    'UtopiaRemixRootComponent routes',
  )

  const defaultExports = useEditorState(
    Substores.derived,
    (store) => {
      const routeModuleCreators = store.derived.remixData?.routeModuleCreators ?? {}
      return Object.values(routeModuleCreators).map(
        // FIXME Saving required as this doesn't update with unsaved content?
        (rmc) => getDefaultExportNameAndUidFromFile(projectContentsRef.current, rmc.filePath)?.name,
      )
    },
    '',
  )

  const basePath = props[UTOPIA_PATH_KEY]

  const routeModules = React.useMemo(() => {
    const defaultExportsIgnored = defaultExports // Forcibly update the routeModules only when the default exports have changed

    if (remixDerivedDataRef.current == null) {
      return null
    }

    const routeModulesResult: RouteModules = {}
    for (const [routeId, value] of Object.entries(
      remixDerivedDataRef.current.routeModuleCreators,
    )) {
      const nameAndUid = getDefaultExportNameAndUidFromFile(
        projectContentsRef.current,
        value.filePath,
      )
      if (nameAndUid == null) {
        continue
      }

      const relativePath =
        remixDerivedDataRef.current.routeModulesToRelativePaths[routeId].relativePath

      const defaultComponent = (componentProps: any) =>
        value
          .executionScopeCreator(projectContentsRef.current)
          .scope[nameAndUid.name]?.(componentProps) ?? <React.Fragment />

      routeModulesResult[routeId] = {
        ...value,
        default: PathPropHOC(defaultComponent, EP.toString(appendTwoPaths(basePath, relativePath))),
      }
    }

    return routeModulesResult
  }, [basePath, defaultExports, remixDerivedDataRef, projectContentsRef])

  // The router always needs to be updated otherwise new routes won't work without a refresh
  const router = React.useMemo(() => optionalMap(createMemoryRouter, routes), [routes])

  const setNavigationData = useSetAtom(RemixNavigationAtom)
  const setActiveRemixScene = useSetAtom(ActiveRemixSceneAtom)

  const updateNavigationData = React.useCallback(
    (routerr: RouterType, location: Location) => {
      setNavigationData((navigationData) => ({
        ...navigationData,
        [EP.toString(basePath)]: {
          forward: () => void routerr.navigate(1),
          back: () => void routerr.navigate(-1),
          home: () => void routerr.navigate('/'),
          location: location,
        },
      }))
      setActiveRemixScene(basePath)
    },
    [basePath, setActiveRemixScene, setNavigationData],
  )

  React.useLayoutEffect(() => {
    if (router != null) {
      updateNavigationData(router, router.state.location)
    }
  }, [router, updateNavigationData])

  React.useLayoutEffect(() => {
    if (router != null) {
      return router?.subscribe((newState) => updateNavigationData(router, newState.location))
    }
    return
  }, [router, updateNavigationData])

  if (remixDerivedDataRef.current == null || router == null || routeModules == null) {
    return null
  }

  const { assetsManifest, futureConfig } = remixDerivedDataRef.current

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
