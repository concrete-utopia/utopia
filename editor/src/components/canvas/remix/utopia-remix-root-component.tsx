import { RemixContext } from '@remix-run/react/dist/components'
import type { UNSAFE_RouteModules as RouteModules } from '@remix-run/react'
import React from 'react'
import type { DataRouteObject, Location } from 'react-router'
import { createMemoryRouter, RouterProvider } from 'react-router'
import { UTOPIA_PATH_KEY } from '../../../core/model/utopia-constants'
import type { ElementPath } from '../../../core/shared/project-file-types'
import { useRefEditorState, useEditorState, Substores } from '../../editor/store/store-hook'
import * as EP from '../../../core/shared/element-path'
import { PathPropHOC } from './path-props-hoc'
import { atom, useAtom, useSetAtom } from 'jotai'
import { getDefaultExportNameAndUidFromFile } from '../../../core/model/project-file-utils'
import { OutletPathContext } from './remix-utils'
import { UiJsxCanvasCtxAtom } from '../ui-jsx-canvas'
import type { UiJsxCanvasContextData } from '../ui-jsx-canvas'
import { forceNotNull } from '../../../core/shared/optional-utils'
import { AlwaysFalse, usePubSubAtomReadOnly } from '../../../core/shared/atom-with-pub-sub'
import { useDispatch } from '../../editor/store/dispatch-context'
import { mergeWithPrevUndo, updateNavigationState } from '../../editor/actions/action-creators'

type RouterType = ReturnType<typeof createMemoryRouter>

interface RemixNavigationContext {
  forward: () => void
  back: () => void
  home: () => void
  location: Location
  entries: Array<Location>
}

interface RemixNavigationAtomData {
  [pathString: string]: RemixNavigationContext
}

export const ActiveRemixSceneAtom = atom<ElementPath>(EP.emptyElementPath)
export const RemixNavigationAtom = atom<RemixNavigationAtomData>({})

const locationArraysEqual = (left: Location[], right: Location[]): boolean =>
  left.length === right.length && left.every((element, index) => element.key === right[index].key)

function useGetRouteModules(basePath: ElementPath) {
  const remixDerivedDataRef = useRefEditorState((store) => store.derived.remixData)
  const projectContentsRef = useRefEditorState((store) => store.editor.projectContents)
  const fileBlobsRef = useRefEditorState((store) => store.editor.canvas.base64Blobs)
  const hiddenInstancesRef = useRefEditorState((store) => store.editor.hiddenInstances)
  const displayNoneInstancesRef = useRefEditorState((store) => store.editor.displayNoneInstances)

  let metadataContext: UiJsxCanvasContextData = forceNotNull(
    `Missing UiJsxCanvasCtxAtom provider`,
    usePubSubAtomReadOnly(UiJsxCanvasCtxAtom, AlwaysFalse),
  )

  const defaultExports = useEditorState(
    Substores.derived,
    (store) => {
      const routeModuleCreators = store.derived.remixData?.routeModuleCreators ?? {}
      return Object.values(routeModuleCreators).map(
        (rmc) => getDefaultExportNameAndUidFromFile(projectContentsRef.current, rmc.filePath)?.name,
      )
    },
    '',
  )

  return React.useMemo(() => {
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

      const defaultComponent = (componentProps: any) =>
        value
          .executionScopeCreator(
            projectContentsRef.current,
            fileBlobsRef.current,
            hiddenInstancesRef.current,
            displayNoneInstancesRef.current,
            metadataContext,
          )
          .scope[nameAndUid.name]?.(componentProps) ?? <React.Fragment />

      routeModulesResult[routeId] = {
        ...value,
        default: PathPropHOC(defaultComponent),
      }
    }

    return routeModulesResult
  }, [
    defaultExports,
    metadataContext,
    remixDerivedDataRef,
    projectContentsRef,
    fileBlobsRef,
    hiddenInstancesRef,
    displayNoneInstancesRef,
  ])
}

function useGetRoutes() {
  const routes = useEditorState(
    Substores.derived,
    (store) => store.derived.remixData?.routes ?? [],
    'UtopiaRemixRootComponent routes',
  )

  const remixDerivedDataRef = useRefEditorState((store) => store.derived.remixData)
  const projectContentsRef = useRefEditorState((store) => store.editor.projectContents)
  const fileBlobsRef = useRefEditorState((store) => store.editor.canvas.base64Blobs)
  const hiddenInstancesRef = useRefEditorState((store) => store.editor.hiddenInstances)
  const displayNoneInstancesRef = useRefEditorState((store) => store.editor.displayNoneInstances)

  let metadataContext: UiJsxCanvasContextData = forceNotNull(
    `Missing UiJsxCanvasCtxAtom provider`,
    usePubSubAtomReadOnly(UiJsxCanvasCtxAtom, AlwaysFalse),
  )

  return React.useMemo(() => {
    if (remixDerivedDataRef.current == null) {
      return routes
    }

    const creators = remixDerivedDataRef.current.routeModuleCreators

    function addLoaderAndActionToRoutes(innerRoutes: DataRouteObject[]) {
      innerRoutes.forEach((route) => {
        // FIXME Adding a loader function to the 'root' route causes the `createShouldRevalidate` to fail, because
        // we only ever pass in an empty object for the `routeModules` and never mutate it
        const creatorForRoute = route.id === 'root' ? null : creators[route.id]
        if (creatorForRoute != null) {
          route.action = (args: any) =>
            creatorForRoute
              .executionScopeCreator(
                projectContentsRef.current,
                fileBlobsRef.current,
                hiddenInstancesRef.current,
                displayNoneInstancesRef.current,
                metadataContext,
              )
              .scope['action']?.(args) ?? null
          route.loader = (args: any) =>
            creatorForRoute
              .executionScopeCreator(
                projectContentsRef.current,
                fileBlobsRef.current,
                hiddenInstancesRef.current,
                displayNoneInstancesRef.current,
                metadataContext,
              )
              .scope['loader']?.(args) ?? null
        }

        addLoaderAndActionToRoutes(route.children ?? [])
      })
    }

    addLoaderAndActionToRoutes(routes)

    return routes
  }, [
    displayNoneInstancesRef,
    metadataContext,
    fileBlobsRef,
    hiddenInstancesRef,
    projectContentsRef,
    remixDerivedDataRef,
    routes,
  ])
}

export interface UtopiaRemixRootComponentProps {
  [UTOPIA_PATH_KEY]: ElementPath
}

export const UtopiaRemixRootComponent = React.memo((props: UtopiaRemixRootComponentProps) => {
  const remixDerivedDataRef = useRefEditorState((store) => store.derived.remixData)

  const routes = useGetRoutes()

  const basePath = props[UTOPIA_PATH_KEY]

  const routeModules = useGetRouteModules(basePath)

  const [navigationData, setNavigationData] = useAtom(RemixNavigationAtom)
  const setActiveRemixScene = useSetAtom(ActiveRemixSceneAtom)

  const currentEntries = React.useMemo(
    () => navigationData[EP.toString(basePath)]?.entries ?? [],
    [basePath, navigationData],
  )
  const currentEntriesRef = React.useRef(currentEntries)
  currentEntriesRef.current = currentEntries

  const routerRef = React.useRef<RouterType | null>(null)

  // The router always needs to be updated otherwise new routes won't work without a refresh
  // We need to create the new router with the current location in the initial entries to
  // prevent it thinking that it is rendering '/'
  const router = React.useMemo(() => {
    if (routes == null || routes.length === 0) {
      return null
    }
    const initialEntries = currentEntriesRef.current == null ? undefined : currentEntriesRef.current
    if (routerRef.current == null || initialEntries == null) {
      return createMemoryRouter(routes, { initialEntries: initialEntries })
    }

    if (locationArraysEqual(initialEntries, currentEntries)) {
      return routerRef.current
    }

    return createMemoryRouter(routes, { initialEntries: initialEntries })
  }, [currentEntries, routes])

  React.useEffect(() => {
    routerRef.current = router
  }, [router])

  const updateNavigationData = React.useCallback(
    (innerRouter: RouterType, location: Location) => {
      setNavigationData((current) => {
        const key = EP.toString(basePath)
        const existingEntries = current[key]?.entries ?? []

        const updatedEntries =
          existingEntries.at(-1)?.pathname === location.pathname
            ? existingEntries
            : existingEntries.concat(location)

        return {
          ...current,
          [EP.toString(basePath)]: {
            forward: () => void innerRouter.navigate(1),
            back: () => void innerRouter.navigate(-1),
            home: () => void innerRouter.navigate('/'),
            location: location,
            entries: updatedEntries,
          },
        }
      })
      setActiveRemixScene(basePath)
    },
    [basePath, setActiveRemixScene, setNavigationData],
  )

  // initialize navigation data
  React.useLayoutEffect(() => {
    if (router != null && navigationData[EP.toString(basePath)] == null) {
      updateNavigationData(router, router.state.location)
    }
  }, [router, navigationData, basePath, updateNavigationData])

  const dispatch = useDispatch()

  // apply changes navigation data
  React.useLayoutEffect(() => {
    if (router != null) {
      return router?.subscribe((newState) => {
        updateNavigationData(router, newState.location)
        dispatch(
          newState.navigation.location == null
            ? []
            : [
                mergeWithPrevUndo([
                  updateNavigationState(
                    EP.toString(basePath),
                    currentEntriesRef.current.concat(newState.navigation.location),
                  ),
                ]),
              ],
        )
      })
    }
    return
  }, [router, updateNavigationData, dispatch, basePath])

  if (remixDerivedDataRef.current == null || routerRef.current == null || routeModules == null) {
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
      <OutletPathContext.Provider value={basePath}>
        <RouterProvider
          router={routerRef.current}
          fallbackElement={null}
          future={{ v7_startTransition: true }}
        />
      </OutletPathContext.Provider>
    </RemixContext.Provider>
  )
})
