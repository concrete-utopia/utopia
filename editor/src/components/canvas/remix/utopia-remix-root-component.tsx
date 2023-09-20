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
import { updateNavigationState } from '../../editor/actions/action-creators'
import createCachedSelector from 're-reselect'
import type { RemixNavigationSubstate } from '../../editor/store/store-hook-substore-types'
import { usePrevious } from '../../editor/hook-utils'

type RouterType = ReturnType<typeof createMemoryRouter>

interface RemixNavigationContext {
  forward: () => void
  back: () => void
  home: () => void
  location: Location
}

interface RemixNavigationAtomData {
  [pathString: string]: RemixNavigationContext | undefined
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

const currentEntriesSelector = createCachedSelector(
  (store: RemixNavigationSubstate) => store.editor.remixNavigationState,
  (_: RemixNavigationSubstate, path: ElementPath) => path,
  (remixNavigationState, pathToRemixScene) =>
    remixNavigationState[EP.toString(pathToRemixScene)] ?? null,
)((_, x) => EP.toString(x))

export const UtopiaRemixRootComponent = React.memo((props: UtopiaRemixRootComponentProps) => {
  const remixDerivedDataRef = useRefEditorState((store) => store.derived.remixData)

  const routes = useGetRoutes()

  const basePath = props[UTOPIA_PATH_KEY]

  const routeModules = useGetRouteModules(basePath)

  const [navigationData, setNavigationData] = useAtom(RemixNavigationAtom)

  const currentEntries = useEditorState(
    Substores.remixNavigation,
    (store) => currentEntriesSelector(store, basePath),
    'UtopiaRemixRootComponent currentEntries',
  )
  const currentEntriesRef = React.useRef(currentEntries)
  currentEntriesRef.current = currentEntries

  const previousEntries = usePrevious(currentEntries)

  const routerRef = React.useRef<RouterType | null>(null)

  // The router always needs to be updated otherwise new routes won't work without a refresh
  // We need to create the new router with the current location in the initial entries to
  // prevent it thinking that it is rendering '/'
  const router = React.useMemo(() => {
    if (routes == null || routes.length === 0) {
      return null
    }

    if (routerRef.current == null || previousEntries == null || currentEntries == null) {
      return createMemoryRouter(routes)
    }

    if (locationArraysEqual(previousEntries, currentEntries)) {
      return routerRef.current
    }

    return createMemoryRouter(routes, { initialEntries: currentEntries })
  }, [previousEntries, currentEntries, routes])

  React.useEffect(() => {
    routerRef.current = router
  }, [router])

  const dispatch = useDispatch()
  const setActiveRemixScene = useSetAtom(ActiveRemixSceneAtom)

  const updateNavigationData = React.useCallback(
    (innerRouter: RouterType, location: Location) => {
      const existingEntries = currentEntriesRef.current ?? []

      const updatedEntries =
        existingEntries.at(-1)?.pathname === location.pathname
          ? existingEntries
          : existingEntries.concat(location)

      setNavigationData((current) => {
        return {
          ...current,
          [EP.toString(basePath)]: {
            forward: () => void innerRouter.navigate(1),
            back: () => void innerRouter.navigate(-1),
            home: () => void innerRouter.navigate('/'),
            location: location,
          },
        }
      })
      setActiveRemixScene(basePath)
      dispatch([updateNavigationState(EP.toString(basePath), updatedEntries)])
    },
    [basePath, dispatch, setActiveRemixScene, setNavigationData],
  )

  // initialize navigation data
  React.useLayoutEffect(() => {
    if (routerRef.current != null && navigationData[EP.toString(basePath)] == null) {
      updateNavigationData(routerRef.current, routerRef.current.state.location)
    }
  }, [navigationData, basePath, updateNavigationData])

  // apply changes navigation data
  React.useLayoutEffect(() => {
    if (router != null) {
      return router?.subscribe((newState) => {
        if (newState.navigation.location == null) {
          // newState.navigation.location will hold an intended navigation, so when it is null
          // that will have completed
          updateNavigationData(router, newState.location)
        }
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
