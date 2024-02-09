import { UNSAFE_RemixContext as RemixContext } from '@remix-run/react'
import type { UNSAFE_RouteModules as RouteModules } from '@remix-run/react'
import React from 'react'
import type { DataRouteObject, Location, RouteObject } from 'react-router'
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
import { CreateRemixDerivedDataRefsGLOBAL } from '../../editor/store/remix-derived-data'
import { patchRoutesWithContext } from '../../../third-party/remix/create-remix-stub'
import type { AppLoadContext } from '@remix-run/server-runtime'

type RouteModule = RouteModules[keyof RouteModules]
type RouterType = ReturnType<typeof createMemoryRouter>

interface RemixNavigationContext {
  forward: () => void
  back: () => void
  home: () => void
  navigate: (loc: string) => void
  location: Location
  entries: Array<Location>
}

export interface RemixNavigationAtomData {
  [pathString: string]: RemixNavigationContext | undefined
}

export const ActiveRemixSceneAtom = atom<ElementPath>(EP.emptyElementPath)
export const RemixNavigationAtom = atom<RemixNavigationAtomData>({})

export function useRemixNavigationContext(
  scenePath: ElementPath | null,
): RemixNavigationContext | null {
  const [remixNavigationState] = useAtom(RemixNavigationAtom)
  const remixContext = scenePath != null ? remixNavigationState[EP.toString(scenePath)] : null
  return remixContext ?? null
}

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
    'useGetRouteModules defaultExports',
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

      const createExecutionScope = () =>
        value.executionScopeCreator(
          projectContentsRef.current,
          fileBlobsRef.current,
          hiddenInstancesRef.current,
          displayNoneInstancesRef.current,
          metadataContext,
        ).scope

      const defaultComponent = (componentProps: any) =>
        createExecutionScope()[nameAndUid.name]?.(componentProps) ?? <React.Fragment />

      const errorBoundary = value.createErrorBoundary
        ? (componentProps: any) => createExecutionScope()['ErrorBoundary']?.(componentProps) ?? null
        : undefined

      const links: RouteModule['links'] = () => createExecutionScope()['links']?.() ?? null
      const meta: RouteModule['meta'] = (args) => createExecutionScope()['meta']?.(args) ?? null

      const routeModule: RouteModule = {
        ...value,
        ErrorBoundary: errorBoundary == undefined ? undefined : PathPropHOC(errorBoundary),
        default: PathPropHOC(defaultComponent),
        links: links,
        meta: meta,
      }

      routeModulesResult[routeId] = routeModule
      CreateRemixDerivedDataRefsGLOBAL.routeModulesCache.current[routeId] = routeModule
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

export const RouteExportsForRouteObject: Array<keyof RouteObject> = [
  'action',
  'loader',
  'handle',
  'shouldRevalidate',
]

function useGetRoutes(
  getLoadContext?: (request: Request) => Promise<AppLoadContext> | AppLoadContext,
) {
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

    function addExportsToRoutes(innerRoutes: DataRouteObject[]) {
      innerRoutes.forEach((route) => {
        // FIXME Adding a loader function to the 'root' route causes the `createShouldRevalidate` to fail, because
        // we only ever pass in an empty object for the `routeModules` and never mutate it
        const creatorForRoute = creators[route.id] ?? null
        if (creatorForRoute != null) {
          for (const routeExport of RouteExportsForRouteObject) {
            route[routeExport] = (args: any) =>
              creatorForRoute
                .executionScopeCreator(
                  projectContentsRef.current,
                  fileBlobsRef.current,
                  hiddenInstancesRef.current,
                  displayNoneInstancesRef.current,
                  metadataContext,
                )
                .scope[routeExport]?.(args) ?? null
          }
        }

        addExportsToRoutes(route.children ?? [])
      })
    }

    addExportsToRoutes(routes)

    const routesWithContext = patchRoutesWithContext(routes, getLoadContext)

    return routesWithContext
  }, [
    getLoadContext, // I'm not sure about the performance impact of this, maybe it'd be better to pass in getLoadContext as a Ref?
    displayNoneInstancesRef,
    metadataContext,
    fileBlobsRef,
    hiddenInstancesRef,
    projectContentsRef,
    remixDerivedDataRef,
    routes,
  ])
}

function useRoutesWithIdEquality(
  getLoadContext?: (request: Request) => Promise<AppLoadContext> | AppLoadContext,
) {
  const previousRoutes = React.useRef<Array<RouteObject | DataRouteObject> | null>(null)

  const nextRoutes = useGetRoutes(getLoadContext)

  const routes = React.useMemo(() => {
    if (previousRoutes.current == null || previousRoutes.current.length !== nextRoutes.length) {
      previousRoutes.current = nextRoutes
      return nextRoutes
    }

    let prevIdsSet = new Set(previousRoutes.current.map((r) => r.id ?? '0'))
    let currentIdsSet = new Set(nextRoutes.map((r) => r.id ?? '0'))

    const same = [...prevIdsSet].every((id) => currentIdsSet.has(id))

    if (!same) {
      previousRoutes.current = nextRoutes
      return nextRoutes
    }

    return previousRoutes.current
  }, [nextRoutes])

  return routes
}

export interface UtopiaRemixRootComponentProps {
  [UTOPIA_PATH_KEY]: ElementPath
  getLoadContext?: (request: Request) => Promise<AppLoadContext> | AppLoadContext
}

export const UtopiaRemixRootComponent = (props: UtopiaRemixRootComponentProps) => {
  const remixDerivedDataRef = useRefEditorState((store) => store.derived.remixData)

  const routes = useRoutesWithIdEquality(props.getLoadContext)

  const basePath = props[UTOPIA_PATH_KEY]

  const routeModules = useGetRouteModules(basePath)

  const [navigationData, setNavigationData] = useAtom(RemixNavigationAtom)

  const currentEntries = navigationData[EP.toString(basePath)]?.entries
  const currentEntriesRef = React.useRef(currentEntries)
  currentEntriesRef.current = currentEntries

  // The router always needs to be updated otherwise new routes won't work without a refresh
  // We need to create the new router with the current location in the initial entries to
  // prevent it thinking that it is rendering '/'
  const router = React.useMemo(() => {
    if (routes == null || routes.length === 0) {
      return null
    }
    const initialEntries = currentEntriesRef.current == null ? undefined : currentEntriesRef.current
    return createMemoryRouter(routes, { initialEntries: initialEntries })
  }, [routes])

  const setNavigationDataForRouter = React.useCallback(
    (
      innerRouter: RouterType,
      location: Location,
      updateEntries: 'append-to-entries' | 'replace-entries',
    ) => {
      setNavigationData((current) => {
        const key = EP.toString(basePath)
        const existingEntries = current[key]?.entries ?? []

        const shouldUpdateEntries =
          updateEntries === 'append-to-entries' &&
          existingEntries.at(-1)?.pathname !== location.pathname

        const updatedEntries = shouldUpdateEntries
          ? existingEntries.concat(location)
          : existingEntries

        return {
          ...current,
          [EP.toString(basePath)]: {
            forward: () => void innerRouter.navigate(1),
            back: () => void innerRouter.navigate(-1),
            home: () => void innerRouter.navigate('/'),
            navigate: (loc: string) => void innerRouter.navigate(loc),
            location: location,
            entries: updatedEntries,
          },
        }
      })
    },
    [basePath, setNavigationData],
  )

  const updateNavigationData = React.useCallback(
    (innerRouter: RouterType, location: Location) => {
      setNavigationDataForRouter(innerRouter, location, 'append-to-entries')
    },
    [setNavigationDataForRouter],
  )

  const initialiseNavigationData = React.useCallback(
    (innerRouter: RouterType) => {
      setNavigationDataForRouter(innerRouter, innerRouter.state.location, 'replace-entries')
    },
    [setNavigationDataForRouter],
  )

  const setActiveRemixScene = useSetAtom(ActiveRemixSceneAtom)
  React.useLayoutEffect(() => {
    setActiveRemixScene(basePath)
  }, [basePath, setActiveRemixScene])

  // Initialise navigation data. We also need to do this if the router has changed,
  // so that the navigation functions use the current router
  React.useLayoutEffect(() => {
    if (router != null) {
      initialiseNavigationData(router)
    }
  }, [router, initialiseNavigationData])

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
      <OutletPathContext.Provider value={basePath}>
        <RouterProvider
          router={router}
          fallbackElement={null}
          future={{ v7_startTransition: true }}
        />
      </OutletPathContext.Provider>
    </RemixContext.Provider>
  )
}
