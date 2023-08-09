import React from 'react'

import type { DataRouteObject } from 'react-router'
import { Outlet, RouterProvider, createMemoryRouter } from 'react-router'

import type {
  UNSAFE_RemixContextObject as RemixContextObject,
  UNSAFE_FutureConfig as FutureConfig,
  UNSAFE_AssetsManifest as AssetsManifest,
  UNSAFE_EntryRoute as EntryRoute,
  UNSAFE_RouteManifest as RouteManifest,
  UNSAFE_RouteModules as RouteModules,
} from '@remix-run/react'
import { UNSAFE_RemixContext as RemixContext } from '@remix-run/react'

import { evaluator } from '../../../core/es-modules/evaluator/evaluator'
import { resolveBuiltInDependency } from '../../../core/es-modules/package-manager/built-in-dependencies'
import type { ProjectContentFile, ProjectContentsTree } from '../../assets'
import { useEditorState, Substores } from '../../editor/store/store-hook'
import { getRoutesFromFiles } from './remix-utils'
import { foldEither } from '../../../core/shared/either'
import { UtopiaRemixRootErrorBoundary } from './utopia-remix-root-error-boundary'

function invariant<T>(value: T | null | undefined, message: string): asserts value is T {
  if (value == null) {
    console.error(`Invariant error: ${message}`)
    throw new Error(message)
  }
}

// Not exported from Remix
// FIXME: either find where this component is export from remix, submit PR to export it, or leave it as is
function useRemixContext(): RemixContextObject {
  let context = React.useContext(RemixContext)
  invariant(context, 'You must render this element inside a <Remix> element')
  return context
}

// Not exported from Remix
// FIXME: either find where this component is export from remix, submit PR to export it, or leave it as is
export const RemixRoute = React.memo(({ id }: { id: string }) => {
  let { routeModules, future } = useRemixContext()

  invariant(
    routeModules,
    "Cannot initialize 'routeModules'. This normally occurs when you have server code in your client modules.\n" +
      'Check this link for more details:\nhttps://remix.run/pages/gotchas#server-code-in-client-bundles',
  )

  let { default: Component, ErrorBoundary, CatchBoundary } = routeModules[id]

  // Default Component to Outlet if we expose boundary UI components
  if (
    Component == null &&
    (ErrorBoundary != null || (!future.v2_errorBoundary && CatchBoundary != null))
  ) {
    Component = Outlet
  }

  invariant(
    Component,
    `Route "${id}" has no component! Please go add a \`default\` export in the route module file.\n` +
      'If you were trying to navigate or submit to a resource route, use `<a>` instead of `<Link>` or `<Form reloadDocument>`.',
  )

  return <Component />
})

function createAssetsManifest(routes: RouteManifest<EntryRoute>): AssetsManifest {
  return {
    entry: { imports: [], module: 'TODO' },
    url: '/',
    version: '1',
    routes: routes,
  }
}

function routeFromEntry(route: EntryRoute): DataRouteObject {
  return {
    caseSensitive: false,
    element: <RemixRoute id={route.id} />,
    errorElement: undefined,
    id: route.id,
    index: route.index,
    path: route.path,
    handle: null,
  }
}

const defaultFutureConfig: FutureConfig = {
  v2_dev: true,
  unstable_postcss: false,
  unstable_tailwind: false,
  v2_errorBoundary: false,
  v2_headers: false,
  v2_meta: false,
  v2_normalizeFormMethod: false,
  v2_routeConvention: false,
}

export const UtopiaRemixRootComponent = React.memo(() => {
  const projectContents = useEditorState(
    Substores.projectContents,
    (_) => _.editor.projectContents,
    'RemixRootComponent projectContents',
  )

  const builtInDependencies = useEditorState(
    Substores.builtInDependencies,
    (_) => _.builtInDependencies,
    'RemixRootComponent builtInDependencies',
  )

  const routeManifest = React.useMemo(() => {
    const getFlatFilePaths = (root: ProjectContentsTree): ProjectContentFile[] =>
      root.type === 'PROJECT_CONTENT_FILE'
        ? [root]
        : Object.values(root.children).flatMap((c) => getFlatFilePaths(c))

    const flatFiles = Object.values(projectContents).flatMap(getFlatFilePaths)

    return foldEither(
      () => ({}),
      (r) => r,
      getRoutesFromFiles(flatFiles),
    )
  }, [projectContents])

  const assetsManifest = React.useMemo(() => createAssetsManifest(routeManifest), [routeManifest])

  const { routeModules, routes } = React.useMemo(() => {
    const partialRequire = (toImport: string) => {
      const builtInDependency = resolveBuiltInDependency(builtInDependencies, toImport)
      if (builtInDependency != null) {
        return builtInDependency
      }
      throw new Error(`Cannot resolve dependency: ${toImport}`)
    }

    const routeManifestResult: RouteModules = {}
    const routesResult: DataRouteObject[] = []

    for (const route of Object.values(routeManifest)) {
      try {
        const module = evaluator(
          route.filePath,
          route.moduleContents,
          { exports: {} },
          partialRequire,
        )

        routeManifestResult[route.id] = {
          default: module.exports.default,
        }

        routesResult.push({
          ...routeFromEntry(route),
          loader: module.exports.loader != null ? module.exports.loader : undefined,
        })
      } catch (e) {
        console.error(e)
      }
    }

    return { routeModules: routeManifestResult, routes: routesResult }
  }, [builtInDependencies, routeManifest])

  const router = React.useMemo(() => createMemoryRouter(routes), [routes])

  let [location, setLocation] = React.useState(router.state.location)

  React.useLayoutEffect(() => {
    return router.subscribe((newState) => {
      if (newState.location !== location) {
        setLocation(newState.location)
      }
    })
  }, [location, router])

  const onClickBack = React.useCallback(() => router.navigate(-1), [router])
  const onClickForward = React.useCallback(() => router.navigate(1), [router])

  return (
    <div>
      <div
        style={{
          display: 'flex',
          flexDirection: 'row',
          gap: 5,
          padding: 5,
          marginBottom: 10,
          borderRadius: 3,
          border: '1px solid gray',
          backgroundColor: '#CEE5FE',
        }}
      >
        <div onClick={onClickBack}>⬅️</div>
        <div onClick={onClickForward}>➡️</div>
        {router.state.location.pathname}
      </div>
      <RemixContext.Provider
        value={{
          manifest: assetsManifest,
          routeModules: routeModules,
          future: defaultFutureConfig,
        }}
      >
        <UtopiaRemixRootErrorBoundary location={location}>
          <RouterProvider
            router={router}
            fallbackElement={null}
            future={{ v7_startTransition: true }}
          />
        </UtopiaRemixRootErrorBoundary>
      </RemixContext.Provider>
    </div>
  )
})
