import React from 'react'

import type { DataRouteObject } from 'react-router'
import { Outlet, RouterProvider, createMemoryRouter } from 'react-router'

import type {
  UNSAFE_RemixContextObject as RemixContextObject,
  UNSAFE_FutureConfig as FutureConfig,
  UNSAFE_AssetsManifest as AssetsManifest,
  UNSAFE_RouteModules as RouteModules,
} from '@remix-run/react'
import { UNSAFE_RemixContext as RemixContext } from '@remix-run/react'

import { getContentsTreeFileFromString } from '../assets'
import { Substores, useEditorState } from '../editor/store/store-hook'
import { UtopiaRemixRootErrorBoundary } from './UtopiaRemixRootErrorBoundary'

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

function createAssetsManifest(): AssetsManifest {
  return {
    entry: { imports: [], module: 'TODO' },
    url: '/',
    version: '1',
    routes: {
      root: {
        hasAction: false,
        hasLoader: false,
        hasCatchBoundary: false,
        hasErrorBoundary: false,
        module: 'TODO',
        id: 'root',
      },
    },
  }
}

async function createRouteModules(fileContents: string): Promise<RouteModules> {
  const module = await import(fileContents)
  return {
    root: {
      default: module.default,
    },
  }
}

function parseRoutes(): DataRouteObject[] {
  let dataRoute: DataRouteObject = {
    caseSensitive: false,
    element: <RemixRoute id={'root'} />,
    errorElement: undefined,
    id: 'root', // FIXME: needs to be generalized
    index: true,
    path: '/',
    handle: null,
    // Note: we don't need loader/action/shouldRevalidate on these routes
    // since they're for a static render
  }

  return [dataRoute]
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
  const [routeModules, setRouteModules] = React.useState<RouteModules | null>(null)
  const projectContents = useEditorState(
    Substores.projectContents,
    (_) => _.editor.projectContents,
    'RemixRootComponent projectContents',
  )

  const assetsManifest = React.useMemo(() => createAssetsManifest(), [])

  React.useEffect(() => {
    const root = getContentsTreeFileFromString(projectContents, 'src/root.tsx')
    if (root != null && root.type === 'TEXT_FILE') {
      // taken from https://stackoverflow.com/a/57255653
      const contents = `data:text/javascript;base64,${btoa(root.fileContents.code)}`
      void createRouteModules(contents).then((routes) => setRouteModules(routes))
    }
  }, [projectContents])

  const router = React.useMemo(() => {
    const routes = parseRoutes()
    return createMemoryRouter(routes)
  }, [])

  let [location, setLocation] = React.useState(router.state.location)

  React.useLayoutEffect(() => {
    return router.subscribe((newState) => {
      if (newState.location !== location) {
        setLocation(newState.location)
      }
    })
  }, [location, router])

  if (routeModules == null) {
    return null
  }

  return (
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
  )
})
