import React from 'react'
import { UNSAFE_RemixContext as RemixContext } from '@remix-run/react'
import { DefaultFutureConfig } from './remix-utils'
import {
  RemixErrorBoundary,
  V2_RemixRootDefaultErrorBoundary,
} from '@remix-run/react/dist/errorBoundaries'
import { RouterProvider, createMemoryRouter } from 'react-router'

// TODO temporary, we need the real manifest
const dummyManifest = {
  entry: {
    imports: [],
    module: '',
  },
  routes: {
    root: {
      hasAction: false,
      hasCatchBoundary: false,
      hasErrorBoundary: false,
      hasLoader: false,
      id: 'root',
      imports: [],
      module: '/',
      path: '',
    },
  },
  url: '',
  version: '',
}

// TODO temporary, this is just a placeholder until we have the real content
const DummyRemixPlaceholder = React.memo(() => <div>Remix content is coming soon</div>)

// TODO temporary, we need the real routes
const dummyRoutes = [
  {
    path: '/',
    element: <DummyRemixPlaceholder />,
    children: [],
  },
]

export const UtopiaRemixRootComponent = React.memo(() => {
  const router = React.useMemo(() => createMemoryRouter(dummyRoutes), [])

  let [location] = React.useState(router.state.location)

  return (
    <RemixContext.Provider
      value={{
        manifest: dummyManifest,
        routeModules: {},
        future: DefaultFutureConfig,
      }}
    >
      <RemixErrorBoundary location={location} component={V2_RemixRootDefaultErrorBoundary}>
        <RouterProvider
          router={router}
          fallbackElement={null}
          future={{ v7_startTransition: true }}
        />
      </RemixErrorBoundary>
    </RemixContext.Provider>
  )
})
