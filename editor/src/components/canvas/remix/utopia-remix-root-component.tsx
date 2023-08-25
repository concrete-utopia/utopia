import React from 'react'
import { UNSAFE_RemixContext as RemixContext } from '@remix-run/react'
import { DefaultFutureConfig } from './remix-utils'
import {
  RemixErrorBoundary,
  V2_RemixRootDefaultErrorBoundary,
} from '@remix-run/react/dist/errorBoundaries'
import { RouterProvider, createMemoryRouter } from 'react-router'

// TODO temporary
const dummyManifest = {
  entry: {
    imports: [],
    module: '',
  },
  routes: {},
  url: '',
  version: '',
}

export const UtopiaRemixRootComponent = React.memo(() => {
  const router = React.useMemo(() => createMemoryRouter([]), [])

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
