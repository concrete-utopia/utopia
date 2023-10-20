import React from 'react'
import invariant from './invariant'
import type { UNSAFE_RemixContextObject as RemixContextObject } from '@remix-run/react'
import { RemixRootDefaultErrorBoundary } from '@remix-run/react/dist/errorBoundaries'
import { UNSAFE_RemixContext as RemixContext } from '@remix-run/react'
import { Outlet, useRouteError } from 'react-router'

function useRemixContext(): RemixContextObject {
  let context = React.useContext(RemixContext)
  invariant(context, 'You must render this element inside a <Remix> element')
  return context
}
export function RemixRoute({ id }: { id: string }) {
  let { routeModules } = useRemixContext()

  invariant(
    routeModules,
    "Cannot initialize 'routeModules'. This normally occurs when you have server code in your client modules.\n" +
      'Check this link for more details:\nhttps://remix.run/pages/gotchas#server-code-in-client-bundles',
  )

  let { default: Component, ErrorBoundary } = routeModules[id]

  // Default Component to Outlet if we expose boundary UI components
  if (!Component && ErrorBoundary) {
    Component = Outlet
  }

  invariant(
    Component,
    `Route "${id}" has no component! Please go add a \`default\` export in the route module file.\n` +
      'If you were trying to navigate or submit to a resource route, use `<a>` instead of `<Link>` or `<Form reloadDocument>`.',
  )

  return <Component />
}

export function RemixRouteError({ id }: { id: string }) {
  let { routeModules } = useRemixContext()

  // This checks prevent cryptic error messages such as: 'Cannot read properties of undefined (reading 'root')'
  invariant(
    routeModules,
    "Cannot initialize 'routeModules'. This normally occurs when you have server code in your client modules.\n" +
      'Check this link for more details:\nhttps://remix.run/pages/gotchas#server-code-in-client-bundles',
  )

  let error = useRouteError()
  let { ErrorBoundary } = routeModules[id]

  if (ErrorBoundary) {
    return <ErrorBoundary />
  } else if (id === 'root') {
    return <RemixRootDefaultErrorBoundary error={error} />
  }

  throw error
}
