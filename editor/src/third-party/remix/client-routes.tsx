/* eslint-disable */

// Copy pasted and adapted from Remix: https://github.com/remix-run/remix/blob/8779b24d0e51cc49a887d16afab9789557b80124/packages/remix-react/routes.tsx

import React from 'react'
import {
  UNSAFE_FutureConfig as FutureConfig,
  UNSAFE_RouteModules as RouteModules,
} from '@remix-run/react'
import { DataRouteObject, ShouldRevalidateFunction } from 'react-router'
import invariant from './invariant'
import { isRouteErrorResponse, useRouteError } from '@remix-run/react'
import { RemixRoute, RemixRouteError } from './remix-route'
import { ErrorBoundaryHandling } from 'src/components/editor/store/editor-state'

export interface RouteManifest<Route> {
  [routeId: string]: Route
}

// NOTE: make sure to change the Route in server-runtime if you change this
interface Route {
  index?: boolean
  caseSensitive?: boolean
  id: string
  parentId?: string
  path?: string
}

// NOTE: make sure to change the EntryRoute in server-runtime if you change this
export interface EntryRoute extends Route {
  hasAction: boolean
  hasLoader: boolean
  hasErrorBoundary: boolean
  imports?: string[]
  module: string
  parentId?: string
}

// Create a map of routes by parentId to use recursively instead of
// repeatedly filtering the manifest.
export function groupRoutesByParentId(manifest: RouteManifest<EntryRoute>) {
  let routes: Record<string, Omit<EntryRoute, 'children'>[]> = {}

  Object.values(manifest).forEach((route) => {
    let parentId = route.parentId || ''
    if (!routes[parentId]) {
      routes[parentId] = []
    }
    routes[parentId].push(route)
  })

  return routes
}

export function createClientRoutesWithHMRRevalidationOptOut(
  needsRevalidation: Set<string>,
  manifest: RouteManifest<EntryRoute>,
  routeModulesCache: RouteModules,
  future: FutureConfig,
  errorBoundaryHandling: ErrorBoundaryHandling,
): ReturnType<typeof createClientRoutes> {
  return createClientRoutes(
    manifest,
    routeModulesCache,
    future,
    '',
    groupRoutesByParentId(manifest),
    needsRevalidation,
    errorBoundaryHandling,
  )
}

export function createClientRoutes(
  manifest: RouteManifest<EntryRoute>,
  routeModulesCache: RouteModules,
  future: FutureConfig,
  parentId: string = '',
  routesByParentId: Record<string, Omit<EntryRoute, 'children'>[]> = groupRoutesByParentId(
    manifest,
  ),
  needsRevalidation: Set<string> | null,
  errorBoundaryHandling: ErrorBoundaryHandling,
): DataRouteObject[] {
  return (routesByParentId[parentId] ?? []).map((route) => {
    const errorElement =
      route.hasErrorBoundary && errorBoundaryHandling === 'use-error-boundaries' ? (
        <RemixRouteError id={route.id} />
      ) : (
        <ErrorThrower />
      )

    let dataRoute: DataRouteObject = {
      caseSensitive: route.caseSensitive,
      element: <RemixRoute id={route.id} />,
      errorElement: errorElement,
      id: route.id,
      index: route.index,
      path: route.path,
      // handle gets added in via useMatches since we aren't guaranteed to
      // have the route module available here
      handle: undefined,
      loader: undefined,
      action: undefined,
      shouldRevalidate: createShouldRevalidate(route, routeModulesCache, needsRevalidation),
    }
    let children = createClientRoutes(
      manifest,
      routeModulesCache,
      future,
      route.id,
      routesByParentId,
      needsRevalidation,
      errorBoundaryHandling,
    )
    if (children.length > 0) dataRoute.children = children
    return dataRoute
  })
}

function createShouldRevalidate(
  route: EntryRoute,
  routeModules: RouteModules,
  needsRevalidation: Set<string> | null,
): ShouldRevalidateFunction {
  let handledRevalidation = false
  return function (arg) {
    let module = routeModules[route.id]
    invariant(module, `Expected route module to be loaded for ${route.id}`)

    // When an HMR / HDR update happens we opt out of all user-defined
    // revalidation logic and the do as the dev server tells us the first
    // time router.revalidate() is called.
    if (needsRevalidation != null && !handledRevalidation) {
      handledRevalidation = true
      return needsRevalidation.has(route.id)
    }

    if (module.shouldRevalidate) {
      return module.shouldRevalidate(arg as any)
    }

    return arg.defaultShouldRevalidate
  }
}

function ErrorThrower() {
  const error = useRouteError()

  if (isRouteErrorResponse(error)) {
    return (
      <div>
        <h1>
          {error.status} {error.statusText}
        </h1>
        <p>{error.data}</p>
      </div>
    )
  } else {
    // Throw the error so that we can show the error overlay across the entire canvas
    throw error
  }
}
