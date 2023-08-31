/* eslint-disable */
import React from 'react'
import { FutureConfig } from '@remix-run/react/dist/entry'
import { RouteModules } from '@remix-run/react/dist/routeModules'
import { DataRouteObject, ShouldRevalidateFunction } from 'react-router'
import { RemixRoute, RemixRouteError } from '@remix-run/react/dist/components'
import invariant from './invariant'

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
  hasCatchBoundary: boolean
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

export function createServerRoutes(
  manifest: RouteManifest<EntryRoute>,
  routeModules: RouteModules,
  future: FutureConfig,
  parentId: string = '',
  routesByParentId: Record<string, Omit<EntryRoute, 'children'>[]> = groupRoutesByParentId(
    manifest,
  ),
): DataRouteObject[] {
  return (routesByParentId[parentId] || []).map((route) => {
    let hasErrorBoundary =
      future.v2_errorBoundary === true
        ? route.id === 'root' || route.hasErrorBoundary
        : route.id === 'root' || route.hasCatchBoundary || route.hasErrorBoundary
    let dataRoute: DataRouteObject = {
      caseSensitive: route.caseSensitive,
      element: <RemixRoute id={route.id} />,
      errorElement: hasErrorBoundary ? <RemixRouteError id={route.id} /> : undefined,
      id: route.id,
      index: route.index,
      path: route.path,
      handle: routeModules[route.id].handle,
      // Note: we don't need loader/action/shouldRevalidate on these routes
      // since they're for a static render
    }

    let children = createServerRoutes(manifest, routeModules, future, route.id, routesByParentId)
    if (children.length > 0) dataRoute.children = children
    return dataRoute
  })
}

export function createClientRoutesWithHMRRevalidationOptOut(
  needsRevalidation: Set<string>,
  manifest: RouteManifest<EntryRoute>,
  routeModulesCache: RouteModules,
  future: FutureConfig,
) {
  return createClientRoutes(
    manifest,
    routeModulesCache,
    future,
    '',
    groupRoutesByParentId(manifest),
    needsRevalidation,
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
  needsRevalidation?: Set<string>,
): DataRouteObject[] {
  return (routesByParentId[parentId] || []).map((route) => {
    let hasErrorBoundary =
      future.v2_errorBoundary === true
        ? route.id === 'root' || route.hasErrorBoundary
        : route.id === 'root' || route.hasCatchBoundary || route.hasErrorBoundary

    let dataRoute: DataRouteObject = {
      caseSensitive: route.caseSensitive,
      element: <RemixRoute id={route.id} />,
      errorElement: hasErrorBoundary ? <RemixRouteError id={route.id} /> : undefined,
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
    )
    if (children.length > 0) dataRoute.children = children
    return dataRoute
  })
}

function createShouldRevalidate(
  route: EntryRoute,
  routeModules: RouteModules,
  needsRevalidation?: Set<string>,
): ShouldRevalidateFunction {
  let handledRevalidation = false
  return function (arg) {
    let module = routeModules[route.id]
    invariant(module, `Expected route module to be loaded for ${route.id}`)

    // When an HMR / HDR update happens we opt out of all user-defined
    // revalidation logic and the do as the dev server tells us the first
    // time router.revalidate() is called.
    if (needsRevalidation !== undefined && !handledRevalidation) {
      handledRevalidation = true
      return needsRevalidation.has(route.id)
    }

    if (module.shouldRevalidate) {
      return module.shouldRevalidate(arg as any)
    }

    return arg.defaultShouldRevalidate
  }
}
