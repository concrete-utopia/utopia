/* eslint-disable */
import React from 'react'
import { FutureConfig } from '@remix-run/react/dist/entry'
import { RouteModules } from '@remix-run/react/dist/routeModules'
import { EntryRoute, RouteManifest } from '@remix-run/react/dist/routes'
import { DataRouteObject, ShouldRevalidateFunction } from 'react-router'
import { UtopiaRemixRoute, invariant } from '../remix-utils'
import { RemixRouteError } from '@remix-run/react/dist/components'

export interface EntryRouteWithFileMeta extends EntryRoute {
  filePath: string
}

export type DataRouteWithFilePath = DataRouteObject & {
  children?: DataRouteWithFilePath[]
  filePath: string
}

// Create a map of routes by parentId to use recursively instead of
// repeatedly filtering the manifest.
export function groupRoutesByParentId(manifest: RouteManifest<EntryRouteWithFileMeta>) {
  let routes: Record<string, Omit<EntryRouteWithFileMeta, 'children'>[]> = {}

  Object.values(manifest).forEach((route) => {
    let parentId = route.parentId || ''
    if (!routes[parentId]) {
      routes[parentId] = []
    }
    routes[parentId].push(route)
  })

  return routes
}

export function createClientRoutes(
  manifest: RouteManifest<EntryRouteWithFileMeta>,
  routeModulesCache: RouteModules,
  future: FutureConfig,
  parentId: string = '',
  routesByParentId: Record<
    string,
    Omit<EntryRouteWithFileMeta, 'children'>[]
  > = groupRoutesByParentId(manifest),
  needsRevalidation?: Set<string>,
): DataRouteWithFilePath[] {
  return (routesByParentId[parentId] || []).map((route) => {
    let hasErrorBoundary =
      future.v2_errorBoundary === true
        ? route.id === 'root' || route.hasErrorBoundary
        : route.id === 'root' || route.hasCatchBoundary || route.hasErrorBoundary

    let dataRoute: DataRouteWithFilePath = {
      caseSensitive: route.caseSensitive,
      element: <UtopiaRemixRoute id={route.id} />,
      errorElement: hasErrorBoundary ? <RemixRouteError id={route.id} /> : undefined,
      id: route.id,
      index: route.index,
      path: route.path,
      // handle gets added in via useMatches since we aren't guaranteed to
      // have the route module available here
      handle: undefined,
      shouldRevalidate: createShouldRevalidate(route, routeModulesCache, needsRevalidation),
      filePath: route.filePath,
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
      return module.shouldRevalidate(arg)
    }

    return arg.defaultShouldRevalidate
  }
}
