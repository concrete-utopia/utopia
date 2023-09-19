import type {
  UNSAFE_FutureConfig as FutureConfig,
  UNSAFE_AssetsManifest as AssetsManifest,
  UNSAFE_RouteModules as RouteModules,
} from '@remix-run/react'
import type { MutableUtopiaCtxRefData } from '../../canvas/ui-jsx-canvas-renderer/ui-jsx-canvas-contexts'
import type { MapLike } from 'typescript'
import type { ComponentRendererComponent } from '../../canvas/ui-jsx-canvas-renderer/ui-jsx-canvas-component-renderer'
import type { DataRouteObject } from 'react-router'
import { isProjectContentDirectory, isProjectContentFile } from '../../assets'
import type { ProjectContentTreeRoot } from '../../assets'
import type {
  RouteIdsToModuleCreators,
  RouteModulesWithRelativePaths,
} from '../../canvas/remix/remix-utils'
import {
  DefaultFutureConfig,
  createAssetsManifest,
  createRouteManifestFromProjectContents,
  getRoutesAndModulesFromManifest,
} from '../../canvas/remix/remix-utils'
import type { CurriedUtopiaRequireFn, CurriedResolveFn } from '../../custom-code/code-file'
import { memoize } from '../../../core/shared/memoize'
import { shallowEqual } from '../../../core/shared/equality-utils'

export interface RemixRoutingTable {
  [rootElementUid: string]: string /* file path */
}

export interface RemixDerivedData {
  futureConfig: FutureConfig
  assetsManifest: AssetsManifest
  routeModuleCreators: RouteIdsToModuleCreators
  routeModulesToRelativePaths: RouteModulesWithRelativePaths
  routes: Array<DataRouteObject>
  routingTable: RemixRoutingTable
}

const CreateRemixDerivedDataRefs: {
  mutableContext: { current: MutableUtopiaCtxRefData }
  topLevelComponentRendererComponents: { current: MapLike<MapLike<ComponentRendererComponent>> }
  routeModulesCache: { current: RouteModules }
} = {
  mutableContext: { current: {} },
  topLevelComponentRendererComponents: { current: {} },
  routeModulesCache: { current: {} },
}

// Important Note: When updating the params here, you must evaluate whether the change should
// have an effect on the memoization, and if so update paramsEqualityFn below
export function createRemixDerivedData(
  projectContents: ProjectContentTreeRoot,
  curriedRequireFn: CurriedUtopiaRequireFn,
  curriedResolveFn: CurriedResolveFn,
): RemixDerivedData | null {
  const routeManifest = createRouteManifestFromProjectContents(projectContents)
  if (routeManifest == null) {
    return null
  }

  const assetsManifest = createAssetsManifest(routeManifest)

  const routesAndModulesFromManifestResult = getRoutesAndModulesFromManifest(
    routeManifest,
    DefaultFutureConfig,
    curriedRequireFn,
    curriedResolveFn,
    projectContents,
    CreateRemixDerivedDataRefs.routeModulesCache.current,
  )

  if (routesAndModulesFromManifestResult == null) {
    return null
  }

  const { routeModuleCreators, routes, routeModulesToRelativePaths, routingTable } =
    routesAndModulesFromManifestResult

  return {
    futureConfig: DefaultFutureConfig,
    routes: routes,
    assetsManifest: assetsManifest,
    routeModuleCreators: routeModuleCreators,
    routeModulesToRelativePaths: routeModulesToRelativePaths,
    routingTable: routingTable,
  }
}

function isProjectContentTreeRoot(v: unknown): v is ProjectContentTreeRoot {
  if (v != null && typeof v === 'object' && !Array.isArray(v)) {
    const firstValue = Object.values(v)[0]
    return isProjectContentDirectory(firstValue) || isProjectContentFile(firstValue)
  }

  return false
}

function paramsEqualityFn(l: unknown, r: unknown): boolean {
  if (isProjectContentTreeRoot(l) && isProjectContentTreeRoot(r)) {
    return shallowEqual(l, r)
  }

  return l === r
}

export const patchedCreateRemixDerivedDataMemo = memoize(createRemixDerivedData, {
  maxSize: 1,
  matchesArg: paramsEqualityFn,
})

export const unpatchedCreateRemixDerivedDataMemo = memoize(createRemixDerivedData, {
  maxSize: 1,
  matchesArg: paramsEqualityFn,
})

export type RemixDerivedDataFactory = typeof createRemixDerivedData
