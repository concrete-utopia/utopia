import type {
  UNSAFE_FutureConfig as FutureConfig,
  UNSAFE_AssetsManifest as AssetsManifest,
} from '@remix-run/react'
import type { MutableUtopiaCtxRefData } from '../../canvas/ui-jsx-canvas-renderer/ui-jsx-canvas-contexts'
import type { MapLike } from 'typescript'
import type { ComponentRendererComponent } from '../../canvas/ui-jsx-canvas-renderer/ui-jsx-canvas-component-renderer'
import type { DataRouteObject } from 'react-router'
import type { ProjectContentTreeRoot } from '../../assets'
import type {
  RouteIdsToModuleCreators,
  RouteModulesWithRelativePaths,
} from '../../canvas/remix/remix-utils'
import {
  createAssetsManifest,
  createRouteManifestFromProjectContents,
  getRoutesAndModulesFromManifest,
} from '../../canvas/remix/remix-utils'
import type { CurriedUtopiaRequireFn, CurriedResolveFn } from '../../custom-code/code-file'
import type { ElementPath } from '../../../core/shared/project-file-types'

export interface RemixStaticRoutingTable {
  [rootElementUid: string]: string /* file path */
}

export interface RemixDerivedData {
  futureConfig: FutureConfig
  assetsManifest: AssetsManifest
  routeModuleCreators: RouteIdsToModuleCreators
  routeModulesToRelativePaths: RouteModulesWithRelativePaths
  routes: Array<DataRouteObject>
  routingTable: RemixStaticRoutingTable
}

const CreateRemixDerivedDataRefs: {
  mutableContext: { current: MutableUtopiaCtxRefData }
  topLevelComponentRendererComponents: { current: MapLike<MapLike<ComponentRendererComponent>> }
  resolvedFiles: { current: MapLike<Array<string>> }
  resolvedFileNames: { current: Array<string> }
} = {
  mutableContext: { current: {} },
  topLevelComponentRendererComponents: { current: {} },
  resolvedFiles: { current: {} },
  resolvedFileNames: { current: [] },
}

const defaultFutureConfig: FutureConfig = {
  v2_dev: true,
  unstable_postcss: false,
  unstable_tailwind: false,
  v2_errorBoundary: false,
  v2_headers: false,
  v2_meta: false,
  v2_normalizeFormMethod: false,
  v2_routeConvention: true,
}

// Problem: passing these via args breaks the memo
export const spyContainer = { current: {} }
export const propsContainer = { current: {} }
export const fileBlobsContainer = { current: {} }
export const displayNoneInstancesContainer = { current: [] as ElementPath[] }
export const hiddenInstancesContainer = { current: [] as ElementPath[] }

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

  const metadataCtx = {
    current: {
      spyValues: { metadata: spyContainer.current, allElementProps: propsContainer.current },
    },
  }

  const routesAndModulesFromManifestResult = getRoutesAndModulesFromManifest(
    routeManifest,
    defaultFutureConfig,
    curriedRequireFn,
    curriedResolveFn,
    metadataCtx,
    projectContents,
    CreateRemixDerivedDataRefs.mutableContext,
    CreateRemixDerivedDataRefs.topLevelComponentRendererComponents,
    fileBlobsContainer.current,
    displayNoneInstancesContainer.current,
    hiddenInstancesContainer.current,
  )

  if (routesAndModulesFromManifestResult == null) {
    return null
  }

  const { routeModuleCreators, routes, routeModulesToRelativePaths, routingTable } =
    routesAndModulesFromManifestResult

  return {
    futureConfig: defaultFutureConfig,
    routes: routes,
    assetsManifest: assetsManifest,
    routeModuleCreators: routeModuleCreators,
    routeModulesToRelativePaths: routeModulesToRelativePaths,
    routingTable: routingTable,
  }
}
