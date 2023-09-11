import type {
  UNSAFE_FutureConfig as FutureConfig,
  UNSAFE_AssetsManifest as AssetsManifest,
  UNSAFE_RouteModules as RouteModules,
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
  DefaultFutureConfig,
  createAssetsManifest,
  createRouteManifestFromProjectContents,
  getRoutesAndModulesFromManifest,
} from '../../canvas/remix/remix-utils'
import type { CurriedUtopiaRequireFn, CurriedResolveFn } from '../../custom-code/code-file'
import type { ElementPath } from '../../../core/shared/project-file-types'
import type { ElementInstanceMetadataMap } from '../../../core/shared/element-template'
import type { AllElementProps, CanvasBase64Blobs } from './editor-state'
import { memoize } from '../../../core/shared/memoize'

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
  routeModulesCache: { current: RouteModules }
} = {
  mutableContext: { current: {} },
  topLevelComponentRendererComponents: { current: {} },
  routeModulesCache: { current: {} },
}

export function createRemixDerivedData(
  projectContents: ProjectContentTreeRoot,
  spyMetadata: ElementInstanceMetadataMap,
  allElementProps: AllElementProps,
  fileBlobs: CanvasBase64Blobs,
  displayNoneInstances: Array<ElementPath>,
  hiddenInstances: Array<ElementPath>,
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
      spyValues: { metadata: spyMetadata, allElementProps: allElementProps },
    },
  }

  const routesAndModulesFromManifestResult = getRoutesAndModulesFromManifest(
    routeManifest,
    DefaultFutureConfig,
    curriedRequireFn,
    curriedResolveFn,
    metadataCtx,
    projectContents,
    CreateRemixDerivedDataRefs.mutableContext,
    CreateRemixDerivedDataRefs.topLevelComponentRendererComponents,
    CreateRemixDerivedDataRefs.routeModulesCache.current,
    fileBlobs,
    displayNoneInstances,
    hiddenInstances,
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

export const patchedCreateRemixDerivedDataMemo = memoize(createRemixDerivedData, { maxSize: 1 })

export const unpatchedCreateRemixDerivedDataMemo = memoize(createRemixDerivedData, { maxSize: 1 })

export type RemixDerivedDataFactory = typeof createRemixDerivedData
