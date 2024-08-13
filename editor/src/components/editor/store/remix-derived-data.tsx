import type {
  UNSAFE_FutureConfig as FutureConfig,
  UNSAFE_AssetsManifest as AssetsManifest,
  UNSAFE_RouteModules as RouteModules,
} from '@remix-run/react'
import type { MutableUtopiaCtxRefData } from '../../canvas/ui-jsx-canvas-renderer/ui-jsx-canvas-contexts'
import type { MapLike } from 'typescript'
import type { ComponentRendererComponent } from '../../canvas/ui-jsx-canvas-renderer/component-renderer-component'
import type { DataRouteObject } from 'react-router'
import {
  getProjectFileByFilePath,
  isProjectContentDirectory,
  isProjectContentFile,
} from '../../assets'
import type { ProjectContentTreeRoot } from '../../assets'
import type {
  RouteIdsToModuleCreators,
  RouteModulesWithRelativePaths,
} from '../../canvas/remix/remix-utils'
import {
  DefaultFutureConfig,
  createAssetsManifest,
  createRouteManifestFromProjectContents,
  getRemixRootFile,
  getRoutesAndModulesFromManifest,
} from '../../canvas/remix/remix-utils'
import type { CurriedUtopiaRequireFn, CurriedResolveFn } from '../../custom-code/code-file'
import { memoize } from '../../../core/shared/memoize'
import { shallowEqual } from '../../../core/shared/equality-utils'
import { evaluator } from '../../../core/es-modules/evaluator/evaluator'
import type { EditorRemixConfig } from './editor-state'

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

export const CreateRemixDerivedDataRefsGLOBAL: {
  mutableContext: { current: MutableUtopiaCtxRefData }
  topLevelComponentRendererComponents: { current: MapLike<MapLike<ComponentRendererComponent>> }
  routeModulesCache: { current: RouteModules }
} = {
  mutableContext: { current: {} },
  topLevelComponentRendererComponents: { current: {} },
  routeModulesCache: { current: {} },
}
export const REMIX_CONFIG_JS_PATH = '/remix.config.js'

export function getRemixRootDir(projectContents: ProjectContentTreeRoot): string {
  const defaultRootDirName = 'app'
  const makeRootDirPath = (dir: string = defaultRootDirName) => `/${dir}`

  const remixConfigFile = getProjectFileByFilePath(projectContents, REMIX_CONFIG_JS_PATH)
  if (remixConfigFile == null || remixConfigFile.type !== 'TEXT_FILE') {
    return makeRootDirPath()
  }

  const m = evaluator(
    REMIX_CONFIG_JS_PATH,
    remixConfigFile.fileContents.code,
    {
      exports: {},
    },
    () => null,
  )

  const dir = m?.['exports']?.['appDirectory'] ?? defaultRootDirName
  return makeRootDirPath(dir)
}

// Important Note: When updating the params here, you must evaluate whether the change should
// have an effect on the memoization, and if so update paramsEqualityFn below
export function createRemixDerivedData(
  remixConfig: EditorRemixConfig,
  projectContents: ProjectContentTreeRoot,
  curriedRequireFn: CurriedUtopiaRequireFn,
  curriedResolveFn: CurriedResolveFn,
): RemixDerivedData | null {
  const rootDir = getRemixRootDir(projectContents)
  const rootJsFile = getRemixRootFile(rootDir, projectContents)
  if (rootJsFile == null) {
    return null
  }

  const routeManifest = createRouteManifestFromProjectContents(
    { rootFilePath: rootJsFile.path, rootDir: rootDir },
    projectContents,
  )
  if (routeManifest == null) {
    return null
  }

  const assetsManifest = createAssetsManifest(routeManifest)

  const routesAndModulesFromManifestResult = getRoutesAndModulesFromManifest(
    rootJsFile.file,
    routeManifest,
    DefaultFutureConfig,
    curriedRequireFn,
    curriedResolveFn,
    projectContents,
    CreateRemixDerivedDataRefsGLOBAL.routeModulesCache.current,
    remixConfig,
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
