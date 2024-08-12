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
import type { Either } from '../../../core/shared/either'
import { defaultEither, foldEither, left, mapEither, right } from '../../../core/shared/either'
import type { ArbitraryJSBlock } from '../../../core/shared/element-template'
import { resolveParamsAndRunJsCode } from '../../../core/shared/javascript-cache'
import { buildBaseExecutionScope } from '../../../components/canvas/ui-jsx-canvas-renderer/ui-jsx-canvas-execution-scope'
import type { ProjectFile } from '../../../core/shared/project-file-types'
import { isParseSuccess, isTextFile } from '../../../core/shared/project-file-types'
import type { Optic } from '../../../core/shared/optics/optics'
import { fromField, fromTypeGuard, notNull } from '../../../core/shared/optics/optic-creators'
import { toFirst } from '../../../core/shared/optics/optic-utilities'
import { applyBlockReturnFunctions } from '../../../core/shared/dom-utils'
import type { FancyError } from '../../../core/shared/code-exec-utils'
import { processErrorWithSourceMap } from '../../../core/shared/code-exec-utils'

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

const defaultRootDirName = 'app'

const remixConfigBlockOptic: Optic<ProjectFile | null, ArbitraryJSBlock> = notNull<ProjectFile>()
  .compose(fromTypeGuard(isTextFile))
  .compose(fromField('fileContents'))
  .compose(fromField('parsed'))
  .compose(fromTypeGuard(isParseSuccess))
  .compose(fromField('combinedTopLevelArbitraryBlock'))
  .compose(notNull())

export function getRemixRootDir(
  projectContents: ProjectContentTreeRoot,
  curriedRequireFn: CurriedUtopiaRequireFn,
): Either<FancyError, string> {
  try {
    const possibleConfigFile = getProjectFileByFilePath(projectContents, REMIX_CONFIG_JS_PATH)
    const possibleConfigBlock = toFirst(remixConfigBlockOptic, possibleConfigFile)
    return foldEither(
      () => {
        return right(`/${defaultRootDirName}`)
      },
      (codeBlock) => {
        const requireFn = curriedRequireFn(projectContents)
        const customRequire = (importOrigin: string, toImport: string) =>
          requireFn(importOrigin, toImport, true)
        const executionScope = buildBaseExecutionScope(REMIX_CONFIG_JS_PATH, customRequire, {})
        applyBlockReturnFunctions(executionScope)
        const requireResult: any = {}
        resolveParamsAndRunJsCode(REMIX_CONFIG_JS_PATH, codeBlock, requireResult, executionScope)
        const dir = executionScope?.['module']?.['exports']?.['appDirectory'] ?? defaultRootDirName
        return right(`/${dir}`)
      },
      possibleConfigBlock,
    )
  } catch (error) {
    if (error instanceof Error) {
      return left(processErrorWithSourceMap(null, REMIX_CONFIG_JS_PATH, error, true))
    } else {
      throw new Error(`Unexpected error type: ${error}`)
    }
  }
}

export function getDefaultedRemixRootDir(
  projectContents: ProjectContentTreeRoot,
  curriedRequireFn: CurriedUtopiaRequireFn,
): string {
  return defaultEither(`/${defaultRootDirName}`, getRemixRootDir(projectContents, curriedRequireFn))
}

// Important Note: When updating the params here, you must evaluate whether the change should
// have an effect on the memoization, and if so update paramsEqualityFn below
export function createRemixDerivedData(
  projectContents: ProjectContentTreeRoot,
  curriedRequireFn: CurriedUtopiaRequireFn,
  curriedResolveFn: CurriedResolveFn,
): Either<FancyError, RemixDerivedData | null> {
  const possibleRootDir = getRemixRootDir(projectContents, curriedRequireFn)
  return mapEither((rootDir) => {
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
  }, possibleRootDir)
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
