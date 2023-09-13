import type {
  UNSAFE_FutureConfig as FutureConfig,
  UNSAFE_EntryRoute as EntryRoute,
  UNSAFE_RouteManifest as RouteManifest,
  UNSAFE_AssetsManifest as AssetsManifest,
  UNSAFE_RouteModules as RouteModules,
} from '@remix-run/react'
import type { ProjectContentTreeRoot } from '../../assets'
import { getContentsTreeFromPath, getProjectFileByFilePath } from '../../assets'
import type { FileOps } from '../../../third-party/remix/flat-routes'
import { flatRoutes } from '../../../third-party/remix/flat-routes'
import type { ConfigRoute } from '../../../third-party/remix/routes'
import type { ActionFunction, DataRouteObject, LoaderFunction } from 'react-router'
import { createClientRoutes, groupRoutesByParentId } from '../../../third-party/remix/client-routes'
import type { CurriedResolveFn, CurriedUtopiaRequireFn } from '../../custom-code/code-file'
import type { MapLike } from 'typescript'
import type { UiJsxCanvasContextData } from '../ui-jsx-canvas'
import { attemptToResolveParsedComponents } from '../ui-jsx-canvas'
import type { ComponentRendererComponent } from '../ui-jsx-canvas-renderer/ui-jsx-canvas-component-renderer'
import type { MutableUtopiaCtxRefData } from '../ui-jsx-canvas-renderer/ui-jsx-canvas-contexts'
import type { ElementPath } from '../../../core/shared/project-file-types'
import type { ExecutionScope } from '../ui-jsx-canvas-renderer/ui-jsx-canvas-execution-scope'
import { createExecutionScope } from '../ui-jsx-canvas-renderer/ui-jsx-canvas-execution-scope'
import type { RemixRoutingTable } from '../../editor/store/remix-derived-data'
import { NO_OP } from '../../../core/shared/utils'
import * as EP from '../../../core/shared/element-path'
import {
  getDefaultExportNameAndUidFromFile,
  getDefaultExportedTopLevelElement,
  isRemixOutletElement,
} from '../../../core/model/project-file-utils'
import type { Either } from '../../../core/shared/either'
import { foldEither, forEachRight, left } from '../../../core/shared/either'
import type { CanvasBase64Blobs } from '../../editor/store/editor-state'
import { findPathToJSXElementChild } from '../../../core/model/element-template-utils'

const ROOT_DIR = '/src'

export type RouteManifestWithContents = RouteManifest<EntryRoute>

export const DefaultFutureConfig: FutureConfig = {
  v2_dev: true,
  unstable_postcss: false,
  unstable_tailwind: false,
  v2_errorBoundary: false,
  v2_headers: false,
  v2_meta: false,
  v2_normalizeFormMethod: false,
  v2_routeConvention: true,
}

// This is necessary to create a simple node.fs-like implementation for Utopia projectContents, which
// can be used by the Remix functions to parse the routes
export function projectContentsToFileOps(projectContents: ProjectContentTreeRoot): FileOps {
  return {
    existsSync: (file: string): boolean => getContentsTreeFromPath(projectContents, file) != null,
    readdirSync: (dir: string): Array<string> => {
      const projectDir = getContentsTreeFromPath(projectContents, dir)
      let entries =
        projectDir != null && projectDir.type === 'PROJECT_CONTENT_DIRECTORY'
          ? Object.values(projectDir.children).map((tree) => tree.fullPath)
          : []
      return entries
    },
    isDirectory: (file: string): boolean => {
      const projectFile = getContentsTreeFromPath(projectContents, file)
      return projectFile != null && projectFile.type === 'PROJECT_CONTENT_DIRECTORY'
    },
    isFile: (file: string): boolean => {
      const projectFile = getContentsTreeFromPath(projectContents, file)
      return projectFile != null && projectFile.type === 'PROJECT_CONTENT_FILE'
    },
  }
}

export function createRouteManifestFromProjectContents(
  projectContents: ProjectContentTreeRoot,
): RouteManifest<EntryRoute> | null {
  const routesFromRemix = (() => {
    try {
      return flatRoutes(ROOT_DIR, projectContentsToFileOps(projectContents))
    } catch (e) {
      return null
    }
  })()

  if (routesFromRemix == null) {
    return null
  }
  return patchRemixRoutes(routesFromRemix)
}

function patchRemixRoutes(routesFromRemix: RouteManifest<ConfigRoute> | null) {
  const routesFromRemixWithRoot: RouteManifest<ConfigRoute> = {
    ...routesFromRemix,
    root: { path: '', id: 'root', file: 'root.js', parentId: '' },
  }

  const resultRoutes = Object.values(routesFromRemixWithRoot).reduce((acc, route) => {
    // Maybe we should fill hasAction and hasLoader properly, but it is not used for anything
    acc[route.id] = {
      ...route,
      parentId: route.parentId ?? 'root',
      module: `${ROOT_DIR}/${route.file}`,
      hasAction: false,
      hasLoader: false,
      hasCatchBoundary: false,
      hasErrorBoundary: false,
    }
    return acc
  }, {} as RouteManifest<EntryRoute>)

  return resultRoutes
}

export function getRoutesFromRouteManifest(
  routeManifest: RouteManifest<EntryRoute>,
  futureConfig: FutureConfig,
): DataRouteObject[] {
  const routesByParentId = groupRoutesByParentId(routeManifest)
  try {
    return createClientRoutes(routeManifest, {}, futureConfig, '', routesByParentId)
  } catch (e) {
    return []
  }
}

export function createAssetsManifest(routes: RouteManifest<EntryRoute>): AssetsManifest {
  return {
    entry: { imports: [], module: '' },
    url: '/',
    version: '1',
    routes: routes,
  }
}

export interface RouteModuleCreator {
  filePath: string
  executionScopeCreator: (projectContents: ProjectContentTreeRoot) => ExecutionScope
}

export interface RouteIdsToModuleCreators {
  [routeId: string]: RouteModuleCreator
}

export interface RouteModulesWithRelativePaths {
  [routeId: string]: {
    relativePath: ElementPath
    filePath: string
  }
}

export interface GetRoutesAndModulesFromManifestResult {
  routeModuleCreators: RouteIdsToModuleCreators
  routes: Array<DataRouteObject>
  routeModulesToRelativePaths: RouteModulesWithRelativePaths
  routingTable: RemixRoutingTable
}

function addLoaderAndActionToRoutes(
  routes: DataRouteObject[],
  routeId: string,
  loader: LoaderFunction | undefined,
  action: ActionFunction | undefined,
) {
  routes.forEach((route) => {
    if (route.id === routeId) {
      route.action = action
      route.loader = loader
    } else {
      addLoaderAndActionToRoutes(route.children ?? [], routeId, loader, action)
    }
  })
}

function getRouteModulesWithPaths(
  projectContents: ProjectContentTreeRoot,
  manifest: RouteManifest<EntryRoute>,
  route: DataRouteObject,
  pathSoFar: ElementPath,
): RouteModulesWithRelativePaths {
  const filePathForRouteObject = manifest[route.id]?.module ?? null
  if (filePathForRouteObject == null) {
    return {}
  }
  const file = getProjectFileByFilePath(projectContents, filePathForRouteObject)
  if (file == null || file.type !== 'TEXT_FILE') {
    return {}
  }

  const topLevelElement = getDefaultExportedTopLevelElement(file)
  if (topLevelElement == null) {
    // for example because the file in question is not syntactially correct
    return {}
  }

  const pathPartToOutlet = findPathToJSXElementChild(
    (e) => isRemixOutletElement(e, filePathForRouteObject, projectContents),
    topLevelElement,
  )

  const isLeafModule = pathPartToOutlet == null
  let routeModulesWithBasePaths: RouteModulesWithRelativePaths = {
    [route.id]: {
      relativePath: pathSoFar,
      filePath: filePathForRouteObject,
    },
  }

  if (isLeafModule) {
    return routeModulesWithBasePaths
  }

  const children = route.children ?? []
  const pathForChildren = EP.appendNewElementPath(pathSoFar, pathPartToOutlet)

  for (const child of children) {
    const paths = getRouteModulesWithPaths(projectContents, manifest, child, pathForChildren)
    for (const [routeId, value] of Object.entries(paths)) {
      routeModulesWithBasePaths[routeId] = value
    }
  }

  return routeModulesWithBasePaths
}

function getRemixExportsOfModule(
  filename: string,
  curriedRequireFn: CurriedUtopiaRequireFn,
  curriedResolveFn: CurriedResolveFn,
  metadataContext: UiJsxCanvasContextData,
  projectContents: ProjectContentTreeRoot,
  mutableContextRef: React.MutableRefObject<MutableUtopiaCtxRefData>,
  topLevelComponentRendererComponents: React.MutableRefObject<
    MapLike<MapLike<ComponentRendererComponent>>
  >,
  fileBlobs: CanvasBase64Blobs,
  hiddenInstances: Array<ElementPath>,
  displayNoneInstances: Array<ElementPath>,
): {
  executionScopeCreator: (innerProjectContents: ProjectContentTreeRoot) => ExecutionScope
  loader: LoaderFunction | undefined
  action: ActionFunction | undefined
  rootComponentUid: string
} {
  const executionScopeCreator = (innerProjectContents: ProjectContentTreeRoot) => {
    let resolvedFiles: MapLike<Array<string>> = {}
    let resolvedFileNames: Array<string> = ['/src/root.js']

    const requireFn = curriedRequireFn(innerProjectContents)
    const resolve = curriedResolveFn(innerProjectContents)

    const customRequire = (importOrigin: string, toImport: string) => {
      if (resolvedFiles[importOrigin] == null) {
        resolvedFiles[importOrigin] = []
      }
      let resolvedFromThisOrigin = resolvedFiles[importOrigin]

      const alreadyResolved = resolvedFromThisOrigin.includes(toImport) // We're inside a cyclic dependency, so trigger the below fallback     const filePathResolveResult = alreadyResolved
        ? left<string, string>('Already resolved')
        : resolve(importOrigin, toImport)

      forEachRight(alreadyResolved, (filepath) => resolvedFileNames.push(filepath))

      const resolvedParseSuccess: Either<string, MapLike<any>> = attemptToResolveParsedComponents(
        resolvedFromThisOrigin,
        toImport,
        innerProjectContents,
        customRequire,
        mutableContextRef,
        topLevelComponentRendererComponents,
        '/src/root.js',
        {},
        [],
        [],
        metadataContext,
        NO_OP,
        false,
        alreadyResolved,
        null,
      )
      return foldEither(
        () => {
          // We did not find a ParseSuccess, fallback to standard require Fn
          return requireFn(importOrigin, toImport, false)
        },
        (scope) => {
          // Return an artificial exports object that contains our ComponentRendererComponents
          return scope
        },
        resolvedParseSuccess,
      )
    }
    return createExecutionScope(
      filename,
      customRequire,
      mutableContextRef,
      topLevelComponentRendererComponents,
      innerProjectContents,
      filename,
      fileBlobs,
      hiddenInstances,
      displayNoneInstances,
      metadataContext,
      NO_OP,
      false,
      null,
    )
  }

  const executionScope = executionScopeCreator(projectContents)

  const nameAndUid = getDefaultExportNameAndUidFromFile(projectContents, filename)

  return {
    executionScopeCreator: executionScopeCreator,
    rootComponentUid: nameAndUid?.uid ?? 'NO-ROOT',
    // FIXME the executionScope should be created at the point where we use the loader and action like we do for the module's default export component
    loader: executionScope.scope['loader'] as LoaderFunction | undefined,
    action: executionScope.scope['action'] as ActionFunction | undefined,
  }
}
export function getRoutesAndModulesFromManifest(
  routeManifest: RouteManifestWithContents,
  futureConfig: FutureConfig,
  curriedRequireFn: CurriedUtopiaRequireFn,
  curriedResolveFn: CurriedResolveFn,
  metadataContext: UiJsxCanvasContextData,
  projectContents: ProjectContentTreeRoot,
  mutableContextRef: React.MutableRefObject<MutableUtopiaCtxRefData>,
  topLevelComponentRendererComponents: React.MutableRefObject<
    MapLike<MapLike<ComponentRendererComponent>>
  >,
  routeModulesCache: RouteModules,
  fileBlobs: CanvasBase64Blobs,
  hiddenInstances: Array<ElementPath>,
  displayNoneInstances: Array<ElementPath>,
): GetRoutesAndModulesFromManifestResult | null {
  const routeModuleCreators: RouteIdsToModuleCreators = {}
  const routingTable: RemixRoutingTable = {}

  const rootJsFile = getProjectFileByFilePath(projectContents, `${ROOT_DIR}/root.js`)
  if (rootJsFile == null || rootJsFile.type !== 'TEXT_FILE') {
    return null
  }

  const rootJSRootElement = getDefaultExportedTopLevelElement(rootJsFile)
  if (rootJSRootElement == null) {
    return null
  }

  const routesByParentId = groupRoutesByParentId(routeManifest)
  const routes: DataRouteObject[] = createClientRoutes(
    routeManifest,
    routeModulesCache,
    futureConfig,
    '',
    routesByParentId,
  )

  if (routes.length !== 1 && routes[0].id !== 'root') {
    throw new Error('The root route module must be `root`')
  }

  const routeModulesToRelativePaths = getRouteModulesWithPaths(
    projectContents,
    routeManifest,
    routes[0],
    EP.emptyElementPath,
  )

  Object.values(routeManifest).forEach((route) => {
    const { executionScopeCreator, loader, action, rootComponentUid } = getRemixExportsOfModule(
      route.module,
      curriedRequireFn,
      curriedResolveFn,
      metadataContext,
      projectContents,
      mutableContextRef,
      topLevelComponentRendererComponents,
      fileBlobs,
      hiddenInstances,
      displayNoneInstances,
    )

    addLoaderAndActionToRoutes(routes, route.id, loader, action)
    routeModuleCreators[route.id] = {
      filePath: route.module,
      executionScopeCreator: executionScopeCreator,
    }

    routingTable[rootComponentUid] = route.module
  })

  return {
    routeModuleCreators,
    routes,
    routeModulesToRelativePaths,
    routingTable,
  }
}
