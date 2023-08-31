import type {
  UNSAFE_FutureConfig as FutureConfig,
  UNSAFE_EntryRoute as EntryRoute,
  UNSAFE_RouteManifest as RouteManifest,
} from '@remix-run/react'
import type { ProjectContentTreeRoot } from '../../assets'
import { getContentsTreeFromPath, getProjectFileByFilePath } from '../../assets'
import type { FileOps } from '../../../third-party/remix/flat-routes'
import { flatRoutes } from '../../../third-party/remix/flat-routes'
import type { ConfigRoute } from '../../../third-party/remix/routes'
import type { ActionFunction, DataRouteObject, LoaderFunction } from 'react-router'
import { createClientRoutes, groupRoutesByParentId } from '../../../third-party/remix/client-routes'
import type {
  ElementPath,
  ElementPathPart,
  ExportDefaultFunctionOrClass,
  TextFile,
} from '../../../core/shared/project-file-types'
import {
  isJSXElement,
  jsxElementNameEquals,
  jsxElementName,
} from '../../../core/shared/element-template'
import type { JSXElementChild, UtopiaJSXComponent } from '../../../core/shared/element-template'
import * as EP from '../../../core/shared/element-path'
import type { CurriedResolveFn, CurriedUtopiaRequireFn } from '../../custom-code/code-file'
import type { UiJsxCanvasContextData } from '../ui-jsx-canvas'
import { attemptToResolveParsedComponents } from '../ui-jsx-canvas'
import type { MutableUtopiaCtxRefData } from '../ui-jsx-canvas-renderer/ui-jsx-canvas-contexts'
import type { ComponentRendererComponent } from '../ui-jsx-canvas-renderer/ui-jsx-canvas-component-renderer'
import type { ExecutionScope } from '../ui-jsx-canvas-renderer/ui-jsx-canvas-execution-scope'
import { createExecutionScope } from '../ui-jsx-canvas-renderer/ui-jsx-canvas-execution-scope'

import { NO_OP } from '../../../core/shared/utils'
import type { Either } from '../../../core/shared/either'
import { foldEither, forEachRight, left } from '../../../core/shared/either'
import type { MapLike } from 'typescript'
import type { RemixStaticRoutingTable } from '../../editor/store/editor-state'

const ROOT_DIR = '/src'

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
          ? Object.values(projectDir.children).map((tree) => `/${tree.fullPath}`)
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

export interface RouteModulesWithRelativePaths {
  [filePath: string]: {
    relativePath: ElementPath
    isLeafModule: boolean
  }
}

export interface RouteModuleCreator {
  filePath: string
  executionScopeCreator: (projectContents: ProjectContentTreeRoot) => ExecutionScope
}

export interface RouteIdsToModuleCreators {
  [routeId: string]: RouteModuleCreator
}

export interface GetRoutesAndRouteModulesFromManifestResult {
  routeModuleCreators: RouteIdsToModuleCreators
  routes: Array<DataRouteObject>
  routeModulesToRelativePaths: RouteModulesWithRelativePaths
  routingTable: RemixStaticRoutingTable
}

export function getRoutesAndRouteModulesFromManifest(
  routeManifest: RouteManifest<EntryRoute>,
  futureConfig: FutureConfig,
  curriedRequireFn: CurriedUtopiaRequireFn,
  curriedResolveFn: CurriedResolveFn,
  metadataContext: UiJsxCanvasContextData,
  projectContents: ProjectContentTreeRoot,
  mutableContextRef: React.MutableRefObject<MutableUtopiaCtxRefData>,
  topLevelComponentRendererComponents: React.MutableRefObject<
    MapLike<MapLike<ComponentRendererComponent>>
  >,
): GetRoutesAndRouteModulesFromManifestResult | null {
  const routeModuleCreators: RouteIdsToModuleCreators = {}
  const routingTable: RemixStaticRoutingTable = {}

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
    {},
    futureConfig,
    '',
    routesByParentId,
  )

  if (routes.length !== 1 && routes[0].id !== 'root') {
    throw new Error('The root route module must be `root`')
  }

  const routeModulesToRelativePaths = getRouteModulesWithPaths(
    projectContents,
    routes[0],
    routeManifest,
    EP.emptyElementPath,
  )

  Object.values(routeManifest).forEach((route) => {
    const filePath = routeManifest[route.id]?.module
    if (filePath == null) {
      return
    }
    const { executionScopeCreator, loader, action, rootComponentUid } = getRemixExportsOfModule(
      filePath,
      curriedRequireFn,
      curriedResolveFn,
      metadataContext,
      projectContents,
      mutableContextRef,
      topLevelComponentRendererComponents,
    )

    addLoaderAndActionToRoute(routes, route.id, loader, action)
    routeModuleCreators[route.id] = {
      filePath: filePath,
      executionScopeCreator: executionScopeCreator,
    }

    routingTable[rootComponentUid] = filePath
  })

  return {
    routeModuleCreators,
    routes,
    routeModulesToRelativePaths,
    routingTable,
  }
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
): {
  executionScopeCreator: (innerProjectContents: ProjectContentTreeRoot) => ExecutionScope
  loader: LoaderFunction | undefined
  action: ActionFunction | undefined
  rootComponentUid: string
} {
  const executionScopeCreator = (innerProjectContents: ProjectContentTreeRoot) => {
    let resolvedFiles: MapLike<Array<string>> = {}
    let resolvedFileNames: Array<string> = [`${ROOT_DIR}/root.js`]

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
      {},
      [],
      [],
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
    loader: executionScope.scope['loader'] as LoaderFunction | undefined,
    action: executionScope.scope['action'] as ActionFunction | undefined,
    rootComponentUid: nameAndUid?.uid ?? 'NO-ROOT',
  }
}

function getRouteModulesWithPaths(
  projectContents: ProjectContentTreeRoot,
  route: DataRouteObject,
  manifest: RouteManifest<EntryRoute>,
  pathSoFar: ElementPath,
): RouteModulesWithRelativePaths {
  const manifestItem = manifest[route.id]
  if (manifestItem == null) {
    return {}
  }
  const filePath = manifestItem.module
  const file = getProjectFileByFilePath(projectContents, filePath)
  if (file == null || file.type !== 'TEXT_FILE') {
    return {}
  }

  const topLevelElement = getDefaultExportedTopLevelElement(file)
  if (topLevelElement == null) {
    // for example because the file in question is not syntactially correct
    return {}
  }

  const pathPartToOutlet = findPathToOutlet(topLevelElement)
  const isLeafModule = pathPartToOutlet == null
  let routeModulesWithBasePaths: RouteModulesWithRelativePaths = {
    [filePath]: {
      relativePath: pathSoFar,
      isLeafModule: isLeafModule,
    },
  }

  if (isLeafModule) {
    return routeModulesWithBasePaths
  }

  const children = route.children ?? []
  const pathForChildren = EP.appendNewElementPath(pathSoFar, pathPartToOutlet)

  for (const child of children) {
    const paths = getRouteModulesWithPaths(projectContents, child, manifest, pathForChildren)
    for (const [childPath, path] of Object.entries(paths)) {
      routeModulesWithBasePaths[childPath] = path
    }
  }

  return routeModulesWithBasePaths
}

export function getDefaultExportedTopLevelElement(file: TextFile): JSXElementChild | null {
  if (file.fileContents.parsed.type !== 'PARSE_SUCCESS') {
    return null
  }

  const defaultExportName =
    file.fileContents.parsed.exportsDetail.find(
      (e): e is ExportDefaultFunctionOrClass => e.type === 'EXPORT_DEFAULT_FUNCTION_OR_CLASS',
    )?.name ?? null

  if (defaultExportName == null) {
    return null
  }

  return (
    file.fileContents.parsed.topLevelElements.find(
      (t): t is UtopiaJSXComponent =>
        t.type === 'UTOPIA_JSX_COMPONENT' && t.name === defaultExportName,
    )?.rootElement ?? null
  )
}

// TODO: needs better `Outlet` detection
export function findPathToOutlet(element: JSXElementChild): ElementPathPart | null {
  if (isJSXElement(element) && jsxElementNameEquals(jsxElementName('Outlet', []), element.name)) {
    return [element.uid]
  }

  if (element.type === 'JSX_FRAGMENT' || element.type === 'JSX_ELEMENT') {
    for (const child of element.children) {
      const path = findPathToOutlet(child)
      if (path != null) {
        return [element.uid, ...path]
      }
    }
  }

  // TODO: handle missing cases

  return null
}

export function getDefaultExportNameAndUidFromFile(
  projectContents: ProjectContentTreeRoot,
  filePath: string,
): { name: string; uid: string } | null {
  const file = getProjectFileByFilePath(projectContents, filePath)
  if (file == null || file.type != 'TEXT_FILE' || file.lastParseSuccess == null) {
    return null
  }

  const defaultExportName =
    file.lastParseSuccess.exportsDetail.find(
      (e): e is ExportDefaultFunctionOrClass => e.type === 'EXPORT_DEFAULT_FUNCTION_OR_CLASS',
    )?.name ?? null

  if (defaultExportName == null) {
    return null
  }

  const elementUid = file.lastParseSuccess.topLevelElements.find(
    (t): t is UtopiaJSXComponent =>
      t.type === 'UTOPIA_JSX_COMPONENT' && t.name === defaultExportName,
  )?.rootElement.uid
  if (elementUid == null) {
    return null
  }

  return { name: defaultExportName, uid: elementUid }
}

function addLoaderAndActionToRoute(
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
      addLoaderAndActionToRoute(route.children ?? [], routeId, loader, action)
    }
  })
}
