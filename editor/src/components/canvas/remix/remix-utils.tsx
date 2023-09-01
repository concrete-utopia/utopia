import React from 'react'
import type { ActionFunction, LoaderFunction } from 'react-router'
import { Outlet } from 'react-router'
import type {
  UNSAFE_AssetsManifest as AssetsManifest,
  UNSAFE_RemixContextObject as RemixContextObject,
} from '@remix-run/react'

import type { Either } from '../../../core/shared/either'
import { foldEither, forEachRight, left, right } from '../../../core/shared/either'
import { UNSAFE_RemixContext as RemixContext } from '@remix-run/react'
import { getProjectFileByFilePath } from '../../assets'
import type {
  JSXElementChild,
  TopLevelElement,
  UtopiaJSXComponent,
} from '../../../core/shared/element-template'
import {
  getJSXElementNameAsString,
  isUtopiaJSXComponent,
  jsxElementName,
  jsxElementNameEquals,
} from '../../../core/shared/element-template'
import type {
  ElementPath,
  ElementPathPart,
  ExportDefaultFunctionOrClass,
  TextFile,
} from '../../../core/shared/project-file-types'
import type { RouteModule, RouteModules } from '@remix-run/react/dist/routeModules'
import { UTOPIA_PATH_KEY } from '../../../core/model/utopia-constants'
import type { ExecutionScope } from '../ui-jsx-canvas-renderer/ui-jsx-canvas-execution-scope'
import { createExecutionScope } from '../ui-jsx-canvas-renderer/ui-jsx-canvas-execution-scope'
import { attemptToResolveParsedComponents } from '../ui-jsx-canvas'
import type { UiJsxCanvasContextData } from '../ui-jsx-canvas'
import type { UtopiaRemixRootComponentProps } from './utopia-remix-root-component'
import type { MutableUtopiaCtxRefData } from '../ui-jsx-canvas-renderer/ui-jsx-canvas-contexts'
import { NO_OP } from '../../../core/shared/utils'
import * as EP from '../../../core/shared/element-path'
import type { ComponentRendererComponent } from '../ui-jsx-canvas-renderer/ui-jsx-canvas-component-renderer'
import type { MapLike } from 'typescript'
import { pathPartsFromJSXElementChild } from '../../../core/model/element-template-utils'
import type { RemixStaticRoutingTable } from '../../editor/store/editor-state'
import type { CurriedResolveFn, CurriedUtopiaRequireFn } from '../../custom-code/code-file'

import type {
  UNSAFE_FutureConfig as FutureConfig,
  UNSAFE_EntryRoute as EntryRoute,
  UNSAFE_RouteManifest as RouteManifest,
} from '@remix-run/react'
import type { ProjectContentTreeRoot } from '../../assets'
import { getContentsTreeFromPath } from '../../assets'
import type { FileOps } from '../../../third-party/remix/flat-routes'
import { flatRoutes } from '../../../third-party/remix/flat-routes'
import type { ConfigRoute } from '../../../third-party/remix/routes'
import type { DataRouteObject } from 'react-router'
import { createClientRoutes, groupRoutesByParentId } from '../../../third-party/remix/client-routes'

const ROOT_DIR = '/src'

export const RemixRoutingTableGLOBAL: { current: RemixStaticRoutingTable | null } = {
  current: null,
}

export function invariant<T>(value: T | null | undefined, message: string): asserts value is T {
  if (value == null) {
    throw new Error(message)
  }
}

// Not exported from Remix
// FIXME: either find where this component is export from remix, submit PR to export it, or leave it as is
function useRemixContext(): RemixContextObject {
  let context = React.useContext(RemixContext)
  invariant(context, 'You must render this element inside a <Remix> element')
  return context
}

interface UtopiaRemixRouteProps {
  id: string
}

// Not exported from Remix
// FIXME: either find where this component is export from remix, submit PR to export it, or leave it as is
export const UtopiaRemixRoute = React.memo(({ id }: UtopiaRemixRouteProps) => {
  let { routeModules, future } = useRemixContext()
  invariant(
    routeModules,
    "Cannot initialize 'routeModules'. This normally occurs when you have server code in your client modules.\n" +
      'Check this link for more details:\nhttps://remix.run/pages/gotchas#server-code-in-client-bundles',
  )

  let { default: Component, ErrorBoundary, CatchBoundary } = routeModules[id]

  // Default Component to Outlet if we expose boundary UI components
  if (
    Component == null &&
    (ErrorBoundary != null || (!future.v2_errorBoundary && CatchBoundary != null))
  ) {
    Component = Outlet
  }

  invariant(
    Component,
    `Route "${id}" has no component! Please go add a \`default\` export in the route module file.\n` +
      'If you were trying to navigate or submit to a resource route, use `<a>` instead of `<Link>` or `<Form reloadDocument>`.',
  )

  return <Component />
})

export function createAssetsManifest(routes: RouteManifest<EntryRoute>): AssetsManifest {
  return {
    entry: { imports: [], module: 'TODO' },
    url: '/',
    version: '1',
    routes: routes,
  }
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

export const PathPropHOC = (Wrapped: any, path: string) => (props: any) => {
  const propsWithPath = {
    [UTOPIA_PATH_KEY]: path,
    ...props,
  }
  return <Wrapped {...propsWithPath} />
}

export interface RouteModuleWithFilePath extends RouteModule {
  filePath: string
}

export interface RouteModulesWithFilePaths {
  [routeId: string]: RouteModuleWithFilePath
}

export interface RouteModuleCreator {
  filePath: string
  executionScopeCreator: (projectContents: ProjectContentTreeRoot) => ExecutionScope
}

export interface RouteIdsToModuleCreators {
  [routeId: string]: RouteModuleCreator
}

export interface GetRoutesAndModulesFromManifestResult {
  routeModuleCreators: RouteIdsToModuleCreators
  routes: Array<DataRouteObject>
  routeModulesToRelativePaths: RouteModulesWithRelativePaths
  routingTable: RemixStaticRoutingTable
}

export function getRoutesAndModulesFromManifest(
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
): GetRoutesAndModulesFromManifestResult | null {
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
    )

    addLoaderAndActionToRoute(routes, route.id, loader, action)
    routeModuleCreators[route.id] = {
      filePath: route.module,
      executionScopeCreator: executionScopeCreator,
    }

    routingTable[rootComponentUid] = route.module
  })

  RemixRoutingTableGLOBAL.current = routingTable

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

// TODO: needs better `Outlet` detection
export function findPathToOutlet(element: JSXElementChild): ElementPathPart | null {
  if (
    element.type === 'JSX_ELEMENT' &&
    jsxElementNameEquals(jsxElementName('Outlet', []), element.name)
  ) {
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

export interface RouteModulesWithRelativePaths {
  [routeId: string]: {
    relativePath: ElementPath
    filePath: string
  }
}

function getRouteModulesWithPaths(
  projectContents: ProjectContentTreeRoot,
  routeManifest: RouteManifest<EntryRoute>,
  route: DataRouteObject,
  pathSoFar: ElementPath,
): RouteModulesWithRelativePaths {
  const filePathForRoute = routeManifest[route.id]?.module
  if (filePathForRoute == null) {
    return {}
  }
  const file = getProjectFileByFilePath(projectContents, filePathForRoute)
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
    [route.id]: {
      relativePath: pathSoFar,
      filePath: filePathForRoute,
    },
  }

  if (isLeafModule) {
    return routeModulesWithBasePaths
  }

  const children = route.children ?? []
  const pathForChildren = EP.appendNewElementPath(pathSoFar, pathPartToOutlet)

  for (const child of children) {
    const paths = getRouteModulesWithPaths(projectContents, routeManifest, child, pathForChildren)
    for (const [filePath, path] of Object.entries(paths)) {
      routeModulesWithBasePaths[filePath] = path
    }
  }

  return routeModulesWithBasePaths
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
