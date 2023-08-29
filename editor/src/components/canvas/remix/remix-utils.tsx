import React from 'react'
import type { ActionFunction, DataRouteObject, LoaderFunction } from 'react-router'
import { Outlet } from 'react-router'
import type {
  UNSAFE_RouteManifest as RouteManifest,
  UNSAFE_EntryRoute as EntryRoute,
  UNSAFE_FutureConfig as FutureConfig,
  UNSAFE_AssetsManifest as AssetsManifest,
  UNSAFE_RemixContextObject as RemixContextObject,
} from '@remix-run/react'

import type { Either } from '../../../core/shared/either'
import { foldEither, forEachRight, left, right } from '../../../core/shared/either'
import { UNSAFE_RemixContext as RemixContext } from '@remix-run/react'
import type { ProjectContentFile, ProjectContentTreeRoot, ProjectContentsTree } from '../../assets'
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
import type { MutableUtopiaCtxRefData } from '../ui-jsx-canvas-renderer/ui-jsx-canvas-contexts'
import { NO_OP } from '../../../core/shared/utils'
import * as EP from '../../../core/shared/element-path'
import type { ComponentRendererComponent } from '../ui-jsx-canvas-renderer/ui-jsx-canvas-component-renderer'
import type { MapLike } from 'typescript'
import { flatRoutes } from './from-remix/flat-routes'
import type { DataRouteWithFilePath, EntryRouteWithFileMeta } from './from-remix/client-routes'
import { createClientRoutes, groupRoutesByParentId } from './from-remix/client-routes'
import type { RemixStaticRoutingTable } from '../../editor/store/editor-state'
import type { CurriedResolveFn, CurriedUtopiaRequireFn } from '../../custom-code/code-file'

const ROOT_DIR = '/src'

export const RemixRoutingTableGLOBAL: { current: RemixStaticRoutingTable | null } = {
  current: null,
}

export type RouteManifestWithContents = RouteManifest<EntryRouteWithFileMeta>

export function getTopLevelElement(topLevelElements: TopLevelElement[]): UtopiaJSXComponent | null {
  return (
    topLevelElements.find((e): e is UtopiaJSXComponent => {
      return isUtopiaJSXComponent(e)
    }) ?? null
  )
}

interface JSXElementWalkResult {
  uid: string
  pathPart: ElementPathPart
  componentName: string
}

export function* jsxElementUidsPostOrder(
  element: JSXElementChild,
  pathPart: ElementPathPart,
): Generator<JSXElementWalkResult, void, unknown> {
  switch (element.type) {
    case 'JSX_FRAGMENT':
    case 'JSX_ELEMENT':
      for (const child of element.children) {
        yield* jsxElementUidsPostOrder(child, [...pathPart, element.uid])
      }
      yield {
        uid: element.uid,
        pathPart: [...pathPart, element.uid],
        componentName:
          element.type === 'JSX_FRAGMENT' ? 'Fragment' : getJSXElementNameAsString(element.name),
      }
      return
    default:
      return
  }
}

export function invariant<T>(value: T | null | undefined, message: string): asserts value is T {
  if (value == null) {
    console.error(`Invariant error: ${message}`)
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

export function routeFromEntry(route: EntryRoute): DataRouteObject {
  return {
    caseSensitive: false,
    element: <UtopiaRemixRoute id={route.id} />,
    errorElement: undefined,
    id: route.id,
    index: route.index,
    path: route.path,
    handle: null,
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

function getDefaultExportedTopLevelElement(file: TextFile): JSXElementChild | null {
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

export function createRouteManifestFromProjectContents(
  projectContents: ProjectContentTreeRoot,
): RouteManifestWithContents | null {
  const routesFromFlatRoutes = flatRoutes(ROOT_DIR, projectContents)
  if (routesFromFlatRoutes == null) {
    return null
  }
  const routes = {
    ...routesFromFlatRoutes,
    root: { path: '', id: 'root', file: 'root.js', parentId: '' },
  }

  let resultRoutes: RouteManifestWithContents = {}
  for (let route of Object.values(routes)) {
    // this still feels temporary, probably we should the hasAction and the hasLoader up properly from projectContents,
    // but it is not used for anything, and we get the action/loader/default exports of the modules manually later.
    resultRoutes[route.id] = {
      ...route,
      parentId: route.parentId ?? 'root',
      module: '',
      hasAction: false,
      hasLoader: false,
      hasCatchBoundary: false,
      hasErrorBoundary: false,
      filePath: `${ROOT_DIR}/${route.file}`,
    }
  }

  return resultRoutes
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
): GetRoutesAndModulesFromManifestResult | null {
  const routeModuleCreators: RouteIdsToModuleCreators = {}
  const routingTable: RemixStaticRoutingTable = {}

  const indexJSRootElement = getRootJSRootElement(projectContents)
  if (indexJSRootElement == null) {
    return null
  }

  const routesByParentId = groupRoutesByParentId(routeManifest)
  const routes: DataRouteWithFilePath[] = createClientRoutes(
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
    EP.emptyElementPath,
  )

  Object.values(routeManifest).forEach((route) => {
    const { executionScopeCreator, loader, action, rootComponentUid } = getRemixExportsOfModule(
      route.filePath,
      curriedRequireFn,
      curriedResolveFn,
      metadataContext,
      projectContents,
      mutableContextRef,
      topLevelComponentRendererComponents,
    )

    addLoaderAndActionToRoute(routes, route.id, loader, action)
    routeModuleCreators[route.id] = {
      filePath: route.filePath,
      executionScopeCreator: executionScopeCreator,
    }

    routingTable[rootComponentUid] = route.filePath
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
  invariant(nameAndUid, 'a default export should be provided')

  // console.log('nameAndUid.name', filename, executionScope.scope)
  const fallbackElement = () => <React.Fragment />

  return {
    executionScopeCreator: executionScopeCreator,
    loader: executionScope.scope['loader'] as LoaderFunction | undefined,
    action: executionScope.scope['action'] as ActionFunction | undefined,
    rootComponentUid: nameAndUid.uid,
  }
}

function getRootJSRootElement(projectContents: ProjectContentTreeRoot): JSXElementChild | null {
  const file = getProjectFileByFilePath(projectContents, `${ROOT_DIR}/root.js`)
  if (
    file == null ||
    file.type !== 'TEXT_FILE' ||
    file.lastParseSuccess?.type !== 'PARSE_SUCCESS'
  ) {
    return null
  }

  const defaultExportName =
    file.lastParseSuccess.exportsDetail.find(
      (e): e is ExportDefaultFunctionOrClass => e.type === 'EXPORT_DEFAULT_FUNCTION_OR_CLASS',
    )?.name ?? null

  if (defaultExportName == null) {
    return null
  }

  return (
    file.lastParseSuccess.topLevelElements.find(
      (t): t is UtopiaJSXComponent =>
        t.type === 'UTOPIA_JSX_COMPONENT' && t.name === defaultExportName,
    )?.rootElement ?? null
  )
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
  [filePath: string]: {
    relativePath: ElementPath
    isLeafModule: boolean
  }
}

function getRouteModulesWithPaths(
  projectContents: ProjectContentTreeRoot,
  route: DataRouteWithFilePath,
  pathSoFar: ElementPath,
): RouteModulesWithRelativePaths {
  const file = getProjectFileByFilePath(projectContents, route.filePath)
  if (file == null || file.type !== 'TEXT_FILE') {
    return {}
  }

  const topLevelElement = getDefaultExportedTopLevelElement(file)
  invariant(topLevelElement, 'Route module should provide a default export')

  const pathPartToOutlet = findPathToOutlet(topLevelElement)
  const isLeafModule = pathPartToOutlet == null
  let routeModulesWithBasePaths: RouteModulesWithRelativePaths = {
    [route.filePath]: {
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
    const paths = getRouteModulesWithPaths(projectContents, child, pathForChildren)
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
