import React from 'react'
import type { DataRouteObject } from 'react-router'
import { Outlet } from 'react-router'
import type {
  UNSAFE_RouteManifest as RouteManifest,
  UNSAFE_EntryRoute as EntryRoute,
  UNSAFE_FutureConfig as FutureConfig,
  UNSAFE_AssetsManifest as AssetsManifest,
  UNSAFE_RemixContextObject as RemixContextObject,
} from '@remix-run/react'

import type { Either } from '../../../core/shared/either'
import { foldEither, left, right } from '../../../core/shared/either'
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
} from '../../../core/shared/project-file-types'
import type { RouteModules } from '@remix-run/react/dist/routeModules'
import { UTOPIA_PATH_KEY } from '../../../core/model/utopia-constants'
import { createExecutionScope } from '../ui-jsx-canvas-renderer/ui-jsx-canvas-execution-scope'
import type { UiJsxCanvasContextData } from '../ui-jsx-canvas'
import type { UtopiaRemixRootComponentProps } from './utopia-remix-root-component'
import type { MutableUtopiaCtxRefData } from '../ui-jsx-canvas-renderer/ui-jsx-canvas-contexts'
import { NO_OP } from '../../../core/shared/utils'
import * as EP from '../../../core/shared/element-path'
import type { ComponentRendererComponent } from '../ui-jsx-canvas-renderer/ui-jsx-canvas-component-renderer'
import type { MapLike } from 'typescript'
import { pathPartsFromJSXElementChild } from '../../../core/model/element-template-utils'

interface PathFromFileNameResult {
  parentId: string
  path: string
  id: string
  index: boolean
}

function getPathInner(id: string, segments: string[]): PathFromFileNameResult {
  let pathSoFar: string[] = []
  for (const segment of segments) {
    // TODO: path segments postfixed with _ are not handled yet
    if (segment.startsWith('$')) {
      pathSoFar.push(':' + segment.slice(1))
    } else {
      pathSoFar.push(segment)
    }
  }

  const pathPrefix = pathSoFar.slice(0, -1)
  const parentId = pathPrefix.length === 0 ? 'root' : pathSoFar.join('/')
  const lastSegment = pathSoFar[pathSoFar.length - 1]
  const index = lastSegment === '_index'
  const path = (index ? pathSoFar.slice(0, -1) : pathSoFar).join('/')

  return {
    parentId,
    path,
    index,
    id,
  }
}

// `root` is special-cased outside of this function
// all files are assumed to be located under `routes/`
export function parsePathFromFileName(fileName: string): PathFromFileNameResult {
  const id = fileName
  const segments = fileName.split('.')
  segments.pop() // the file extension

  return getPathInner(id, segments)
}

export interface EntryRouteWithFileMeta extends EntryRoute {
  filePath: string
}

export type RouteManifestWithContents = RouteManifest<EntryRouteWithFileMeta>

type GetRoutesError = 'No root file provided'

export const ROOT_FILE_NAME = '/src/root.js'
export const ROUTE_FILE_PREFIX = '/src/routes/'

export function getRoutesFromFiles(
  files: ProjectContentFile[],
): Either<GetRoutesError, RouteManifestWithContents> {
  const root = files.find((f) => f.fullPath === ROOT_FILE_NAME)
  if (root == null) {
    return left('No root file provided')
  }

  const routeManifest: RouteManifestWithContents = {
    root: {
      hasAction: false,
      hasLoader: false,
      hasCatchBoundary: false,
      hasErrorBoundary: false,
      module: '',
      id: 'root',
      path: '',
      filePath: root.fullPath,
    },
  }

  files.forEach((file) => {
    if (!file.fullPath.startsWith(ROUTE_FILE_PREFIX)) {
      return
    }

    const pathWithoutPrefix = file.fullPath.slice(ROUTE_FILE_PREFIX.length)
    const routePathResult = parsePathFromFileName(pathWithoutPrefix)
    routeManifest[routePathResult.id] = {
      hasAction: false,
      hasLoader: false,
      hasCatchBoundary: false,
      hasErrorBoundary: false,
      module: '',
      id: routePathResult.id,
      parentId: routePathResult.parentId,
      index: routePathResult.index,
      path: routePathResult.path,
      filePath: file.fullPath,
    }
  })

  return right(routeManifest)
}

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

export const defaultFutureConfig: FutureConfig = {
  v2_dev: true,
  unstable_postcss: false,
  unstable_tailwind: false,
  v2_errorBoundary: false,
  v2_headers: false,
  v2_meta: false,
  v2_normalizeFormMethod: false,
  v2_routeConvention: false,
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

export const PathPropHOC = (Wrapped: any, path: string) => (props: any) => {
  const propsWithPath = {
    [UTOPIA_PATH_KEY]: path,
    ...props,
  }
  return <Wrapped {...propsWithPath} />
}

export function createRouteManifestFromProjectContents(
  projectContents: ProjectContentTreeRoot,
): RouteManifestWithContents {
  const getFlatFilePaths = (root: ProjectContentsTree): ProjectContentFile[] =>
    root.type === 'PROJECT_CONTENT_FILE'
      ? [root]
      : Object.values(root.children).flatMap((c) => getFlatFilePaths(c))

  const flatFiles = Object.values(projectContents).flatMap(getFlatFilePaths)

  return foldEither(
    () => ({}),
    (r) => r,
    getRoutesFromFiles(flatFiles),
  )
}

export function getRoutesAndModulesFromManifest(
  routeManifest: RouteManifestWithContents,
  customRequire: (importOrigin: string, toImport: string) => any,
  metadataContext: UiJsxCanvasContextData,
  projectContents: ProjectContentTreeRoot,
  remixAppContainerPath: ElementPath,
  mutableContextRef: React.MutableRefObject<MutableUtopiaCtxRefData>,
  topLevelComponentRendererComponents: React.MutableRefObject<
    MapLike<MapLike<ComponentRendererComponent>>
  >,
): {
  routeModules: RouteModules
  routes: Array<DataRouteObject>
} {
  const routeManifestResult: RouteModules = {}
  const routesResult: DataRouteObject[] = []

  const indexJSRootElement = getRootJSRootElement(projectContents)
  invariant(indexJSRootElement, 'There should be an root.js in the spike project')

  const pathPartForRootJs = findPathToOutlet(indexJSRootElement) ?? []

  Object.values(routeManifest).forEach((route) => {
    // TODO: unhardcode when we have access to the hierarchy
    const basePath =
      route.id === 'root'
        ? remixAppContainerPath
        : EP.appendNewElementPath(remixAppContainerPath, pathPartForRootJs)

    const { defaultExport, loader, action } = getRemixExportsOfModule(
      route.filePath,
      customRequire,
      metadataContext,
      projectContents,
      basePath,
      mutableContextRef,
      topLevelComponentRendererComponents,
    )

    try {
      routeManifestResult[route.id] = {
        default: defaultExport,
      }

      // HACK LVL: >9000
      // `children` should be filled out properly
      const routeObject: DataRouteObject = {
        ...routeFromEntry(route),
        loader: loader,
        action: action,
      }

      if (routeObject.id === '_index.js') {
        routesResult.find((r) => r.id === 'root')!.children = [routeObject]
      } else {
        routesResult.push(routeObject)
      }
    } catch (e) {
      console.error(e)
    }
  })

  return { routeModules: routeManifestResult, routes: routesResult }
}

function getRemixExportsOfModule(
  filename: string,
  customRequire: (importOrigin: string, toImport: string) => any,
  metadataContext: UiJsxCanvasContextData,
  projectContents: ProjectContentTreeRoot,
  basePath: ElementPath,
  mutableContextRef: React.MutableRefObject<MutableUtopiaCtxRefData>,
  topLevelComponentRendererComponents: React.MutableRefObject<
    MapLike<MapLike<ComponentRendererComponent>>
  >,
): {
  defaultExport: (props: any) => JSX.Element
  loader: any
  action: any
} {
  const executionScope = createExecutionScope(
    filename,
    customRequire,
    mutableContextRef,
    topLevelComponentRendererComponents,
    projectContents,
    filename,
    {},
    [],
    [],
    metadataContext,
    NO_OP,
    false,
    null,
  )

  const nameAndUid = getDefaultExportNameAndUidFromFile(projectContents, filename)
  invariant(nameAndUid, 'a default export should be provided')

  // console.log('nameAndUid.name', filename, executionScope.scope)
  const fallbackElement = () => <React.Fragment />

  return {
    defaultExport: PathPropHOC(
      executionScope.scope[nameAndUid.name] ?? fallbackElement,
      EP.toString(basePath),
    ),
    loader: executionScope.scope['loader'],
    action: executionScope.scope['action'],
  }
}

function getRootJSRootElement(projectContents: ProjectContentTreeRoot): JSXElementChild | null {
  const file = getProjectFileByFilePath(projectContents, '/src/root.js')
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
