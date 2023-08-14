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
import { left, right } from '../../../core/shared/either'
import { UNSAFE_RemixContext as RemixContext } from '@remix-run/react'
import type { ProjectContentFile } from '../../assets'
import type {
  JSXElementChild,
  TopLevelElement,
  UtopiaJSXComponent,
} from '../../../core/shared/element-template'
import { isUtopiaJSXComponent } from '../../../core/shared/element-template'
import type { ElementPathPart } from '../../../core/shared/project-file-types'

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

interface UidWithPathPart {
  uid: string
  pathPart: ElementPathPart
}

export function* jsxElementUidsPostOrder(
  element: JSXElementChild,
  pathPart: ElementPathPart,
): Generator<UidWithPathPart, void, unknown> {
  switch (element.type) {
    case 'JSX_FRAGMENT':
    case 'JSX_ELEMENT':
      for (const child of element.children) {
        yield* jsxElementUidsPostOrder(child, [...pathPart, element.uid])
      }
      yield { uid: element.uid, pathPart: [...pathPart, element.uid] }
      return
    default:
      return
  }
}

function invariant<T>(value: T | null | undefined, message: string): asserts value is T {
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
