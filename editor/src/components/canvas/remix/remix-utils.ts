// FIXME: this function is not fully remix-route compliant, it only
// implement what's necessary at a given time

import type { Either } from '../../../core/shared/either'
import { left, right } from '../../../core/shared/either'
import type {
  UNSAFE_RouteManifest as RouteManifest,
  UNSAFE_EntryRoute as EntryRoute,
} from '@remix-run/react'
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

export function* postOrderTraversal(
  element: JSXElementChild,
  pathPart: ElementPathPart,
): Generator<UidWithPathPart, void, unknown> {
  switch (element.type) {
    case 'JSX_FRAGMENT':
    case 'JSX_ELEMENT':
      for (const child of element.children) {
        yield* postOrderTraversal(child, [...pathPart, element.uid])
      }
      yield { uid: element.uid, pathPart: pathPart }
      return
    default:
      yield { uid: element.uid, pathPart: pathPart }
  }
}
