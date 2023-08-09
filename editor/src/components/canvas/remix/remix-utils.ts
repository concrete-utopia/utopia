// FIXME: this function is not fully remix-route compliant, it only
// implement what's necessary at a given time

import type { Either } from '../../../core/shared/either'
import { left, right } from '../../../core/shared/either'
import type {
  UNSAFE_RouteManifest as RouteManifest,
  UNSAFE_EntryRoute as EntryRoute,
} from '@remix-run/react'
import type { ProjectContentFile } from '../../assets'

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
  moduleContents: string
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

  const moduleContents = (file: ProjectContentFile) =>
    file.content.type === 'TEXT_FILE' ? file.content.fileContents.code : ''

  const routeManifest: RouteManifestWithContents = {
    root: {
      hasAction: false,
      hasLoader: false,
      hasCatchBoundary: false,
      hasErrorBoundary: false,
      module: '',
      id: 'root',
      path: '',
      moduleContents: moduleContents(root),
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
      moduleContents: moduleContents(file),
      filePath: file.fullPath,
    }
  })

  return right(routeManifest)
}
