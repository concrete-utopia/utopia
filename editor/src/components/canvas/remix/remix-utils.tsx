import type {
  UNSAFE_FutureConfig as FutureConfig,
  UNSAFE_EntryRoute as EntryRoute,
  UNSAFE_RouteManifest as RouteManifest,
} from '@remix-run/react'
import type { ProjectContentTreeRoot } from '../../assets'
import { flatRoutes } from '../../../third-party/remix/flat-routes'
import type { ConfigRoute } from '../../../third-party/remix/routes'

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

export interface EntryRouteWithFilePath extends EntryRoute {
  filePath: string
}

export function createRouteManifestFromProjectContents(
  projectContents: ProjectContentTreeRoot,
): RouteManifest<EntryRouteWithFilePath> | null {
  const routesFromRemix = flatRoutes(ROOT_DIR, projectContents)

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
      module: '',
      hasAction: false,
      hasLoader: false,
      hasCatchBoundary: false,
      hasErrorBoundary: false,
      filePath: `${ROOT_DIR}/${route.file}`,
    }
    return acc
  }, {} as RouteManifest<EntryRouteWithFilePath>)

  return resultRoutes
}
