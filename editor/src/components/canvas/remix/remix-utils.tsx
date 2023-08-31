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
