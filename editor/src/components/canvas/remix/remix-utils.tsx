import React from 'react'
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
import type { DataRouteObject, Path } from 'react-router-dom'
import type { CurriedResolveFn, CurriedUtopiaRequireFn } from '../../custom-code/code-file'
import type { MapLike } from 'typescript'
import type { UiJsxCanvasContextData } from '../ui-jsx-canvas'
import { attemptToResolveParsedComponents } from '../ui-jsx-canvas'
import type { ComponentRendererComponent } from '../ui-jsx-canvas-renderer/component-renderer-component'
import type { MutableUtopiaCtxRefData } from '../ui-jsx-canvas-renderer/ui-jsx-canvas-contexts'
import type { ElementPath, TextFile } from '../../../core/shared/project-file-types'
import type { ExecutionScope } from '../ui-jsx-canvas-renderer/ui-jsx-canvas-execution-scope'
import { createExecutionScope } from '../ui-jsx-canvas-renderer/ui-jsx-canvas-execution-scope'
import { type RemixRoutingTable } from '../../editor/store/remix-derived-data'
import { NO_OP } from '../../../core/shared/utils'
import * as EP from '../../../core/shared/element-path'
import {
  fileExportsFunctionWithName,
  getDefaultExportNameAndUidFromFile,
  getDefaultExportedTopLevelElement,
  isRemixOutletElement,
} from '../../../core/model/project-file-utils'
import type { Either } from '../../../core/shared/either'
import { foldEither, forEachRight, left } from '../../../core/shared/either'
import type { CanvasBase64Blobs } from '../../editor/store/editor-state'
import { findPathToJSXElementChild } from '../../../core/model/element-template-utils'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { type ElementInstanceMetadataMap } from '../../../core/shared/element-template'
import type { ElementPathTrees } from '../../../core/shared/element-path-tree'
import { getAllUniqueUids } from '../../../core/model/get-unique-ids'
import { safeIndex } from '../../../core/shared/array-utils'
import { createClientRoutes, groupRoutesByParentId } from '../../../third-party/remix/client-routes'
import path from 'path'
import urljoin from 'url-join'
import json5 from 'json5'

export const OutletPathContext = React.createContext<ElementPath | null>(null)

export type RouteManifestWithContents = RouteManifest<EntryRoute>

export const DefaultFutureConfig: FutureConfig = {
  v2_dev: true,
  unstable_postcss: false,
  unstable_tailwind: false,
  v2_errorBoundary: true,
  v2_headers: false,
  v2_meta: false,
  v2_normalizeFormMethod: false,
  v2_routeConvention: true,
}

// This is necessary to create a simple node.fs-like implementation for Utopia projectContents, which
// can be used by the Remix functions to parse the routes
function projectContentsToFileOps(projectContents: ProjectContentTreeRoot): FileOps {
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
  {
    rootFilePath,
    rootDir,
  }: {
    rootFilePath: string
    rootDir: string
  },
  projectContents: ProjectContentTreeRoot,
): RouteManifest<EntryRoute> | null {
  const routesFromRemix = (() => {
    try {
      return flatRoutes(rootDir, projectContentsToFileOps(projectContents))
    } catch (e) {
      return null
    }
  })()

  if (routesFromRemix == null) {
    return null
  }
  return patchRemixRoutes(rootFilePath, rootDir, routesFromRemix, projectContents)
}

function patchRemixRoutes(
  rootFilePath: string,
  rootDir: string,
  routesFromRemix: RouteManifest<ConfigRoute> | null,
  projectContents: ProjectContentTreeRoot,
) {
  const routesFromRemixWithRoot: RouteManifest<ConfigRoute> = {
    ...routesFromRemix,
    root: { path: '', id: 'root', file: path.basename(rootFilePath), parentId: '' },
  }

  const resultRoutes = Object.values(routesFromRemixWithRoot).reduce((acc, route) => {
    const filePath = `${rootDir}/${route.file}`

    // Maybe we should fill hasAction and hasLoader properly, but it is not used for anything
    acc[route.id] = {
      ...route,
      parentId: route.parentId ?? 'root',
      module: filePath,
      hasAction: false,
      hasLoader: false,
      hasErrorBoundary: fileExportsFunctionWithName(projectContents, filePath, 'ErrorBoundary'),
    }
    return acc
  }, {} as RouteManifest<EntryRoute>)

  return resultRoutes
}

export function createAssetsManifest(routes: RouteManifest<EntryRoute>): AssetsManifest {
  return {
    entry: { imports: [], module: '' },
    url: '/',
    version: '1',
    routes: routes,
  }
}

interface RouteModuleCreator {
  filePath: string
  createErrorBoundary: boolean
  executionScopeCreator: ExecutionScopeCreator
}

export interface RouteIdsToModuleCreators {
  [routeId: string]: RouteModuleCreator
}

export interface RouteModulesWithRelativePaths {
  [routeId: string]: {
    relativePaths: Array<ElementPath>
    filePath: string
  }
}

interface GetRoutesAndModulesFromManifestResult {
  routeModuleCreators: RouteIdsToModuleCreators
  routes: Array<DataRouteObject>
  routeModulesToRelativePaths: RouteModulesWithRelativePaths
  routingTable: RemixRoutingTable
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

  const pathPartsToOutlets = findPathToJSXElementChild(
    (e) => isRemixOutletElement(e, filePathForRouteObject, projectContents),
    topLevelElement,
  )

  const isLeafModule = pathPartsToOutlets == null
  let routeModulesWithBasePaths: RouteModulesWithRelativePaths = {
    [route.id]: {
      relativePaths: [pathSoFar],
      filePath: filePathForRouteObject,
    },
  }

  if (isLeafModule) {
    return routeModulesWithBasePaths
  }

  const children = route.children ?? []

  for (const pathPartToOutlet of pathPartsToOutlets) {
    for (const child of children) {
      const pathForChildren = EP.appendNewElementPath(pathSoFar, pathPartToOutlet)

      const paths = getRouteModulesWithPaths(projectContents, manifest, child, pathForChildren)

      for (const [routeId, value] of Object.entries(paths)) {
        if (routeModulesWithBasePaths[routeId] == null) {
          routeModulesWithBasePaths[routeId] = value
        } else {
          routeModulesWithBasePaths[routeId].relativePaths.push(...value.relativePaths)
        }
      }
    }
  }

  return routeModulesWithBasePaths
}

export type ExecutionScopeCreator = (
  innerProjectContents: ProjectContentTreeRoot,
  fileBlobs: CanvasBase64Blobs,
  hiddenInstances: Array<ElementPath>,
  displayNoneInstances: Array<ElementPath>,
  metadataContext: UiJsxCanvasContextData,
) => ExecutionScope

function getRemixExportsOfModule(
  filename: string,
  curriedRequireFn: CurriedUtopiaRequireFn,
  curriedResolveFn: CurriedResolveFn,
  projectContents: ProjectContentTreeRoot,
): {
  executionScopeCreator: ExecutionScopeCreator
  rootComponentUid: string
} {
  let mutableContextRef: { current: MutableUtopiaCtxRefData } = { current: {} }
  let topLevelComponentRendererComponents: {
    current: MapLike<MapLike<ComponentRendererComponent>>
  } = { current: {} }

  const executionScopeCreator = (
    innerProjectContents: ProjectContentTreeRoot,
    fileBlobs: CanvasBase64Blobs,
    hiddenInstances: Array<ElementPath>,
    displayNoneInstances: Array<ElementPath>,
    metadataContext: UiJsxCanvasContextData,
  ) => {
    let resolvedFiles: MapLike<MapLike<any>> = {}
    let resolvedFileNames: Array<string> = [filename]

    const requireFn = curriedRequireFn(innerProjectContents)
    const resolve = curriedResolveFn(innerProjectContents)

    const customRequire = (importOrigin: string, toImport: string) => {
      if (resolvedFiles[importOrigin] == null) {
        resolvedFiles[importOrigin] = []
      }
      let resolvedFromThisOrigin = resolvedFiles[importOrigin]

      const alreadyResolved = resolvedFromThisOrigin[toImport] !== undefined
      const filePathResolveResult = alreadyResolved
        ? left<string, string>('Already resolved')
        : resolve(importOrigin, toImport)

      forEachRight(filePathResolveResult, (filepath) => resolvedFileNames.push(filepath))

      const resolvedParseSuccess: Either<string, MapLike<any>> = attemptToResolveParsedComponents(
        resolvedFromThisOrigin,
        toImport,
        innerProjectContents,
        customRequire,
        mutableContextRef,
        topLevelComponentRendererComponents,
        filename,
        fileBlobs,
        hiddenInstances,
        displayNoneInstances,
        metadataContext,
        NO_OP,
        false,
        filePathResolveResult,
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

  const nameAndUid = getDefaultExportNameAndUidFromFile(projectContents, filename)

  return {
    executionScopeCreator: executionScopeCreator,
    rootComponentUid: nameAndUid?.uid ?? 'NO-ROOT',
  }
}

function safeGetClientRoutes(
  routeManifest: RouteManifestWithContents,
  routeModulesCache: RouteModules,
  futureConfig: FutureConfig,
): DataRouteObject[] | null {
  const routesByParentId = groupRoutesByParentId(routeManifest)
  try {
    return createClientRoutes(routeManifest, routeModulesCache, futureConfig, '', routesByParentId)
  } catch (e) {
    console.error(e)
    return null
  }
}

export function getRemixRootFile(
  rootDir: string,
  projectContents: ProjectContentTreeRoot,
): { file: TextFile; path: string } | null {
  function getTextFileAtPath(filePath: string) {
    const maybeTextFile = getProjectFileByFilePath(projectContents, filePath)
    if (maybeTextFile == null || maybeTextFile.type !== 'TEXT_FILE') {
      return null
    }
    return { file: maybeTextFile, path: filePath }
  }

  return (
    getTextFileAtPath(path.join(rootDir, 'root.js')) ??
    getTextFileAtPath(path.join(rootDir, 'root.jsx')) ??
    null
  )
}

export function getRoutesAndModulesFromManifest(
  rootJsFile: TextFile,
  routeManifest: RouteManifestWithContents,
  futureConfig: FutureConfig,
  curriedRequireFn: CurriedUtopiaRequireFn,
  curriedResolveFn: CurriedResolveFn,
  projectContents: ProjectContentTreeRoot,
  routeModulesCache: RouteModules,
): GetRoutesAndModulesFromManifestResult | null {
  const routeModuleCreators: RouteIdsToModuleCreators = {}
  const routingTable: RemixRoutingTable = {}

  const rootJSRootElement = getDefaultExportedTopLevelElement(rootJsFile)
  if (rootJSRootElement == null) {
    return null
  }

  const routes = safeGetClientRoutes(routeManifest, routeModulesCache, futureConfig)
  if (routes == null) {
    return null
  }

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
    const { executionScopeCreator, rootComponentUid } = getRemixExportsOfModule(
      route.module,
      curriedRequireFn,
      curriedResolveFn,
      projectContents,
    )

    routeModuleCreators[route.id] = {
      filePath: route.module,
      createErrorBoundary: route.hasErrorBoundary,
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

export function getRouteComponentNameForOutlet(
  elementPath: ElementPath,
  metadata: ElementInstanceMetadataMap,
  projectContents: ProjectContentTreeRoot,
  pathTrees: ElementPathTrees,
): string | null {
  if (!MetadataUtils.isProbablyRemixOutlet(metadata, elementPath)) {
    return null
  }

  const outletChildren = MetadataUtils.getImmediateChildrenPathsOrdered(
    metadata,
    pathTrees,
    elementPath,
  )
  const outletChild = safeIndex(outletChildren, 0)
  if (outletChild == null) {
    return null
  }

  const uidsToFilePath = getAllUniqueUids(projectContents).uidsToFilePaths
  const filePath = uidsToFilePath[EP.toUid(outletChild)]
  if (filePath == null) {
    return null
  }

  const defaultExport = getDefaultExportNameAndUidFromFile(projectContents, filePath)
  if (defaultExport == null) {
    return null
  }

  return defaultExport.name
}

export const RemixIndexPathLabel = '/'

export function getRemixUrlFromLocation(remixPath: Path | undefined): string | null {
  if (remixPath == null) {
    return null
  }
  return getRemixLocationLabel(urljoin(remixPath.pathname, remixPath.search, remixPath.hash))
}

export function getRemixLocationLabel(location: string | null | undefined): string | null {
  if (location == null) {
    return null
  }

  if (location === '/' || location === '') {
    return RemixIndexPathLabel
  }

  return location
}

function modifyFeaturedRoutesInPackageJson(
  packageJson: string,
  modifyFn: (routes: string[]) => string[],
): string {
  const parsedJSON = json5.parse(packageJson)

  // if the utopia prop is not defined, set it to an empty object
  if (parsedJSON.utopia == null) {
    parsedJSON.utopia = {}
  } else if (typeof parsedJSON.utopia !== 'object') {
    throw new Error("the 'utopia' key in package.json should be an object")
  }

  // if the featuredRoutes prop is not defined, set it to an empty array
  if (parsedJSON.utopia.featuredRoutes == null) {
    parsedJSON.utopia.featuredRoutes = []
  } else if (!Array.isArray(parsedJSON.utopia.featuredRoutes)) {
    throw new Error("the 'utopia.featuredRoutes' key in package.json should be an array")
  }

  parsedJSON.utopia.featuredRoutes = modifyFn(parsedJSON.utopia.featuredRoutes)

  return JSON.stringify(parsedJSON, null, 2)
}

export function addNewFeaturedRouteToPackageJson(urlRoute: string) {
  return function (packageJson: string): string {
    if (urlRoute === '') {
      throw new Error('Cannot add an empty route to the featured routes')
    }
    const newRoute = urljoin('/', urlRoute)
    return modifyFeaturedRoutesInPackageJson(packageJson, (currentRoutes) => {
      if (!currentRoutes.includes(newRoute)) {
        return [...currentRoutes, newRoute]
      }
      return currentRoutes
    })
  }
}

export function removeFeaturedRouteFromPackageJson(routeToRemove: string) {
  return function (packageJson: string): string {
    const actualRouteToRemove = urljoin('/', routeToRemove)
    return modifyFeaturedRoutesInPackageJson(packageJson, (currentRoutes) => {
      return currentRoutes.filter((route) => route !== actualRouteToRemove)
    })
  }
}

export function addOrReplaceFeaturedRouteToPackageJson(oldRoute: string, newRoute: string) {
  return function (packageJson: string) {
    return modifyFeaturedRoutesInPackageJson(packageJson, (currentRoutes): string[] => {
      return currentRoutes.map((route) => {
        return route.startsWith(oldRoute)
          ? route.replace(oldRoute, newRoute) // just once (string replace instead of a regex), it's ok
          : route
      })
    })
  }
}

export type PageTemplate = { label: string; path: string }

export function isPageTemplate(u: unknown): u is PageTemplate {
  const maybe = u as PageTemplate
  return u != null && typeof u == 'object' && maybe.label != null && maybe.path != null
}

function appRoutesPrefix(remixRootDir: string): string {
  return urljoin(remixRootDir, 'routes') + '/' // the trailing slash acts as a separator
}

export function isInsideRemixFolder(remixRootDir: string, filename: string): boolean {
  return filename.startsWith(appRoutesPrefix(remixRootDir))
}

const possibleRemixSuffixSeparators = ['.', '_.']

/**
 * Return whether the given filename is a valid Remix route filename that is
 * a prefix of the oldPath.
 */
export function remixFilenameMatchPrefix(
  remixRootDir: string,
  filename: string,
  oldPath: string,
): boolean {
  // if it's not a remix route (meaning a .jsx file inside the /app/routes/ folder), stop here
  const isRemixRoute =
    isInsideRemixFolder(remixRootDir, oldPath) &&
    isInsideRemixFolder(remixRootDir, filename) &&
    path.extname(filename) === '.jsx'
  if (!isRemixRoute) {
    return false
  }

  const remixRootDirPrefix = appRoutesPrefix(remixRootDir)

  // to make it easier to compare paths, make them relative to the /app/routes folder and remove any optional prefixes
  const relativeOldPath = oldPath.replace(remixRootDirPrefix, '') // without /app/routes
  const relativeFilename = filename
    .replace(remixRootDirPrefix, '') // without /app/routes
    .replace(/^\([^)]+\)\./, '') // without optional prefix

  return possibleRemixSuffixSeparators.some((sep) =>
    relativeFilename.startsWith(relativeOldPath + sep),
  )
}

export function renameRemixFile({
  remixRootDir,
  filename,
  oldPath,
  newPath,
}: {
  remixRootDir: string
  filename: string
  oldPath: string
  newPath: string
}): {
  filename: string
  renamedOptionalPrefix: boolean
} {
  const remixRootDirPrefix = appRoutesPrefix(remixRootDir)
  // 1. to make things easier, make all the paths relative to the Remix folder
  const relativeFilename = filename.replace(remixRootDirPrefix, '')
  const relativeOldPath = oldPath.replace(remixRootDirPrefix, '')
  const relativeNewPath = newPath.replace(remixRootDirPrefix, '')

  // 2. tokenize the relative paths, where each token is a "piece" of the path (see the comment doc for tokenizeRemixFilename).
  const filenameTokens = tokenizeRemixFilename(relativeFilename)
  const oldPathTokens = tokenizeRemixFilename(relativeOldPath)
  const newPathTokens = tokenizeRemixFilename(relativeNewPath)

  let renamedOptionalPrefix = false

  // 3. build the result tokens by replacing the right tokens
  let resultTokens: string[] = []
  for (let i = 0; i < filenameTokens.length; i++) {
    function getNewToken() {
      if (i >= oldPathTokens.length) {
        // the cursor is beyond the boundaries of the old tokens, so we can just return the original token
        return filenameTokens[i]
      } else if (filenameTokens[i] === oldPathTokens[i]) {
        // the filename token matches exactly the old one, so we can replace immediately
        return newPathTokens[i]
      } else if (filenameTokens[i] === oldPathTokens[i] + '_') {
        // the filename token matches exactly the old one with a trailing underscore, so we can replace immediately
        return newPathTokens[i] + '_'
      } else if (filenameTokens[i].endsWith(').' + oldPathTokens[i])) {
        // the filename token contains optional prefixes, so manipulate the replacement accordingly
        renamedOptionalPrefix = true
        return replaceTokenForOptionalPrefix({
          originalToken: filenameTokens[i],
          portionToBeReplaced: oldPathTokens[i],
          replacementPortion: newPathTokens[i],
        })
      } else if (filenameTokens[i].endsWith(').' + oldPathTokens[i] + '_')) {
        // the filename token contains optional prefixes and an underscore suffix, so manipulate the replacement accordingly
        renamedOptionalPrefix = true
        return replaceTokenForOptionalPrefix({
          originalToken: filenameTokens[i],
          portionToBeReplaced: oldPathTokens[i] + '_',
          replacementPortion: newPathTokens[i] + '_',
        })
      } else {
        // no match, return the original token and keep going
        return filenameTokens[i]
      }
    }
    resultTokens.push(getNewToken())
  }

  // 4. merge the result tokens with the dot separator and prepend /app/routes for the final filename
  const result = urljoin(remixRootDirPrefix, resultTokens.join('.'))

  return {
    filename: result,
    renamedOptionalPrefix: renamedOptionalPrefix,
  }
}

/**
 * Merge the input tokens array into another array where optional tokens are prefixed
 * to non-optional tokens.
 *
 * For example:
 * 	['($foo)', 'bar', 'baz', '(qux)', 'waldo']
 * 	becomes
 * 	['($foo).bar', 'baz', '(qux).waldo']
 */
function tokenizeRemixFilename(filename: string): string[] {
  const tokens = filename.split('.')
  const merged: string[] = []
  let currentToken: string[] = []
  for (let i = 0; i < tokens.length; i++) {
    const token = tokens[i]
    currentToken.push(token)
    if (!token.startsWith('(')) {
      merged.push(currentToken.join('.'))
      currentToken = []
    }
  }
  if (currentToken.length > 0) {
    merged.push(currentToken.join('.'))
  }
  return merged
}

/**
 * Replaces the target `portionToBeReplaced` suffix of the `originalToken` with
 * the given `replacementPortion`, keeping the optional prefix.
 *
 * For example:
 * 	replaceTokenForOptionalPrefix('($foo).(bar).baz', 'baz', 'qux')
 * 	returns
 * 	'($foo).(bar).qux'
 */
function replaceTokenForOptionalPrefix({
  originalToken,
  portionToBeReplaced,
  replacementPortion,
}: {
  originalToken: string
  portionToBeReplaced: string
  replacementPortion: string
}): string {
  const lastPortionOfOptional = '\\)\\.'
  const endOfString = '$'
  const re = new RegExp(lastPortionOfOptional + portionToBeReplaced + endOfString)
  return originalToken.replace(re, ').' + replacementPortion)
}
