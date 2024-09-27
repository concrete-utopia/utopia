import type {
  ProjectFile,
  NodeModules,
  ESCodeFile,
  ESRemoteDependencyPlaceholder,
} from '../../shared/project-file-types'
import { isEsCodeFile, esCodeFile } from '../../shared/project-file-types'
import type { ParseResult } from '../../../utils/value-parser-utils'
import {
  optionalObjectKeyParser,
  parseString,
  parseAlternative,
  parseObject,
  parseFalse,
  parseNull,
} from '../../../utils/value-parser-utils'
import type { Either } from '../../shared/either'
import { applicative6Either, foldEither, isLeft, isRight, left, right } from '../../shared/either'
import { setOptionalProp } from '../../shared/object-utils'
import type { ProjectContentTreeRoot } from '../../../components/assets'
import { getContentsTreeFileFromElements } from '../../../components/assets'
import { dropLast, last } from '../../shared/array-utils'
import {
  getParentDirectory,
  getPartsFromPath,
  makePathFromParts,
  normalizePath,
  stripTrailingSlash,
} from '../../../utils/path-utils'
import type { MapLike } from 'typescript'

import LRU from 'lru-cache'
import type { BuiltInDependencies } from './built-in-dependencies-list'
import { getFilePathMappings } from '../../model/project-file-utils'
import { applyFilePathMappingsToFilePath } from '../../../core/workers/common/project-file-utils'
import { getFileExtension } from '../../shared/file-utils'

const partialPackageJsonCache: LRU<string, ParseResult<PartialPackageJsonDefinition>> = new LRU({
  max: 20,
})
function getPartialPackageJson(contents: string): ParseResult<PartialPackageJsonDefinition> {
  const fromCache = partialPackageJsonCache.get(contents)
  if (fromCache == null) {
    const jsonParsed = JSON.parse(contents)
    const result = parsePartialPackageJsonDefinition(jsonParsed)
    partialPackageJsonCache.set(contents, result)
    return result
  } else {
    return fromCache
  }
}

interface ResolveSuccess<T> {
  type: 'RESOLVE_SUCCESS'
  success: T
}

function resolveSuccess<T>(success: T): ResolveSuccess<T> {
  return {
    type: 'RESOLVE_SUCCESS',
    success: success,
  }
}

interface ResolveNotPresent {
  type: 'RESOLVE_NOT_PRESENT'
}

const resolveNotPresent: ResolveNotPresent = {
  type: 'RESOLVE_NOT_PRESENT',
}

interface ResolveSuccessIgnoreModule {
  type: 'RESOLVE_SUCCESS_IGNORE_MODULE'
}

export function isResolveSuccessIgnoreModule<T>(
  resolveResult: ResolveResult<T>,
): resolveResult is ResolveSuccessIgnoreModule {
  return resolveResult.type === 'RESOLVE_SUCCESS_IGNORE_MODULE'
}

const resolveSuccessIgnoreModule: ResolveSuccessIgnoreModule = {
  type: 'RESOLVE_SUCCESS_IGNORE_MODULE',
}

type ResolveResult<T> = ResolveNotPresent | ResolveSuccessIgnoreModule | ResolveSuccess<T>

export function isResolveSuccess<T>(
  resolveResult: ResolveResult<T>,
): resolveResult is ResolveSuccess<T> {
  return resolveResult.type === 'RESOLVE_SUCCESS'
}

interface FoundFile {
  path: string
  file: ESCodeFile | ESRemoteDependencyPlaceholder
}

type FileLookupResult = ResolveResult<FoundFile>

function fileLookupResult(
  path: string,
  file: ESCodeFile | ESRemoteDependencyPlaceholder | null,
): FileLookupResult {
  if (file == null) {
    return resolveNotPresent
  } else {
    return resolveSuccess({
      path: path,
      file: file,
    })
  }
}

type FileLookupFn = (path: string, importOrigin: string) => FileLookupResult

function allNodeModulesPathsForPath(path: string): Array<string> {
  const pathParts = getPartsFromPath(path)
  return ['/node_modules/'].concat(
    pathParts.map((_part, index) =>
      makePathFromParts(pathParts.slice(0, index + 1).concat('node_modules')),
    ),
  )
}

function nodeModulesFileLookup(
  nodeModules: NodeModules,
  path: string,
  originPath: string,
  lookupFn: FileLookupFn,
): FileLookupResult {
  const nodeModulesPaths = allNodeModulesPathsForPath(originPath).reverse()
  const targetOrigins = [originPath, ...nodeModulesPaths]

  for (const targetOrigin of targetOrigins) {
    const packageJsonLookupFn = fileLookupForNodeModules(nodeModules)

    const pathForLookup = path.startsWith('/')
      ? path
      : `${stripTrailingSlash(targetOrigin)}/${path}`

    // Load package exports
    const packageExportsResult = packageExportsLookup(
      path,
      pathForLookup,
      packageJsonLookupFn,
      lookupFn,
    )
    if (isResolveSuccess(packageExportsResult)) {
      return packageExportsResult
    }

    const pathParts = getPartsFromPath(pathForLookup)
    const normalisedPathParts = normalizePath(pathParts)

    // Load as file
    const asFileResult = abstractFileLookup(packageJsonLookupFn, normalisedPathParts)
    if (isResolveSuccess(asFileResult)) {
      return asFileResult
    }

    // Load as directory
    const asDirectoryResult = abstractDirectoryLookup(packageJsonLookupFn, normalisedPathParts)
    if (isResolveSuccess(asDirectoryResult)) {
      return asDirectoryResult
    }
  }

  return resolveNotPresent
}

function getProjectFileContentsAsString(file: ProjectFile): string | null {
  switch (file.type) {
    case 'ASSET_FILE':
      return ''
    case 'DIRECTORY':
      return null
    case 'IMAGE_FILE':
      return file.base64 ?? ''
    case 'TEXT_FILE':
      return file.fileContents.code
    default:
      const _exhaustiveCheck: never = file
      throw new Error(`Unhandled file type ${JSON.stringify(file)}`)
  }
}

type LocalFileLookup = (pathParts: string[]) => FileLookupResult
function fileLookupForProjectContents(projectContents: ProjectContentTreeRoot): LocalFileLookup {
  return (pathParts: string[]) => {
    const projectFile = getContentsTreeFileFromElements(projectContents, pathParts)
    if (projectFile == null) {
      return resolveNotPresent
    } else {
      const fileContents = getProjectFileContentsAsString(projectFile)
      if (fileContents == null) {
        return resolveNotPresent
      } else {
        const filename = makePathFromParts(pathParts) // TODO Is this still correct?
        return fileLookupResult(filename, esCodeFile(fileContents, 'PROJECT_CONTENTS', filename))
      }
    }
  }
}

function fileLookupForNodeModules(nodeModules: NodeModules): LocalFileLookup {
  return (pathParts: string[]) => {
    const filename = makePathFromParts(pathParts)
    const nodeModulesFile = nodeModules[filename]
    if (nodeModulesFile == null) {
      return resolveNotPresent
    } else {
      return fileLookupResult(filename, nodeModulesFile)
    }
  }
}

function projectContentsFileLookup(
  projectContents: ProjectContentTreeRoot,
  pathParts: string[],
): FileLookupResult {
  return abstractFileLookup(fileLookupForProjectContents(projectContents), pathParts)
}

function abstractFileLookup(
  localFileLookup: LocalFileLookup, // FIXME That's a bad name dawg
  pathParts: string[],
): FileLookupResult {
  const normalisedPathParts = normalizePath(pathParts)
  const pathToFile = dropLast(normalisedPathParts)
  const lastPart = last(normalisedPathParts)
  if (lastPart == null) {
    return resolveNotPresent
  }

  // If X is a file, load X as its file extension format
  const asFileResult = localFileLookup(normalisedPathParts)
  if (isResolveSuccess(asFileResult)) {
    return asFileResult
  }

  // If X.js(x) is a file
  const withJs = lastPart + '.js'
  const withJsResult = localFileLookup(pathToFile.concat(withJs))
  if (isResolveSuccess(withJsResult)) {
    return withJsResult
  }

  const withJsx = lastPart + '.jsx'
  const withJsxResult = localFileLookup(pathToFile.concat(withJsx))
  if (isResolveSuccess(withJsxResult)) {
    return withJsxResult
  }

  // If X.json is a file, load X.json to a JavaScript Object
  // TODO this also needs JSON parsing
  const withJson = lastPart + '.json'
  const withJsonResult = localFileLookup(pathToFile.concat(withJson))
  if (isResolveSuccess(withJsonResult)) {
    return withJsonResult
  }

  return resolveNotPresent
}

function projectContentsDirectoryLookup(
  projectContents: ProjectContentTreeRoot,
  pathParts: string[],
): FileLookupResult {
  const innerLookup = fileLookupForProjectContents(projectContents)
  return abstractDirectoryLookup(innerLookup, pathParts)
}

function abstractDirectoryLookup(
  localFileLookup: LocalFileLookup, // FIXME That's a bad name dawg
  pathParts: string[],
): FileLookupResult {
  const normalisedPathParts = normalizePath(pathParts)

  // If X/package.json is a file
  const jsonFileResult = localFileLookup(normalisedPathParts.concat('package.json'))
  if (isResolveSuccess(jsonFileResult) && isEsCodeFile(jsonFileResult.success.file)) {
    // Parse X/package.json, and look for "browser", "main", "module" or "name" field
    // TODO The spec only includes "browser" and "main" here...
    const mainEntryPath = processPackageJson(
      jsonFileResult.success.file.fileContents,
      normalisedPathParts,
    )

    if (isResolveSuccess(mainEntryPath)) {
      // try loading the entry path as a file
      const mainEntryResult = localFileLookup(mainEntryPath.success)
      if (isResolveSuccess(mainEntryResult)) {
        return mainEntryResult
      }

      // attempt to load it as a folder with an index.js
      const mainEntryIndexJSResult = localFileLookup(mainEntryPath.success.concat('index.js'))
      if (isResolveSuccess(mainEntryIndexJSResult)) {
        return mainEntryIndexJSResult
      }

      // attempt to load the containing folder as a folder with an index.js (this is deprecated in the spec so maybe we shouldn't)
      const containingFolderIndexJSResult = localFileLookup(normalisedPathParts.concat('index.js'))
      if (isResolveSuccess(containingFolderIndexJSResult)) {
        return containingFolderIndexJSResult
      }
    }
  }

  // no main entry, so attempt to load the containing folder as a folder with an index.js
  const containingFolderIndexJSResult = localFileLookup(normalisedPathParts.concat('index.js'))
  if (isResolveSuccess(containingFolderIndexJSResult)) {
    return containingFolderIndexJSResult
  }

  return resolveNotPresent
}

type PackageLookupResult = { packageJsonFileResult: FileLookupResult; packageJsonDir: string }
function findClosestPackageScopeToPath(
  path: string,
  localFileLookup: LocalFileLookup,
): PackageLookupResult {
  const originPathPartsToTest = getPartsFromPath(path)
  const allPossiblePackageJsonPaths = [['package.json']]
    .concat(
      originPathPartsToTest.map((_part, index) =>
        originPathPartsToTest.slice(0, index + 1).concat('package.json'),
      ),
    )
    .reverse()

  for (const possiblePath of allPossiblePackageJsonPaths) {
    const packageJsonFileResult = localFileLookup(possiblePath)
    if (isResolveSuccess(packageJsonFileResult)) {
      return {
        packageJsonFileResult: packageJsonFileResult,
        packageJsonDir: makePathFromParts(possiblePath.slice(0, -1)),
      }
    }
  }

  return {
    packageJsonFileResult: resolveNotPresent,
    packageJsonDir: '',
  }
}

// Loading a module from the imports field mapping https://nodejs.org/api/packages.html#imports
function packageImportsLookup(
  path: string,
  originPath: string,
  packageJsonLookupFn: LocalFileLookup,
  lookupFn: FileLookupFn,
): FileLookupResult {
  return packageImportsExportsLookup(path, originPath, packageJsonLookupFn, lookupFn, 'imports')
}

function packageExportsLookup(
  path: string,
  originPath: string,
  packageJsonLookupFn: LocalFileLookup,
  lookupFn: FileLookupFn,
): FileLookupResult {
  // Find the closest package scope SCOPE
  const { packageJsonFileResult, packageJsonDir } = findClosestPackageScopeToPath(
    originPath,
    packageJsonLookupFn,
  )

  if (isResolveSuccess(packageJsonFileResult) && isEsCodeFile(packageJsonFileResult.success.file)) {
    let possiblePackageJson: ParseResult<PartialPackageJsonDefinition>
    try {
      possiblePackageJson = getPartialPackageJson(packageJsonFileResult.success.file.fileContents)
    } catch {
      return resolveNotPresent
    }
    return foldEither(
      (_) => resolveNotPresent,
      (packageJson) => {
        const nameEntry = packageJson.name
        if (nameEntry == null || !path.startsWith(nameEntry)) {
          return resolveNotPresent
        }

        return innerPackageImportsExportsLookup(
          packageJson,
          `.${path.slice(nameEntry.length)}`,
          packageJsonDir,
          lookupFn,
          'exports',
        )
      },
      possiblePackageJson,
    )
  }

  return resolveNotPresent
}

function innerPackageImportsExportsLookup(
  packageJson: PartialPackageJsonDefinition,
  path: string,
  packageJsonDir: string,
  lookupFn: FileLookupFn,
  importsOrExportsField: 'imports' | 'exports',
): FileLookupResult {
  const importsOrExportsEntry = packageJson[importsOrExportsField]
  if (importsOrExportsEntry == null) {
    return resolveNotPresent
  }

  // FIXME partial path lookup
  const importsOrExportsResult =
    typeof importsOrExportsEntry === 'string' ? importsOrExportsEntry : importsOrExportsEntry[path]
  if (importsOrExportsResult == null) {
    return resolveNotPresent
  }

  if (typeof importsOrExportsResult === 'string') {
    // Lookup the import relative to the package.json we have found
    return lookupFn(importsOrExportsResult, packageJsonDir)
  }

  // Otherwise importsOrExportsResult is an object, so now we need to check for the specific keys we're interested in
  const defaultImportsOrExportsEntry = importsOrExportsResult['default']
  const browserImportsOrExportsEntry = importsOrExportsResult['browser'] // The nodejs algorithm uses `node` here
  const requireImportsOrExportsEntry = importsOrExportsResult['require']

  if (defaultImportsOrExportsEntry != null) {
    const fileToLookup =
      typeof defaultImportsOrExportsEntry === 'string'
        ? defaultImportsOrExportsEntry
        : defaultImportsOrExportsEntry['default']
    if (fileToLookup != null) {
      return lookupFn(fileToLookup, packageJsonDir)
    }
  }
  if (browserImportsOrExportsEntry != null) {
    const fileToLookup =
      typeof browserImportsOrExportsEntry === 'string'
        ? browserImportsOrExportsEntry
        : browserImportsOrExportsEntry['default']
    if (fileToLookup != null) {
      return lookupFn(fileToLookup, packageJsonDir)
    }
  }
  if (requireImportsOrExportsEntry != null) {
    const fileToLookup =
      typeof requireImportsOrExportsEntry === 'string'
        ? requireImportsOrExportsEntry
        : requireImportsOrExportsEntry['default']
    if (fileToLookup != null) {
      return lookupFn(fileToLookup, packageJsonDir)
    }
  }

  return resolveNotPresent
}

function packageImportsExportsLookup(
  path: string,
  originPath: string,
  packageJsonLookupFn: LocalFileLookup,
  lookupFn: FileLookupFn,
  importsOrExportsField: 'imports' | 'exports',
): FileLookupResult {
  // Find the closest package scope SCOPE
  const { packageJsonFileResult, packageJsonDir } = findClosestPackageScopeToPath(
    originPath,
    packageJsonLookupFn,
  )

  if (isResolveSuccess(packageJsonFileResult) && isEsCodeFile(packageJsonFileResult.success.file)) {
    let possiblePackageJson: ParseResult<PartialPackageJsonDefinition>
    try {
      possiblePackageJson = getPartialPackageJson(packageJsonFileResult.success.file.fileContents)
    } catch {
      return resolveNotPresent
    }
    return foldEither(
      (_) => resolveNotPresent,
      (packageJson) =>
        innerPackageImportsExportsLookup(
          packageJson,
          path,
          packageJsonDir,
          lookupFn,
          importsOrExportsField,
        ),
      possiblePackageJson,
    )
  }

  return resolveNotPresent
}

type ImportsExportsObjectField = MapLike<string | MapLike<string | null | MapLike<string | null>>>
type ExportsField = string | ImportsExportsObjectField

interface PartialPackageJsonDefinition {
  name?: string
  main?: string
  module?: string
  browser?: string | MapLike<string | false>
  imports?: ImportsExportsObjectField
  exports?: ExportsField
}

const importsExportsObjectParser = parseObject(
  parseAlternative<string | MapLike<string | null | MapLike<string | null>>>(
    [
      parseString,
      parseObject(
        parseAlternative<string | null | MapLike<string | null>>(
          [
            parseString,
            parseNull,
            parseObject(
              parseAlternative<string | null>(
                [parseString, parseNull],
                `package.imports and package.exports replacement entries must be either string or null`,
              ),
            ),
          ],
          `package.imports and package.exports replacement entries must be either string or null`,
        ),
      ),
    ],
    'package.imports and package.exports object must be an object with type {[key: string]: string | null}',
  ),
)

const exportsParser = parseAlternative<string | ImportsExportsObjectField>(
  [parseString, importsExportsObjectParser],
  'package.imports and package.exports field must either be a string or an object with type {[key: string]: string | null}',
)

export function parsePartialPackageJsonDefinition(
  value: unknown,
): ParseResult<PartialPackageJsonDefinition> {
  return applicative6Either(
    (name, main, module, browser, imports, exports) => {
      let result: PartialPackageJsonDefinition = {}
      setOptionalProp(result, 'name', name)
      setOptionalProp(result, 'main', main)
      setOptionalProp(result, 'module', module)
      setOptionalProp(result, 'browser', browser)
      setOptionalProp(result, 'imports', imports)
      setOptionalProp(result, 'exports', exports)
      return result
    },
    optionalObjectKeyParser(parseString, 'name')(value),
    optionalObjectKeyParser(parseString, 'main')(value),
    optionalObjectKeyParser(parseString, 'module')(value),
    optionalObjectKeyParser(
      parseAlternative<string | MapLike<string | false>>(
        [
          parseString,
          parseObject(
            parseAlternative<string | false>(
              [parseString, parseFalse],
              `package.browser replacement entries must be either string or 'false' for ignoring a package`,
            ),
          ),
        ],
        'package.browser field must either be a string or an object with type {[key: string]: string | false}',
      ),
      'browser',
    )(value),
    optionalObjectKeyParser(importsExportsObjectParser, 'imports')(value),
    optionalObjectKeyParser(exportsParser, 'exports')(value),
  )
}

function processPackageJson(
  potentiallyJsonCode: string,
  containerFolder: string[],
): ResolveResult<Array<string>> {
  let possiblePackageJson: ParseResult<PartialPackageJsonDefinition>
  try {
    possiblePackageJson = getPartialPackageJson(potentiallyJsonCode)
  } catch {
    return resolveNotPresent
  }
  return foldEither(
    (_) => resolveNotPresent,
    (packageJson) => {
      const moduleName: string | null = packageJson.name ?? null
      const browserEntry: string | MapLike<string | false> | null = packageJson.browser ?? null
      const mainEntry: string | null = packageJson.main ?? null
      const moduleEntry: string | null = packageJson.module ?? null

      if (browserEntry != null && typeof browserEntry === 'string') {
        return resolveSuccess(
          normalizePath([...containerFolder, ...getPartsFromPath(browserEntry)]),
        )
      } else if (mainEntry != null) {
        return resolveSuccess(normalizePath([...containerFolder, ...getPartsFromPath(mainEntry)]))
      } else if (moduleEntry != null) {
        return resolveSuccess(normalizePath([...containerFolder, ...getPartsFromPath(moduleEntry)]))
      } else if (moduleName != null) {
        return resolveSuccess(normalizePath([...containerFolder, ...getPartsFromPath(moduleName)]))
      }

      return resolveNotPresent
    },
    possiblePackageJson,
  )
}

function findPackageJsonForPath(
  fileLookupFn: FileLookupFn,
  originPathParts: string[],
): FileLookupResult {
  // 1. look for <origin>/package.json
  const packageJsonResult = fileLookupFn(`./package.json`, makePathFromParts(originPathParts))
  if (isResolveSuccess(packageJsonResult)) {
    return packageJsonResult
  } else {
    // 2. repeat in the parent folder
    if (originPathParts.length === 0) {
      // we exhausted all folders without success
      return resolveNotPresent
    } else {
      return findPackageJsonForPath(fileLookupFn, originPathParts.slice(0, -1))
    }
  }
}

function applyBrowserFieldReplacements(
  browserField: MapLike<string | false>,
  toImport: string,
): Either<boolean, string> {
  // The pkg.browser field can contain potential substitutions https://github.com/defunctzombie/package-browser-field-spec
  if (toImport in browserField) {
    const replacement = browserField[toImport]
    if (replacement === false) {
      // special case, null substitution means ignoring a module, resolving it as null
      return left(false)
    } else {
      // the browser field is sneaky and supports recursive replacements
      return applyBrowserFieldReplacements(browserField, replacement)
    }
  }

  // we couldn't find any replacements, return the original toImport string
  return right(toImport)
}

function findSubstitutionsForImport(
  fileLookupFn: FileLookupFn,
  importOrigin: string,
  toImport: string,
): Either<boolean, string> {
  const originPackageJson = findPackageJsonForPath(fileLookupFn, getPartsFromPath(importOrigin))
  if (isResolveSuccess(originPackageJson) && isEsCodeFile(originPackageJson.success.file)) {
    try {
      const possiblePackageJson = getPartialPackageJson(originPackageJson.success.file.fileContents)
      if (isRight(possiblePackageJson)) {
        if (
          possiblePackageJson.value.browser != null &&
          typeof possiblePackageJson.value.browser === 'object'
        ) {
          return applyBrowserFieldReplacements(possiblePackageJson.value.browser, toImport)
        }
      }
    } catch {
      // empty catch block
    }
  }
  // Fail fallback – we couldn't find a substitution
  return right(toImport)
}

function resolveModuleAndApplySubstitutions(
  projectContents: ProjectContentTreeRoot,
  nodeModules: NodeModules,
  importOrigin: string,
  toImport: string,
): FileLookupResult {
  // 1 Check if it's a core module - Node JS only
  // 2 If path begins with /, start searching from file system root
  // 3 If path begins with /, ./, ../
  //   3.1 Attempt to load file
  //   3.2 Attempt to load directory
  //   3.3 Throw "not found"
  // 4 If path begins with #
  //   4.1 Attempt to load package imports
  // 5 Attempt to load package self
  // 6 Attempt to load node modules
  // 7 Throw "not found"

  function lookupFn(path: string, innerImportOrigin: string): FileLookupResult {
    if (path.startsWith('/') || path.startsWith('./') || path.startsWith('../')) {
      const pathForLookup = path.startsWith('/')
        ? path
        : `${stripTrailingSlash(innerImportOrigin)}/${path}`
      const pathParts = getPartsFromPath(pathForLookup)
      const lastPart = last(pathParts)
      const projectContentsFileLookupResult = projectContentsFileLookup(projectContents, pathParts)

      if (isResolveSuccess(projectContentsFileLookupResult)) {
        return projectContentsFileLookupResult
      }
      if (lastPart == null || getFileExtension(lastPart) === '') {
        // Don't attempt the directory lookup if the target is an actual file that we failed to find
        const projectContentsDirectoryLookupResult = projectContentsDirectoryLookup(
          projectContents,
          pathParts,
        )
        if (isResolveSuccess(projectContentsDirectoryLookupResult)) {
          return projectContentsDirectoryLookupResult
        }
      }
    } else if (path.startsWith('#')) {
      // load package imports
      return packageImportsLookup(
        path,
        innerImportOrigin,
        innerImportOrigin.startsWith('/node_modules')
          ? fileLookupForNodeModules(nodeModules)
          : fileLookupForProjectContents(projectContents),
        lookupFn,
      )
    }

    // load package self
    const packageExportsResult = packageExportsLookup(
      path,
      innerImportOrigin,
      fileLookupForProjectContents(projectContents),
      lookupFn,
    )

    if (isResolveSuccess(packageExportsResult)) {
      return packageExportsResult
    }

    // finally, load from node modules
    return nodeModulesFileLookup(
      nodeModules,
      path,
      innerImportOrigin.startsWith('/node_modules') ? innerImportOrigin : '/node_modules',
      lookupFn,
    )
  }

  // We first need to check for the presence of a `browser` field in the current module's package.json, as that
  // can suggest an alternative mapping to use for modules it is importing https://github.com/defunctzombie/package-browser-field-spec
  const substitutedImport: Either<boolean, string> = findSubstitutionsForImport(
    lookupFn,
    importOrigin,
    toImport,
  )

  if (isLeft(substitutedImport)) {
    return resolveSuccessIgnoreModule
  } else {
    const filePathMappings = getFilePathMappings(projectContents)
    const unAliasedImport = applyFilePathMappingsToFilePath(
      substitutedImport.value,
      filePathMappings,
    )

    return lookupFn(unAliasedImport, getParentDirectory(importOrigin))
  }
}

export function resolveModule(
  projectContents: ProjectContentTreeRoot,
  nodeModules: NodeModules,
  importOrigin: string,
  toImport: string,
): FileLookupResult {
  return resolveModuleAndApplySubstitutions(projectContents, nodeModules, importOrigin, toImport)
}

export function resolveModulePath(
  projectContents: ProjectContentTreeRoot,
  nodeModules: NodeModules,
  importOrigin: string,
  toImport: string,
): Either<string, string> {
  const resolveResult = resolveModule(projectContents, nodeModules, importOrigin, toImport)
  if (isResolveSuccess(resolveResult)) {
    return right(resolveResult.success.path)
  } else {
    return left(`Cannot find module ${toImport}`)
  }
}

export function resolveModulePathIncludingBuiltIns(
  builtInDependencies: BuiltInDependencies,
  projectContents: ProjectContentTreeRoot,
  nodeModules: NodeModules,
  importOrigin: string,
  toImport: string,
): Either<string, string> {
  // Resolve against the built in dependencies before falling back to `resolveModulePath`.
  for (const builtInDependency of builtInDependencies) {
    if (builtInDependency.moduleName === toImport) {
      return right(builtInDependency.moduleName)
    }
  }
  return resolveModulePath(projectContents, nodeModules, importOrigin, toImport)
}
