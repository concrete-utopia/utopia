import type { MapLike } from 'typescript'
import type { ProjectContentTreeRoot } from '../../../components/assets'
import { getContentsTreeFileFromElements } from '../../../components/assets'
import { applyFilePathMappingsToFilePath } from '../../../core/workers/common/project-file-utils'
import {
  getParentDirectory,
  getPartsFromPath,
  makePathFromParts,
  normalizePath,
  stripTrailingSlash,
} from '../../../utils/path-utils'
import type { ParseResult } from '../../../utils/value-parser-utils'
import { getFilePathMappings } from '../../model/project-file-utils'
import { dropLast, last } from '../../shared/array-utils'
import type { Either } from '../../shared/either'
import { foldEither, isLeft, isRight, left, right } from '../../shared/either'
import type { NodeModules, ProjectFile } from '../../shared/project-file-types'
import { esCodeFile, isEsCodeFile } from '../../shared/project-file-types'
import type { BuiltInDependencies } from './built-in-dependencies-list'
import { loadPackageSelf, loadPackageImports, loadPackageExports } from './module-resolution-esm'
import type {
  FileForPathParts,
  FileLookupFn,
  FileLookupResult,
  PartialPackageJsonDefinition,
} from './module-resolution-utils'
import {
  fileLookupResult,
  getPartialPackageJson,
  isResolveSuccess,
  resolveNotPresent,
  resolveSuccessIgnoreModule,
} from './module-resolution-utils'

// Using the logic from https://nodejs.org/api/modules.html#all-together

function loadAsFile(
  projectContents: ProjectContentTreeRoot,
  pathParts: string[],
): FileLookupResult {
  return abstractLoadAsFile(projectContentsFileForPathParts(projectContents), pathParts)
}

// LOAD_AS_FILE(X)
function abstractLoadAsFile(
  fileForPathParts: FileForPathParts,
  pathParts: string[],
): FileLookupResult {
  const normalisedPathParts = normalizePath(pathParts)
  const pathToFile = dropLast(normalisedPathParts)
  const lastPart = last(normalisedPathParts)
  if (lastPart == null) {
    return resolveNotPresent
  }

  // If X is a file, load X as its file extension format
  const asFileResult = fileForPathParts(normalisedPathParts)
  if (isResolveSuccess(asFileResult)) {
    return asFileResult
  }

  // If X.js(x) is a file
  const withJs = lastPart + '.js'
  const withJsResult = fileForPathParts(pathToFile.concat(withJs))
  if (isResolveSuccess(withJsResult)) {
    return withJsResult
  }

  const withJsx = lastPart + '.jsx'
  const withJsxResult = fileForPathParts(pathToFile.concat(withJsx))
  if (isResolveSuccess(withJsxResult)) {
    return withJsxResult
  }

  // If X.json is a file, load X.json to a JavaScript Object
  const withJson = lastPart + '.json'
  const withJsonResult = fileForPathParts(pathToFile.concat(withJson))
  if (isResolveSuccess(withJsonResult)) {
    return withJsonResult
  }

  return resolveNotPresent
}

function processPackageJson(potentiallyJsonCode: string): string | null {
  let possiblePackageJson: ParseResult<PartialPackageJsonDefinition>
  try {
    possiblePackageJson = getPartialPackageJson(potentiallyJsonCode)
  } catch {
    return null
  }
  return foldEither(
    (_) => null,
    (packageJson) => {
      // const moduleName: string | null = packageJson.name ?? null
      const browserEntry: string | MapLike<string | false> | null = packageJson.browser ?? null
      const mainEntry: string | null = packageJson.main ?? null
      // const moduleEntry: string | null = packageJson.module ?? null

      if (browserEntry != null && typeof browserEntry === 'string') {
        return browserEntry
      } else if (mainEntry != null) {
        return mainEntry
        // } else if (moduleEntry != null) {
        //   return moduleEntry
        // } else if (moduleName != null) {
        //   return moduleName
      }

      return null
    },
    possiblePackageJson,
  )
}

function loadAsDirectory(
  projectContents: ProjectContentTreeRoot,
  pathParts: string[],
): FileLookupResult {
  return abstractLoadAsDirectory(projectContentsFileForPathParts(projectContents), pathParts)
}

// LOAD_AS_DIRECTORY(X)
function abstractLoadAsDirectory(
  fileForPathParts: FileForPathParts,
  pathParts: string[],
): FileLookupResult {
  const normalisedPathParts = normalizePath(pathParts) // X

  // If X/package.json is a file
  const jsonFileResult = fileForPathParts(normalisedPathParts.concat('package.json'))
  if (isResolveSuccess(jsonFileResult) && isEsCodeFile(jsonFileResult.success.file)) {
    // Parse X/package.json, and look for "browser" or "main" field
    const mainEntryPath = processPackageJson(jsonFileResult.success.file.fileContents)

    if (mainEntryPath != null) {
      // let M = X + (json main field)
      const mainEntryPathParts = [...normalisedPathParts, ...getPartsFromPath(mainEntryPath)]

      // LOAD_AS_FILE(M)
      const mainEntryResult = abstractLoadAsFile(fileForPathParts, mainEntryPathParts)
      if (isResolveSuccess(mainEntryResult)) {
        return mainEntryResult
      }

      // LOAD_INDEX(M)
      const loadIndexMResult = loadIndex(fileForPathParts, mainEntryPathParts)
      if (isResolveSuccess(loadIndexMResult)) {
        return loadIndexMResult
      }

      // LOAD_INDEX(X) DEPRECATED - note that this is only deprecated in the case where "main" is truthy
      return loadIndex(fileForPathParts, normalisedPathParts)
    }
  }

  // If "main" is a falsy value
  // LOAD_INDEX(X)
  return loadIndex(fileForPathParts, normalisedPathParts)
}

// LOAD_INDEX(X)
function loadIndex(fileForPathParts: FileForPathParts, pathParts: string[]): FileLookupResult {
  // 1. If X/index.js is a file
  //   a. Find the closest package scope SCOPE to X.
  //   b. If no scope was found, load X/index.js as a CommonJS module. STOP.
  //   c. If the SCOPE/package.json contains "type" field,
  //     1. If the "type" field is "module", load X/index.js as an ECMAScript module. STOP.
  //     2. Else, load X/index.js as an CommonJS module. STOP.
  // Note: We don't attempt to load as an ECMAScript, as we can't - instead we'll try to load whatever
  // it is as CommonJS, and if it turns out to be an esm we'll attempt to transpile it to CommonJS

  const indexJSResult = abstractLoadAsFile(fileForPathParts, pathParts.concat('index.js'))
  if (isResolveSuccess(indexJSResult)) {
    return indexJSResult
  }

  // If X/index.json is a file, parse X/index.json to a JavaScript object
  return abstractLoadAsFile(fileForPathParts, pathParts.concat('index.json'))

  // If X/index.node is a file, load X/index.node as binary addon - not applicable
}

// NODE_MODULES_PATHS
function nodeModulesPaths(path: string): Array<string> {
  // let PARTS = path split(START)
  const pathParts = getPartsFromPath(path)

  // let DIRS = []
  let dirs: Array<string> = []

  // The algorithm builds dirs in descending order, but we'll do it in ascending and reverse
  pathParts.forEach((part, index) => {
    // if PARTS[I] = "node_modules" CONTINUE
    if (part === 'node_modules') {
      return
    }

    // DIR = path join(PARTS[0 .. I] + "node_modules")
    const dir = makePathFromParts(pathParts.slice(0, index + 1).concat('node_modules'))

    // DIRS = DIR + DIRS
    dirs.push(dir)
  })
  dirs.reverse()

  // return DIRS + GLOBAL_FOLDERS
  return dirs.concat('/node_modules/')
}

// LOAD_NODE_MODULES
function loadNodeModules(
  nodeModules: NodeModules,
  path: string,
  originPath: string,
  lookupFn: FileLookupFn,
): FileLookupResult {
  const packageJsonLookupFn = nodeModulesFileForPathParts(nodeModules)

  // let DIRS = NODE_MODULES_PATHS(START)
  const dirs = nodeModulesPaths(originPath).reverse()
  const targetOrigins = [originPath, ...dirs]

  for (const targetOrigin of targetOrigins) {
    // for each DIR in DIRS:

    // LOAD_PACKAGE_EXPORTS(X, DIR)
    const packageExportsResult = loadPackageExports(
      path,
      targetOrigin,
      packageJsonLookupFn,
      lookupFn,
    )
    if (isResolveSuccess(packageExportsResult)) {
      return packageExportsResult
    }

    const pathForLookup = path.startsWith('/')
      ? path
      : `${stripTrailingSlash(targetOrigin)}/${path}`

    const pathParts = getPartsFromPath(pathForLookup)
    const normalisedPathParts = normalizePath(pathParts)

    // LOAD_AS_FILE(DIR/X)
    const asFileResult = abstractLoadAsFile(packageJsonLookupFn, normalisedPathParts)
    if (isResolveSuccess(asFileResult)) {
      return asFileResult
    }

    // LOAD_AS_DIRECTORY(DIR/X)
    const asDirectoryResult = abstractLoadAsDirectory(packageJsonLookupFn, normalisedPathParts)
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

function projectContentsFileForPathParts(
  projectContents: ProjectContentTreeRoot,
): FileForPathParts {
  return (pathParts: string[]) => {
    const projectFile = getContentsTreeFileFromElements(projectContents, pathParts)
    if (projectFile == null) {
      return resolveNotPresent
    } else {
      const fileContents = getProjectFileContentsAsString(projectFile)
      if (fileContents == null) {
        return resolveNotPresent
      } else {
        const filename = makePathFromParts(pathParts)
        return fileLookupResult(filename, esCodeFile(fileContents, 'PROJECT_CONTENTS', filename))
      }
    }
  }
}

function nodeModulesFileForPathParts(nodeModules: NodeModules): FileForPathParts {
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
  // require(X) from module at path Y
  function lookupFn(path: string, innerImportOrigin: string): FileLookupResult {
    // 1 Check if it's a core module - this first step is irrelevant to us, since the browser has no core modules
    if (path.startsWith('/') || path.startsWith('./') || path.startsWith('../')) {
      // pathForLookup here is Y + X
      const pathForLookup = path.startsWith('/') // If X begins with '/' set Y to be the file system root
        ? path
        : `${stripTrailingSlash(innerImportOrigin)}/${path}`
      const pathParts = getPartsFromPath(pathForLookup)

      // LOAD_AS_FILE(Y + X)
      const projectContentsFileLookupResult = loadAsFile(projectContents, pathParts)

      if (isResolveSuccess(projectContentsFileLookupResult)) {
        return projectContentsFileLookupResult
      }

      // LOAD_AS_DIRECTORY(Y + X)
      const projectContentsDirectoryLookupResult = loadAsDirectory(projectContents, pathParts)
      if (isResolveSuccess(projectContentsDirectoryLookupResult)) {
        return projectContentsDirectoryLookupResult
      }
    } else if (path.startsWith('#')) {
      // If X begins with '#'
      // LOAD_PACKAGE_IMPORTS(X, dirname(Y))
      return loadPackageImports(
        path,
        innerImportOrigin,
        innerImportOrigin.startsWith('/node_modules')
          ? nodeModulesFileForPathParts(nodeModules)
          : projectContentsFileForPathParts(projectContents),
        lookupFn,
      )
    }

    // LOAD_PACKAGE_SELF(X, dirname(Y))
    const packageExportsResult = loadPackageSelf(
      path,
      innerImportOrigin,
      projectContentsFileForPathParts(projectContents),
      lookupFn,
    )

    if (isResolveSuccess(packageExportsResult)) {
      return packageExportsResult
    }

    // LOAD_NODE_MODULES(X, dirname(Y)), returning the result here either way in place of `throw 'not fount'`
    return loadNodeModules(
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
