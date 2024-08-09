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
} from '../../../utils/value-parser-utils'
import type { Either } from '../../shared/either'
import {
  applicative3Either,
  applicative4Either,
  applicative5Either,
  foldEither,
  isLeft,
  isRight,
  left,
  right,
} from '../../shared/either'
import { setOptionalProp } from '../../shared/object-utils'
import type { ProjectContentTreeRoot } from '../../../components/assets'
import { getContentsTreeFileFromElements } from '../../../components/assets'
import { dropLast, last } from '../../shared/array-utils'
import { getPartsFromPath, makePathFromParts, normalizePath } from '../../../utils/path-utils'
import type { MapLike } from 'typescript'

import LRU from 'lru-cache'
import type { BuiltInDependencies } from './built-in-dependencies-list'
import { getFilePathMappings } from '../../model/project-file-utils'
import { applyFilePathMappingsToFilePath } from '../../../core/workers/common/project-file-utils'

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

type FileLookupFn = (path: string[], direct: boolean) => FileLookupResult

function fallbackLookup(
  lookupFn: FileLookupFn,
  path: Array<string>,
  direct: boolean,
): FileLookupResult {
  const asIsPath = normalizePath(path)
  const pathToFile = dropLast(asIsPath)
  const lastPart = last(asIsPath)
  if (lastPart == null) {
    return resolveNotPresent
  } else {
    const asIsResult = lookupFn(asIsPath, direct)
    if (isResolveSuccess(asIsResult)) {
      return asIsResult
    }

    if (!direct) {
      const withJs = lastPart + '.js'
      const withJsResult = lookupFn(pathToFile.concat(withJs), direct)
      if (isResolveSuccess(withJsResult)) {
        return withJsResult
      }

      const withJsx = lastPart + '.jsx'
      const withJsxResult = lookupFn(pathToFile.concat(withJsx), direct)
      if (isResolveSuccess(withJsxResult)) {
        return withJsxResult
      }

      // TODO this also needs JSON parsing
      const withJson = lastPart + '.json'
      const withJsonResult = lookupFn(pathToFile.concat(withJson), direct)
      if (isResolveSuccess(withJsonResult)) {
        return withJsonResult
      }
    }

    return resolveNotPresent
  }
}

function nodeModulesFileLookup(
  nodeModules: NodeModules,
  path: Array<string>,
  direct: boolean,
): FileLookupResult {
  return fallbackLookup(
    (innerPath: string[], innerDirect: boolean) => {
      const filename = makePathFromParts(innerPath)
      return fileLookupResult(filename, nodeModules[filename])
    },
    path,
    direct,
  )
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

function projectContentsFileLookup(
  projectContents: ProjectContentTreeRoot,
  path: Array<string>,
  direct: boolean,
): FileLookupResult {
  return fallbackLookup(
    (innerPath: string[]) => {
      const projectFile = getContentsTreeFileFromElements(projectContents, innerPath)
      if (projectFile == null) {
        return resolveNotPresent
      } else {
        const fileContents = getProjectFileContentsAsString(projectFile)
        if (fileContents == null) {
          return resolveNotPresent
        } else {
          const filename = makePathFromParts(innerPath)
          return fileLookupResult(filename, esCodeFile(fileContents, 'PROJECT_CONTENTS', filename))
        }
      }
    },
    path,
    direct,
  )
}

interface PartialPackageJsonDefinition {
  name?: string
  main?: string
  module?: string
  browser?: string | MapLike<string | false>
  exports?: MapLike<string | MapLike<string>>
}

export function parsePartialPackageJsonDefinition(
  value: unknown,
): ParseResult<PartialPackageJsonDefinition> {
  return applicative5Either(
    (name, main, module, browser, exports) => {
      let result: PartialPackageJsonDefinition = {}
      setOptionalProp(result, 'name', name)
      setOptionalProp(result, 'main', main)
      setOptionalProp(result, 'module', module)
      setOptionalProp(result, 'browser', browser)
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
        'package.browser field must either be a string or an object with type {[key: string]: string}',
      ),
      'browser',
    )(value),
    optionalObjectKeyParser(
      parseObject(
        parseAlternative<string | MapLike<string>>(
          [parseString, parseObject(parseString)],
          'package.exports field must either an object with type {[key: string]: string | { [key: string]: string }}',
        ),
      ),
      'exports',
    )(value),
  )
}

function processPackageJson(
  potentiallyJsonCode: string,
  containerFolder: string[],
  localPath?: string,
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
      const exportsEntry: MapLike<string | MapLike<string>> | null = packageJson.exports ?? null

      const resolvedExportEntry =
        localPath != null && exportsEntry != null ? exportsEntry[localPath] : null
      if (typeof resolvedExportEntry === 'string') {
        return resolveSuccess(
          normalizePath([...containerFolder, ...getPartsFromPath(resolvedExportEntry)]),
        )
      } else if (
        resolvedExportEntry != null &&
        typeof resolvedExportEntry === 'object' &&
        resolvedExportEntry['require'] != null
      ) {
        return resolveSuccess(
          normalizePath([...containerFolder, ...getPartsFromPath(resolvedExportEntry['require'])]),
        )
      } else if (browserEntry != null && typeof browserEntry === 'string') {
        return resolveSuccess(
          normalizePath([...containerFolder, ...getPartsFromPath(browserEntry)]),
        )
      } else if (mainEntry != null) {
        return resolveSuccess(normalizePath([...containerFolder, ...getPartsFromPath(mainEntry)]))
      } else if (moduleEntry != null) {
        return resolveSuccess(normalizePath([...containerFolder, ...getPartsFromPath(moduleEntry)]))
      } else if (moduleName != null) {
        return resolveSuccess(normalizePath([...containerFolder, ...getPartsFromPath(moduleName)]))
      } else if (containerFolder.length > 0) {
        return resolveSuccess(normalizePath(containerFolder))
      }

      return resolveNotPresent
    },
    possiblePackageJson,
  )
}

function resolvePackageJson(
  fileLookupFn: FileLookupFn,
  packageJsonFolder: string[],
  localPath?: string,
): FileLookupResult {
  const normalizedFolderPath = normalizePath(packageJsonFolder)
  const folderPackageJson = fileLookupFn([...normalizedFolderPath, 'package.json'], true)
  if (isResolveSuccess(folderPackageJson) && isEsCodeFile(folderPackageJson.success.file)) {
    const mainEntryPath = processPackageJson(
      folderPackageJson.success.file.fileContents,
      normalizedFolderPath,
      localPath,
    )
    if (isResolveSuccess(mainEntryPath)) {
      // try loading the entry path as a file
      const mainEntryResult = fileLookupFn(mainEntryPath.success, false)
      if (isResolveSuccess(mainEntryResult)) {
        return mainEntryResult
      } else {
        // fallback to loading it as a folder with an index.js
        const indexJsPath = [...mainEntryPath.success, 'index']
        return fileLookupFn(indexJsPath, false)
      }
    } else {
      return resolveNotPresent
    }
  }
  return resolveNotPresent
}

function findPackageJsonForPath(fileLookupFn: FileLookupFn, origin: string[]): FileLookupResult {
  // 1. look for <origin>/package.json
  const packageJsonResult = fileLookupFn([...origin, 'package.json'], true)
  if (isResolveSuccess(packageJsonResult)) {
    return packageJsonResult
  } else {
    // 2. repeat in the parent folder
    if (origin.length === 0) {
      // we exhausted all folders without success
      return resolveNotPresent
    } else {
      return findPackageJsonForPath(fileLookupFn, origin.slice(0, -1))
    }
  }
}

function resolveNonRelativeModule(
  fileLookupFn: FileLookupFn,
  importOrigin: string[],
  toImport: string[],
): FileLookupResult {
  if (toImport[0] != null && toImport[0].startsWith('@')) {
    const moduleSpecifier = `${toImport[0]}/${toImport[1]}`
    const pathElements = [...importOrigin, 'node_modules', toImport[0], toImport[1]]
    const localPath = ['.', ...toImport.slice(2)].join('/')
    // 1. look for ./node_modules/<package_name>.js
    const packageNameResult = fileLookupFn(pathElements, false)
    if (isResolveSuccess(packageNameResult)) {
      return packageNameResult
    } else {
      // 2. look for ./node_modules/<package_name>/package.json
      const packageJsonResult = resolvePackageJson(fileLookupFn, pathElements, localPath)
      if (isResolveSuccess(packageJsonResult)) {
        return packageJsonResult
      } else {
        // 3. look for ./node_modules/<package_name>/index.js
        const indexJsPath = [...pathElements, 'index']
        const indexJSResult = fileLookupFn(indexJsPath, false)
        if (isResolveSuccess(indexJSResult)) {
          return indexJSResult
        } else {
          // 4. repeat in the parent folder
          if (importOrigin.length === 0) {
            // we exhausted all folders without success
            return resolveNotPresent
          } else {
            return resolveNonRelativeModule(fileLookupFn, importOrigin.slice(0, -1), toImport)
          }
        }
      }
    }
  } else {
    const pathElements = [...importOrigin, 'node_modules', ...toImport]
    // 1. look for ./node_modules/<package_name>.js
    const packageNameResult = fileLookupFn(pathElements, false)
    if (isResolveSuccess(packageNameResult)) {
      return packageNameResult
    } else {
      // 2. look for ./node_modules/<package_name>/package.json
      const packageJsonResult = resolvePackageJson(fileLookupFn, pathElements)
      if (isResolveSuccess(packageJsonResult)) {
        return packageJsonResult
      } else {
        // 3. look for ./node_modules/<package_name>/index.js
        const indexJsPath = [...pathElements, 'index']
        const indexJSResult = fileLookupFn(indexJsPath, false)
        if (isResolveSuccess(indexJSResult)) {
          return indexJSResult
        } else {
          // 4. repeat in the parent folder
          if (importOrigin.length === 0) {
            // we exhausted all folders without success
            return resolveNotPresent
          } else {
            return resolveNonRelativeModule(fileLookupFn, importOrigin.slice(0, -1), toImport)
          }
        }
      }
    }
  }
}

function resolveRelativeModule(
  fileLookupFn: FileLookupFn,
  importOrigin: string[],
  toImport: string[],
): FileLookupResult {
  const pathElements = [...importOrigin, ...toImport]
  // 1. look for a file named <import_name>
  const importNameResult = fileLookupFn(pathElements, false)
  if (isResolveSuccess(importNameResult)) {
    return importNameResult
  } else {
    // 2. look for <import_name>/package.json
    const packageJsonResult = resolvePackageJson(fileLookupFn, pathElements)
    if (isResolveSuccess(packageJsonResult)) {
      return packageJsonResult
    } else {
      // 3. look for <import_name>/index.js
      return fileLookupFn([...pathElements, 'index'], false)
    }
  }
}

// Module resolution logic based on what Node / Typescript does https://www.typescriptlang.org/docs/handbook/module-resolution.html#node

// TODO there's way more module resolution rules here https://parceljs.org/module_resolution.html#module-resolution

// TODO an even more comprehensive writeup https://nodejs.org/api/modules.html#modules_all_together

function resolveModuleInternal(
  fileLookupFn: FileLookupFn,
  importOrigin: string,
  toImport: string,
): FileLookupResult {
  if (toImport.startsWith('/')) {
    // absolute import
    return resolveRelativeModule(
      fileLookupFn,
      [], // this import is relative to the root
      getPartsFromPath(toImport),
    )
  } else {
    if (toImport.startsWith('.')) {
      return resolveRelativeModule(
        fileLookupFn,
        getPartsFromPath(importOrigin).slice(0, -1),
        getPartsFromPath(toImport),
      )
    } else {
      return resolveNonRelativeModule(
        fileLookupFn,
        getPartsFromPath(importOrigin).slice(0, -1),
        getPartsFromPath(toImport),
      )
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
  function lookupFn(path: string[], direct: boolean): FileLookupResult {
    const nodeModulesResult = nodeModulesFileLookup(nodeModules, path, direct)
    if (isResolveSuccess(nodeModulesResult)) {
      return nodeModulesResult
    } else {
      return projectContentsFileLookup(projectContents, path, direct)
    }
  }

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
    return resolveModuleInternal(lookupFn, importOrigin, unAliasedImport)
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
