import type { MapLike } from 'typescript'
import { getPartsFromPath, makePathFromParts, normalizePath } from '../../../utils/path-utils'
import type { ParseResult } from '../../../utils/value-parser-utils'
import {
  optionalObjectKeyParser,
  parseAlternative,
  parseFalse,
  parseNull,
  parseObject,
  parseString,
} from '../../../utils/value-parser-utils'
import { applicative6Either } from '../../shared/either'
import { setOptionalProp } from '../../shared/object-utils'
import type { ESCodeFile, ESRemoteDependencyPlaceholder } from '../../shared/project-file-types'

import LRU from 'lru-cache'

export type ImportsExportsObjectField = MapLike<
  string | MapLike<string | null | MapLike<string | null>>
>
export type ExportsField = string | ImportsExportsObjectField

export interface PartialPackageJsonDefinition {
  name?: string
  main?: string
  module?: string
  browser?: string | MapLike<string | false>
  imports?: ImportsExportsObjectField
  exports?: ExportsField
}

const partialPackageJsonCache: LRU<string, ParseResult<PartialPackageJsonDefinition>> = new LRU({
  max: 20,
})
export function getPartialPackageJson(contents: string): ParseResult<PartialPackageJsonDefinition> {
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

export interface ResolveSuccess<T> {
  type: 'RESOLVE_SUCCESS'
  success: T
}

export function resolveSuccess<T>(success: T): ResolveSuccess<T> {
  return {
    type: 'RESOLVE_SUCCESS',
    success: success,
  }
}

export interface ResolveNotPresent {
  type: 'RESOLVE_NOT_PRESENT'
}

export const resolveNotPresent: ResolveNotPresent = {
  type: 'RESOLVE_NOT_PRESENT',
}

export interface ResolveSuccessIgnoreModule {
  type: 'RESOLVE_SUCCESS_IGNORE_MODULE'
}

export function isResolveSuccessIgnoreModule<T>(
  resolveResult: ResolveResult<T>,
): resolveResult is ResolveSuccessIgnoreModule {
  return resolveResult.type === 'RESOLVE_SUCCESS_IGNORE_MODULE'
}

export const resolveSuccessIgnoreModule: ResolveSuccessIgnoreModule = {
  type: 'RESOLVE_SUCCESS_IGNORE_MODULE',
}

export type ResolveResult<T> = ResolveNotPresent | ResolveSuccessIgnoreModule | ResolveSuccess<T>

export function isResolveSuccess<T>(
  resolveResult: ResolveResult<T>,
): resolveResult is ResolveSuccess<T> {
  return resolveResult.type === 'RESOLVE_SUCCESS'
}

export interface FoundFile {
  path: string
  file: ESCodeFile | ESRemoteDependencyPlaceholder
}

export type FileLookupResult = ResolveResult<FoundFile>

export function fileLookupResult(
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

export type FileLookupFn = (path: string, importOrigin: string) => FileLookupResult
export type FileForPathParts = (pathParts: string[]) => FileLookupResult

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

export const exportsParser = parseAlternative<string | ImportsExportsObjectField>(
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

export type PackageLookupResult = {
  packageJsonFileResult: FileLookupResult
  packageJsonDir: string
}
export function findClosestPackageScopeToPath(
  path: string,
  localFileLookup: FileForPathParts,
): PackageLookupResult {
  const originPathPartsToTest = normalizePath(getPartsFromPath(path))
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
