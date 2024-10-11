import { getPartsFromPath, makePathFromParts, normalizePath } from '../../../utils/path-utils'
import type { ParseResult } from '../../../utils/value-parser-utils'
import { foldEither } from '../../shared/either'
import { isEsCodeFile } from '../../shared/project-file-types'
import type {
  ExportsField,
  FileForPathParts,
  FileLookupFn,
  FileLookupResult,
  PartialPackageJsonDefinition,
} from './module-resolution-utils'
import {
  findClosestPackageScopeToPath,
  getPartialPackageJson,
  isResolveSuccess,
  resolveNotPresent,
} from './module-resolution-utils'

// Using the logic from https://nodejs.org/api/modules.html#all-together
// This is separated to emphasize that these functions act an ESM compatibility
// layer in CommonJS

// LOAD_PACKAGE_IMPORTS(X, DIR)
export function loadPackageImports(
  path: string,
  originPath: string,
  packageJsonLookupFn: FileForPathParts,
  lookupFn: FileLookupFn,
): FileLookupResult {
  // Loading a module from the imports field mapping https://nodejs.org/api/packages.html#imports
  // Find the closest package scope SCOPE to DIR.
  const { packageJsonFileResult, packageJsonDir } = findClosestPackageScopeToPath(
    originPath,
    packageJsonLookupFn,
  )

  // If no scope was found, return
  if (
    !(isResolveSuccess(packageJsonFileResult) && isEsCodeFile(packageJsonFileResult.success.file))
  ) {
    return resolveNotPresent
  }

  let possiblePackageJson: ParseResult<PartialPackageJsonDefinition>
  try {
    possiblePackageJson = getPartialPackageJson(packageJsonFileResult.success.file.fileContents)
  } catch {
    return resolveNotPresent
  }

  return foldEither(
    (_) => resolveNotPresent,
    (packageJson) => {
      // If the SCOPE/package.json "imports" is null or undefined, return
      const importsEntry = packageJson['imports']
      if (importsEntry == null) {
        return resolveNotPresent
      }

      return packageImportsExportsResolve(path, importsEntry, packageJsonDir, lookupFn)
    },
    possiblePackageJson,
  )
}

// LOAD_PACKAGE_EXPORTS(X, DIR)
export function loadPackageExports(
  path: string,
  originPath: string,
  packageJsonLookupFn: FileForPathParts,
  lookupFn: FileLookupFn,
): FileLookupResult {
  // Try to interpret X as a combination of NAME and SUBPATH where the name
  // may have a @scope/ prefix and the subpath begins with a slash (`/`)
  const regex = /((?:@[\w|\-|\.]+\/)*[\w|\-|\.]+)(\/[\w|\-|\.]*)*/
  const matches = path.match(regex)
  if (matches == null) {
    // If X does not match this pattern, return
    return resolveNotPresent
  }

  const [_combined, name, subpath] = matches

  // If DIR/NAME/package.json is not a file, return
  const pathToModule = normalizePath([...getPartsFromPath(originPath), name])
  const packageJsonPathParts = pathToModule.concat('package.json')
  const packageJsonFileResult = packageJsonLookupFn(packageJsonPathParts)
  if (
    !(isResolveSuccess(packageJsonFileResult) && isEsCodeFile(packageJsonFileResult.success.file))
  ) {
    return resolveNotPresent
  }

  // Parse DIR/NAME/package.json, and look for "exports" field
  let possiblePackageJson: ParseResult<PartialPackageJsonDefinition>
  try {
    possiblePackageJson = getPartialPackageJson(packageJsonFileResult.success.file.fileContents)
  } catch {
    return resolveNotPresent
  }

  return foldEither(
    (_) => resolveNotPresent,
    (packageJson) => {
      // If "exports" is null or undefined, return.
      const exportsEntry = packageJson['exports']
      if (exportsEntry == null) {
        return resolveNotPresent
      }

      return packageImportsExportsResolve(
        `.${subpath}`,
        exportsEntry,
        makePathFromParts(pathToModule),
        lookupFn,
      )
    },
    possiblePackageJson,
  )
}

// LOAD_PACKAGE_SELF(X, DIR)
export function loadPackageSelf(
  path: string,
  originPath: string,
  packageJsonLookupFn: FileForPathParts,
  lookupFn: FileLookupFn,
): FileLookupResult {
  // Find the closest package scope SCOPE to DIR.
  const { packageJsonFileResult, packageJsonDir } = findClosestPackageScopeToPath(
    originPath,
    packageJsonLookupFn,
  )

  // If no scope was found, return.
  if (
    !(isResolveSuccess(packageJsonFileResult) && isEsCodeFile(packageJsonFileResult.success.file))
  ) {
    return resolveNotPresent
  }

  let possiblePackageJson: ParseResult<PartialPackageJsonDefinition>
  try {
    possiblePackageJson = getPartialPackageJson(packageJsonFileResult.success.file.fileContents)
  } catch {
    return resolveNotPresent
  }
  return foldEither(
    (_) => resolveNotPresent,
    (packageJson) => {
      // If "exports" is null or undefined, return.
      const exportsEntry = packageJson['exports']
      if (exportsEntry == null) {
        return resolveNotPresent
      }

      // If "name" is not the first segment of X, return
      const nameEntry = packageJson.name
      if (nameEntry == null || !path.startsWith(nameEntry)) {
        return resolveNotPresent
      }

      return packageImportsExportsResolve(
        `.${path.slice(nameEntry.length)}`,
        exportsEntry,
        packageJsonDir,
        lookupFn,
      )
    },
    possiblePackageJson,
  )
}

// PACKAGE_IMPORTS_EXPORTS_RESOLVE
export function packageImportsExportsResolve(
  path: string,
  importsOrExportsEntry: ExportsField,
  packageJsonDir: string,
  lookupFn: FileLookupFn,
): FileLookupResult {
  // FIXME partial path lookup because the exports field could be along the lines of `{ '@thing': { stuff: { ... }} }`
  // in which case we would want to match that against the import `@thing/stuff`
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
