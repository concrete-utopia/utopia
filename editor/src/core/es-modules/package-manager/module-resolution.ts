import {
  ProjectFile,
  NodeModules,
  ESCodeFile,
  ESRemoteDependencyPlaceholder,
  isEsCodeFile,
  esCodeFile,
} from '../../shared/project-file-types'
import {
  optionalObjectKeyParser,
  parseString,
  ParseResult,
} from '../../../utils/value-parser-utils'
import { applicative3Either, Either, foldEither, left, right } from '../../shared/either'
import { setOptionalProp } from '../../shared/object-utils'
import { getContentsTreeFileFromElements, ProjectContentTreeRoot } from '../../../components/assets'
import { loaderExistsForFile } from '../../webpack-loaders/loaders'
import { dropLast, last } from '../../shared/array-utils'
import { getPartsFromPath, makePathFromParts, normalizePath } from '../../../utils/path-utils'

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

type ResolveResult<T> = ResolveNotPresent | ResolveSuccess<T>

export function isResolveSuccess<T>(
  resolveResult: ResolveResult<T>,
): resolveResult is ResolveSuccess<T> {
  return resolveResult.type === 'RESOLVE_SUCCESS'
}

function failoverResolveResults<T>(
  resolveResultCalls: Array<() => ResolveResult<T>>,
): ResolveResult<T> {
  for (const call of resolveResultCalls) {
    const result = call()
    if (isResolveSuccess(result)) {
      return result
    }
  }
  return resolveNotPresent
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

type FileLookupFn = (path: string[]) => FileLookupResult
const fallbackLookup: (lookupFn: FileLookupFn) => FileLookupFn = (lookupFn: FileLookupFn) => {
  return (path: string[]) => {
    const asIsPath = normalizePath(path)
    const pathToFile = dropLast(asIsPath)
    const lastPart = last(asIsPath)
    if (lastPart == null) {
      return resolveNotPresent
    } else {
      if (loaderExistsForFile(lastPart)) {
        const asIsResult = lookupFn(asIsPath)
        if (isResolveSuccess(asIsResult)) {
          return asIsResult
        }
      }

      const withJs = lastPart + '.js'
      if (loaderExistsForFile(withJs)) {
        const withJsResult = lookupFn(pathToFile.concat(withJs))
        if (isResolveSuccess(withJsResult)) {
          return withJsResult
        }
      }

      const withJsx = lastPart + '.jsx'
      if (loaderExistsForFile(withJsx)) {
        const withJsxResult = lookupFn(pathToFile.concat(withJsx))
        if (isResolveSuccess(withJsxResult)) {
          return withJsxResult
        }
      }

      // TODO this also needs JSON parsing
      const withJson = lastPart + '.json'
      if (loaderExistsForFile(withJson)) {
        const withJsonResult = lookupFn(pathToFile.concat(withJson))
        if (isResolveSuccess(withJsonResult)) {
          return withJsonResult
        }
      }

      return resolveNotPresent
    }
  }
}

const nodeModulesFileLookup: (nodeModules: NodeModules) => FileLookupFn = (
  nodeModules: NodeModules,
) => {
  return fallbackLookup((path: string[]) => {
    const filename = makePathFromParts(path)
    return fileLookupResult(filename, nodeModules[filename])
  })
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

const projectContentsFileLookup: (projectContents: ProjectContentTreeRoot) => FileLookupFn = (
  projectContents: ProjectContentTreeRoot,
) => {
  return fallbackLookup((path: string[]) => {
    const projectFile = getContentsTreeFileFromElements(projectContents, path)
    const fileContents = projectFile == null ? null : getProjectFileContentsAsString(projectFile)
    if (fileContents == null) {
      return resolveNotPresent
    } else {
      const filename = makePathFromParts(path)
      return fileLookupResult(filename, esCodeFile(fileContents, 'PROJECT_CONTENTS', filename))
    }
  })
}

const combinedFileLookup: (lookupFns: Array<FileLookupFn>) => FileLookupFn = (
  lookupFns: Array<FileLookupFn>,
) => {
  return (path: string[]) => {
    for (const lookupFn of lookupFns) {
      const result = lookupFn(path)
      if (isResolveSuccess(result)) {
        return result
      }
    }
    return resolveNotPresent
  }
}

interface PartialPackageJsonDefinition {
  name?: string
  main?: string
  module?: string
}

export function parsePartialPackageJsonDefinition(
  value: unknown,
): ParseResult<PartialPackageJsonDefinition> {
  return applicative3Either(
    (name, main, module) => {
      let result: PartialPackageJsonDefinition = {}
      setOptionalProp(result, 'name', name)
      setOptionalProp(result, 'main', main)
      setOptionalProp(result, 'module', module)
      return result
    },
    optionalObjectKeyParser(parseString, 'name')(value),
    optionalObjectKeyParser(parseString, 'main')(value),
    optionalObjectKeyParser(parseString, 'module')(value),
  )
}

function processPackageJson(
  potentiallyJsonCode: string,
  containerFolder: string[],
): ResolveResult<Array<string>> {
  let possiblePackageJson: ParseResult<PartialPackageJsonDefinition>
  try {
    const jsonParsed = JSON.parse(potentiallyJsonCode)
    possiblePackageJson = parsePartialPackageJsonDefinition(jsonParsed)
  } catch {
    return resolveNotPresent
  }
  return foldEither(
    (_) => resolveNotPresent,
    (packageJson) => {
      const moduleName: string = packageJson.name ?? makePathFromParts(containerFolder)
      const mainEntry: string | null = packageJson.main ?? null
      const moduleEntry: string | null = packageJson.module ?? null
      if (moduleEntry != null && mainEntry == null) {
        return resolveSuccess(normalizePath([...containerFolder, ...getPartsFromPath(moduleName)]))
      }
      if (mainEntry != null) {
        return resolveSuccess(normalizePath([...containerFolder, ...getPartsFromPath(mainEntry)]))
      }
      return resolveNotPresent
    },
    possiblePackageJson,
  )
}

function resolvePackageJson(
  fileLookupFn: FileLookupFn,
  packageJsonFolder: string[],
): FileLookupResult {
  const normalizedFolderPath = normalizePath(packageJsonFolder)
  const folderPackageJson = fileLookupFn([...normalizedFolderPath, 'package.json'])
  if (isResolveSuccess(folderPackageJson) && isEsCodeFile(folderPackageJson.success.file)) {
    const mainEntryPath = processPackageJson(
      folderPackageJson.success.file.fileContents,
      normalizedFolderPath,
    )
    if (isResolveSuccess(mainEntryPath)) {
      return failoverResolveResults([
        // try loading the entry path as a file
        () => fileLookupFn(mainEntryPath.success),
        // fallback to loading it as a folder with an index.js
        () => {
          const indexJsPath = [...mainEntryPath.success, 'index']
          return fileLookupFn(indexJsPath)
        },
      ])
    } else {
      return resolveNotPresent
    }
  }
  return resolveNotPresent
}

function resolveNonRelativeModule(
  fileLookupFn: FileLookupFn,
  importOrigin: string[],
  toImport: string[],
): FileLookupResult {
  return failoverResolveResults([
    // 1. look for ./node_modules/<package_name>.js
    () => fileLookupFn([...importOrigin, 'node_modules', ...toImport]),
    // 2. look for ./node_modules/<package_name>/package.json
    () => resolvePackageJson(fileLookupFn, [...importOrigin, 'node_modules', ...toImport]),
    // 3. look for ./node_modules/<package_name>/index.js
    () => {
      const indexJsPath = [...importOrigin, 'node_modules', ...toImport, 'index']
      return fileLookupFn(indexJsPath)
    },
    // 4. repeat in the parent folder
    () => {
      if (importOrigin.length === 0) {
        // we exhausted all folders without success
        return resolveNotPresent
      } else {
        return resolveNonRelativeModule(fileLookupFn, importOrigin.slice(0, -1), toImport)
      }
    },
  ])
}

function resolveRelativeModule(
  fileLookupFn: FileLookupFn,
  importOrigin: string[],
  toImport: string[],
): FileLookupResult {
  return failoverResolveResults([
    // 1. look for a file named <import_name>
    () => fileLookupFn([...importOrigin, ...toImport]),
    // 2. look for <import_name>/package.json
    () => resolvePackageJson(fileLookupFn, [...importOrigin, ...toImport]),
    // 3. look for <import_name>/index.js
    () => fileLookupFn([...importOrigin, ...toImport, 'index']),
  ])
}

// Module resolution logic based on what Node / Typescript does https://www.typescriptlang.org/docs/handbook/module-resolution.html#node

// TODO there's way more module resolution rules here https://parceljs.org/module_resolution.html#module-resolution

// TODO an even more comprehensive writeup https://nodejs.org/api/modules.html#modules_all_together

export function resolveModule(
  projectContents: ProjectContentTreeRoot,
  nodeModules: NodeModules,
  importOrigin: string,
  toImport: string,
): FileLookupResult {
  const lookupFn = combinedFileLookup([
    nodeModulesFileLookup(nodeModules),
    projectContentsFileLookup(projectContents),
  ])

  if (toImport.startsWith('/')) {
    // absolute import
    return resolveRelativeModule(
      lookupFn,
      [], // this import is relative to the root
      getPartsFromPath(toImport),
    )
  } else {
    if (toImport.startsWith('.')) {
      return resolveRelativeModule(
        lookupFn,
        getPartsFromPath(importOrigin).slice(0, -1),
        getPartsFromPath(toImport),
      )
    } else {
      return resolveNonRelativeModule(
        lookupFn,
        getPartsFromPath(importOrigin).slice(0, -1),
        getPartsFromPath(toImport),
      )
    }
  }
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
