import {
  NodeModules,
  ESCodeFile,
  ESRemoteDependencyPlaceholder,
  isEsCodeFile,
} from '../../shared/project-file-types'
import {
  optionalObjectKeyParser,
  parseString,
  ParseResult,
} from '../../../utils/value-parser-utils'
import { applicative3Either, foldEither } from '../../shared/either'
import { setOptionalProp } from '../../shared/object-utils'

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

function isResolveSuccess<T>(resolveResult: ResolveResult<T>): resolveResult is ResolveSuccess<T> {
  return resolveResult.type === 'RESOLVE_SUCCESS'
}

function isResolveNotPresent<T>(
  resolveResult: ResolveResult<T>,
): resolveResult is ResolveNotPresent {
  return resolveResult.type === 'RESOLVE_NOT_PRESENT'
}

type ResolveResultType = ResolveResult<any>['type']

const resolveResultTypes: Array<ResolveResultType> = ['RESOLVE_NOT_PRESENT', 'RESOLVE_SUCCESS']

export function failoverResolveResults<T>(
  resolveResultCalls: Array<() => ResolveResult<T>>,
): ResolveResult<T> {
  let result: ResolveResult<T> = resolveNotPresent
  for (const call of resolveResultCalls) {
    if (isResolveSuccess(result)) {
      break
    }
    const newResult = call()
    // Prefer elements further along in the array of the types.
    if (resolveResultTypes.indexOf(newResult.type) > resolveResultTypes.indexOf(result.type)) {
      result = newResult
    }
  }
  return result
}

function pathToElements(path: string): string[] {
  return path.split('/')
}

function normalizePath(path: string[]): string[] {
  return path.reduce((pathSoFar: string[], pathElem: string, index: number) => {
    if (pathElem === '' && index !== 0) {
      return pathSoFar
    }
    if (pathElem === '.') {
      return pathSoFar
    }
    if (pathElem === '..') {
      return pathSoFar.slice(0, -1)
    }
    return [...pathSoFar, pathElem]
  }, [])
}

function findFileForPath(
  nodeModules: NodeModules,
  path: string[],
): ESCodeFile | ESRemoteDependencyPlaceholder | null {
  return nodeModules[path.join('/')] ?? null
}

function findFileURIForPath(nodeModules: NodeModules, path: string[]): ResolveResult<string> {
  const normalizedPath = normalizePath(path).join('/')
  const uriAsIs = normalizedPath
  if (nodeModules[uriAsIs] != null) {
    return resolveSuccess(uriAsIs)
  }
  const uriWithJs = normalizedPath + '.js'
  if (nodeModules[uriWithJs] != null) {
    return resolveSuccess(uriWithJs)
  }
  const uriWithJsx = normalizedPath + '.jsx'
  if (nodeModules[uriWithJsx] != null) {
    return resolveSuccess(uriWithJsx)
  }
  // TODO this also needs JSON parsing
  const uriWithJson = normalizedPath + '.json'
  if (nodeModules[uriWithJson] != null) {
    return resolveSuccess(uriWithJson)
  }
  return resolveNotPresent
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
      const moduleName: string = packageJson.name ?? containerFolder.join('/')
      const mainEntry: string | null = packageJson.main ?? null
      const moduleEntry: string | null = packageJson.module ?? null
      if (moduleEntry != null && mainEntry == null) {
        return resolveSuccess(normalizePath([...containerFolder, ...pathToElements(moduleName)]))
        //return resolveESModuleFailure(moduleName)
      }
      if (mainEntry != null) {
        return resolveSuccess(normalizePath([...containerFolder, ...pathToElements(mainEntry)]))
      }
      return resolveNotPresent
    },
    possiblePackageJson,
  )
}

function resolvePackageJson(
  nodeModules: NodeModules,
  packageJsonFolder: string[],
): ResolveResult<string> {
  const normalizedFolderPath = normalizePath(packageJsonFolder)
  const folderPackageJson = findFileForPath(nodeModules, [...normalizedFolderPath, 'package.json'])
  if (folderPackageJson != null && isEsCodeFile(folderPackageJson)) {
    const mainEntryPath = processPackageJson(folderPackageJson.fileContents, normalizedFolderPath)
    if (isResolveSuccess(mainEntryPath)) {
      return failoverResolveResults([
        // try loading the entry path as a file
        () => findFileURIForPath(nodeModules, mainEntryPath.success),
        // fallback to loading it as a folder with an index.js
        () => {
          const indexJsPath = [...mainEntryPath.success, 'index']
          return findFileURIForPath(nodeModules, indexJsPath)
        },
      ])
    } else {
      return mainEntryPath
    }
  }
  return resolveNotPresent
}

function resolveNonRelativeModule(
  nodeModules: NodeModules,
  importOrigin: string[],
  toImport: string[],
): ResolveResult<string> {
  if (importOrigin.length === 0) {
    // we exhausted all folders without success
    return resolveNotPresent
  }

  return failoverResolveResults([
    // 1. look for ./node_modules/<package_name>.js
    () => findFileURIForPath(nodeModules, [...importOrigin, 'node_modules', ...toImport]),
    // 2. look for ./node_modules/<package_name>/package.json
    () => resolvePackageJson(nodeModules, [...importOrigin, 'node_modules', ...toImport]),
    // 3. look for ./node_modules/<package_name>/index.js
    () => {
      const indexJsPath = [...importOrigin, 'node_modules', ...toImport, 'index']
      return findFileURIForPath(nodeModules, indexJsPath)
    },
    // 4. repeat in the parent folder
    () => resolveNonRelativeModule(nodeModules, importOrigin.slice(0, -1), toImport),
  ])
}

function resolveRelativeModule(
  nodeModules: NodeModules,
  importOrigin: string[],
  toImport: string[],
): ResolveResult<string> {
  return failoverResolveResults([
    // 1. look for a file named <import_name>
    () => findFileURIForPath(nodeModules, [...importOrigin, ...toImport]),
    // 2. look for <import_name>/package.json
    () => resolvePackageJson(nodeModules, [...importOrigin, ...toImport]),
    // 3. look for <import_name>/index.js
    () => findFileURIForPath(nodeModules, [...importOrigin, ...toImport, 'index']),
  ])
}

// Module resolution logic based on what Node / Typescript does https://www.typescriptlang.org/docs/handbook/module-resolution.html#node

// TODO there's way more module resolution rules here https://parceljs.org/module_resolution.html#module-resolution

// TODO an even more comprehensive writeup https://nodejs.org/api/modules.html#modules_all_together

export function resolveModule(
  nodeModules: NodeModules,
  importOrigin: string,
  toImport: string,
): string | null {
  let resolveResult: ResolveResult<string> = resolveNotPresent
  if (toImport.startsWith('/')) {
    // absolute import
    resolveResult = resolveRelativeModule(
      nodeModules,
      [], // this import is relative to the root
      pathToElements(toImport),
    )
  } else {
    if (toImport.startsWith('.')) {
      resolveResult = resolveRelativeModule(
        nodeModules,
        pathToElements(importOrigin).slice(0, -1),
        pathToElements(toImport),
      )
    } else {
      resolveResult = resolveNonRelativeModule(
        nodeModules,
        pathToElements(importOrigin).slice(0, -1),
        pathToElements(toImport),
      )
    }
  }

  switch (resolveResult.type) {
    case 'RESOLVE_SUCCESS':
      return resolveResult.success
    case 'RESOLVE_NOT_PRESENT':
      return null
    default:
      const _exhaustiveCheck: never = resolveResult
      throw new Error(`Unhandled case ${JSON.stringify(resolveResult)}`)
  }
}
