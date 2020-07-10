import {
  NodeModules,
  ESCodeFile,
  ESRemoteDependencyPlaceholder,
  isEsCodeFile,
} from '../../shared/project-file-types'
import { createEsModuleError } from './package-manager'

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
  return nodeModules[path.join('/')]
}

function findFileURIForPath(nodeModules: NodeModules, path: string[]): string | null {
  const normalizedPath = normalizePath(path).join('/')
  const uriAsIs = normalizedPath
  if (nodeModules[uriAsIs] != null) {
    return uriAsIs
  }
  const uriWithJs = normalizedPath + '.js'
  if (nodeModules[uriWithJs] != null) {
    return uriWithJs
  }
  const uriWithJsx = normalizedPath + '.jsx'
  if (nodeModules[uriWithJsx] != null) {
    return uriWithJsx
  }
  // TODO this also needs JSON parsing
  const uriWithJson = normalizedPath + '.json'
  if (nodeModules[uriWithJson] != null) {
    return uriWithJson
  }
  return null
}

function processPackageJson(
  potentiallyJsonCode: string,
  containerFolder: string[],
): string[] | null {
  var packageJson: any
  try {
    packageJson = JSON.parse(potentiallyJsonCode)
  } catch {
    return null
  }
  const moduleName = packageJson.name ?? containerFolder
  const mainEntry = packageJson.main ?? null
  const moduleEntry = packageJson.module ?? null
  if (moduleEntry != null && mainEntry == null) {
    throw createEsModuleError(
      moduleName,
      new Error('Module error: package.json has a missing `main` entry'),
    )
  }
  if (mainEntry != null) {
    return normalizePath([...containerFolder, ...pathToElements(mainEntry)])
  }
  return null
}

function resolvePackageJson(nodeModules: NodeModules, packageJsonFolder: string[]): string | null {
  const normalizedFolderPath = normalizePath(packageJsonFolder)
  const folderPackageJson = findFileForPath(nodeModules, [...normalizedFolderPath, 'package.json'])
  if (folderPackageJson != null && isEsCodeFile(folderPackageJson)) {
    const mainEntryPath = processPackageJson(folderPackageJson.fileContents, normalizedFolderPath)
    if (mainEntryPath != null) {
      // try loading the entry path as a file
      const mainEntryUri = findFileURIForPath(nodeModules, mainEntryPath)
      if (mainEntryUri != null) {
        return mainEntryUri
      }
      // fallback to loading it as a folder with an index.js
      const indexJsPath = [...mainEntryPath, 'index']
      const indexJsUri = findFileURIForPath(nodeModules, indexJsPath)
      if (indexJsUri != null) {
        return indexJsUri
      }
    }
  }
  return null
}

function resolveNonRelativeModule(
  nodeModules: NodeModules,
  importOrigin: string[],
  toImport: string[],
): string | null {
  if (importOrigin.length === 0) {
    // we exhausted all folders without success
    return null
  }

  // 1. look for ./node_modules/<package_name>.js
  const fileWithName = findFileURIForPath(nodeModules, [
    ...importOrigin,
    'node_modules',
    ...toImport,
  ])
  if (fileWithName != null) {
    return fileWithName
  }

  // 2. look for ./node_modules/<package_name>/package.json
  const packageJsonBasedUri = resolvePackageJson(nodeModules, [
    ...importOrigin,
    'node_modules',
    ...toImport,
  ])
  if (packageJsonBasedUri != null) {
    return packageJsonBasedUri
  }

  // 3. look for ./node_modules/<package_name>/index.js
  const indexJsPath = [...importOrigin, 'node_modules', ...toImport, 'index']
  const indexJsUri = findFileURIForPath(nodeModules, indexJsPath)
  if (indexJsUri != null) {
    return indexJsUri
  }

  // 4. repeat in the parent folder
  return resolveNonRelativeModule(nodeModules, importOrigin.slice(0, -1), toImport)
}

function resolveRelativeModule(
  nodeModules: NodeModules,
  importOrigin: string[],
  toImport: string[],
): string | null {
  // 1. look for a file named <import_name>
  const fileExistsUri = findFileURIForPath(nodeModules, [...importOrigin, ...toImport])
  if (fileExistsUri != null) {
    return fileExistsUri
  }

  // 2. look for <import_name>/package.json
  const packageJsonBasedUri = resolvePackageJson(nodeModules, [...importOrigin, ...toImport])
  if (packageJsonBasedUri != null) {
    return packageJsonBasedUri
  }

  // 3. look for <import_name>/index.js
  const indexJsUri = findFileURIForPath(nodeModules, [...importOrigin, ...toImport, 'index'])
  if (indexJsUri != null) {
    return indexJsUri
  }

  return null
}

// Module resolution logic based on what Node / Typescript does https://www.typescriptlang.org/docs/handbook/module-resolution.html#node

// TODO there's way more module resolution rules here https://parceljs.org/module_resolution.html#module-resolution

// TODO an even more comprehensive writeup https://nodejs.org/api/modules.html#modules_all_together

export function resolveModule(
  nodeModules: NodeModules,
  importOrigin: string,
  toImport: string,
): string | null {
  if (toImport.startsWith('/')) {
    // absolute import
    return resolveRelativeModule(
      nodeModules,
      [], // this import is relative to the root
      pathToElements(toImport),
    )
  }
  if (toImport.startsWith('.')) {
    return resolveRelativeModule(
      nodeModules,
      pathToElements(importOrigin).slice(0, -1),
      pathToElements(toImport),
    )
  } else {
    return resolveNonRelativeModule(
      nodeModules,
      pathToElements(importOrigin).slice(0, -1),
      pathToElements(toImport),
    )
  }
}
