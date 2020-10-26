import { CSSLoader } from './css-loader'
import { DefaultLoader } from './default-loader'
import { FileLoader } from './file-loader'
import { loadModuleResult, LoadModuleResult, ModuleLoader } from './loader-types'

const moduleLoaders: Array<ModuleLoader> = [FileLoader, CSSLoader, DefaultLoader]

export function filenameWithoutJSSuffix(filename: string): string | undefined {
  // The TS compiler will attempt to search for modules by appending .js to the file name,
  // so for non-module files we will want to try stripping that suffix
  return filename.endsWith('.js') ? filename.slice(0, -3) : undefined
}

function loadersForFile(filename: string): Array<ModuleLoader> {
  return moduleLoaders.filter((loader) => loader.match(filename))
}

function applyMatchedLoaders(
  filename: string,
  contents: string,
  matchedLoaders: Array<ModuleLoader>,
): LoadModuleResult {
  return matchedLoaders.reduce(
    (loadedModuleResult, nextLoader) =>
      nextLoader.load(loadedModuleResult.filename, loadedModuleResult.loadedContents),
    loadModuleResult(filename, contents),
  )
}

export function loaderExistsForFile(filename: string): boolean {
  return loadersForFile(filename).length > 0
}

export function applyLoaders(filename: string, contents: string): LoadModuleResult {
  const matchingLoaders = loadersForFile(filename)
  return applyMatchedLoaders(filename, contents, matchingLoaders)
}
