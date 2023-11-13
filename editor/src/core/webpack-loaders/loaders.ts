import { CSSLoader } from './css-loader'
import { DefaultLoader } from './default-loader'
import { FileLoader } from './file-loader'
import { JSONLoader } from './json-loader'
import type { LoadModuleResult, ModuleLoader } from './loader-types'
import { loadModuleResult } from './loader-types'

const moduleLoaders: Array<ModuleLoader> = [CSSLoader, JSONLoader, DefaultLoader, FileLoader]

export function filenameWithoutJSSuffix(filename: string): string | undefined {
  // The TS compiler will attempt to search for modules by appending .js to the file name,
  // so for non-module files we will want to try stripping that suffix
  return filename.endsWith('.js') ? filename.slice(0, -3) : undefined
}

function loaderForFile(filename: string): ModuleLoader | undefined {
  return moduleLoaders.find((loader) => loader.match(filename))
}

export function applyLoaders(filename: string, contents: string): LoadModuleResult {
  const matchedLoader = loaderForFile(filename)
  if (matchedLoader != null) {
    return matchedLoader.load(filename, contents)
  } else {
    return loadModuleResult(filename, contents)
  }
}
