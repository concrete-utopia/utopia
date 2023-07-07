import type { LoadModule, MatchFile, ModuleLoader } from './loader-types'
import { loadModuleResult } from './loader-types'

const acceptableExtensions: Set<string> = new Set(['cjs', 'mjs', 'js', 'jsx', 'ts', 'tsx'])

const matchFile: MatchFile = (filename: string) => {
  const lastDot = filename.lastIndexOf('.')
  if (lastDot < 0) {
    return false
  } else {
    const extension = filename.slice(lastDot + 1)
    return acceptableExtensions.has(extension)
  }
}

const loadModule: LoadModule = (filename: string, contents: string) => {
  // TODO We should investigate running the Babel transform for converting to Common JS here rather than inside evaluator.js
  return loadModuleResult(filename, contents)
}

export const DefaultLoader: ModuleLoader = {
  match: matchFile,
  load: loadModule,
}
