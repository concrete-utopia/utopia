import type { LoadModule, MatchFile, ModuleLoader } from './loader-types'
import { loadModuleResult } from './loader-types'

const matchFile: MatchFile = (filename: string) => {
  return filename.endsWith('.json')
}

const loadModule: LoadModule = (filename: string, contents: string) => {
  const loadedContents = `module.exports = ${contents}`
  return loadModuleResult(filename + '.js', loadedContents)
}

export const JSONLoader: ModuleLoader = {
  match: matchFile,
  load: loadModule,
}
