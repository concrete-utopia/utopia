import type { LoadModule, MatchFile, ModuleLoader } from './loader-types'
import { loadModuleResult } from './loader-types'

const matchFile: MatchFile = (filename: string) => {
  return true
}

const loadModule: LoadModule = (filename: string, contents: string) => {
  const exportValue = contents.length > 0 ? contents : `.${filename}` // if the contents is a non-empty string then it is base64, so load that
  const loadedContents = `module.exports = '${exportValue}'`
  return loadModuleResult(filename + '.js', loadedContents)
}

export const FileLoader: ModuleLoader = {
  match: matchFile,
  load: loadModule,
}
