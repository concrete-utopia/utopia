import { LoadModule, loadModuleResult, MatchFile, ModuleLoader } from './loader-types'

const matchFile: MatchFile = (filename: string) => {
  return ['.json'].some((extension) => filename.endsWith(extension))
}

const loadModule: LoadModule = (filename: string, contents: string) => {
  const loadedContents = `module.exports = ${contents}`
  return loadModuleResult(filename + '.js', loadedContents)
}

export const JSONLoader: ModuleLoader = {
  match: matchFile,
  load: loadModule,
}
