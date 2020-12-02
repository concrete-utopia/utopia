import { LoadModule, loadModuleResult, MatchFile, ModuleLoader } from './loader-types'

const matchFile: MatchFile = (filename: string) => {
  return ['.js', '.jsx', '.ts', '.tsx', '.d.ts', '.json'].some((extension) =>
    filename.endsWith(extension),
  )
}

const loadModule: LoadModule = (filename: string, contents: string) => {
  // TODO We should investigate running the Babel transform for converting to Common JS here rather than inside evaluator.js
  return loadModuleResult(filename, contents)
}

export const DefaultLoader: ModuleLoader = {
  match: matchFile,
  load: loadModule,
}
