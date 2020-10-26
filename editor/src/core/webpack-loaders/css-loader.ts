import { LoadModule, MatchFile, ModuleLoader } from './loader-types'

const matchFile: MatchFile = (filename: string) => {
  return ['.css'].some((extension) => filename.endsWith(extension))
}

const loadModule: LoadModule = (_: string, contents: string) => {
  // FIXME Replace CSS custom evaluation from evaluator.ts with this
  return contents
}

export const CSSLoader: ModuleLoader = {
  match: matchFile,
  load: loadModule,
}
