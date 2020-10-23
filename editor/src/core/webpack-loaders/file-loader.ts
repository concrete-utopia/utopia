import { LoadModule, MatchFile, ModuleLoader } from './loader-types'

const matchFile: MatchFile = (filename: string) => {
  return ['.avif', '.bmp', '.gif', '.jpg', '.jpeg', '.png'].some((extension) =>
    filename.endsWith(extension),
  )
}

const loadModule: LoadModule = (filename: string, contents: string) => {
  const exportValue = contents.length > 0 ? contents : `.${filename}` // if the contents is a non-empty string then it is base64, so load that
  return `module.exports = '${exportValue}'`
}

export const FileLoader: ModuleLoader = {
  match: matchFile,
  load: loadModule,
}
