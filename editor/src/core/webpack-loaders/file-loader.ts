import { ProjectFile } from '../shared/project-file-types'
import { LoadModule, MatchFile, ModuleLoader } from './loader-types'

const matchFile: MatchFile = (filename: string) => {
  return ['.avif', '.bmp', '.gif', '.jpg', '.jpeg', '.png'].some((extension) =>
    filename.endsWith(extension),
  )
}

const loadModule: LoadModule = (filename: string, _: ProjectFile) => {
  return `module.exports = '.${filename}'`
}

export const FileLoader: ModuleLoader = {
  match: matchFile,
  load: loadModule,
}
