import { ProjectFile } from '../shared/project-file-types'

export type MatchFile = (filename: string) => boolean
export type LoadModule = (filename: string, file: ProjectFile) => string
export interface ModuleLoader {
  match: MatchFile
  load: LoadModule
}
