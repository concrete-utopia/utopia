import { ProjectFile } from '../shared/project-file-types'

export type MatchFile = (filename: string) => boolean

export interface LoadModuleResult {
  filename: string
  loadedContents: string
}

export function loadModuleResult(filename: string, loadedContents: string): LoadModuleResult {
  return {
    filename: filename,
    loadedContents: loadedContents,
  }
}

export type LoadModule = (filename: string, contents: string) => LoadModuleResult
export interface ModuleLoader {
  match: MatchFile
  load: LoadModule
}
