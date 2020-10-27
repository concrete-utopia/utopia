export type MatchFile = (filename: string) => boolean
// TODO This is the point we should extend if we want to support all conditions outlined in https://webpack.js.org/configuration/module/#rule-conditions

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
