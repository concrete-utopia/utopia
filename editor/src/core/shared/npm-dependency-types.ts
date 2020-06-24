export type RequireFn = (importOrigin: string, toImport: string) => any
export type TypeDefinitions = {
  [fileName: string]: string
} // the strings are the contents of .d.ts files

export type NpmDependency = {
  name: string
  version: string
}

export function npmDependency(name: string, version: string) {
  return {
    name: name,
    version: version,
  }
}

export interface PackagerServerFileDescriptor {
  content: string
}

export interface PackagerServerResponse {
  contents: {
    [filepath: string]: PackagerServerFileDescriptor
  }
}

export interface JsdelivrResponse {
  default: string
  files: Array<{
    name: string
    hash: string
    time: string
    size: number
  }>
}
