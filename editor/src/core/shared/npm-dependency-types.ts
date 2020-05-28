export type RequireFn = (importOrigin: string, toImport: string) => any
export type TypeDefinitions = {
  [fileName: string]: string
} // the strings are the contents of .d.ts files

type NpmDependencyVersion = string
export type NpmDependencies = {
  [dependencyName: string]: NpmDependencyVersion
}

export type NpmDependency = {
  name: string
  version: NpmDependencyVersion
}

export function npmDependency(name: string, version: NpmDependencyVersion) {
  return {
    name: name,
    version: version,
  }
}

export interface NpmBundleResult {
  require: RequireFn
  typeDefinitions: TypeDefinitions
}

interface PackagerServerDependency {
  name: string
  version: string
}

export interface PackagerServerFileDescriptor {
  content: string
  isModule: boolean
  requires: Array<string>
}

export interface PackagerServerResponse {
  contents: {
    [filepath: string]: PackagerServerFileDescriptor
  }
  dependency: PackagerServerDependency
  peerDependencies: {
    [packageName: string]: PackagerServerDependency
  }
  dependencyDependencies: {
    [packageName: string]: PackagerServerDependency
  }
}

export interface TypingsServerResponse {
  files: {
    [filepath: string]: {
      module: {
        code: string
      }
    }
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
