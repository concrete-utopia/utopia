export type RequireFn = (importOrigin: string, toImport: string) => any
export type TypeDefinitions = {
  [fileName: string]: string
} // the strings are the contents of .d.ts files

export type PackageStatus =
  | 'version-lookup'
  | 'loading'
  | 'updating'
  | 'loaded'
  | 'error'
  | 'default-package'
  | 'not-found'
export interface PackageDetails {
  status: PackageStatus
}

function packageDetails(status: PackageStatus): PackageDetails {
  return {
    status: status,
  }
}
export type PackageStatusMap = { [packagename: string]: PackageDetails }

export interface NpmDependency {
  type: 'NPM_DEPENDENCY'
  name: string
  version: string
}

export function npmDependency(name: string, version: string): NpmDependency {
  return {
    type: 'NPM_DEPENDENCY',
    name: name,
    version: version,
  }
}

export interface UnversionedNpmDependency {
  type: 'UNVERSIONED_NPM_DEPENDENCY'
  name: string
}

export function unversionedNpmDependency(name: string): UnversionedNpmDependency {
  return {
    type: 'UNVERSIONED_NPM_DEPENDENCY',
    name: name,
  }
}

export type PossiblyUnversionedNpmDependency = NpmDependency | UnversionedNpmDependency

export function isNpmDependency(
  dependency: PossiblyUnversionedNpmDependency,
): dependency is NpmDependency {
  return dependency.type === 'NPM_DEPENDENCY'
}

export function isUnversionedNpmDependency(
  dependency: PossiblyUnversionedNpmDependency,
): dependency is UnversionedNpmDependency {
  return dependency.type === 'UNVERSIONED_NPM_DEPENDENCY'
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
