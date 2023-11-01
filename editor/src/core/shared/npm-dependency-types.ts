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

export function packageDetails(status: PackageStatus): PackageDetails {
  return {
    status: status,
  }
}
export type PackageStatusMap = { [packagename: string]: PackageDetails }

export interface RequestedNpmDependency {
  type: 'REQUESTED_NPM_DEPENDENCY'
  name: string
  version: string
}

export function requestedNpmDependency(name: string, version: string): RequestedNpmDependency {
  return {
    type: 'REQUESTED_NPM_DEPENDENCY',
    name: name,
    version: version,
  }
}

export interface ResolvedNpmDependency {
  type: 'RESOLVED_NPM_DEPENDENCY'
  name: string
  version: string
}

export function resolvedNpmDependency(name: string, version: string): ResolvedNpmDependency {
  return {
    type: 'RESOLVED_NPM_DEPENDENCY',
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

export type PossiblyUnversionedNpmDependency = ResolvedNpmDependency | UnversionedNpmDependency

export function isResolvedNpmDependency(
  dependency: PossiblyUnversionedNpmDependency,
): dependency is ResolvedNpmDependency {
  return dependency.type === 'RESOLVED_NPM_DEPENDENCY'
}

export function isUnversionedNpmDependency(
  dependency: PossiblyUnversionedNpmDependency,
): dependency is UnversionedNpmDependency {
  return dependency.type === 'UNVERSIONED_NPM_DEPENDENCY'
}

export interface PackagerServerFileDescriptor {
  content: string
}

export type PackagerServerFile = PackagerServerFileDescriptor | 'PLACEHOLDER_FILE'

export function isPackagerServerFileDescriptor(
  file: PackagerServerFile,
): file is PackagerServerFileDescriptor {
  return file !== 'PLACEHOLDER_FILE'
}

export interface PackagerServerFileEntry {
  fileEntry: {
    filename: string
    fileContents: PackagerServerFile
  }
}

export interface PackagerServerErrorEntry {
  error: string
}

export type PackagerServerEntry = PackagerServerFileEntry | PackagerServerErrorEntry

export function isPackagerServerFileEntry(
  entry: PackagerServerEntry,
): entry is PackagerServerFileEntry {
  return 'fileEntry' in entry
}

export function isPackagerServerErrorEntry(
  entry: PackagerServerEntry,
): entry is PackagerServerErrorEntry {
  return 'error' in entry
}

export interface PackagerServerResponse {
  contents: Array<PackagerServerEntry>
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
