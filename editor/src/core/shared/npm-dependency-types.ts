export type RequireFn = (importOrigin: string, toImport: string) => any
export type TypeDefinitions = {
  [fileName: string]: string
} // the strings are the contents of .d.ts files

type NpmDependencyVersion = string
export type NpmDependencies = {
  [dependencyName: string]: NpmDependencyVersion
}
export interface NpmBundleResult {
  require: RequireFn
  typeDefinitions: TypeDefinitions
}
