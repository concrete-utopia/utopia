import { NpmDependency } from '../../shared/npm-dependency-types'

export function getPackagerUrl(dep: NpmDependency) {
  return `/v1/javascript/packager/${encodeURIComponent(dep.name)}/${dep.version}.json`
}

export function getJsDelivrListUrl(dep: NpmDependency) {
  return `https://data.jsdelivr.com/v1/package/npm/${dep.name}@${dep.version}/flat`
}

export function getJsDelivrFileUrl(dep: NpmDependency, localFilePath: string) {
  return `https://cdn.jsdelivr.net/npm/${dep.name}@${dep.version}${localFilePath}`
}
