import { NpmDependency } from '../../shared/npm-dependency-types'
import { ESRemoteDependencyPlaceholder } from '../../shared/project-file-types'

export function getPackagerUrl(dep: NpmDependency) {
  return `/v1/javascript/packager/${dep.name}/${dep.version}.json`
}

export function getJsDelivrListUrl(dep: NpmDependency) {
  return `https://data.jsdelivr.com/v1/package/npm/${dep.name}@${dep.version}/flat`
}
