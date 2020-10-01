import { STATIC_BASE_URL } from '../../../common/env-vars'
import { ResolvedNpmDependency } from '../../shared/npm-dependency-types'

export function getPackagerUrl(dep: ResolvedNpmDependency): string {
  return `${STATIC_BASE_URL}v1/javascript/packager/${encodeURIComponent(dep.name)}/${
    dep.version
  }.json`
}

export function getJsDelivrListUrl(dep: ResolvedNpmDependency): string {
  return `https://data.jsdelivr.com/v1/package/npm/${dep.name}@${dep.version}/flat`
}

export function getJsDelivrFileUrl(dep: ResolvedNpmDependency, localFilePath: string): string {
  return `https://cdn.jsdelivr.net/npm/${dep.name}@${dep.version}${localFilePath}`
}
