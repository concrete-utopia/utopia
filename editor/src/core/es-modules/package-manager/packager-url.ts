import { STATIC_BASE_URL } from '../../../common/env-vars'

export function getPackagerUrl(versionedDependency: string): string {
  return `${STATIC_BASE_URL}v1/javascript/packager/${encodeURIComponent(versionedDependency)}`
}

export function getJsDelivrFileUrl(versionedDependency: string, localFilePath: string): string {
  return `https://cdn.jsdelivr.net/npm/${versionedDependency}${localFilePath}`
}
