// you can turn on/off debug features individually here

export const PROBABLY_ELECTRON: boolean =
  typeof window === 'undefined' ||
  (window && (window as any)['process'] && (window as any)['process']['type'])

export const HOST: string = typeof window === 'undefined' ? '' : window.location.host
export const BASE_URL = (productionConfig: boolean) =>
  productionConfig ? `https://${HOST}/` : `http://${HOST}/`

export const STATIC_BASE_URL = (productionConfig: boolean) =>
  productionConfig ? `https://cdn.${HOST}/` : `http://${HOST}/`

export const FLOATING_PREVIEW_BASE_URL = (productionConfig: boolean) =>
  productionConfig ? `https://utopia.fm/` : BASE_URL(productionConfig)

export const UTOPIA_BACKEND = (productionConfig: boolean) => BASE_URL(productionConfig) + 'v1/'
export const ASSET_ENDPOINT = (productionConfig: boolean) =>
  UTOPIA_BACKEND(productionConfig) + 'asset/'
export const THUMBNAIL_ENDPOINT = (productionConfig: boolean) =>
  UTOPIA_BACKEND(productionConfig) + 'thumbnail/'

export const PREVIEW_IS_EMBEDDED = isEmbedded()

export function requireElectron() {
  if (PROBABLY_ELECTRON) {
    // TODO webpack should be able to just not import 'electron', but the 'utopia-runner' module somehow broke the previous hack :(
    if (typeof global !== 'undefined') {
      return (global as any)['require']('electron')
    }
    if (typeof window !== 'undefined') {
      return (window as any)['require']('electron')
    }
    return null
  }
}

export function getQueryParam(paramName: string): string | null {
  if (typeof window !== 'undefined') {
    const urlParams = new URLSearchParams(window.location.search)
    return urlParams.get(paramName)
  }
  return null
}

export function getProjectID(): string | null {
  if (typeof window !== 'undefined') {
    const utopiaProjectID = (window as any).utopiaProjectID
    if (utopiaProjectID == null) {
      return getQueryParam('projectId')
    } else {
      return utopiaProjectID
    }
  }
  return null
}

export function isEmbedded(): boolean {
  if (typeof window !== 'undefined') {
    return getQueryParam('embedded') === 'true'
  } else {
    return false
  }
}
