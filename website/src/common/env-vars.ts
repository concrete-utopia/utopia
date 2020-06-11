// you can turn on/off debug features individually here

export const PRODUCTION_ENV: boolean = process.env.NODE_ENV === 'production'
export const PRODUCTION_CONFIG: boolean = process.env.REACT_APP_ENVIRONMENT_CONFIG === 'production'

export const PROBABLY_ELECTRON: boolean =
  typeof window === 'undefined' ||
  (window && (window as any)['process'] && (window as any)['process']['type'])

export const USE_WEBPACK_SERVER = process.env.WEBPACK_DEV_SERVER === 'true'

export const SHOW_FPS = false
export const DEEP_FREEZE_STATE = !PRODUCTION_ENV
export const RUN_PERFORMANCE_CHECK = false
export const REFERENCE_EQUALITY_CHECK = false

export const MIRROR_SERVER: string = PRODUCTION_CONFIG
  ? 'wss://apps.utopia2d.com/'
  : 'ws://localhost:8080/'
export const MIRROR_SERVER_PUBLISH_ENDPOINT: string = MIRROR_SERVER + 'app'
export const MIRROR_SERVER_LISTEN_ENDPOINT: string = MIRROR_SERVER + 'listen'

export const HOST: string = typeof window === 'undefined' ? '' : window.location.host
export const BASE_URL: string = PRODUCTION_CONFIG ? `https://${HOST}/` : `http://${HOST}/`

export const BASE_WS: string = PRODUCTION_CONFIG ? `wss://${HOST}/` : `ws://${HOST}/`
export const FLOATING_PREVIEW_BASE_URL: string = PRODUCTION_CONFIG ? `https://utopia.fm/` : BASE_URL
export const UTOPIA_BACKEND = BASE_URL + 'v1/'
export const UTOPIA_BACKEND_WS = BASE_WS + 'v1/'
export const ASSET_ENDPOINT = UTOPIA_BACKEND + 'asset/'
export const THUMBNAIL_ENDPOINT = UTOPIA_BACKEND + 'thumbnail/'

export const PREVIEW_IS_EMBEDDED = isEmbedded()

export const AUTH0_REDIRECT_URI: string = process.env.REACT_APP_AUTH0_REDIRECT_URI || ''
export const AUTH0_CLIENT_ID: string = process.env.REACT_APP_AUTH0_CLIENT_ID || ''
export const AUTH0_HOST: string = process.env.REACT_APP_AUTH0_ENDPOINT || ''
const USE_AUTH0 = AUTH0_REDIRECT_URI != '' && AUTH0_CLIENT_ID != '' && AUTH0_HOST != ''

export const auth0Url = USE_AUTH0
  ? `https://${AUTH0_HOST}/authorize?scope=openid%20profile%20email&response_type=code&client_id=${AUTH0_CLIENT_ID}&redirect_uri=${AUTH0_REDIRECT_URI}`
  : `${BASE_URL}authenticate?code=logmein`

const COMMIT_HASH = process.env.REACT_APP_COMMIT_HASH || ''
export const URL_HASH = COMMIT_HASH === '' ? 'nocommit' : COMMIT_HASH

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
