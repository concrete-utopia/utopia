// you can turn on/off debug features individually here

export const HOST: string = typeof window === 'undefined' ? '' : window.location.host
export const SCHEME: string = typeof window === 'undefined' ? 'http' : window.location.protocol
export const BARE_HOST = HOST.startsWith('www.') ? HOST.slice(4) : HOST
export const BASE_URL: string = `${SCHEME}//${HOST}/`

export const PRODUCTION_ENV: boolean = process.env.NODE_ENV === 'production'
const PRODUCTION_CONFIG: boolean = process.env.REACT_APP_ENVIRONMENT_CONFIG === 'production'
const STAGING_CONFIG: boolean = process.env.REACT_APP_ENVIRONMENT_CONFIG === 'staging'
const PRODUCTION_OR_STAGING_CONFIG = PRODUCTION_CONFIG || STAGING_CONFIG

export const PROBABLY_ELECTRON: boolean =
  typeof window === 'undefined' || (window as any)?.['process']?.['type'] != null

export const USE_WEBPACK_SERVER = process.env.WEBPACK_DEV_SERVER === 'true'

export const SHOW_FPS = false
export const DEEP_FREEZE_STATE = !PRODUCTION_ENV
export const RUN_PERFORMANCE_CHECK = false
export const REFERENCE_EQUALITY_CHECK = false

export const BASE_WS: string = PRODUCTION_OR_STAGING_CONFIG ? `wss://${HOST}/` : `ws://${HOST}/`

export const STATIC_BASE_URL: string =
  PRODUCTION_OR_STAGING_CONFIG && BARE_HOST !== 'localhost:8000'
    ? `https://cdn.${BARE_HOST}/`
    : `http://${BARE_HOST}/`

export const FLOATING_PREVIEW_BASE_URL: string = PRODUCTION_CONFIG
  ? `https://utopia.fm/`
  : STAGING_CONFIG
  ? 'https://utopia.baby/'
  : BASE_URL
export const PROPERTY_CONTROLS_INFO_BASE_URL: string = PRODUCTION_CONFIG
  ? `https://utopia.fm/`
  : STAGING_CONFIG
  ? 'https://utopia.baby/'
  : BASE_URL
export const MONACO_EDITOR_IFRAME_BASE_URL: string = PRODUCTION_CONFIG
  ? `https://utopia.fm/`
  : STAGING_CONFIG
  ? 'https://utopia.baby/'
  : BASE_URL
export const VSCODE_EDITOR_IFRAME_BASE_URL: string = PRODUCTION_CONFIG
  ? `https://utopia.app/`
  : STAGING_CONFIG
  ? 'https://utopia.pizza/'
  : BASE_URL
export const UTOPIA_BACKEND = BASE_URL + 'v1/'
export const UTOPIA_BACKEND_WS = BASE_WS + 'v1/'
export const ASSET_ENDPOINT = UTOPIA_BACKEND + 'asset/'
export const THUMBNAIL_ENDPOINT = UTOPIA_BACKEND + 'thumbnail/'

export const PREVIEW_IS_EMBEDDED = isEmbedded()

export const AUTH0_REDIRECT_URI: string = process.env.REACT_APP_AUTH0_REDIRECT_URI ?? ''
export const AUTH0_CLIENT_ID: string = process.env.REACT_APP_AUTH0_CLIENT_ID ?? ''
export const AUTH0_HOST: string = process.env.REACT_APP_AUTH0_ENDPOINT ?? ''
const USE_AUTH0 = AUTH0_REDIRECT_URI != '' && AUTH0_CLIENT_ID != '' && AUTH0_HOST != ''

export const GOOGLE_WEB_FONTS_KEY =
  process.env.GOOGLE_WEB_FONTS_KEY !== ''
    ? process.env.GOOGLE_WEB_FONTS_KEY
    : 'AIzaSyBffJtCo2vL68hdQKH3IYjo0ELFAAGYNW4'

export type AuthRedirectBehaviour = 'redirect' | 'auto-close'

export function auth0Url(behaviour: AuthRedirectBehaviour): string {
  let url: URL
  if (USE_AUTH0) {
    url = new URL(`https://${AUTH0_HOST}/authorize`)
    url.searchParams.set('scope', 'openid profile email')
    url.searchParams.set('response_type', 'code')
    url.searchParams.set('client_id', AUTH0_CLIENT_ID)

    const redirectURL = new URL(AUTH0_REDIRECT_URI)
    redirectURL.searchParams.set('onto', behaviour)

    url.searchParams.set('redirect_uri', redirectURL.href)
  } else {
    url = new URL(`${BASE_URL}authenticate`)
    url.searchParams.set('code', 'logmein')
    url.searchParams.set('onto', behaviour)
  }
  return url.href
}

const COMMIT_HASH = process.env.REACT_APP_COMMIT_HASH ?? ''
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
