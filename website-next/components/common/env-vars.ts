// you can turn on/off debug features individually here

export const HOSTNAME: string = typeof window === 'undefined' ? '' : window.location.hostname
export const SCHEME: string = typeof window === 'undefined' ? 'http' : window.location.protocol
export const BARE_HOSTNAME = HOSTNAME.startsWith('www.') ? HOSTNAME.slice(4) : HOSTNAME
const isLocalHost = BARE_HOSTNAME.startsWith('localhost')
export const PORT: string =
  typeof window === 'undefined' || window.location.port.length === 0
    ? ''
    : `:${window.location.port}`
export const BARE_HOST = `${BARE_HOSTNAME}${PORT}`
export const BASE_URL: string = `${SCHEME}//${HOSTNAME}${PORT}/`

export const PRODUCTION_ENV: boolean = process.env.NODE_ENV === 'production'
export const PRODUCTION_CONFIG: boolean = process.env.REACT_APP_ENVIRONMENT_CONFIG === 'production'
const STAGING_CONFIG: boolean = process.env.REACT_APP_ENVIRONMENT_CONFIG === 'staging'
const BRANCHES_CONFIG: boolean = process.env.REACT_APP_ENVIRONMENT_CONFIG === 'branches'
const PRODUCTION_OR_STAGING_CONFIG = PRODUCTION_CONFIG || STAGING_CONFIG || BRANCHES_CONFIG

export const IS_JEST_ENVIRONMENT = process.env.JEST_WORKER_ID != null

export const IS_TEST_ENVIRONMENT: boolean =
  IS_JEST_ENVIRONMENT ||
  (typeof window != 'undefined' && (window as any)?.KarmaTestEnvironment != null)

export const DEVELOPMENT_ENV: boolean =
  !PRODUCTION_OR_STAGING_CONFIG && !IS_TEST_ENVIRONMENT && HOSTNAME === 'localhost'

const USE_BFF: boolean = process.env.USE_BFF === 'true'

type BackendType =
  | 'bff' // proxied calls via the Remix BFF
  | 'direct' // direct calls to the backend

const LOCAL_BACKEND_PORTS: { [type in BackendType]: number } = {
  direct: 8002,
  bff: 8000,
}

export const BACKEND_TYPE: BackendType = DEVELOPMENT_ENV || USE_BFF ? 'bff' : 'direct'

export function isBackendBFF(): boolean {
  return BACKEND_TYPE === 'bff'
}

export const UTOPIA_BACKEND_BASE_URL = isLocalHost
  ? `${SCHEME}//${HOSTNAME}:${LOCAL_BACKEND_PORTS[BACKEND_TYPE]}/`
  : BASE_URL

export const UTOPIA_BACKEND = UTOPIA_BACKEND_BASE_URL + 'v1/'

const SECONDARY_BASE_URL: string = PRODUCTION_CONFIG
  ? `https://utopia.fm/`
  : STAGING_CONFIG
  ? 'https://utopia95.com/'
  : BRANCHES_CONFIG
  ? 'https://momentumworks.co/'
  : BARE_HOST === 'localhost:8000'
  ? 'http://localhost:8001'
  : BASE_URL

export const PROBABLY_ELECTRON: boolean =
  typeof window === 'undefined' || (window as any)?.['process']?.['type'] != null

export const HMR: boolean = typeof process.env.HMR === 'boolean' ? process.env.HMR : false

export const SHOW_FPS = false
export const DEEP_FREEZE_STATE = !PRODUCTION_ENV
export const RUN_PERFORMANCE_CHECK = false
export const REFERENCE_EQUALITY_CHECK = false

export const STATIC_BASE_URL: string =
  PRODUCTION_OR_STAGING_CONFIG && BARE_HOST !== 'localhost:8000'
    ? `https://cdn.${BARE_HOST}/`
    : `http://${BARE_HOST}/`

export const FLOATING_PREVIEW_BASE_URL: string = SECONDARY_BASE_URL
export const PROPERTY_CONTROLS_INFO_BASE_URL: string = SECONDARY_BASE_URL
export const MONACO_EDITOR_IFRAME_BASE_URL: string = SECONDARY_BASE_URL

export const ASSET_ENDPOINT = UTOPIA_BACKEND + 'asset/'
export const THUMBNAIL_ENDPOINT = UTOPIA_BACKEND + 'thumbnail/'

export const PREVIEW_IS_EMBEDDED = isEmbedded()

export const AUTH0_REDIRECT_URI: string = BASE_URL + 'authenticate'
export const AUTH0_CLIENT_ID: string = process.env.REACT_APP_AUTH0_CLIENT_ID ?? ''
export const AUTH0_HOST: string = process.env.REACT_APP_AUTH0_ENDPOINT ?? ''
const USE_AUTH0 = AUTH0_CLIENT_ID != '' && AUTH0_HOST != ''

export const GOOGLE_WEB_FONTS_KEY =
  process.env.GOOGLE_WEB_FONTS_KEY !== ''
    ? process.env.GOOGLE_WEB_FONTS_KEY
    : 'AIzaSyBffJtCo2vL68hdQKH3IYjo0ELFAAGYNW4'

export type AuthRedirectBehaviour = 'redirect' | 'auto-close'

export function auth0Url(behaviour: AuthRedirectBehaviour): string {
  const searchParams = new URLSearchParams(window?.location?.search ?? '')
  const fakeUser = searchParams.get('fakeUser')
  if (fakeUser != null) {
    const url = new URL(`${BASE_URL}authenticate`)
    url.searchParams.set('code', fakeUser)
    url.searchParams.set('onto', behaviour)
    return url.href
  }

  if (USE_AUTH0) {
    const url = new URL(`https://${AUTH0_HOST}/authorize`)
    url.searchParams.set('scope', 'openid profile email')
    url.searchParams.set('response_type', 'code')
    url.searchParams.set('client_id', AUTH0_CLIENT_ID)

    const redirectURL = new URL(AUTH0_REDIRECT_URI)
    redirectURL.searchParams.set('onto', behaviour)

    url.searchParams.set('redirect_uri', redirectURL.href)
    return url.href
  }

  const url = new URL(`${BASE_URL}authenticate`)
  url.searchParams.set('code', 'logmein')
  url.searchParams.set('onto', behaviour)

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

export const PERFORMANCE_MARKS_ALLOWED =
  typeof window !== 'undefined' && typeof window?.performance?.mark === 'function'
