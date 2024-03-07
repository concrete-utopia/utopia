declare global {
  namespace NodeJS {
    interface ProcessEnv {
      APP_ENV?: AppEnv
    }
  }
}

export type AppEnv = 'local' | 'stage' | 'prod' | 'test'

export const ServerEnvironment = {
  environment: mustEnv('APP_ENV') as AppEnv,
  // The URL of the actual backend server in the form <scheme>://<host>:<port>
  BackendURL: mustEnv('BACKEND_URL'),
  // the CORS allowed origin for incoming requests
  CORSOrigin: mustEnv('CORS_ORIGIN'),
  // Auth0 credentials
  AUTH0_ENDPOINT: optionalEnv('AUTH0_ENDPOINT', '<AUTH0_ENDPOINT>'),
  AUTH0_CLIENT_ID: optionalEnv('AUTH0_CLIENT_ID', '<AUTH0_CLIENT_ID>'),
  AUTH0_REDIRECT_URI: optionalEnv('AUTH0_REDIRECT_URI', '<AUTH0_REDIRECT_URI>'),
  // Github OAuth credentials
  GITHUB_OAUTH_CLIENT_ID: optionalEnv('GITHUB_OAUTH_CLIENT_ID', ''),
  GITHUB_OAUTH_REDIRECT_URL: optionalEnv('GITHUB_OAUTH_REDIRECT_URL', ''),
}

export type BrowserEnvironment = {
  EDITOR_URL: string
  UTOPIA_CDN_URL: string
}

export const BrowserEnvironment: BrowserEnvironment = {
  EDITOR_URL: mustEnv('REACT_APP_EDITOR_URL'),
  UTOPIA_CDN_URL: optionalEnv('UTOPIA_CDN_URL', ''),
}

function mustEnv(key: string): string {
  const value = process.env[key]
  if (value == null) {
    throw new Error(`missing required environment variable ${key}`)
  }
  return value
}

function optionalEnv(key: string, defaultValue: string): string {
  const value = process.env[key]
  return value ?? defaultValue
}
