/* eslint-disable @typescript-eslint/no-namespace */

declare global {
  namespace NodeJS {
    interface ProcessEnv {
      APP_ENV?: AppEnv
    }
  }
}

export type AppEnv = 'local' | 'stage' | 'prod' | 'test'

const environment = getAppEnv()
console.info(`Remix app environment: ${environment}`)

export const ServerEnvironment = {
  environment: environment,
  // The URL of the actual backend server in the form <scheme>://<host>:<port>
  BACKEND_URL: mustEnvOrLocalFallback('BACKEND_URL', 'http://localhost:8002'),
  // the CORS allowed origin for incoming requests (comma separated)
  CORS_ORIGIN: mustEnvOrLocalFallback('CORS_ORIGIN', 'http://localhost:8000'),
  // Auth0 credentials
  AUTH0_ENDPOINT: mustEnvOrLocalFallback('AUTH0_ENDPOINT', ''),
  AUTH0_CLIENT_ID: mustEnvOrLocalFallback('AUTH0_CLIENT_ID', ''),
  AUTH0_REDIRECT_URI: mustEnvOrLocalFallback('AUTH0_REDIRECT_URI', ''),
  // FGA Credentials
  FGA_STORE_ID: mustEnvOrLocalFallback('FGA_STORE_ID', ''),
  FGA_CLIENT_ID: mustEnvOrLocalFallback('FGA_CLIENT_ID', ''),
  FGA_SECRET: mustEnvOrLocalFallback('FGA_SECRET', ''),
  FGA_API_HOST: mustEnvOrLocalFallback('FGA_API_HOST', ''),
  FGA_API_TOKEN_ISSUER: mustEnvOrLocalFallback('FGA_API_TOKEN_ISSUER', ''),
  FGA_API_AUDIENCE: mustEnvOrLocalFallback('FGA_API_AUDIENCE', ''),
  // Github OAuth credentials
  GITHUB_OAUTH_CLIENT_ID: mustEnvOrLocalFallback('GITHUB_OAUTH_CLIENT_ID', ''),
  GITHUB_OAUTH_REDIRECT_URL: mustEnvOrLocalFallback('GITHUB_OAUTH_REDIRECT_URL', ''),
  // S3 Configuration
  AWS_S3_BUCKET: mustEnvOrLocalFallback('AWS_S3_BUCKET', ''),
  AWS_ACCESS_KEY_ID: mustEnvOrLocalFallback('AWS_ACCESS_KEY_ID', ''),
  AWS_SECRET_ACCESS_KEY: mustEnvOrLocalFallback('AWS_SECRET_ACCESS_KEY', ''),
  AWS_REGION: mustEnvOrLocalFallback('AWS_REGION', ''),
  // Local assets
  LOCAL_ASSETS_FOLDER: mustEnvOrLocalFallback('LOCAL_ASSETS_FOLDER', '../server/utopia-local'),
}

export type BrowserEnvironmentType = {
  EDITOR_URL: string
  UTOPIA_CDN_URL: string
}

export const BrowserEnvironment: BrowserEnvironmentType = {
  EDITOR_URL: mustEnvOrLocalFallback('EDITOR_URL', 'http://localhost:8000'),
  UTOPIA_CDN_URL: mustEnvOrLocalFallback('UTOPIA_CDN_URL', 'http://localhost:8000'),
}

/**
 * If the APP_ENV variable is set, return that.
 * If it's not set, try to guess it from the NODE_ENV.
 * If everything else fails, throw an error, because this should _never_ happen.
 */
function getAppEnv(): AppEnv {
  const env = process.env['APP_ENV'] ?? ''
  if (env !== '') {
    return env
  }

  switch (process.env.NODE_ENV) {
    case 'development':
      return 'local'
    case 'production':
      return 'prod'
    case 'test':
      return 'test'
    default:
      throw new Error('cannot determine app environment')
  }
}

function mustEnvOrLocalFallback(key: string, localFallback: string) {
  const value = process.env[key] ?? ''
  if (value !== '') {
    return value
  }
  if (environment === 'local' || environment === 'test') {
    return localFallback
  }
  throw new Error(`missing required environment variable ${key}`)
}
