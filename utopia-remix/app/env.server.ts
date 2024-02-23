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
}

export type BrowserEnvironment = {
  EDITOR_URL?: string
}

export const BrowserEnvironment: BrowserEnvironment = {
  EDITOR_URL: process.env.REACT_APP_EDITOR_URL,
}

function mustEnv(key: string): string {
  const value = process.env[key]
  if (value == null) {
    throw new Error(`missing required environment variable ${key}`)
  }
  return value
}
