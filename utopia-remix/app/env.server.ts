declare global {
  namespace NodeJS {
    interface ProcessEnv {
      APP_ENV?: 'local' | 'stage' | 'prod' | 'test'
    }
  }
}

export const ServerEnvironment = {
  environment: process.env.APP_ENV,
  // The URL of the actual backend server in the form <scheme>://<host>:<port>
  BackendURL: process.env.BACKEND_URL ?? '',
  // the CORS allowed origin for incoming requests
  CORSOrigin: process.env.CORS_ORIGIN ?? '',
  // the Liveblocks secret key
  LiveblocksSecretKey: process.env.LIVEBLOCKS_SECRET_KEY,
}

export type BrowserEnvironment = {
  EDITOR_URL?: string
}

export const BrowserEnvironment: BrowserEnvironment = {
  EDITOR_URL: process.env.REACT_APP_EDITOR_URL,
}
