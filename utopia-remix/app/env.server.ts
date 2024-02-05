declare global {
  namespace NodeJS {
    interface ProcessEnv {
      SERVER_ENV?: "local" | "stage" | "prod" | "test";
    }
  }
}

export const ServerEnvironment = {
  environment: process.env.SERVER_ENV,
  // The URL of the actual backend server in the form <scheme>://<host>:<port>
  BackendURL: process.env.BACKEND_URL ?? "",
  // the CORS allowed origin for incoming requests
  CORSOrigin: process.env.CORS_ORIGIN ?? "",
};
