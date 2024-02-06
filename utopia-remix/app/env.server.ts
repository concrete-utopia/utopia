export const ServerEnvironment = {
  // The URL of the actual backend server in the form <scheme>://<host>:<port>
  BackendURL: process.env.BACKEND_URL ?? "",
  // the CORS allowed origin for incoming requests
  CORSOrigin: process.env.CORS_ORIGIN ?? "",
};

export type BrowserEnvironment = {
  EDITOR_URL?: string;
};

export const BrowserEnvironment: BrowserEnvironment = {
  EDITOR_URL: process.env.REACT_APP_EDITOR_URL,
};
