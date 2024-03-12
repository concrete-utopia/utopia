import { CredentialsMethod, ErrorCode, FgaApiValidationError, OpenFgaClient } from '@openfga/sdk'
import { ServerEnvironment } from '../env.server'

const mockOpenFgaClient = {
  write: async () => {},
  check: async () => ({ allowed: true }),
}

export const fgaClient = createFgaClient()

export function createFgaClient() {
  const fgaClientMode = getFgaClientMode()
  if (fgaClientMode === 'local_mock') {
    return mockOpenFgaClient
  }
  return new OpenFgaClient({
    apiScheme: ServerEnvironment.FGA_API_SCHEME,
    apiHost: ServerEnvironment.FGA_API_HOST,
    storeId: ServerEnvironment.FGA_STORE_ID,
    // authorizationModelId: 'YOUR_MODEL_ID', // optional
    credentials: getCredentials(fgaClientMode),
  })
}

function getCredentials(fgaClientMode: 'local' | 'remote') {
  // if working against a local server, we don't need credentials
  if (fgaClientMode === 'local') {
    return { method: CredentialsMethod.None } as const
  } else {
    return {
      method: CredentialsMethod.ClientCredentials,
      config: {
        apiTokenIssuer: ServerEnvironment.FGA_API_TOKEN_ISSUER,
        apiAudience: ServerEnvironment.FGA_API_AUDIENCE,
        clientId: ServerEnvironment.FGA_CLIENT_ID,
        clientSecret: ServerEnvironment.FGA_SECRET,
      },
    } as const
  }
}

function getFgaClientMode(): 'local_mock' | 'local' | 'remote' {
  if (ServerEnvironment.environment === 'local') {
    if (ServerEnvironment.FGA_API_HOST == '') {
      return 'local_mock'
    }
    if (ServerEnvironment.FGA_API_HOST.includes('localhost')) {
      return 'local'
    }
  }
  return 'remote'
}

export async function safeFgaWrite(params: object) {
  try {
    await fgaClient.write(params)
  } catch (err) {
    if ((err as FgaApiValidationError).apiErrorCode === ErrorCode.WriteFailedDueToInvalidInput) {
      // FGA throws a hard error on that, but we want to ignore it (since it's a no-op)
      console.error('Failed writing an existing tuple, or deleting a non-existing tuple', err)
    } else {
      throw err
    }
  }
}
