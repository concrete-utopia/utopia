import { CredentialsMethod, ErrorCode, OpenFgaClient } from '@openfga/sdk'
import type {
  ClientRequestOptsWithAuthZModelId,
  ClientWriteRequest,
  ClientWriteRequestOpts,
  ClientWriteResponse,
  FgaApiValidationError,
} from '@openfga/sdk'
import { ServerEnvironment } from '../env.server'
import { singleton } from '../singleton.server'

const mockOpenFgaClient = {
  write: async () => {},
  check: async () => ({ allowed: true }),
}

export const fgaClient = singleton('fgaClient', createFgaClient)

class WriteSafeOpenFgaClient extends OpenFgaClient {
  async write(
    body: ClientWriteRequest,
    options?: ClientRequestOptsWithAuthZModelId & ClientWriteRequestOpts,
  ): Promise<ClientWriteResponse> {
    let writeResponse: ClientWriteResponse = { writes: [], deletes: [] }
    try {
      writeResponse = await super.write(body, options)
    } catch (err) {
      if ((err as FgaApiValidationError).apiErrorCode === ErrorCode.WriteFailedDueToInvalidInput) {
        // FGA throws a hard error on that, but we want to ignore it (since it's a no-op)
        console.error('Failed writing an existing tuple, or deleting a non-existing tuple', err)
      } else {
        throw err
      }
    }
    return writeResponse
  }
}

export function createFgaClient() {
  const fgaClientMode = getFgaClientMode()
  if (fgaClientMode === 'local_mock') {
    return mockOpenFgaClient
  }
  return new WriteSafeOpenFgaClient({
    apiScheme: ServerEnvironment.FGA_API_SCHEME,
    apiHost: ServerEnvironment.FGA_API_HOST,
    storeId: ServerEnvironment.FGA_STORE_ID,
    // authorizationModelId: 'YOUR_MODEL_ID', // optional
    credentials: getCredentials(fgaClientMode),
  })
}

type CredentialsConfig =
  | {
      method: CredentialsMethod.None
    }
  | {
      method: CredentialsMethod.ClientCredentials
      config: {
        apiTokenIssuer: string
        apiAudience: string
        clientId: string
        clientSecret: string
      }
    }
function getCredentials(fgaClientMode: 'local' | 'remote'): CredentialsConfig {
  // if working against a local server, we don't need credentials
  if (fgaClientMode === 'local') {
    return { method: CredentialsMethod.None }
  } else {
    return {
      method: CredentialsMethod.ClientCredentials,
      config: {
        apiTokenIssuer: ServerEnvironment.FGA_API_TOKEN_ISSUER,
        apiAudience: ServerEnvironment.FGA_API_AUDIENCE,
        clientId: ServerEnvironment.FGA_CLIENT_ID,
        clientSecret: ServerEnvironment.FGA_SECRET,
      },
    }
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
