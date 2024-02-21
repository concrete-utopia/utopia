import { CredentialsMethod, OpenFgaClient } from '@openfga/sdk'
import { AccessLevel } from '~/types'

const fgaClient = new OpenFgaClient({
  apiScheme: 'https',
  apiHost: 'api.us1.fga.dev',
  storeId: process.env.FGA_STORE_ID!,
  // authorizationModelId: 'YOUR_MODEL_ID', // Optionally, you can specify a model id to target, which can improve latency
  credentials: {
    method: CredentialsMethod.ClientCredentials,
    config: {
      apiTokenIssuer: 'fga.us.auth0.com',
      apiAudience: 'https://api.us1.fga.dev/',
      clientId: process.env.FGA_CLIENT_ID!,
      clientSecret: process.env.FGA_SECRET!,
    },
  },
})

export async function updateAccessLevel(projectId: string, accessLevel: number) {
  console.log('projectId', projectId)
  switch (accessLevel) {
    case AccessLevel.PUBLIC:
      await fgaClient.write({
        writes: [
          {
            user: 'user:*',
            relation: 'viewer',
            object: `project:${projectId}`,
          },
        ],
      })
      break
    case AccessLevel.PRIVATE:
      await fgaClient.write({
        deletes: [
          {
            user: 'user:*',
            relation: 'viewer',
            object: `project:${projectId}`,
          },
        ],
      })
      break
  }
}

export async function canViewProject(projectId: string, userId: string) {
  const { allowed } = await fgaClient.check({
    user: `user:${userId}`,
    relation: 'can_view',
    object: `project:${projectId}`,
  })
  return allowed
}
