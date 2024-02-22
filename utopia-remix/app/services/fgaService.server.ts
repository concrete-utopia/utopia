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

type UserProjectPermission =
  | 'can_view'
  | 'can_fork'
  | 'can_play'
  | 'can_edit'
  | 'can_comment'
  | 'can_show_presence'
  | 'can_see_live_changes'
  | 'can_request_access'

async function checkUserProjectPermission(
  projectId: string,
  userId: string,
  permission: UserProjectPermission,
) {
  const { allowed } = await fgaClient.check({
    user: `user:${userId}`,
    relation: permission,
    object: `project:${projectId}`,
  })
  return !!allowed
}

export async function canViewProject(projectId: string, userId: string) {
  return checkUserProjectPermission(projectId, userId, 'can_view')
}

export async function canForkProject(projectId: string, userId: string) {
  return checkUserProjectPermission(projectId, userId, 'can_fork')
}

export async function canPlayProject(projectId: string, userId: string) {
  return checkUserProjectPermission(projectId, userId, 'can_play')
}

export async function canEditProject(projectId: string, userId: string) {
  return checkUserProjectPermission(projectId, userId, 'can_edit')
}

export async function canCommentOnProject(projectId: string, userId: string) {
  return checkUserProjectPermission(projectId, userId, 'can_comment')
}

export async function canShowPresence(projectId: string, userId: string) {
  return checkUserProjectPermission(projectId, userId, 'can_show_presence')
}

export async function canRequestAccess(projectId: string, userId: string) {
  return checkUserProjectPermission(projectId, userId, 'can_request_access')
}

export async function canSeeLiveChanges(projectId: string, userId: string) {
  return checkUserProjectPermission(projectId, userId, 'can_see_live_changes')
}
