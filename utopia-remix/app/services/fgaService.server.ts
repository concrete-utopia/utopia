import { CredentialsMethod, OpenFgaClient } from '@openfga/sdk'
import { ServerEnvironment } from '../env.server'
import { AccessLevel } from '../types'

export const fgaClient = new OpenFgaClient({
  apiScheme: 'https',
  apiHost: 'api.us1.fga.dev',
  storeId: ServerEnvironment.FGA_STORE_ID,
  // authorizationModelId: 'YOUR_MODEL_ID', // Optionally, you can specify a model id to target, which can improve latency
  credentials: {
    method: CredentialsMethod.ClientCredentials,
    config: {
      apiTokenIssuer: 'fga.us.auth0.com',
      apiAudience: 'https://api.us1.fga.dev/',
      clientId: ServerEnvironment.FGA_CLIENT_ID,
      clientSecret: ServerEnvironment.FGA_SECRET,
    },
  },
})

export async function updateAccessLevel(projectId: string, accessLevel: AccessLevel) {
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

const userProjectPermission = [
  'can_view',
  'can_fork',
  'can_play',
  'can_edit',
  'can_comment',
  'can_show_presence',
  'can_see_live_changes',
  'can_request_access',
  'can_manage',
] as const

type UserProjectPermission = (typeof userProjectPermission)[number]

async function checkUserProjectPermission(
  projectId: string,
  userId: string,
  permission: UserProjectPermission,
): Promise<boolean> {
  const { allowed } = await fgaClient.check({
    user: `user:${userId}`,
    relation: permission,
    object: `project:${projectId}`,
  })
  return allowed ?? false
}

export async function canViewProject(projectId: string, userId: string): Promise<boolean> {
  return checkUserProjectPermission(projectId, userId, 'can_view')
}

export async function canForkProject(projectId: string, userId: string): Promise<boolean> {
  return checkUserProjectPermission(projectId, userId, 'can_fork')
}

export async function canPlayProject(projectId: string, userId: string): Promise<boolean> {
  return checkUserProjectPermission(projectId, userId, 'can_play')
}

export async function canEditProject(projectId: string, userId: string): Promise<boolean> {
  return checkUserProjectPermission(projectId, userId, 'can_edit')
}

export async function canCommentOnProject(projectId: string, userId: string): Promise<boolean> {
  return checkUserProjectPermission(projectId, userId, 'can_comment')
}

export async function canShowPresence(projectId: string, userId: string): Promise<boolean> {
  return checkUserProjectPermission(projectId, userId, 'can_show_presence')
}

export async function canRequestAccess(projectId: string, userId: string): Promise<boolean> {
  return checkUserProjectPermission(projectId, userId, 'can_request_access')
}

export async function canSeeLiveChanges(projectId: string, userId: string): Promise<boolean> {
  return checkUserProjectPermission(projectId, userId, 'can_see_live_changes')
}

export async function canManageProject(projectId: string, userId: string): Promise<boolean> {
  return checkUserProjectPermission(projectId, userId, 'can_manage')
}
