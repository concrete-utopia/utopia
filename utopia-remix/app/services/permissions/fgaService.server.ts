import { CredentialsMethod, OpenFgaClient } from '@openfga/sdk'
import { ServerEnvironment } from '../../env.server'
import { AccessLevel } from '../../types'

export const fgaClient = new OpenFgaClient({
  apiScheme: ServerEnvironment.FGA_API_SCHEME,
  apiHost: ServerEnvironment.FGA_API_HOST,
  storeId: ServerEnvironment.FGA_STORE_ID,
  // authorizationModelId: <ODEL_ID>, // we can specifiy a model id here, otherwise it defaults to the latest
  credentials: getCredentials(),
})

function getCredentials() {
  if (ServerEnvironment.environment === 'local') {
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
