import { assertNever } from '../util/assertNever'
import type { AccessLevel } from '../types'
import { UserProjectPermission, UserProjectRole } from '../types'
import * as fgaService from './fgaService.server'

export const ANONYMOUS_USER_ID = '__ANON__'

export async function hasUserProjectPermission(
  projectId: string,
  requestUserId: string | null,
  permission: UserProjectPermission,
): Promise<boolean> {
  const userId = requestUserId ?? ANONYMOUS_USER_ID
  switch (permission) {
    case UserProjectPermission.CAN_VIEW_PROJECT:
      return fgaService.canViewProject(projectId, userId)
    case UserProjectPermission.CAN_FORK_PROJECT:
      return fgaService.canForkProject(projectId, userId)
    case UserProjectPermission.CAN_PLAY_PROJECT:
      return fgaService.canPlayProject(projectId, userId)
    case UserProjectPermission.CAN_EDIT_PROJECT:
      return fgaService.canEditProject(projectId, userId)
    case UserProjectPermission.CAN_COMMENT_PROJECT:
      return fgaService.canCommentOnProject(projectId, userId)
    case UserProjectPermission.CAN_SHOW_PRESENCE:
      return fgaService.canShowPresence(projectId, userId)
    case UserProjectPermission.CAN_REQUEST_ACCESS:
      return fgaService.canRequestAccess(projectId, userId)
    case UserProjectPermission.CAN_SEE_LIVE_CHANGES:
      return fgaService.canSeeLiveChanges(projectId, userId)
    case UserProjectPermission.CAN_MANAGE_PROJECT:
      return fgaService.canManageProject(projectId, userId)
    default:
      assertNever(permission)
  }
}

export async function setProjectAccess(projectId: string, accessLevel: AccessLevel) {
  await fgaService.updateAccessLevel(projectId, accessLevel)
}

export async function getAllPermissions(
  projectId: string,
  userId: string | null,
  ownerId: string,
): Promise<fgaService.PermissionsMatrix> {
  // this is here since it's part of our internal logic, not part of FGA
  if (userId === ownerId) {
    return fgaService.getPermissionsOverride(true)
  }
  return fgaService.getAllPermissions(projectId, userId ?? ANONYMOUS_USER_ID)
}

export async function grantProjectRoleToUser(
  projectId: string,
  userId: string,
  role: UserProjectRole,
) {
  switch (role) {
    case UserProjectRole.VIEWER:
      return await Promise.all([
        fgaService.makeUserViewer(projectId, userId),
        // we're keeping collaborator role separate from viewer, to be able to grant it separately in the future
        fgaService.makeUserCollaborator(projectId, userId),
      ])
    case UserProjectRole.COLLABORATOR:
      return await fgaService.makeUserCollaborator(projectId, userId)
    case UserProjectRole.EDITOR:
      return await fgaService.makeUserEditor(projectId, userId)
    case UserProjectRole.ADMIN:
      return await fgaService.makeUserAdmin(projectId, userId)
    default:
      assertNever(role)
  }
}

export async function revokeAllRolesFromUser(projectId: string, userId: string) {
  return await fgaService.revokeRelations(projectId, userId, [
    'viewer',
    'collaborator',
    'editor',
    'admin',
  ])
}

export async function makeUserCreator(projectId: string, userId: string) {
  return fgaService.makeUserCreator(projectId, userId)
}
