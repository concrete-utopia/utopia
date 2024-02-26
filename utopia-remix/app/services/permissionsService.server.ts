import * as fgaService from './fgaService.server'

export enum UserProjectPermission {
  CAN_VIEW_PROJECT,
  CAN_FORK_PROJECT,
  CAN_PLAY_PROJECT,
  CAN_EDIT_PROJECT,
  CAN_COMMENT_PROJECT,
  CAN_SHOW_PRESENCE,
  CAN_REQUEST_ACCESS,
  CAN_SEE_LIVE_CHANGES,
}

export async function hasUserProjectPermission(
  projectId: string,
  userId: string,
  permission: UserProjectPermission,
) {
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
    default:
      exaustiveMatch(permission)
  }
}

function exaustiveMatch(x: never) {
  throw new Error(`Unmatched value: ${x}`)
}

export async function getAllPermissions(projectId: string, userId: string) {
  return fgaService.getAllPermissions(projectId, userId)
}
