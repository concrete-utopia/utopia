import * as fgaService from './fgaService.server'

export const UserProjectPermission = {
  CAN_VIEW_PROJECT: 0,
  CAN_FORK_PROJECT: 1,
  CAN_PLAY_PROJECT: 2,
  CAN_EDIT_PROJECT: 3,
  CAN_COMMENT_PROJECT: 4,
  CAN_SHOW_PRESENCE: 5,
  CAN_REQUEST_ACCESS: 6,
  CAN_SEE_LIVE_CHANGES: 7,
} as const

export type UserProjectPermission =
  (typeof UserProjectPermission)[keyof typeof UserProjectPermission]

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
