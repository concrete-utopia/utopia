import type { ClientWriteRequest } from '@openfga/sdk'
import { AccessLevel } from '../types'
import { fgaClient } from './fgaClient.server'
import { assertNever } from '../util/assertNever'
import { mapArrayToDictionary } from '../util/common'

export async function updateAccessLevel(projectId: string, accessLevel: AccessLevel) {
  const writes = accessLevelToFgaWrites(projectId, accessLevel)
  return await Promise.all(writes.map((write) => fgaClient.write(write)))
}

export const fgaUserProjectPermission = [
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

export type FgaUserProjectPermission = (typeof fgaUserProjectPermission)[number]

export type PermissionsMatrix = Record<FgaUserProjectPermission, boolean>

async function checkUserProjectPermission(
  projectId: string,
  userId: string,
  permission: FgaUserProjectPermission,
): Promise<boolean> {
  const { allowed } = await fgaClient.check({
    user: `user:${userId}`,
    relation: permission,
    object: `project:${projectId}`,
  })
  return allowed ?? false
}

export async function getAllPermissions(
  projectId: string,
  userId: string,
): Promise<PermissionsMatrix> {
  const { relations } = await fgaClient.listRelations({
    user: `user:${userId}`,
    object: `project:${projectId}`,
    relations: fgaUserProjectPermission as unknown as string[],
  })
  return mapArrayToDictionary(
    fgaUserProjectPermission,
    (permission) => permission,
    (permission) => relations.includes(permission),
  )
}

export function getPermissionsOverride(value: boolean): PermissionsMatrix {
  return mapArrayToDictionary(
    fgaUserProjectPermission,
    (permission) => permission,
    () => value,
  )
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

export async function makeUserViewer(projectId: string, userId: string) {
  return fgaClient.write({
    writes: [{ user: `user:${userId}`, relation: 'viewer', object: `project:${projectId}` }],
  })
}

export async function makeUserCollaborator(projectId: string, userId: string) {
  return fgaClient.write({
    writes: [{ user: `user:${userId}`, relation: 'collaborator', object: `project:${projectId}` }],
  })
}

export async function makeUserEditor(projectId: string, userId: string) {
  return fgaClient.write({
    writes: [{ user: `user:${userId}`, relation: 'editor', object: `project:${projectId}` }],
  })
}

export async function makeUserAdmin(projectId: string, userId: string) {
  return fgaClient.write({
    writes: [{ user: `user:${userId}`, relation: 'admin', object: `project:${projectId}` }],
  })
}

export async function makeUserCreator(projectId: string, userId: string) {
  return fgaClient.write({
    writes: [{ user: `user:${userId}`, relation: 'creator', object: `project:${projectId}` }],
  })
}

function generalRelation(projectId: string, relation: string) {
  return {
    user: 'user:*',
    relation: relation,
    object: `project:${projectId}`,
  }
}

function accessLevelToFgaWrites(projectId: string, accessLevel: AccessLevel): ClientWriteRequest[] {
  switch (accessLevel) {
    case AccessLevel.PUBLIC:
      return [
        { writes: [generalRelation(projectId, 'viewer')] },
        { deletes: [generalRelation(projectId, 'collaborator')] },
      ]

    case AccessLevel.PRIVATE:
      return [
        { deletes: [generalRelation(projectId, 'viewer')] },
        { deletes: [generalRelation(projectId, 'collaborator')] },
        { deletes: [generalRelation(projectId, 'can_request_access')] },
      ]

    case AccessLevel.WITH_LINK:
      return [
        { writes: [generalRelation(projectId, 'viewer')] },
        { writes: [generalRelation(projectId, 'collaborator')] },
      ]

    case AccessLevel.COLLABORATIVE:
      return [
        { writes: [generalRelation(projectId, 'can_request_access')] },
        { deletes: [generalRelation(projectId, 'viewer')] },
        { deletes: [generalRelation(projectId, 'collaborator')] },
      ]

    default:
      assertNever(accessLevel)
  }
}

export async function revokeRelations(projectId: string, userId: string, relations: string[]) {
  return await Promise.all(
    relations.map((relation) =>
      fgaClient.write({
        deletes: [{ user: `user:${userId}`, relation: relation, object: `project:${projectId}` }],
      }),
    ),
  )
}
