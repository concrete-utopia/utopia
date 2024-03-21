import type { ProjectAccessRequest, UserDetails } from 'prisma-client'
import { Prisma } from 'prisma-client'
import { assertNever } from './util/assertNever'

const fullProjectFromDB = Prisma.validator<Prisma.ProjectDefaultArgs>()({
  include: {
    ProjectAccess: true,
  },
})

type FullProjectFromDB = Prisma.ProjectGetPayload<typeof fullProjectFromDB>

export type ProjectWithoutContentFromDB = Omit<FullProjectFromDB, 'content'>

export type ProjectListing = ProjectWithoutContentFromDB & {
  hasPendingRequests?: boolean
}

// Legacy response
export interface ProjectListingV1 {
  id: string
  ownerName: string | null
  ownerPicture: string | null
  title: string
  description: string | null
  createdAt: string
  modifiedAt: string
}

export type ListProjectsResponseV1 = {
  projects: ProjectListingV1[]
}

export interface Collaborator {
  id: string
  name: string | null
  avatar: string | null
}

export type CollaboratorsByProject = { [projectId: string]: Collaborator[] }

export function userToCollaborator(user: UserDetails): Collaborator {
  return {
    id: user.user_id,
    name: user.name,
    avatar: user.picture,
  }
}

export const AccessLevel = {
  PRIVATE: 0,
  PUBLIC: 1,
  WITH_LINK: 2,
  COLLABORATIVE: 3,
} as const

export type AccessLevel = (typeof AccessLevel)[keyof typeof AccessLevel]

export function asAccessLevel(accessLevel: number | undefined | null): AccessLevel | null {
  switch (accessLevel) {
    case AccessLevel.PRIVATE:
      return AccessLevel.PRIVATE
    case AccessLevel.PUBLIC:
      return AccessLevel.PUBLIC
    case AccessLevel.WITH_LINK:
      return AccessLevel.WITH_LINK
    case AccessLevel.COLLABORATIVE:
      return AccessLevel.COLLABORATIVE
    default:
      return null
  }
}

export const UserProjectPermission = {
  CAN_VIEW_PROJECT: 0,
  CAN_FORK_PROJECT: 1,
  CAN_PLAY_PROJECT: 2,
  CAN_EDIT_PROJECT: 3,
  CAN_COMMENT_PROJECT: 4,
  CAN_SHOW_PRESENCE: 5,
  CAN_REQUEST_ACCESS: 6,
  CAN_SEE_LIVE_CHANGES: 7,
  CAN_MANAGE_PROJECT: 8,
} as const

export type UserProjectPermission =
  (typeof UserProjectPermission)[keyof typeof UserProjectPermission]

export const UserProjectRole = {
  VIEWER: 0,
  COLLABORATOR: 1,
  EDITOR: 2,
  ADMIN: 3,
} as const

export type UserProjectRole = (typeof UserProjectRole)[keyof typeof UserProjectRole]

export function asUserProjectRole(role: number | undefined | null): UserProjectRole | null {
  switch (role) {
    case UserProjectRole.VIEWER:
      return UserProjectRole.VIEWER
    case UserProjectRole.COLLABORATOR:
      return UserProjectRole.COLLABORATOR
    case UserProjectRole.EDITOR:
      return UserProjectRole.EDITOR
    case UserProjectRole.ADMIN:
      return UserProjectRole.ADMIN
    default:
      return null
  }
}
interface BaseOperation {
  projectId: string
}

function baseOperation(project: ProjectListing): BaseOperation {
  return {
    projectId: project.proj_id,
  }
}

type OperationRename = BaseOperation & {
  type: 'rename'
  newTitle: string
}

export function operationRename(project: ProjectListing, newTitle: string): OperationRename {
  return {
    type: 'rename',
    ...baseOperation(project),
    newTitle: newTitle,
  }
}

type OperationDelete = BaseOperation & {
  type: 'delete'
}

export function operationDelete(project: ProjectListing): OperationDelete {
  return { type: 'delete', ...baseOperation(project) }
}

type OperationDestroy = BaseOperation & {
  type: 'destroy'
}

export function operationDestroy(project: ProjectListing): OperationDestroy {
  return { type: 'destroy', ...baseOperation(project) }
}

type OperationRestore = BaseOperation & {
  type: 'restore'
}

export function operationRestore(project: ProjectListing): OperationRestore {
  return { type: 'restore', ...baseOperation(project) }
}

type OperationChangeAccess = BaseOperation & {
  type: 'changeAccess'
  newAccessLevel: AccessLevel
}

export function operationChangeAccess(
  project: ProjectListing,
  newAccessLevel: AccessLevel,
): OperationChangeAccess {
  return { type: 'changeAccess', ...baseOperation(project), newAccessLevel: newAccessLevel }
}

type OperationApproveAccessRequest = BaseOperation & {
  type: 'approveAccessRequest'
  tokenId: string
}

export function operationApproveAccessRequest(
  project: ProjectListing,
  tokenId: string,
): OperationApproveAccessRequest {
  return { type: 'approveAccessRequest', ...baseOperation(project), tokenId: tokenId }
}

export type Operation =
  | OperationRename
  | OperationDelete
  | OperationDestroy
  | OperationRestore
  | OperationChangeAccess
  | OperationApproveAccessRequest

export type OperationType =
  | 'rename'
  | 'delete'
  | 'destroy'
  | 'restore'
  | 'changeAccess'
  | 'approveAccessRequest'

export function areBaseOperationsEquivalent(a: Operation, b: Operation): boolean {
  return a.projectId === b.projectId && a.type === b.type
}

export function getOperationDescription(op: Operation, project: ProjectListing) {
  switch (op.type) {
    case 'delete':
      return `Deleting project ${project.title}`
    case 'destroy':
      return `Destroying project ${project.title}`
    case 'rename':
      return `Renaming project ${project.title} to ${op.newTitle}`
    case 'restore':
      return `Restoring project ${project.title}`
    case 'changeAccess':
      return `Changing access level of project ${project.title}`
    case 'approveAccessRequest':
      return `Granting access request to project ${project.title}`
    default:
      assertNever(op)
  }
}

export enum AccessRequestStatus {
  PENDING,
  APPROVED,
  REJECTED,
}

export type ProjectAccessRequestWithUserDetails = ProjectAccessRequest & {
  User: UserDetails | null
}

export function isProjectAccessRequestWithUserDetails(
  u: unknown,
): u is ProjectAccessRequestWithUserDetails {
  const maybe = u as ProjectAccessRequestWithUserDetails
  return (
    u != null &&
    typeof u === 'object' &&
    maybe.id != null &&
    maybe.status != null &&
    maybe.user_id != null &&
    maybe.project_id != null
  )
}

export function isProjectAccessRequestWithUserDetailsArray(
  u: unknown,
): u is ProjectAccessRequestWithUserDetails[] {
  const maybe = u as ProjectAccessRequestWithUserDetails[]
  return (
    u != null &&
    typeof u === 'object' &&
    Array.isArray(u) &&
    maybe.every(isProjectAccessRequestWithUserDetails)
  )
}

export type HasPendingRequests = {
  hasPendingRequests: boolean
}

export function isHasPendingRequests(u: unknown): u is HasPendingRequests {
  const maybe = u as HasPendingRequests
  return u != null && typeof u === 'object' && maybe.hasPendingRequests != null
}
