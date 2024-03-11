import { Prisma, UserDetails } from 'prisma-client'
import { assertNever } from './util/assertNever'

const fullProject = Prisma.validator<Prisma.ProjectDefaultArgs>()({
  include: {
    ProjectAccess: true,
  },
})

type FullProject = Prisma.ProjectGetPayload<typeof fullProject>

export interface ProjectListing {
  id: string
  ownerName: string | null
  ownerPicture: string | null
  title: string
  description: string | null
  createdAt: string
  modifiedAt: string
}

export type ListProjectsResponse = {
  projects: ProjectListing[]
}

export type ProjectWithoutContent = Omit<FullProject, 'content'>

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
interface BaseOperation {
  projectId: string
}

function baseOperation(project: ProjectWithoutContent): BaseOperation {
  return {
    projectId: project.proj_id,
  }
}

type OperationRename = BaseOperation & {
  type: 'rename'
  newTitle: string
}

export function operationRename(project: ProjectWithoutContent, newTitle: string): OperationRename {
  return {
    type: 'rename',
    ...baseOperation(project),
    newTitle: newTitle,
  }
}

type OperationDelete = BaseOperation & {
  type: 'delete'
}

export function operationDelete(project: ProjectWithoutContent): OperationDelete {
  return { type: 'delete', ...baseOperation(project) }
}

type OperationDestroy = BaseOperation & {
  type: 'destroy'
}

export function operationDestroy(project: ProjectWithoutContent): OperationDestroy {
  return { type: 'destroy', ...baseOperation(project) }
}

type OperationRestore = BaseOperation & {
  type: 'restore'
}

export function operationRestore(project: ProjectWithoutContent): OperationRestore {
  return { type: 'restore', ...baseOperation(project) }
}

type OperationChangeAccess = BaseOperation & {
  type: 'changeAccess'
  newAccessLevel: AccessLevel
}

export function operationChangeAccess(
  project: ProjectWithoutContent,
  newAccessLevel: AccessLevel,
): OperationChangeAccess {
  return { type: 'changeAccess', ...baseOperation(project), newAccessLevel }
}

export type Operation =
  | OperationRename
  | OperationDelete
  | OperationDestroy
  | OperationRestore
  | OperationChangeAccess

export type OperationType = 'rename' | 'delete' | 'destroy' | 'restore' | 'changeAccess'

export function areBaseOperationsEquivalent(a: Operation, b: Operation): boolean {
  return a.projectId === b.projectId && a.type === b.type
}

export function getOperationDescription(op: Operation, project: ProjectWithoutContent) {
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
    default:
      assertNever(op)
  }
}
