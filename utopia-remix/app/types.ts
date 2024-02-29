import { Project, UserDetails } from 'prisma-client'
import { assertNever } from './util/assertNever'

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

export type ProjectWithoutContent = Omit<Project, 'content'>

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

interface BaseOperation {
  projectId: string
  projectName: string
}

function baseOperation(project: ProjectWithoutContent): BaseOperation {
  return {
    projectId: project.proj_id,
    projectName: project.title,
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

export type Operation = OperationRename | OperationDelete | OperationDestroy | OperationRestore

export type OperationType = 'rename' | 'delete' | 'destroy' | 'restore'

export function areBaseOperationsEquivalent(a: Operation, b: Operation): boolean {
  return a.projectId === b.projectId && a.type === b.type
}

export function getOperationVerb(op: Operation) {
  switch (op.type) {
    case 'delete':
      return 'Deleting'
    case 'destroy':
      return 'Destroying'
    case 'rename':
      return 'Renaming'
    case 'restore':
      return 'Restoring'
    default:
      assertNever(op)
  }
}
