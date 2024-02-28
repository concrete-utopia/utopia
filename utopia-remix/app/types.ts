import { Project, UserDetails } from 'prisma-client'

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

export interface Operation {
  projectId: string
  projectName: string
  type: OperationType
}

export type OperationType = 'rename' | 'delete' | 'destroy' | 'restore'

export function areBaseOperationsEquivalent(a: Operation, b: Operation): boolean {
  return a.projectId === b.projectId && a.type === b.type
}

export function operation(project: ProjectWithoutContent, type: OperationType): Operation {
  return {
    projectId: project.proj_id,
    projectName: project.title,
    type: type,
  }
}
