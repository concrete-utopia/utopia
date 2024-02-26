import { Prisma, UserDetails } from 'prisma-client'

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

export const AccessLevels = {
  PRIVATE: 0,
  PUBLIC: 1,
  WITH_LINK: 2,
} as const

export type AccessLevel = (typeof AccessLevels)[keyof typeof AccessLevels]
