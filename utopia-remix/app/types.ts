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

export const AccessLevel = {
  PRIVATE: 0,
  PUBLIC: 1,
  WITH_LINK: 2,
} as const

export type AccessLevel = (typeof AccessLevel)[keyof typeof AccessLevel]

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
