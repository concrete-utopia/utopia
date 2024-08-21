import type { UserDetails } from 'prisma-client'
import { emptyCollaborators, userToCollaborator, type Collaborators } from '../types'

export function buildCollaboratorsFromProjects(
  projects: {
    proj_id: string
    ProjectCollaborator: {
      User: UserDetails
    }[]
  }[],
): Collaborators {
  let result = emptyCollaborators()
  for (const project of projects) {
    result.byProjectId[project.proj_id] = project.ProjectCollaborator.map((c) => c.User.user_id)
    for (const { User } of project.ProjectCollaborator) {
      result.byUserId[User.user_id] = userToCollaborator(User)
    }
  }
  return result
}

export function mergeCollaborators(list: Collaborators[]): Collaborators {
  let merged = emptyCollaborators()
  for (const collabs of list) {
    merged = {
      byUserId: {
        ...merged.byUserId,
        ...collabs.byUserId,
      },
      byProjectId: {
        ...merged.byProjectId,
        ...collabs.byProjectId,
      },
    }
  }
  return merged
}
