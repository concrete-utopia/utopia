import type { UserDetails } from 'prisma-client'
import { prisma } from '../db.server'
import type { CollaboratorsByProject } from '../types'
import { userToCollaborator } from '../types'

export async function getCollaborators(params: {
  ids: string[]
  userId: string
}): Promise<CollaboratorsByProject> {
  const projects = await prisma.project.findMany({
    where: { proj_id: { in: params.ids }, owner_id: params.userId },
    include: { ProjectCollaborator: { include: { User: true } } },
  })

  let collaboratorsByProject: CollaboratorsByProject = {}
  for (const project of projects) {
    const collaboratorUserDetails = project.ProjectCollaborator.map(({ User }) => User)
    collaboratorsByProject[project.proj_id] = collaboratorUserDetails.map(userToCollaborator)
  }
  return collaboratorsByProject
}

export async function addToProjectCollaborators(params: {
  id: string
  userId: string
}): Promise<void> {
  // the Prisma equivalent of INSERT ... ON CONFLICT DO NOTHING
  await prisma.projectCollaborator.upsert({
    where: {
      unique_project_collaborator_project_id_user_id: {
        user_id: params.userId,
        project_id: params.id,
      },
    },
    update: {},
    create: {
      project_id: params.id,
      user_id: params.userId,
    },
  })
}

export async function listProjectCollaborators(params: { id: string }): Promise<UserDetails[]> {
  const collaborators = await prisma.projectCollaborator.findMany({
    where: { project_id: params.id },
    include: { User: true },
  })
  return collaborators.map((c) => c.User)
}
