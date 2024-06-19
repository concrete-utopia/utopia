import type { UserDetails } from 'prisma-client'
import type { UtopiaPrismaClient } from '../db.server'
import { prisma } from '../db.server'
import { userToCollaborator, type CollaboratorsByProject } from '../types'
import type { GetBatchResult } from 'prisma-client/runtime/library.js'

export async function getCollaborators(params: {
  ids: string[]
  userId: string
}): Promise<CollaboratorsByProject> {
  const projects = await prisma.project.findMany({
    where: { proj_id: { in: params.ids }, owner_id: params.userId },
    select: {
      proj_id: true,
      ProjectCollaborator: {
        select: { User: { select: { user_id: true, name: true, picture: true } } },
      },
    },
  })

  let collaboratorsByProject: CollaboratorsByProject = {}
  for (const project of projects) {
    const collaboratorUserDetails = project.ProjectCollaborator.map(({ User }) => User)
    collaboratorsByProject[project.proj_id] = collaboratorUserDetails.map(userToCollaborator)
  }
  return collaboratorsByProject
}

export async function addToProjectCollaborators(params: {
  projectId: string
  userId: string
}): Promise<void> {
  return addToProjectCollaboratorsWithRunner(prisma, params)
}

export async function addToProjectCollaboratorsWithRunner(
  runner: Pick<UtopiaPrismaClient, 'projectCollaborator'>,
  params: { projectId: string; userId: string },
) {
  // the Prisma equivalent of INSERT ... ON CONFLICT DO NOTHING
  await runner.projectCollaborator.upsert({
    where: {
      unique_project_collaborator_project_id_user_id: {
        user_id: params.userId,
        project_id: params.projectId,
      },
    },
    update: {},
    create: {
      project_id: params.projectId,
      user_id: params.userId,
    },
  })
}

export async function removeFromProjectCollaborators(params: {
  projectId: string
  userId: string
}): Promise<GetBatchResult> {
  return removeFromProjectCollaboratorsWithRunner(prisma, params)
}

export async function removeFromProjectCollaboratorsWithRunner(
  runner: Pick<UtopiaPrismaClient, 'projectCollaborator'>,
  params: { projectId: string; userId: string },
) {
  // using delete many so not to explode if the record is not found
  return await runner.projectCollaborator.deleteMany({
    where: {
      user_id: params.userId,
      project_id: params.projectId,
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
