import { UserDetails } from 'prisma-client'
import { LiveblocksAPI } from '../clients/liveblocks.server'
import { prisma } from '../db.server'
import { Collaborator, CollaboratorsByProject } from '../types'

function userToCollaborator(user: UserDetails): Collaborator {
  return {
    id: user.user_id,
    name: user.name ?? '',
    avatar: user.picture ?? '',
  }
}

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

export async function updateCollaborators(params: { id: string; userId: string }): Promise<void> {
  const collaboratorIds = [params.userId]
  await prisma.$transaction(async (tx) => {
    // ignore non-existing users
    const existingUsers = await tx.userDetails.findMany({
      where: { user_id: { in: collaboratorIds } },
    })
    for (const user of existingUsers) {
      // the Prisma equivalent of INSERT ... ON CONFLICT DO NOTHING
      await tx.projectCollaborator.upsert({
        where: {
          unique_project_collaborator_project_id_user_id: {
            user_id: user.user_id,
            project_id: params.id,
          },
        },
        update: {},
        create: {
          project_id: params.id,
          user_id: user.user_id,
        },
      })
    }
  })
}

export async function listProjectCollaborators(params: {
  id: string
  userId: string
}): Promise<UserDetails[]> {
  return await prisma.$transaction(async (tx) => {
    await tx.projectCollaborator.upsert({
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
    const collaborators = await tx.projectCollaborator.findMany({
      where: { project_id: params.id },
      include: { User: true },
    })
    return collaborators.map((c) => c.User)
  })
}
