import { LiveblocksAPI } from '../clients/liveblocks.server'
import { prisma } from '../db.server'
import { Collaborator, CollaboratorsByProject } from '../types'
import { ensure } from '../util/api.server'
import { Status } from '../util/statusCodes.server'

export async function getCollaborators(params: {
  ids: string[]
  userId: string
}): Promise<CollaboratorsByProject> {
  let collaboratorsByProject: CollaboratorsByProject = {}
  await prisma.$transaction(async (tx) => {
    for (const id of params.ids) {
      async function getCollaborators() {
        const project = await tx.project.findFirst({
          where: { proj_id: id, owner_id: params.userId },
        })
        ensure(project != null, 'Project not found', Status.NOT_FOUND)

        const user = await tx.userDetails.findFirst({ where: { user_id: params.userId } })
        ensure(user != null, 'User not found', Status.NOT_FOUND)

        const collaboratorsData = await tx.projectCollaborator.findMany({
          where: { project_id: id },
          include: { User: true },
          orderBy: { created_at: 'asc' },
        })
        if (collaboratorsData.length === 0) {
          await tx.projectCollaborator.create({
            data: { user_id: params.userId, project_id: project.proj_id },
          })
          return [{ id: params.userId, name: user.name ?? '', avatar: user.picture ?? '' }]
        }
        return collaboratorsData.map(
          ({ User }): Collaborator => ({
            id: User.user_id,
            name: User.name ?? '',
            avatar: User.picture ?? '',
          }),
        )
      }
      collaboratorsByProject[id] = await getCollaborators()
    }
  })
  return collaboratorsByProject
}

export async function updateCollaborators(params: { id: string }): Promise<void> {
  const storage = await LiveblocksAPI.getRoomStorage(params.id)
  const collaborators = Object.values(storage.data.collaborators.data).map((c) => c.data)
  await prisma.$transaction(async (tx) => {
    const existingUsers = await tx.userDetails.findMany({
      where: { user_id: { in: collaborators.map((c) => c.id) } },
    })
    for (const user of existingUsers) {
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
