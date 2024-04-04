import type { AccessLevel } from '../types'
import { prisma } from '../db.server'
import * as permissionsService from '../services/permissionsService.server'

export async function setProjectAccess(params: {
  projectId: string
  accessLevel: AccessLevel
}): Promise<void> {
  await prisma.$transaction(async (tx) => {
    await tx.projectAccess.upsert({
      where: {
        project_id: params.projectId,
      },
      update: {
        access_level: params.accessLevel,
        modified_at: new Date(),
      },
      create: {
        project_id: params.projectId,
        access_level: params.accessLevel,
        modified_at: new Date(),
      },
    })
    await permissionsService.setProjectAccess(params.projectId, params.accessLevel)
  })
}

export async function createProjectAccess(params: {
  projectId: string
  accessLevel: AccessLevel
  creatorId: string | null
}): Promise<void> {
  await prisma.$transaction(async (tx) => {
    // check if project access already exists
    const projectAccess = await tx.projectAccess.findUnique({
      where: {
        project_id: params.projectId,
      },
    })
    if (projectAccess == null) {
      await tx.projectAccess.create({
        data: {
          project_id: params.projectId,
          access_level: params.accessLevel,
          modified_at: new Date(),
        },
      })
      await Promise.all([
        permissionsService.setProjectAccess(params.projectId, params.accessLevel),
        params.creatorId != null
          ? permissionsService.makeUserCreator(params.projectId, params.creatorId)
          : null,
      ])
    } else {
      console.error(`Project access already exists for project ${params.projectId}`)
    }
  })
}
