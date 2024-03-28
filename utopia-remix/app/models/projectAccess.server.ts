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
}): Promise<void> {
  await prisma.$transaction(async (tx) => {
    await tx.projectAccess.upsert({
      where: {
        project_id: params.projectId,
      },
      update: {}, // ON CONFLICT DO NOTHING
      create: {
        project_id: params.projectId,
        access_level: params.accessLevel,
        modified_at: new Date(),
      },
    })
    await permissionsService.setProjectAccess(params.projectId, params.accessLevel)
  })
}
