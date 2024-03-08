import { AccessLevel } from '../types'
import { prisma } from '../db.server'
import * as permissionsService from '../services/permissions/permissionsService.server'

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
