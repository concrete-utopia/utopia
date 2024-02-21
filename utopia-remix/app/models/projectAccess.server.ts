import { AccessLevel } from '../types'
import { prisma } from '../db.server'
import * as fgaService from '../services/fgaService.server'

export async function setProjectAccess(params: {
  projectId: string
  accessLevel: AccessLevel
}): Promise<void> {
  await prisma.projectAccess.upsert({
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
  await fgaService.updateAccessLevel(params.projectId, params.accessLevel)
}

export async function getProjectAccess(params: { projectId: string }): Promise<AccessLevel | null> {
  const projectAccess = await prisma.projectAccess.findUnique({
    where: {
      project_id: params.projectId,
    },
  })
  return projectAccess?.access_level ?? null
}
