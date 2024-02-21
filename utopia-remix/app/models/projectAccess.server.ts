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

export async function getProjectsAccess(params: {
  ids: string[]
  userId: string
}): Promise<Record<string, AccessLevel>> {
  const projects = await prisma.project.findMany({
    where: { proj_id: { in: params.ids }, owner_id: params.userId },
    include: { ProjectAccess: true },
  })

  let projectAccess: Record<string, AccessLevel> = {}
  for (const project of projects) {
    projectAccess[project.proj_id] = project.ProjectAccess?.access_level ?? AccessLevel.PRIVATE
  }
  return projectAccess
}
