import { prisma } from '../db.server'
import { ProjectWithoutContent } from '../types'

const selectProjectWithoutContent: Record<keyof ProjectWithoutContent, true> = {
  id: true,
  proj_id: true,
  owner_id: true,
  title: true,
  created_at: true,
  modified_at: true,
  deleted: true,
  ProjectAccess: true,
}

export async function listProjects(params: { ownerId: string }): Promise<ProjectWithoutContent[]> {
  return prisma.project.findMany({
    select: selectProjectWithoutContent,
    where: {
      owner_id: params.ownerId,
      OR: [{ deleted: null }, { deleted: false }],
    },
    orderBy: { modified_at: 'desc' },
  })
}

export async function getProjectOwnerById(
  params: { id: string },
  config: { includeDeleted: boolean } = { includeDeleted: false },
): Promise<string | null> {
  const whereOptions: {
    proj_id: string
    OR?: { deleted: boolean | null }[]
  } = {
    proj_id: params.id,
  }
  if (!config.includeDeleted) {
    whereOptions['OR'] = [{ deleted: null }, { deleted: false }]
  }
  const project = await prisma.project.findFirst({
    select: { owner_id: true },
    where: whereOptions,
  })
  return project?.owner_id ?? null
}

export async function renameProject(params: {
  id: string
  userId: string
  title: string
}): Promise<ProjectWithoutContent> {
  return prisma.project.update({
    where: {
      proj_id: params.id,
      owner_id: params.userId,
      OR: [{ deleted: null }, { deleted: false }],
    },
    data: { title: params.title },
    select: selectProjectWithoutContent,
  })
}

export async function softDeleteProject(params: { id: string; userId: string }): Promise<void> {
  await prisma.project.update({
    where: {
      proj_id: params.id,
      owner_id: params.userId,
      OR: [{ deleted: null }, { deleted: false }],
    },
    data: { deleted: true },
  })
}

export async function restoreDeletedProject(params: { id: string; userId: string }): Promise<void> {
  await prisma.project.update({
    where: {
      proj_id: params.id,
      owner_id: params.userId,
      deleted: true,
    },
    data: { deleted: null },
  })
}

export async function listDeletedProjects(params: {
  ownerId: string
}): Promise<ProjectWithoutContent[]> {
  return await prisma.project.findMany({
    select: selectProjectWithoutContent,
    where: {
      owner_id: params.ownerId,
      deleted: true,
    },
    orderBy: { modified_at: 'desc' },
  })
}

export async function hardDeleteProject(params: { id: string; userId: string }): Promise<void> {
  await prisma.project.delete({
    where: {
      proj_id: params.id,
      owner_id: params.userId,
      deleted: true,
    },
  })
}

export async function hardDeleteAllProjects(params: { userId: string }): Promise<void> {
  await prisma.project.deleteMany({
    where: {
      owner_id: params.userId,
      deleted: true,
    },
  })
}
