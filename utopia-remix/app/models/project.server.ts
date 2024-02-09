import { Project } from 'prisma-client'
import { prisma } from '../db.server'

export type ProjectWithoutContent = Omit<Project, 'content'>

const selectProjectWithoutContent: Record<keyof ProjectWithoutContent, true> = {
  id: true,
  proj_id: true,
  owner_id: true,
  title: true,
  created_at: true,
  modified_at: true,
  deleted: true,
}

export async function listProjects(params: { ownerId: string }): Promise<ProjectWithoutContent[]> {
  return prisma.project.findMany({
    select: selectProjectWithoutContent,
    where: { owner_id: params.ownerId },
    orderBy: { modified_at: 'desc' },
  })
}

export async function renameProject(params: {
  id: string
  userId: string
  title: string
}): Promise<ProjectWithoutContent> {
  console.log('RENAMING WITH', params.title)
  return prisma.project.update({
    where: { proj_id: params.id, owner_id: params.userId },
    data: { title: params.title },
    select: selectProjectWithoutContent,
  })
}
