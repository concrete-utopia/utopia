import { Project } from 'prisma-client'
import { prisma } from '../db.server'

export type ProjectWithoutContent = Omit<Project, 'content'>

export async function listProjects(params: { ownerId: string }): Promise<ProjectWithoutContent[]> {
  return prisma.project.findMany({
    select: {
      id: true,
      proj_id: true,
      owner_id: true,
      title: true,
      created_at: true,
      modified_at: true,
      deleted: true,
    },
    where: { owner_id: params.ownerId },
    orderBy: { modified_at: 'desc' },
  })
}
