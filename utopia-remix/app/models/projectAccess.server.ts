import { prisma } from '../db.server'

enum AccessLevel {
  PRIVATE,
  PUBLIC,
  WITH_LINK,
}

export async function setProjectAccess(params: {
  projectId: string
  accessLevel: AccessLevel
}): Promise<void> {
  await prisma.projectAccess.update({
    where: {
      project_id: params.projectId,
    },
    data: {
      access_level: params.accessLevel,
    },
  })
}
