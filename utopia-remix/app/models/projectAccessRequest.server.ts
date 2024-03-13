import { prisma } from '../db.server'
import { AccessRequestStatus } from '../types'
import * as uuid from 'uuid'
import { ensure } from '../util/api.server'
import { Status } from '../util/statusCodes'

function makeRequestToken(): string {
  return uuid.v4()
}

export async function createAccessRequest(params: { projectId: string; userId: string }) {
  const token = makeRequestToken()
  await prisma.$transaction(async (tx) => {
    const project = await tx.project.findFirst({
      where: {
        proj_id: params.projectId,
        OR: [{ deleted: null }, { deleted: false }],
      },
      select: { owner_id: true },
    })
    ensure(project != null, 'project not found', Status.NOT_FOUND)
    if (project.owner_id === params.userId) {
      // nothing to do
      return
    }

    // the request can be created. it cannot be re-issued (for now…?)
    await tx.projectAccessRequest.upsert({
      where: {
        project_id_user_id: {
          project_id: params.projectId,
          user_id: params.userId,
        },
      },
      update: {}, // ON CONFLICT DO NOTHING
      create: {
        status: AccessRequestStatus.PENDING,
        project_id: params.projectId,
        user_id: params.userId,
        token: token,
      },
    })
  })
}

export async function updateAccessRequestStatus(params: {
  projectId: string
  ownerId: string
  token: string
  status: AccessRequestStatus
}) {
  await prisma.$transaction(async (tx) => {
    const project = await tx.project.findFirst({
      where: { proj_id: params.projectId, owner_id: params.ownerId },
      include: { ProjectAccessRequest: { select: { id: true, token: true } } },
    })
    ensure(project != null, 'project not found', Status.NOT_FOUND)

    const request = project.ProjectAccessRequest.find((r) => r.token === params.token)
    ensure(request != null, 'request not found', Status.NOT_FOUND)

    await tx.projectAccessRequest.update({
      where: { id: request.id },
      data: {
        status: params.status,
        updated_at: new Date(),
      },
    })
  })
}
