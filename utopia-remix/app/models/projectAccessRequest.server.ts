import { prisma } from '../db.server'
import { AccessRequestStatus } from '../types'
import * as uuid from 'uuid'
import { ensure } from '../util/api.server'
import { Status } from '../util/statusCodes'

function makeRequestToken(): string {
  return uuid.v4()
}

/**
 * Create a new access request for a given project and the given user.
 * The user must not be an owner of the project.
 * If successful, a new request in the pending state will be created.
 * If there already is a request for the projectId+userId pair, nothing happens.
 */
export async function createAccessRequest(params: { projectId: string; userId: string }) {
  const token = makeRequestToken()
  await prisma.$transaction(async (tx) => {
    // let's check that the project exists, is not soft-deleted, and is not owned by the user
    const project = await tx.project.findFirst({
      where: {
        proj_id: params.projectId,
        OR: [{ deleted: null }, { deleted: false }],
      },
      select: { owner_id: true },
    })
    ensure(project != null, 'project not found', Status.NOT_FOUND)

    // if the request is coming from the owner of the project, there's nothing to do
    if (project.owner_id === params.userId) {
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

/**
 * Update the status for the given request (via its token) for the given project owned by ownerId.
 */
export async function updateAccessRequestStatus(params: {
  projectId: string
  ownerId: string
  token: string
  status: AccessRequestStatus
}) {
  await prisma.$transaction(async (tx) => {
    // check that the project exists
    const projectCount = await tx.project.count({
      where: { proj_id: params.projectId, owner_id: params.ownerId },
    })
    ensure(projectCount === 1, 'project not found', Status.NOT_FOUND)

    await tx.projectAccessRequest.update({
      where: {
        project_id_token: {
          project_id: params.projectId,
          token: params.token,
        },
      },
      data: {
        status: params.status,
        updated_at: new Date(),
      },
    })
  })
}
