import * as uuid from 'uuid'
import { prisma } from '../db.server'
import * as permissionsService from '../services/permissionsService.server'
import { ensure } from '../util/api.server'
import { Status } from '../util/statusCodes'
import type { ProjectAccessRequestWithUserDetails } from '../types'
import { AccessRequestStatus, UserProjectRole } from '../types'
import {
  addToProjectCollaboratorsWithRunner,
  removeFromProjectCollaboratorsWithRunner,
} from './projectCollaborators.server'
import { assertNever } from '../util/assertNever'

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
    // check that the project exists…
    const projectCount = await tx.project.count({
      where: { proj_id: params.projectId, owner_id: params.ownerId },
    })
    ensure(projectCount === 1, 'project not found', Status.NOT_FOUND)

    // …update the request with the given status…
    const request = await tx.projectAccessRequest.update({
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

    // …finally, update FGA permissions
    switch (params.status) {
      case AccessRequestStatus.APPROVED:
        await addToProjectCollaboratorsWithRunner(tx, {
          projectId: params.projectId,
          userId: request.user_id,
        })
        await permissionsService.grantProjectRoleToUser(
          params.projectId,
          request.user_id,
          UserProjectRole.VIEWER,
        )
        break
      case AccessRequestStatus.REJECTED:
        await removeFromProjectCollaboratorsWithRunner(tx, {
          projectId: params.projectId,
          userId: request.user_id,
        })
        await permissionsService.revokeAllRolesFromUser(params.projectId, request.user_id)
        break
      case AccessRequestStatus.PENDING:
        // do nothing
        break
      default:
        assertNever(params.status)
    }
  })
}

export async function listProjectAccessRequests(params: {
  projectId: string
  userId: string
}): Promise<ProjectAccessRequestWithUserDetails[]> {
  const data = await prisma.project.findFirst({
    where: {
      proj_id: params.projectId,
      owner_id: params.userId,
      OR: [{ deleted: null }, { deleted: false }],
    },
    select: {
      ProjectAccessRequest: true,
    },
  })
  ensure(data != null, 'project not found', Status.NOT_FOUND)

  // VERY unfortunately we cannot include the User in the query above, because it will always return the last value :(
  const users = await prisma.userDetails.findMany({
    where: { user_id: { in: data.ProjectAccessRequest.map((r) => r.user_id) } },
  })

  // merge data
  return data.ProjectAccessRequest.map((r) => ({
    ...r,
    User: users.find((u) => u.user_id === r.user_id) ?? null,
  }))
}

export async function destroyAccessRequest(params: {
  projectId: string
  ownerId: string
  token: string
}) {
  await prisma.$transaction(async (tx) => {
    // 1. delete the access request from the DB
    const request = await tx.projectAccessRequest.delete({
      where: {
        project_id_token: {
          project_id: params.projectId,
          token: params.token,
        },
        Project: {
          owner_id: params.ownerId,
        },
      },
    })
    // 2. revoke access on FGA
    await permissionsService.revokeAllRolesFromUser(params.projectId, request.user_id)
    // 3. delete the collaborator, if any
    await removeFromProjectCollaboratorsWithRunner(tx, {
      projectId: params.projectId,
      userId: request.user_id,
    })
  })
}
