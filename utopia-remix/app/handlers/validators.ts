import type { AccessValidator } from '../util/api.server'
import { ensure, getUser } from '../util/api.server'
import { UserProjectPermission } from '../types'
import type { Params } from '@remix-run/react'
import { Status } from '../util/statusCodes'
import { hasUserProjectPermission } from '../services/permissionsService.server'
import { getProjectOwnerById } from '../models/project.server'

export function validateProjectAccess(
  permission: UserProjectPermission,
  {
    errorMessage,
    status,
    getProjectId,
    includeDeleted = false,
    canRequestAccess = false,
  }: {
    errorMessage?: string
    status?: number
    getProjectId: (params: Params<string>) => string | null | undefined
    includeDeleted?: boolean
    canRequestAccess?: boolean
  },
): AccessValidator {
  return async function (req: Request, params: Params<string>) {
    const projectId = getProjectId(params)
    ensure(projectId != null, 'project id is null', Status.BAD_REQUEST)

    const ownerId = await getProjectOwnerById({ id: projectId }, { includeDeleted: includeDeleted })
    ensure(ownerId != null, `Project ${projectId} not found or has no owner`, Status.NOT_FOUND)

    const user = await getUser(req)
    const userId = user?.user_id ?? null
    const isCreator = userId ? ownerId === userId : false

    const allowed = isCreator || (await hasUserProjectPermission(projectId, userId, permission))
    let errorMessageToUse = errorMessage ?? 'Unauthorized Access'
    let statusToUse = status ?? Status.FORBIDDEN
    if (!allowed && canRequestAccess === true) {
      const hasRequestAccessPermission = await hasUserProjectPermission(
        projectId,
        userId,
        UserProjectPermission.CAN_REQUEST_ACCESS,
      )
      if (hasRequestAccessPermission) {
        errorMessageToUse = 'Request access to this project'
        statusToUse = Status.FORBIDDEN
      }
    }
    ensure(allowed, errorMessageToUse, statusToUse)
  }
}

export const ALLOW: AccessValidator = async (request: Request, params: Params<string>) => true
