import { AccessValidator, ensure, getUser } from '../util/api.server'
import { UserProjectPermission } from '../types'
import { Params } from '@remix-run/react'
import { Status } from '../util/statusCodes'
import { hasUserProjectPermission } from '../services/permissionsService.server'
import { getProjectOwnerById } from '../models/project.server'

export function validateProjectAccess(
  permission: UserProjectPermission,
  {
    errorMessage,
    status,
    idRouteParam = 'id',
    allowDeleted = false,
  }: {
    errorMessage?: string
    status?: number
    idRouteParam?: string
    allowDeleted?: boolean
  } = {},
): AccessValidator {
  return async function (req: Request, params: Params<string>) {
    const projectId = params[idRouteParam]
    ensure(projectId != null, 'project id is null', Status.BAD_REQUEST)

    const ownerId = await getProjectOwnerById({ id: projectId }, { allowDeleted: allowDeleted })
    ensure(ownerId != null, `Project ${projectId} not found or has no owner`, Status.NOT_FOUND)

    let user = await getUser(req)
    let userId = user?.user_id ?? null
    const isCreator = userId ? ownerId === userId : false

    const allowed = isCreator || (await hasUserProjectPermission(projectId, userId, permission))
    ensure(allowed, errorMessage ?? 'Unauthorized Access', status ?? Status.UNAUTHORIZED)
  }
}

export const ALLOW: AccessValidator = async (request: Request, params: Params<string>) => true
