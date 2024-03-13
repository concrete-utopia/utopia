import type { AccessValidator} from '../util/api.server';
import { ensure, getUser } from '../util/api.server'
import type { UserProjectPermission } from '../types'
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
  }: {
    errorMessage?: string
    status?: number
    getProjectId: (params: Params<string>) => string | null | undefined
    includeDeleted?: boolean
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
    ensure(allowed, errorMessage ?? 'Unauthorized Access', status ?? Status.UNAUTHORIZED)
  }
}

export const ALLOW: AccessValidator = async (request: Request, params: Params<string>) => true
