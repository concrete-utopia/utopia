import { AccessValidator, ensure, getUser } from '../util/api.server'
import { UserProjectPermission } from '../types'
import { Params } from '@remix-run/react'
import { Status } from '../util/statusCodes'
import { hasUserProjectPermission } from '../services/permissionsService.server'
import { getProjectById, getProjectOwnerById } from '../models/project.server'

export function validateProjectAccess(
  permission: UserProjectPermission,
  {
    errorMessage,
    status,
  }: {
    errorMessage?: string
    status?: number
  } = {},
): AccessValidator {
  return async function (req: Request, params: Params<string>) {
    const { id: projectId } = params
    ensure(projectId != null, 'project id is null', Status.BAD_REQUEST)

    const project = await getProjectById({ id: projectId })
    ensure(project != null, `Project ${projectId} not found`, Status.NOT_FOUND)
    const creatorId = project.owner_id

    let user = await getUser(req)
    let userId = user?.user_id ?? null
    const isCreator = userId ? creatorId === userId : false

    const allowed = isCreator || (await hasUserProjectPermission(projectId, userId, permission))
    ensure(allowed, errorMessage ?? 'Unauthorized Access', status ?? Status.UNAUTHORIZED)
  }
}

export const ALLOW: AccessValidator = async (request: Request, params: Params<string>) => true
