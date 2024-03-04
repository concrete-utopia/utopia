import { AccessValidator, ensure, getUser } from '../util/api.server'
import { UserProjectPermission } from '../types'
import { Params } from '@remix-run/react'
import { Status } from '../util/statusCodes'
import { hasUserProjectPermission } from '../services/permissionsService.server'
import { getProjectById } from '../models/project.server'

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

    const projectData = await getProjectById({ id: projectId })
    ensure(projectData != null, 'Project not found', Status.NOT_FOUND)

    let user = await getUser(req)
    let userId = user?.user_id ?? 'ANON'
    const creatorId = projectData.owner_id
    const isCreator = creatorId === userId

    const allowed = isCreator || (await hasUserProjectPermission(projectId, userId, permission))
    ensure(allowed, errorMessage ?? 'Unauthorized Access', status ?? Status.UNAUTHORIZED)

    return allowed
  }
}

export const ALLOW: AccessValidator = async (request: Request, params: Params<string>) => true
