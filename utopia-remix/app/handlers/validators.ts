import type { AccessValidator, ValidationResult } from '../util/api.server'
import { getUser, validationError, validationOk } from '../util/api.server'
import { UserProjectPermission } from '../types'
import type { Params } from '@remix-run/react'
import { Status } from '../util/statusCodes'
import { hasUserProjectPermission } from '../services/permissionsService.server'
import { getProjectOwnerById } from '../models/project.server'
import { ApiError } from '../util/errors'

export function validateProjectAccess(
  permission: UserProjectPermission,
  {
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
  return async function (req: Request, params: Params<string>): Promise<ValidationResult> {
    const projectId = getProjectId(params)
    if (projectId == null) {
      return validationError(new ApiError('Invalid project id', Status.BAD_REQUEST))
    }

    const ownerId = await getProjectOwnerById({ id: projectId }, { includeDeleted: includeDeleted })
    if (ownerId == null) {
      return validationError(new ApiError('Project not found', Status.NOT_FOUND))
    }

    const user = await getUser(req)
    const userId = user?.user_id ?? null

    // the user owns the project, go on
    if (userId === ownerId) {
      return validationOk()
    }

    // if the user can access the project, go on
    const hasProjectPermissions = await hasUserProjectPermission(projectId, userId, permission)
    if (hasProjectPermissions) {
      return validationOk()
    }

    // if access can be requested…
    if (canRequestAccess === true) {
      // …and the user can request permissions…
      const hasRequestAccessPermission = await hasUserProjectPermission(
        projectId,
        userId,
        UserProjectPermission.CAN_REQUEST_ACCESS,
      )
      if (hasRequestAccessPermission) {
        // …return a 403!
        return validationError(new ApiError('Forbidden', Status.FORBIDDEN))
      }
    }

    // the project is not available, it's conceptually a 401 but just return a 404 so we don't leak
    return validationError(new ApiError('Project not found', Status.NOT_FOUND))
  }
}

export const ALLOW: AccessValidator = async () => validationOk()
