import type { AccessValidator } from '../util/api.server'
import { getUser, validationError, validationOk } from '../util/api.server'
import type { UserProjectPermission } from '../types'
import { AccessLevel } from '../types'
import type { Params } from '@remix-run/react'
import { Status } from '../util/statusCodes'
import {
  hasUserProjectPermission,
  userHasRequestProjectAccessPermission,
} from '../services/permissionsService.server'
import { getProjectAccessLevel, getProjectOwnerById } from '../models/project.server'
import { ApiError } from '../util/errors'

export function validateProjectAccess(
  permission: UserProjectPermission,
  {
    getProjectId,
    includeDeleted = false,
    canRequestAccess = false,
  }: {
    getProjectId: (params: Params<string>) => string | null | undefined
    includeDeleted?: boolean
    canRequestAccess?: boolean
  },
): AccessValidator {
  return async function (req: Request, params: Params<string>) {
    const projectId = getProjectId(params)
    if (projectId == null) {
      return validationError(new ApiError('invalid project id', Status.BAD_REQUEST))
    }

    const ownerId = await getProjectOwnerById({ id: projectId }, { includeDeleted: includeDeleted })
    if (ownerId == null) {
      return validationError(new ApiError('project not found', Status.NOT_FOUND))
    }

    const user = await getUser(req)
    const userId = user?.user_id ?? null

    // the user owns the project, go on
    if (userId === ownerId) {
      return validationOk()
    }

    const accessLevel = await getProjectAccessLevel({ projectId: projectId })
    // if the project is collaborative…
    if (accessLevel === AccessLevel.COLLABORATIVE) {
      // the user can access the project, go on
      const hasProjectPermissions = await hasUserProjectPermission(projectId, userId, permission)
      if (hasProjectPermissions) {
        return validationOk()
      }

      // …and access can be requested…
      if (canRequestAccess === true) {
        // …and the user can request permissions…
        const hasRequestAccessPermission = await userHasRequestProjectAccessPermission(
          projectId,
          userId,
        )
        if (hasRequestAccessPermission) {
          // …return a 403!
          return validationError(new ApiError('forbidden', Status.FORBIDDEN))
        }
      }
    }

    // the project is not available, it's conceptually a 401 but just return a 404 so we don't leak
    return validationError(new ApiError('project not found', Status.NOT_FOUND))
  }
}

export const ALLOW: AccessValidator = async () => validationOk()
