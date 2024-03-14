import type { ActionFunctionArgs } from '@remix-run/node'
import { validateProjectAccess } from '../handlers/validators'
import { ensure, handle, requireUser } from '../util/api.server'
import { UserProjectPermission, UserProjectRole, asUserProjectRole } from '../types'
import type { Params } from '@remix-run/react'
import { Status } from '../util/statusCodes'
import { asNumber } from '../util/common'
import * as permissionService from '../services/permissionsService.server'
import { getUserDetails } from '../models/userDetails.server'

export async function action(args: ActionFunctionArgs) {
  return handle(args, {
    POST: {
      handler: changeProjectUserRole,
      validator: validateProjectAccess(UserProjectPermission.CAN_MANAGE_PROJECT, {
        getProjectId: (params) => params.id,
      }),
    },
  })
}

async function changeProjectUserRole(req: Request, params: Params<string>) {
  await requireUser(req)

  const { id } = params
  ensure(id != null, 'id is null', Status.BAD_REQUEST)

  const formData = await req.formData()

  const userId = formData.get('userId')
  ensure(userId != null && typeof userId === 'string', 'invalid user id', Status.BAD_REQUEST)

  const user = await getUserDetails(userId)
  ensure(user != null, 'user not found', Status.BAD_REQUEST)

  const userRoleStr = formData.get('userRole')
  const userRoleNumber = asNumber(userRoleStr)
  ensure(!isNaN(userRoleNumber), 'invalid user role', Status.BAD_REQUEST)

  const userRole = asUserProjectRole(userRoleNumber)
  ensure(userRole != null, 'invalid user role', Status.BAD_REQUEST)
  await permissionService.grantProjectRoleToUser(id, user.user_id, userRole)

  return {}
}
