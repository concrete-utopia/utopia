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
  const userIdStr = formData.get('userId')
  ensure(userIdStr != null, 'userId is null', Status.BAD_REQUEST)
  const user = await getUserDetails(String(userIdStr))
  ensure(user != null, 'user not found', Status.BAD_REQUEST)
  const userRoleStr = formData.get('userRole')
  const userRoleNumber = asNumber(userRoleStr)
  ensure(!isNaN(userRoleNumber), 'userRole is not a number', Status.BAD_REQUEST)
  const userRole = asUserProjectRole(userRoleNumber)
  ensure(
    userRole != null && Object.values(UserProjectRole).includes(userRole),
    'userRole is not a valid UserProjectRole',
    Status.BAD_REQUEST,
  )
  await permissionService.grantProjectRoleToUser(id, user.user_id, userRole)

  return {}
}
