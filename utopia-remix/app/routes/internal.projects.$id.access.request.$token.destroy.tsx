import type { ActionFunctionArgs } from '@remix-run/node'
import type { Params } from '@remix-run/react'
import { validateProjectAccess } from '../handlers/validators'
import { UserProjectPermission } from '../types'
import { ensure, handle, requireUser } from '../util/api.server'
import { Status } from '../util/statusCodes'
import { destroyAccessRequest } from '../models/projectAccessRequest.server'

export async function action(args: ActionFunctionArgs) {
  return handle(args, {
    POST: {
      handler: handleDestroyAccessRequest,
      validator: validateProjectAccess(UserProjectPermission.CAN_MANAGE_PROJECT, {
        getProjectId: (params) => params.id,
      }),
    },
  })
}

export async function handleDestroyAccessRequest(req: Request, params: Params<string>) {
  const user = await requireUser(req)

  const projectId = params.id
  ensure(projectId != null, 'invalid project id', Status.BAD_REQUEST)

  const token = params.token
  ensure(token != null && typeof token === 'string', 'invalid token', Status.BAD_REQUEST)

  await destroyAccessRequest({
    projectId: projectId,
    ownerId: user.user_id,
    token: token,
  })

  return {}
}
