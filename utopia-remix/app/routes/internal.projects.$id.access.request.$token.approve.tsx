import type { ActionFunctionArgs } from '@remix-run/node'
import type { Params } from '@remix-run/react'
import { ensure, handle, requireUser } from '../util/api.server'
import { Status } from '../util/statusCodes'
import { validateProjectAccess } from '../handlers/validators'
import { updateAccessRequestStatus } from '../models/projectAccessRequest.server'
import { AccessRequestStatus, UserProjectPermission } from '../types'

export async function action(args: ActionFunctionArgs) {
  return handle(args, {
    POST: {
      handler: handleApproveAccessRequest,
      validator: validateProjectAccess(UserProjectPermission.CAN_MANAGE_PROJECT, {
        getProjectId: (params) => params.id,
      }),
    },
  })
}

export async function handleApproveAccessRequest(req: Request, params: Params<string>) {
  const user = await requireUser(req)

  const projectId = params.id
  ensure(projectId != null, 'project id is null', Status.BAD_REQUEST)

  const token = params.token
  ensure(token != null && typeof token === 'string', 'token is invalid', Status.BAD_REQUEST)

  await updateAccessRequestStatus({
    projectId: projectId,
    ownerId: user.user_id,
    token: token,
    status: AccessRequestStatus.APPROVED,
  })

  return {}
}
