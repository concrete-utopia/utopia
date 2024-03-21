import type { LoaderFunctionArgs } from '@remix-run/node'
import type { Params } from '@remix-run/react'
import { validateProjectAccess } from '../handlers/validators'
import { projectHasPendingRequests } from '../models/projectAccessRequest.server'
import { UserProjectPermission } from '../types'
import { ensure, handle, requireUser } from '../util/api.server'
import { Status } from '../util/statusCodes'

export async function loader(args: LoaderFunctionArgs) {
  return handle(args, {
    GET: {
      handler: handleHasPendingRequests,
      validator: validateProjectAccess(UserProjectPermission.CAN_MANAGE_PROJECT, {
        getProjectId: (params) => params.id,
      }),
    },
  })
}

export async function handleHasPendingRequests(req: Request, params: Params<string>) {
  const user = await requireUser(req)

  const projectId = params.id
  ensure(projectId != null, 'project id is null', Status.BAD_REQUEST)

  const hasPendingRequests = await projectHasPendingRequests({
    projectId: projectId,
    userId: user.user_id,
  })
  return { hasPendingRequests: hasPendingRequests }
}
