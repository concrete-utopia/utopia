import type { LoaderFunctionArgs } from '@remix-run/node'
import { json } from '@remix-run/node'
import type { Params } from '@remix-run/react'
import { validateProjectAccess } from '../handlers/validators'
import { listProjectAccessRequests } from '../models/projectAccessRequest.server'
import { UserProjectPermission } from '../types'
import { ensure, handle, requireUser } from '../util/api.server'
import { Status } from '../util/statusCodes'

export async function loader(args: LoaderFunctionArgs) {
  return handle(args, {
    GET: {
      handler: handleListAccessRequests,
      validator: validateProjectAccess(UserProjectPermission.CAN_MANAGE_PROJECT, {
        getProjectId: (params) => params.id,
      }),
    },
  })
}

export async function handleListAccessRequests(req: Request, params: Params<string>) {
  const user = await requireUser(req)

  const projectId = params.id
  ensure(projectId != null, 'project id is null', Status.BAD_REQUEST)

  return listProjectAccessRequests({
    projectId: projectId,
    userId: user.user_id,
  })
}
