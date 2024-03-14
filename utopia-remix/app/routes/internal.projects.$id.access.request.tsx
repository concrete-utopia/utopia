import type { ActionFunctionArgs } from '@remix-run/node'
import type { Params } from '@remix-run/react'
import { ensure, handle, requireUser } from '../util/api.server'
import { Status } from '../util/statusCodes'
import { ALLOW } from '../handlers/validators'
import { createAccessRequest } from '../models/projectAccessRequest.server'

export async function action(args: ActionFunctionArgs) {
  return handle(args, {
    POST: {
      handler: handleRequestAccess,
      validator: ALLOW,
    },
  })
}

export async function handleRequestAccess(req: Request, params: Params<string>) {
  const user = await requireUser(req)

  const projectId = params.id
  ensure(projectId != null, 'project id is null', Status.BAD_REQUEST)

  await createAccessRequest({
    projectId: projectId,
    userId: user.user_id,
  })

  return {}
}
