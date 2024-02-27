import { ActionFunctionArgs } from '@remix-run/node'
import { Params } from '@remix-run/react'
import { ensure, handle, requireUser } from '../util/api.server'
import { Status } from '../util/statusCodes'
import { hardDeleteProject } from '../models/project.server'

export async function action(args: ActionFunctionArgs) {
  return handle(args, { POST: handleDestroyProject })
}

export async function handleDestroyProject(req: Request, params: Params<string>) {
  const user = await requireUser(req)

  const { id } = params
  ensure(id != null, 'id is null', Status.BAD_REQUEST)

  await hardDeleteProject({ id: id, userId: user.user_id })

  return {}
}
