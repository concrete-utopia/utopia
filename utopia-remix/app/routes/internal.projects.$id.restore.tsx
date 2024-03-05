import { ActionFunctionArgs } from '@remix-run/node'
import { Params } from '@remix-run/react'
import { restoreDeletedProject } from '../models/project.server'
import { chain, ensure, handle, requireUser } from '../util/api.server'
import { Status } from '../util/statusCodes'

export async function action(args: ActionFunctionArgs) {
  return handle(args, { POST: chain([handleRestoreDeletedProject]) })
}

export async function handleRestoreDeletedProject(req: Request, params: Params<string>) {
  const user = await requireUser(req)

  const { id } = params
  ensure(id != null, 'id is null', Status.BAD_REQUEST)

  await restoreDeletedProject({ id: id, userId: user.user_id })

  return {}
}
