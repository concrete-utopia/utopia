import { ActionFunctionArgs } from '@remix-run/node'
import { Params } from '@remix-run/react'
import { hardDeleteAllProjects } from '../models/project.server'
import { chain, handle, requireUser } from '../util/api.server'

export async function action(args: ActionFunctionArgs) {
  return handle(args, { POST: chain([handleDestroyAllProjects]) })
}

export async function handleDestroyAllProjects(req: Request, params: Params<string>) {
  const user = await requireUser(req)

  await hardDeleteAllProjects({ userId: user.user_id })

  return {}
}
