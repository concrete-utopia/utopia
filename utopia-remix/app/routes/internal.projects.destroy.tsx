import type { ActionFunctionArgs } from '@remix-run/node'
import type { Params } from '@remix-run/react'
import { hardDeleteAllProjects } from '../models/project.server'
import { handle, requireUser } from '../util/api.server'
import { ALLOW } from '../handlers/validators'

export async function action(args: ActionFunctionArgs) {
  return handle(args, {
    POST: {
      handler: handleDestroyAllProjects,
      validator: ALLOW,
    },
  })
}

export async function handleDestroyAllProjects(req: Request, params: Params<string>) {
  const user = await requireUser(req)

  await hardDeleteAllProjects({ userId: user.user_id })

  return {}
}
