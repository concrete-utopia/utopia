import { ActionFunctionArgs } from '@remix-run/node'
import { Params } from '@remix-run/react'
import { hardDeleteAllProjects } from '../models/project.server'
import { handle, requireUser } from '../util/api.server'
import { validateProjectAccess } from '../handlers/validators'
import { UserProjectPermission } from '../types'

export async function action(args: ActionFunctionArgs) {
  return handle(args, {
    POST: {
      handler: handleDestroyAllProjects,
      validator: validateProjectAccess(UserProjectPermission.CAN_MANAGE_PROJECT),
    },
  })
}

export async function handleDestroyAllProjects(req: Request, params: Params<string>) {
  const user = await requireUser(req)

  await hardDeleteAllProjects({ userId: user.user_id })

  return {}
}
