import { LoaderFunctionArgs } from '@remix-run/node'
import { Params } from '@remix-run/react'
import { listDeletedProjects } from '../models/project.server'
import { handle, requireUser } from '../util/api.server'
import { validateProjectAccess } from '../handlers/validators'
import { UserProjectPermission } from '../types'

export async function loader(args: LoaderFunctionArgs) {
  return handle(args, {
    GET: {
      handler: handleListDeletedProjects,
      validator: validateProjectAccess(UserProjectPermission.CAN_VIEW_PROJECT),
    },
  })
}

export async function handleListDeletedProjects(req: Request, params: Params<string>) {
  const user = await requireUser(req)

  const projects = await listDeletedProjects({ ownerId: user.user_id })

  return {
    projects,
  }
}
