import type { LoaderFunctionArgs } from '@remix-run/node'
import type { Params } from '@remix-run/react'
import { listDeletedProjects } from '../models/project.server'
import { handle, requireUser } from '../util/api.server'
import { ALLOW } from '../handlers/validators'

export async function loader(args: LoaderFunctionArgs) {
  return handle(args, {
    GET: {
      handler: handleListDeletedProjects,
      validator: ALLOW,
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
