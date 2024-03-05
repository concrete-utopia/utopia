import { LoaderFunctionArgs } from '@remix-run/node'
import { Params } from '@remix-run/react'
import { listDeletedProjects } from '../models/project.server'
import { chain, handle, requireUser } from '../util/api.server'

export async function loader(args: LoaderFunctionArgs) {
  return handle(args, { GET: chain([handleListDeletedProjects]) })
}

export async function handleListDeletedProjects(req: Request, params: Params<string>) {
  const user = await requireUser(req)

  const projects = await listDeletedProjects({ ownerId: user.user_id })

  return {
    projects,
  }
}
