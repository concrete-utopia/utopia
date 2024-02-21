import { ActionFunctionArgs, LoaderFunctionArgs } from '@remix-run/node'
import { proxy } from '../util/proxy.server'
import { ensure, handle, handleOptions, requireUser } from '../util/api.server'
import { Params } from '@remix-run/react'
import { canViewProject } from '../services/fgaService.server'
import { Status } from '../util/statusCodes.server'

export async function loader(args: LoaderFunctionArgs) {
  return handle(args, {
    OPTIONS: handleOptions,
    GET: getProject,
  })
}

async function getProject(req: Request, params: Params<string>) {
  const { id: projectId } = params
  ensure(projectId != null, 'project is null', Status.BAD_REQUEST)
  let userId = 'ANON'
  try {
    const user = await requireUser(req)
    userId = user.user_id
  } catch (e) {
    //
  }
  const allowed = await canViewProject(projectId, userId)
  ensure(allowed, 'Access denied', Status.NOT_FOUND)

  return proxy(req, params)
}

export async function action(args: ActionFunctionArgs) {
  return handle(args, {
    PUT: proxy,
  })
}
