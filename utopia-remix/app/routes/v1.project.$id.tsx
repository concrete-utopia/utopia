import { ActionFunctionArgs, LoaderFunctionArgs } from '@remix-run/node'
import { proxy } from '../util/proxy.server'
import { ensure, getUser, handle, handleOptions, requireUser } from '../util/api.server'
import { Params } from '@remix-run/react'
import { Status } from '../util/statusCodes'
import { hasUserProjectPermission } from '../services/permissionsService.server'
import { UserProjectPermission } from '../types'

export async function loader(args: LoaderFunctionArgs) {
  return handle(args, {
    OPTIONS: handleOptions,
    GET: getProject,
  })
}

export async function getProject(req: Request, params: Params<string>) {
  const { id: projectId } = params
  ensure(projectId != null, 'project is null', Status.BAD_REQUEST)
  let user = await getUser(req)
  let userId = user?.user_id ?? 'ANON'
  const projectData = (await proxy(req, params)) as { ownerId: string }
  const creatorId = projectData.ownerId
  const isCreator = creatorId === userId

  const allowed =
    isCreator ||
    (await hasUserProjectPermission(projectId, userId, UserProjectPermission.CAN_VIEW_PROJECT))
  ensure(allowed, 'Project not found', Status.NOT_FOUND)

  return projectData
}

export async function action(args: ActionFunctionArgs) {
  return handle(args, {
    PUT: proxy,
  })
}
