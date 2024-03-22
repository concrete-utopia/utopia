import type { ActionFunctionArgs, LoaderFunctionArgs } from '@remix-run/node'
import type { Params } from '@remix-run/react'
import { validateProjectAccess } from '../handlers/validators'
import { updateGithubRepository } from '../models/project.server'
import { UserProjectPermission, isUpdateGithubRepositoryBody } from '../types'
import { ensure, handle, handleOptions, requireUser } from '../util/api.server'
import { Status } from '../util/statusCodes'

export async function loader(args: LoaderFunctionArgs) {
  return handle(args, {
    OPTIONS: handleOptions,
  })
}

export async function action(args: ActionFunctionArgs) {
  return handle(args, {
    POST: {
      handler: handleUpdateGithubRepository,
      validator: validateProjectAccess(UserProjectPermission.CAN_MANAGE_PROJECT, {
        getProjectId: (params) => params.id,
      }),
    },
  })
}

export async function handleUpdateGithubRepository(req: Request, params: Params<string>) {
  const user = await requireUser(req)

  const { id } = params
  ensure(id != null, 'id is null', Status.BAD_REQUEST)

  const body = await req.json()
  ensure(isUpdateGithubRepositoryBody(body), 'invalid request', Status.BAD_REQUEST)

  await updateGithubRepository({
    projectId: id,
    userId: user.user_id,
    repository: body.githubRepository,
  })

  return {}
}
