import type { ActionFunctionArgs } from '@remix-run/node'
import type { Params } from '@remix-run/react'
import { softDeleteProject } from '../models/project.server'
import { ensure, handle, requireUser } from '../util/api.server'
import { Status } from '../util/statusCodes'
import { validateProjectAccess } from '../handlers/validators'
import { UserProjectPermission } from '../types'

export async function action(args: ActionFunctionArgs) {
  return handle(args, {
    POST: {
      handler: handleDeleteProject,
      validator: validateProjectAccess(UserProjectPermission.CAN_MANAGE_PROJECT, {
        getProjectId: (params) => params.id,
      }),
    },
  })
}

export async function handleDeleteProject(req: Request, params: Params<string>) {
  const user = await requireUser(req)

  const { id } = params
  ensure(id != null, 'id is null', Status.BAD_REQUEST)

  await softDeleteProject({ id: id, userId: user.user_id })

  return {}
}
