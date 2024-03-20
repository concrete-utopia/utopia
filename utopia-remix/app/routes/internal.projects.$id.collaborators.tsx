import type { ActionFunctionArgs, LoaderFunctionArgs } from '@remix-run/node'
import { ensure, handle, requireUser } from '../util/api.server'
import { Status } from '../util/statusCodes'
import type { Params } from '@remix-run/react'
import {
  listProjectCollaborators,
  addToProjectCollaborators,
} from '../models/projectCollaborators.server'
import { UserProjectPermission, userToCollaborator } from '../types'
import { validateProjectAccess } from '../handlers/validators'

export async function loader(args: LoaderFunctionArgs) {
  return handle(args, {
    GET: {
      handler: getProjectCollaborators,
      validator: validateProjectAccess(UserProjectPermission.CAN_VIEW_PROJECT, {
        getProjectId: (params) => params.id,
      }),
    },
  })
}

export async function getProjectCollaborators(req: Request, params: Params<string>) {
  await requireUser(req)

  const { id } = params
  ensure(id != null, 'id is null', Status.BAD_REQUEST)

  const collaborators = await listProjectCollaborators({ id: id })
  return collaborators.map(userToCollaborator)
}

export async function action(args: ActionFunctionArgs) {
  return handle(args, {
    POST: {
      handler: addToCollaborators,
      validator: validateProjectAccess(UserProjectPermission.CAN_VIEW_PROJECT, {
        getProjectId: (params) => params.id,
      }),
    },
  })
}

export async function addToCollaborators(req: Request, params: Params<string>) {
  const user = await requireUser(req)

  const { id } = params
  ensure(id != null, 'id is null', Status.BAD_REQUEST)

  await addToProjectCollaborators({ projectId: id, userId: user.user_id })

  return {}
}
