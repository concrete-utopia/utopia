import { ActionFunctionArgs, LoaderFunctionArgs } from '@remix-run/node'
import { ensure, handle, requireUser } from '../util/api.server'
import { Status } from '../util/statusCodes.server'
import { Params } from '@remix-run/react'
import {
  listProjectCollaborators,
  addToProjectCollaborators,
} from '../models/projectCollaborators.server'

export async function loader(args: LoaderFunctionArgs) {
  return handle(args, { GET: getProjectCollaborators })
}

export async function getProjectCollaborators(req: Request, params: Params<string>) {
  await requireUser(req)

  const { id } = params
  ensure(id != null, 'id is null', Status.BAD_REQUEST)

  const collaborators = await listProjectCollaborators({ id: id })
  return collaborators.map(({ user_id, name, picture }) => ({
    id: user_id,
    name: name,
    avatar: picture,
  }))
}

export async function action(args: ActionFunctionArgs) {
  return handle(args, { POST: addToCollaborators })
}

export async function addToCollaborators(req: Request, params: Params<string>) {
  const user = await requireUser(req)

  const { id } = params
  ensure(id != null, 'id is null', Status.BAD_REQUEST)

  await addToProjectCollaborators({ id: id, userId: user.user_id })

  return {}
}
