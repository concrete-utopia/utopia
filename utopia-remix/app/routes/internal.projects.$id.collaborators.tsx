import { ActionFunctionArgs } from '@remix-run/node'
import { ensure, handle, requireUser } from '../util/api.server'
import { Status } from '../util/statusCodes.server'
import { Params } from '@remix-run/react'
import { updateCollaborators } from '../models/projectCollaborators.server'

export async function action(args: ActionFunctionArgs) {
  return handle(args, { POST: updateProjectCollaborators })
}

export async function updateProjectCollaborators(req: Request, params: Params<string>) {
  await requireUser(req)

  const { id } = params
  ensure(id != null, 'id is null', Status.BAD_REQUEST)

  await updateCollaborators({ id: id })

  return {}
}
