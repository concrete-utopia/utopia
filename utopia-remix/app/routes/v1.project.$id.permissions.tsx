import { LoaderFunctionArgs } from '@remix-run/node'
import { handle, handleOptions, requireUser } from '../util/api.server'
import { Params } from '@remix-run/react'
import { getAllPermissions } from '../services/permissionsService.server'
import { ALLOW } from '../handlers/validators'

export async function loader(args: LoaderFunctionArgs) {
  return handle(args, {
    OPTIONS: handleOptions,
    GET: { handler: getUserProjectPermissions, validator: ALLOW },
  })
}

async function getUserProjectPermissions(req: Request, params: Params<string>) {
  const user = await requireUser(req)
  const { user_id } = user
  const { projectId } = params
  if (user_id == null || projectId == null) {
    throw new Error('user_id or projectId is null')
  }
  const permissionsResult = await getAllPermissions(projectId, user_id)
  return permissionsResult
}
