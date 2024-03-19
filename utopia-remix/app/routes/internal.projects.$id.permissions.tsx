import type { LoaderFunctionArgs } from '@remix-run/node'
import { ensure, getUser, handle, handleOptions } from '../util/api.server'
import type { Params } from '@remix-run/react'
import { getAllPermissions } from '../services/permissionsService.server'
import { ALLOW } from '../handlers/validators'
import { Status } from '../util/statusCodes'
import { getProjectOwnership } from '../models/project.server'

export async function loader(args: LoaderFunctionArgs) {
  return handle(args, {
    OPTIONS: handleOptions,
    GET: { handler: handleGetUserProjectPermissions, validator: ALLOW },
  })
}

export async function handleGetUserProjectPermissions(req: Request, params: Params<string>) {
  const { id } = params
  ensure(id != null, 'Invalid project id', Status.BAD_REQUEST)

  const user = await getUser(req)
  const userId = user?.user_id ?? null

  const ownership = await getProjectOwnership({ id: id })
  ensure(ownership != null, `Project not found`, Status.NOT_FOUND)

  return getAllPermissions(id, userId, ownership.ownerId)
}
