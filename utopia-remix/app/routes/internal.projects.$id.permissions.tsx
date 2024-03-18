import type { LoaderFunctionArgs } from '@remix-run/node'
import { ensure, getUser, handle, handleOptions } from '../util/api.server'
import type { Params } from '@remix-run/react'
import { getAllPermissions } from '../services/permissionsService.server'
import { ALLOW } from '../handlers/validators'
import { Status } from '../util/statusCodes'
import { getProjectOwnerById } from '../models/project.server'

export async function loader(args: LoaderFunctionArgs) {
  return handle(args, {
    OPTIONS: handleOptions,
    GET: { handler: getUserProjectPermissions, validator: ALLOW },
  })
}

async function getUserProjectPermissions(req: Request, params: Params<string>) {
  const user = await getUser(req)
  const userId = user?.user_id ?? null
  const { id } = params
  ensure(id != null, 'projectId is null', Status.BAD_REQUEST)
  const ownerId = await getProjectOwnerById({ id: id })
  ensure(ownerId != null, `Project ${id} not found or has no owner`, Status.NOT_FOUND)
  return getAllPermissions(id, userId, ownerId)
}
