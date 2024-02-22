import { LoaderFunctionArgs } from '@remix-run/node'
import { proxy } from '../util/proxy.server'
import { handle, handleOptions } from '../util/api.server'
import { Params } from '@remix-run/react'
import { getAllPermissions } from '~/services/permissionsService.server'

export async function loader(args: LoaderFunctionArgs) {
  return handle(args, {
    OPTIONS: handleOptions,
    GET: getUserProjectPermissions,
  })
}

async function getUserProjectPermissions(req: Request, params: Params<string>) {
  const { userId, projectId } = params
  if (userId == null || projectId == null) {
    throw new Error('userId or projectId is null')
  }
  const permissionsResult = await getAllPermissions(projectId, userId)
  return permissionsResult
}
