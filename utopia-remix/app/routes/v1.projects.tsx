import { LoaderFunctionArgs } from '@remix-run/node'
import { handleListProjects } from '../handlers/listProjects'
import { handle, handleOptions } from '../util/api.server'
import { validateProjectAccess } from '../handlers/validators'
import { UserProjectPermission } from '../types'

export async function loader(args: LoaderFunctionArgs) {
  return handle(args, {
    OPTIONS: handleOptions,
    GET: {
      handler: handleListProjects,
      validator: validateProjectAccess(UserProjectPermission.CAN_VIEW_PROJECT),
    },
  })
}
