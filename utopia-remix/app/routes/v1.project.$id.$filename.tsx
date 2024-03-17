import type { LoaderFunctionArgs } from '@remix-run/node'
import { proxy } from '../util/proxy.server'
import { handle, handleOptions } from '../util/api.server'
import { validateProjectAccess } from '../handlers/validators'
import { UserProjectPermission } from '../types'

export async function loader(args: LoaderFunctionArgs) {
  return handle(args, {
    OPTIONS: handleOptions,
    GET: {
      handler: proxy,
      validator: validateProjectAccess(UserProjectPermission.CAN_VIEW_PROJECT, {
        getProjectId: (params) => params.id,
      }),
    },
  })
}
