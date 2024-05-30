import type { LoaderFunctionArgs } from '@remix-run/node'
import { validateProjectAccess } from '../handlers/validators'
import { proxy } from '../util/proxy.server'
import { handle } from '../util/api.server'
import { UserProjectPermission } from '../types'

export async function loader(args: LoaderFunctionArgs) {
  return handle(args, {
    GET: {
      validator: validateProjectAccess(UserProjectPermission.CAN_VIEW_PROJECT, {
        getProjectId: (params) => params.id?.split('-')[0] ?? null,
      }),
      handler: (req) => proxy(req, { rawOutput: true }),
    },
  })
}
