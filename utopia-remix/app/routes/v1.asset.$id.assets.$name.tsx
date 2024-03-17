import type { ActionFunctionArgs, LoaderFunctionArgs } from '@remix-run/node'
import { proxy } from '../util/proxy.server'
import { handle, handleOptions } from '../util/api.server'
import { ALLOW, validateProjectAccess } from '../handlers/validators'
import { UserProjectPermission } from '../types'

export async function loader(args: LoaderFunctionArgs) {
  return handle(args, {
    OPTIONS: handleOptions,
  })
}

export async function action(args: ActionFunctionArgs) {
  return handle(args, {
    POST: {
      handler: (req) => proxy(req, { rawOutput: true }),
      validator: validateProjectAccess(UserProjectPermission.CAN_MANAGE_PROJECT, {
        getProjectId: (params) => params.id,
      }),
    },
    DELETE: {
      handler: proxy,
      validator: validateProjectAccess(UserProjectPermission.CAN_MANAGE_PROJECT, {
        getProjectId: (params) => params.id,
      }),
    },
    PUT: {
      handler: proxy,
      validator: validateProjectAccess(UserProjectPermission.CAN_MANAGE_PROJECT, {
        getProjectId: (params) => params.id,
      }),
    },
  })
}
