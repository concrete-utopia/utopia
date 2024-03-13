import type { ActionFunctionArgs, LoaderFunctionArgs } from '@remix-run/node'
import { proxy } from '../util/proxy.server'
import { handle, handleOptions } from '../util/api.server'
import { Status } from '../util/statusCodes'
import { UserProjectPermission } from '../types'
import { ALLOW, validateProjectAccess } from '../handlers/validators'

export async function loader(args: LoaderFunctionArgs) {
  return handle(args, {
    OPTIONS: handleOptions,
    GET: {
      handler: proxy,
      validator: validateProjectAccess(UserProjectPermission.CAN_VIEW_PROJECT, {
        errorMessage: 'Project not found',
        status: Status.NOT_FOUND,
        getProjectId: (params) => params.id,
      }),
    },
  })
}

export async function action(args: ActionFunctionArgs) {
  return handle(args, {
    PUT: {
      handler: proxy,
      validator: ALLOW,
    },
  })
}
