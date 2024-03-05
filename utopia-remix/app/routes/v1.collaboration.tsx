import { ActionFunctionArgs, LoaderFunctionArgs } from '@remix-run/node'
import { handle, handleOptions } from '../util/api.server'
import { proxy } from '../util/proxy.server'
import { validateProjectAccess } from '../handlers/validators'
import { UserProjectPermission } from '../types'

export async function loader(args: LoaderFunctionArgs) {
  return handle(args, {
    OPTIONS: handleOptions,
  })
}

export async function action(args: ActionFunctionArgs) {
  return handle(args, {
    PUT: {
      handler: proxy,
      validator: validateProjectAccess(UserProjectPermission.CAN_MANAGE_PROJECT),
    },
  })
}
