import { ActionFunctionArgs } from '@remix-run/node'
import { alwaysAllow, handle } from '../util/api.server'
import { proxy } from '../util/proxy.server'

export async function action(args: ActionFunctionArgs) {
  return handle(args, {
    DELETE: alwaysAllow(proxy),
  })
}
