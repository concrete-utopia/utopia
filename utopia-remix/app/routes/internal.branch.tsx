import type { ActionFunctionArgs } from '@remix-run/node'
import { handle } from '../util/api.server'
import { proxy } from '../util/proxy.server'
import { ALLOW } from '../handlers/validators'

export async function action(args: ActionFunctionArgs) {
  return handle(args, {
    DELETE: { handler: proxy, validator: ALLOW },
  })
}
