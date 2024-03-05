import { ActionFunctionArgs } from '@remix-run/node'
import { chain, handle } from '../util/api.server'
import { proxy } from '../util/proxy.server'

export async function action(args: ActionFunctionArgs) {
  return handle(args, {
    DELETE: chain([proxy]),
  })
}
