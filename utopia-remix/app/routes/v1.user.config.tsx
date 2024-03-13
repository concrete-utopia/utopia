import type { ActionFunctionArgs, LoaderFunctionArgs } from '@remix-run/node'
import { proxy } from '../util/proxy.server'
import { handle, handleOptions } from '../util/api.server'
import { ALLOW } from '../handlers/validators'

export async function loader(args: LoaderFunctionArgs) {
  return handle(args, {
    OPTIONS: handleOptions,
    GET: { handler: proxy, validator: ALLOW },
  })
}

export async function action(args: ActionFunctionArgs) {
  return handle(args, {
    POST: { handler: proxy, validator: ALLOW },
  })
}
