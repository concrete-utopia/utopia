import type { LoaderFunctionArgs } from '@remix-run/node'
import { handle, handleOptions } from '../util/api.server'
import { proxy } from '../util/proxy.server'
import { ALLOW } from '../handlers/validators'

export async function loader(args: LoaderFunctionArgs) {
  return handle(args, {
    OPTIONS: handleOptions,
    GET: { handler: proxy, validator: ALLOW },
  })
}
