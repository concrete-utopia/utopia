import type { LoaderFunctionArgs } from '@remix-run/node'
import { proxy } from '../util/proxy.server'
import { handle, handleOptions } from '../util/api.server'
import { ALLOW } from '../handlers/validators'

// This is for monitoring/debugging the HS server, exposing it for added convenience.
export async function loader(args: LoaderFunctionArgs) {
  return handle(args, {
    OPTIONS: handleOptions,
    GET: { handler: proxy, validator: ALLOW },
  })
}
