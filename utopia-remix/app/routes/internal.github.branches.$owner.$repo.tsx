import type { LoaderFunctionArgs } from '@remix-run/node'
import { handle, handleOptions } from '../util/api.server'
import { ALLOW } from '../handlers/validators'
import { handleListRepoBranches } from '../handlers/listRepoBranches'

export async function loader(args: LoaderFunctionArgs) {
  return handle(args, {
    OPTIONS: handleOptions,
    GET: {
      validator: ALLOW,
      handler: handleListRepoBranches,
    },
  })
}
