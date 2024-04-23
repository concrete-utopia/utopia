import type { LoaderFunctionArgs } from '@remix-run/node'
import { handle } from '../util/api.server'
import { ALLOW } from '../handlers/validators'
import { handleListRepoBranches } from './handleListRepoBranches'

export async function loader(args: LoaderFunctionArgs) {
  return handle(args, {
    GET: {
      validator: ALLOW,
      handler: handleListRepoBranches,
    },
  })
}
