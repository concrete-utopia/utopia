import { LoaderFunctionArgs } from '@remix-run/node'
import { handleListProjects } from '../handlers/listProjects'
import { handle, handleOptions } from '../util/api.server'
import { ALLOW } from '../handlers/validators'

export async function loader(args: LoaderFunctionArgs) {
  return handle(args, {
    OPTIONS: handleOptions,
    GET: {
      handler: handleListProjects,
      validator: ALLOW,
    },
  })
}
