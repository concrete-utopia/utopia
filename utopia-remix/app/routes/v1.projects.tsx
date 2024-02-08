import { LoaderFunctionArgs } from '@remix-run/node'
import { handleListProjects } from '../handlers/listProjects'
import { handle, handleOptions } from '../util/api.server'

export async function loader(args: LoaderFunctionArgs) {
  return handle(args.request, {
    OPTIONS: handleOptions,
    GET: handleListProjects,
  })
}
