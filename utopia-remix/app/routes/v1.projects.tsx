import { LoaderFunctionArgs } from '@remix-run/node'
import { handleListProjects } from '../handlers/listProjects'
import { chain, handle, handleOptions } from '../util/api.server'

export async function loader(args: LoaderFunctionArgs) {
  return handle(args, {
    OPTIONS: chain([handleOptions]),
    GET: chain([handleListProjects]),
  })
}
