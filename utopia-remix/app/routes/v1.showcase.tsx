import type { LoaderFunctionArgs } from '@remix-run/node'
import { handle, handleOptions } from '../util/api.server'
import type { ListProjectsResponseV1 } from '../types'
import { ALLOW } from '../handlers/validators'

export async function loader(args: LoaderFunctionArgs) {
  return handle(args, {
    OPTIONS: handleOptions,
    GET: { handler: handleShowcase, validator: ALLOW },
  })
}

export async function handleShowcase(): Promise<ListProjectsResponseV1> {
  return { projects: [] }
}
