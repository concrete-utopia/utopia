import { LoaderFunctionArgs } from '@remix-run/node'
import { handle, handleOptions } from '../util/api.server'
import { ListProjectsResponse } from '../types'

export async function loader(args: LoaderFunctionArgs) {
  return handle(args.request, {
    OPTIONS: handleOptions,
    GET: handleShowcase,
  })
}

export async function handleShowcase(): Promise<ListProjectsResponse> {
  return { projects: [] }
}
