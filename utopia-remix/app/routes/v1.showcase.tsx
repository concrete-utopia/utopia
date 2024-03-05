import { LoaderFunctionArgs } from '@remix-run/node'
import { chain, handle, handleOptions } from '../util/api.server'
import { ListProjectsResponse } from '../types'

export async function loader(args: LoaderFunctionArgs) {
  return handle(args, {
    OPTIONS: chain([handleOptions]),
    GET: chain([handleShowcase]),
  })
}

export async function handleShowcase(): Promise<ListProjectsResponse> {
  return { projects: [] }
}
