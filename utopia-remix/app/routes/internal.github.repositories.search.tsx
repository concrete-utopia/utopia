import type { LoaderFunctionArgs } from '@remix-run/node'
import { type ActionFunctionArgs } from '@remix-run/node'
import { handleSearchPublicRepository } from '../handlers/searchPublicRepository'
import { ALLOW } from '../handlers/validators'
import { handle, handleOptions } from '../util/api.server'

export async function loader(args: LoaderFunctionArgs) {
  return handle(args, {
    OPTIONS: handleOptions,
  })
}

export async function action(args: ActionFunctionArgs) {
  return handle(args, {
    POST: {
      validator: ALLOW,
      handler: handleSearchPublicRepository,
    },
  })
}
