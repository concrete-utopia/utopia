import { type ActionFunctionArgs } from '@remix-run/node'
import { handleSearchPublicRepository } from '../handlers/searchPublicRepository'
import { ALLOW } from '../handlers/validators'
import { handle } from '../util/api.server'

export async function action(args: ActionFunctionArgs) {
  return handle(args, {
    POST: {
      validator: ALLOW,
      handler: handleSearchPublicRepository,
    },
  })
}
