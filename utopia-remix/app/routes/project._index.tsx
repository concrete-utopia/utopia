import type { LoaderFunctionArgs } from '@remix-run/node'
import { loggedInRequest } from '../loaders/loggedInProjectLoader'

export async function loader(args: LoaderFunctionArgs) {
  return await loggedInRequest(args)
}
