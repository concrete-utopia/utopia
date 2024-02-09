import { ActionFunctionArgs, LoaderFunctionArgs } from '@remix-run/node'
import { handle, handleOptions } from '../util/api.server'
import { proxy } from '../util/proxy.server'

export async function loader(args: LoaderFunctionArgs) {
  return handle(args, {
    OPTIONS: handleOptions,
  })
}

export async function action(args: ActionFunctionArgs) {
  return handle(args, {
    POST: (req) => proxy(req, { rawOutput: true }),
  })
}
