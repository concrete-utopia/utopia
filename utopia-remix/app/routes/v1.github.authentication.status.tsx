import { LoaderFunctionArgs } from '@remix-run/node'
import { handle, handleOptions } from '../util/api.server'
import { proxy } from '../util/proxy.server'

export async function loader(args: LoaderFunctionArgs) {
  return handle(args, {
    OPTIONS: handleOptions,
    GET: proxy,
  })
}
