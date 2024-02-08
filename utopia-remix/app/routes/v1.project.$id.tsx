import { ActionFunctionArgs, LoaderFunctionArgs } from '@remix-run/node'
import { proxy } from '../util/proxy.server'
import { handle, handleOptions } from '../util/api.server'

export async function loader(args: LoaderFunctionArgs) {
  return handle(args.request, {
    OPTIONS: handleOptions,
    GET: proxy,
  })
}

export async function action(args: ActionFunctionArgs) {
  return handle(args.request, {
    PUT: proxy,
  })
}
