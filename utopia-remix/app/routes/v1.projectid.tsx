import { ActionFunctionArgs, LoaderFunctionArgs } from '@remix-run/node'
import { proxy } from '../util/proxy.server'
import { chain, handle, handleOptions } from '../util/api.server'

export async function loader(args: LoaderFunctionArgs) {
  return handle(args, {
    OPTIONS: chain([handleOptions]),
  })
}

export async function action(args: ActionFunctionArgs) {
  return handle(args, {
    POST: chain([proxy]),
  })
}
