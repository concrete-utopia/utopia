import { ActionFunctionArgs, LoaderFunctionArgs } from '@remix-run/node'
import { proxy } from '../util/proxy.server'
import { ensure, handle, handleOptions } from '../util/api.server'
import { Params } from '@remix-run/react'

export async function loader(args: LoaderFunctionArgs) {
  return handle(args, {
    OPTIONS: handleOptions,
    GET: handleGetThumbnail(args.params),
  })
}

export async function action(args: ActionFunctionArgs) {
  return handle(args, {
    POST: proxy,
  })
}

function handleGetThumbnail(params: Params<string>) {
  return async function () {
    const id = params['projectId']
    ensure(id != null, 'project id is null', 400)
    return fetch(`https://cdn.utopia.app/pyramid_small.png`) // TODO just a placeholder 🙃
  }
}
