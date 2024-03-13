import type { ActionFunctionArgs, LoaderFunctionArgs } from '@remix-run/node'
import { proxy } from '../util/proxy.server'
import { ensure, handle, handleOptions } from '../util/api.server'
import type { Params } from '@remix-run/react'
import { ALLOW, validateProjectAccess } from '../handlers/validators'
import { UserProjectPermission } from '../types'

export async function loader(args: LoaderFunctionArgs) {
  return handle(args, {
    OPTIONS: handleOptions,
    GET: { handler: handleGetThumbnail(args.params), validator: ALLOW },
  })
}

export async function action(args: ActionFunctionArgs) {
  return handle(args, {
    POST: {
      handler: proxy,
      validator: validateProjectAccess(UserProjectPermission.CAN_MANAGE_PROJECT, {
        getProjectId: (params) => params.projectId,
      }),
    },
  })
}

function handleGetThumbnail(params: Params<string>) {
  return async function () {
    const id = params['projectId']
    ensure(id != null, 'project id is null', 400)
    return fetch(`https://cdn.utopia.app/pyramid_small.png`) // TODO just a placeholder 🙃
  }
}
