import type { ActionFunctionArgs } from '@remix-run/node'

import type { Params } from '@remix-run/react'

import { ensure, handle, requireUser } from '../util/api.server'

import { Status } from '../util/statusCodes'

import { validateProjectAccess } from '../handlers/validators'

import { UserProjectPermission } from '../types'

import { ServerEnvironment } from '../env.server'

export async function action(args: ActionFunctionArgs) {
  return handle(args, {
    POST: {
      handler: handleUpdateMetaobject,

      validator: validateProjectAccess(UserProjectPermission.CAN_MANAGE_PROJECT, {
        getProjectId: (params) => params.id,
      }),
    },
  })
}

export async function handleUpdateMetaobject(req: Request, params: Params<string>) {
  await requireUser(req)

  const body = await req.json()

  ensure(body != null, 'body is null', Status.BAD_REQUEST)

  if (body.query == null || typeof body.query !== 'string') {
    throw new Error('query must be a string')
  }

  if (body.variables == null || typeof body.variables !== 'object') {
    throw new Error('variables must be an object')
  }

  const result = await fetch(ServerEnvironment.SHOPIFY_STORE_URL, {
    method: 'POST',

    headers: {
      'Content-Type': 'application/json',

      'X-Shopify-Access-Token': ServerEnvironment.SHOPIFY_STORE_ACCESS_TOKEN,
    },

    body: JSON.stringify({
      query: body.query,

      variables: body.variables,
    }),
  }).then((res) => res.json())

  return { result: result }
}
