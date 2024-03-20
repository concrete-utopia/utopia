import type { LoaderFunctionArgs } from '@remix-run/node'
import { validateProjectAccess } from '../handlers/validators'
import { proxy } from '../util/proxy.server'
import { UserProjectPermission } from '~/types'
import { type Params, redirect } from '@remix-run/react'
import { getProjectIdFromParams } from '../util/api.server'

export async function loader(args: LoaderFunctionArgs) {
  return getProjectForEditor(args.request, args.params)
}

const validator = validateProjectAccess(UserProjectPermission.CAN_VIEW_PROJECT, {
  canRequestAccess: true,
  getProjectId: (params) => getProjectIdFromParams(params, 'id'),
})

export async function getProjectForEditor(req: Request, params: Params<string>) {
  const validatorResult = await validator(req, params)
  if (validatorResult.ok) {
    const proxyResponse: Response = (await proxy(req, {
      rawOutput: true,
    })) as Response
    const body = await proxyResponse.text()
    return new Response(body, {
      headers: { 'content-type': 'text/html' },
      status: proxyResponse.status,
    })
  } else {
    throw redirect(`/project/${params.id}`)
  }
}
