import type { LoaderFunctionArgs } from '@remix-run/node'
import { proxy } from '../util/proxy.server'
import { ensure, getUser, handle, handleOptions, requireUser } from '../util/api.server'
import { validateProjectAccess } from '../handlers/validators'
import { UserProjectPermission } from '../types'
import type { Params } from '@remix-run/react'
import { getProjectMetadataForEditor } from '../models/project.server'
import { Status } from '../util/statusCodes'

export async function loader(args: LoaderFunctionArgs) {
  return handle(args, {
    OPTIONS: handleOptions,
    GET: {
      handler: handleGetMetadata,
      validator: validateProjectAccess(UserProjectPermission.CAN_VIEW_PROJECT, {
        getProjectId: (params) => params.id,
      }),
    },
  })
}

export async function handleGetMetadata(req: Request, params: Params<string>) {
  const projectdId = params.id
  ensure(projectdId != null, 'invalid project id', Status.BAD_REQUEST)

  const proxied = await proxy(req)
  const body = proxied as object
  const user = await getUser(req)
  if (user != null) {
    const meta = await getProjectMetadataForEditor({
      projectId: projectdId,
      userId: user.user_id,
    })
    if (meta != null) {
      return { ...body, ...meta }
    }
  }
  return proxied
}
