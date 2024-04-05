import type { LoaderFunctionArgs } from '@remix-run/node'
import { ensure, handle, handleOptions, requireUser } from '../util/api.server'
import { validateProjectAccess } from '../handlers/validators'
import { UserProjectPermission } from '../types'
import type { Params } from '@remix-run/react'
import { Status } from '../util/statusCodes'
import { getProjectMetadataForEditor } from '../models/project.server'

export function loader(args: LoaderFunctionArgs) {
  return handle(args, {
    OPTIONS: handleOptions,
    GET: {
      handler: handleGetMetadata,
      validator: validateProjectAccess(UserProjectPermission.CAN_MANAGE_PROJECT, {
        getProjectId: (params) => params.id,
      }),
    },
  })
}

export async function handleGetMetadata(req: Request, params: Params<string>) {
  const user = await requireUser(req)

  const projectdId = params.id
  ensure(projectdId != null, 'invalid project id', Status.BAD_REQUEST)

  const meta = await getProjectMetadataForEditor({
    projectId: projectdId,
    userId: user.user_id,
  })

  return meta
}
