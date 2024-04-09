import type { LoaderFunctionArgs } from '@remix-run/node'
import { proxy } from '../util/proxy.server'
import { ensure, handle, handleOptions, requireUser } from '../util/api.server'
import { validateProjectAccess } from '../handlers/validators'
import { UserProjectPermission, isProjectMetadataV1 } from '../types'
import type { Params } from '@remix-run/react'
import { getProjectExtraMetadataForEditor } from '../models/project.server'
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

  const metadataProxyResponse = await proxy(req)

  // if the response is a valid v1 metadata, enrich it with
  // the extra data.
  if (isProjectMetadataV1(metadataProxyResponse)) {
    const user = await requireUser(req)
    const meta = await getProjectExtraMetadataForEditor({
      projectId: projectdId,
      userId: user.user_id,
    })
    if (meta != null) {
      return {
        ...metadataProxyResponse,
        ...meta,
      }
    }
  }

  return metadataProxyResponse
}
