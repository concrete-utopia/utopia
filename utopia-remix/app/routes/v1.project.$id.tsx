import type { ActionFunctionArgs, LoaderFunctionArgs } from '@remix-run/node'
import { proxy } from '../util/proxy.server'
import { ensure, handle, handleOptions } from '../util/api.server'
import { AccessLevel, UserProjectPermission } from '../types'
import { ALLOW, validateProjectAccess } from '../handlers/validators'
import type { Params } from '@remix-run/react'
import { createProjectAccess } from '../models/projectAccess.server'
import { Status } from '../util/statusCodes'
import { ApiError } from '../util/errors'

export async function loader(args: LoaderFunctionArgs) {
  return handle(args, {
    OPTIONS: handleOptions,
    GET: {
      handler: proxy,
      validator: validateProjectAccess(UserProjectPermission.CAN_VIEW_PROJECT, {
        canRequestAccess: true,
        getProjectId: (params) => params.id,
      }),
    },
  })
}

export async function action(args: ActionFunctionArgs) {
  return handle(args, {
    PUT: {
      handler: createProject,
      validator: ALLOW,
    },
    POST: {
      handler: proxy,
      validator: ALLOW,
    },
  })
}

async function createProject(req: Request, params: Params<string>) {
  const { id } = params
  ensure(id != null, 'project id is null', Status.BAD_REQUEST)

  // create the project
  const project = await proxy(req)

  // handle access level setting
  const url = new URL(req.url)
  const accessLevelParam = url.searchParams.get('accessLevel')
  const accessLevel: AccessLevel | null = accessLevelParamToAccessLevel(accessLevelParam)
  if (accessLevel != null) {
    await createProjectAccess({ projectId: id, accessLevel: accessLevel })
  }

  return project
}

function accessLevelParamToAccessLevel(accessLevelParam: string | null): AccessLevel | null {
  if (accessLevelParam == null || accessLevelParam == '') {
    return null
  }
  switch (accessLevelParam) {
    case 'private':
      return AccessLevel.PRIVATE
    case 'public':
      return AccessLevel.PUBLIC
    case 'collaborative':
      return AccessLevel.COLLABORATIVE
    default:
      throw new ApiError('Invalid access level', Status.BAD_REQUEST)
  }
}
