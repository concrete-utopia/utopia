import type { ActionFunctionArgs, LoaderFunctionArgs } from '@remix-run/node'
import { proxy } from '../util/proxy.server'
import { handle, handleOptions } from '../util/api.server'
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
  })
}

async function createProject(req: Request, params: Params<string>) {
  const projectIdRequest = new Request('/v1/projectid', {
    method: 'GET',
    headers: req.headers,
  })
  const projectIdResponse = (await proxy(projectIdRequest)) as { id: string }
  const { id } = projectIdResponse

  const requestData = await req.formData()
  const content = requestData.get('content') as string
  const name = requestData.get('name') as string
  const accessLevelParam = requestData.get('accessLevel') as string

  const projectRequestFormData = new FormData()
  projectRequestFormData.append('content', content)
  projectRequestFormData.append('name', name)

  const createProjectRequest = new Request(`/v1/project/${id}`, {
    method: 'PUT',
    headers: req.headers,
    body: req.body,
  })

  // create the project
  const project = (await proxy(createProjectRequest)) as { owner_id?: string }

  // handle access level setting
  const accessLevel: AccessLevel =
    accessLevelParamToAccessLevel(accessLevelParam) ?? AccessLevel.PRIVATE
  await createProjectAccess({
    projectId: id,
    accessLevel: accessLevel,
    creatorId: project.owner_id ?? null,
  })

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
