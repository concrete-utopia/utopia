import type { ActionFunctionArgs, LoaderFunctionArgs } from '@remix-run/node'
import { handle, handleOptions } from '../util/api.server'
import { AccessLevel } from '../types'
import { ALLOW } from '../handlers/validators'
import type { Params } from '@remix-run/react'
import { createProjectAccess } from '../models/projectAccess.server'
import { Status } from '../util/statusCodes'
import { ApiError } from '../util/errors'
import { proxy } from '../util/proxy.server'

export async function loader(args: LoaderFunctionArgs) {
  return handle(args, {
    OPTIONS: handleOptions,
  })
}

export async function action(args: ActionFunctionArgs) {
  return handle(args, {
    PUT: {
      handler: createNewProject,
      validator: ALLOW,
    },
  })
}

async function createNewProject(req: Request, params: Params<string>) {
  // get a new project id
  const projectIdRequest = new Request(withPath(req, '/v1/projectid'), {
    method: 'POST',
    headers: req.headers,
  })
  const projectIdResponse = (await proxy(projectIdRequest)) as { id: string }
  const { id } = projectIdResponse

  // prepare data for creating the project
  const requestData = (await req.json()) as { content: object; name: string; accessLevel: string }
  const { content, name, accessLevel: accessLevelParam } = requestData
  const headers = new Headers(req.headers)
  headers.delete('content-length')
  const createProjectRequest = new Request(withPath(req, `/v1/project/${id}`), {
    method: 'PUT',
    headers: headers,
    body: JSON.stringify({ content: content, name: name }),
  })

  // create the project
  const project = (await proxy(createProjectRequest)) as {
    ownerId: string | null
  }

  // handle access level setting
  const accessLevel: AccessLevel =
    accessLevelParamToAccessLevel(accessLevelParam) ?? AccessLevel.PRIVATE
  await createProjectAccess({
    projectId: id,
    accessLevel: accessLevel,
    creatorId: project.ownerId ?? null,
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

function withPath(req: Request, path: string): URL {
  const url = new URL(req.url)
  url.pathname = path
  return url
}
