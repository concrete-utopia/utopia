import React from 'react'
import type { LoaderFunctionArgs } from '@remix-run/node'
import { validateProjectAccess } from '../handlers/validators'
import { getProjectIdFromParams, getUser } from '../util/api.server'
import { UserProjectPermission } from '../types'
import { useLoaderData, type Params, json, redirect } from '@remix-run/react'
import type { ApiError } from '../util/errors'
import { Status } from '../util/statusCodes'
import ProjectNotFound from '../components/projectNotFound'
import type { UserDetails } from 'prisma-client'

export async function loader(args: LoaderFunctionArgs) {
  return getProjectForEditor(args.request, args.params)
}

const validator = validateProjectAccess(UserProjectPermission.CAN_VIEW_PROJECT, {
  canRequestAccess: true,
  getProjectId: (params) => getProjectIdFromParams(params, 'id'),
})

export async function getProjectForEditor(req: Request, params: Params<string>) {
  const projectId = getProjectIdFromParams(params, 'id')
  const user = await getUser(req)
  const validatorResult = await validator(req, params)
  if (validatorResult.ok) {
    const url = new URL(req.url)
    return redirect(`/p/${params.id}${url.search}`, {
      headers: { 'cache-control': 'no-cache' },
    })
  }
  const error = validatorResult.error as ApiError
  if (error.status === Status.NOT_FOUND) {
    return json(
      {
        projectId: projectId,
        user: user,
        status: Status.NOT_FOUND,
      },
      {
        status: Status.NOT_FOUND,
      },
    )
  } else if (error.status === Status.FORBIDDEN) {
    return json(
      {
        projectId: projectId,
        user: user,
        status: Status.FORBIDDEN,
      },
      {
        status: Status.FORBIDDEN,
      },
    )
  }
  throw error
}

export default function () {
  const data = useLoaderData() as unknown as {
    projectId: string | null
    user: UserDetails | null
    status: number
  }
  return <ProjectNotFound projectId={data.projectId} user={data.user} status={data.status} />
}
