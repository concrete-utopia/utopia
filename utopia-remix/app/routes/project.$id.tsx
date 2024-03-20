import React from 'react'
import type { LoaderFunctionArgs } from '@remix-run/node'
import { ALLOW, validateProjectAccess } from '../handlers/validators'
import { getUser, handle } from '../util/api.server'
import { UserProjectPermission } from '~/types'
import { useLoaderData, type Params, json, redirect } from '@remix-run/react'
import type { ApiError } from '~/util/errors'
import { Status } from '~/util/statusCodes'
import ProjectNotFound from '../components/projectNotFound'

export async function loader(args: LoaderFunctionArgs) {
  return getProjectForEditor(args.request, args.params)
}

const validator = validateProjectAccess(UserProjectPermission.CAN_VIEW_PROJECT, {
  canRequestAccess: true,
  getProjectId: (params) => params.id?.split('-')[0],
})

export async function getProjectForEditor(req: Request, params: Params<string>) {
  const projectId = params.id?.split('-')[0]
  const user = await getUser(req)
  const validatorResult = await validator(req, params)
  if (validatorResult.ok) {
    throw redirect(`/p/${params.id}`)
  }
  const error = validatorResult.error as ApiError
  if (error.status === Status.NOT_FOUND) {
    return json(
      {
        projectId: null,
        userId: null,
      },
      {
        status: Status.NOT_FOUND,
      },
    )
  } else if (error.status === Status.FORBIDDEN) {
    return json(
      {
        projectId: projectId,
        userId: user?.user_id,
      },
      {
        status: Status.FORBIDDEN,
      },
    )
  }
  throw error
}

export default function () {
  const data = useLoaderData() as unknown as { projectId: string | null; userId: string | null }
  return <ProjectNotFound projectId={data.projectId} userId={data.userId} />
}
