import type { ActionFunctionArgs, LoaderFunctionArgs } from '@remix-run/node'
import { ensure, handle, handleOptions, requireUser } from '../util/api.server'
import { validateProjectAccess } from '../handlers/validators'
import type { Params } from '@remix-run/react'
import type { ExistingAsset } from '../types'
import { UserProjectPermission } from '../types'
import { Status } from '../util/statusCodes'
import { getGithubAuthentication } from '../models/githubAuthentication.server'
import {
  getBranchProjectContents,
  newOctokitClient,
  wrapGithubAPIRequest,
} from '../util/github.server'

export function loader(args: LoaderFunctionArgs) {
  return handle(args, {
    OPTIONS: handleOptions,
  })
}

export function action(args: ActionFunctionArgs) {
  return handle(args, {
    POST: {
      validator: validateProjectAccess(UserProjectPermission.CAN_MANAGE_PROJECT, {
        getProjectId: (params) => params.id,
      }),
      handler: handler,
    },
  })
}

type CloneRequest = {
  owner: string
  repo: string
  branch: string
  existingAssets: ExistingAsset[]
}

function isCloneRequest(u: unknown): u is CloneRequest {
  const maybe = u as CloneRequest
  return (
    u != null &&
    typeof u === 'object' &&
    maybe.owner != null &&
    maybe.repo != null &&
    maybe.branch != null
  )
}

async function handler(req: Request, params: Params<string>) {
  const projectId = params.id
  ensure(projectId != null, 'missing project id', Status.BAD_REQUEST)

  const user = await requireUser(req)

  const body = await req.json()
  ensure(isCloneRequest(body), 'invalid request', Status.BAD_REQUEST)

  const githubAuth = await getGithubAuthentication({ userId: user.user_id })
  ensure(githubAuth != null, 'unauthorized', Status.UNAUTHORIZED)

  const client = newOctokitClient(githubAuth.access_token)

  return wrapGithubAPIRequest(
    client,
    getBranchProjectContents({
      projectId: projectId,
      owner: body.owner,
      repo: body.repo,
      branch: body.branch,
      existingAssets: body.existingAssets,
    }),
  )
}
