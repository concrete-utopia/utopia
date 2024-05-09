import type { ActionFunctionArgs, LoaderFunctionArgs } from '@remix-run/node'
import { ensure, handle, handleOptions, requireUser } from '../util/api.server'
import { validateProjectAccess } from '../handlers/validators'
import type { Params } from '@remix-run/react'
import type { ExistingAsset } from '../types'
import { UserProjectPermission } from '../types'
import { Status } from '../util/statusCodes'
import { getGithubAuthentication } from '../models/githubAuthentication.server'
import { getBranchProjectContents } from '../util/github-branch-contents'
import { newOctokitClient, wrapGithubAPIRequest } from '../util/github'

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

export type GetBranchProjectContentsRequest = {
  existingAssets: ExistingAsset[] | null
  uploadAssets: boolean
}

function isBranchContentsRequest(u: unknown): u is GetBranchProjectContentsRequest {
  const maybe = u as GetBranchProjectContentsRequest
  return (
    u != null &&
    typeof u === 'object' &&
    maybe.existingAssets != null &&
    (maybe.uploadAssets == null || Array.isArray(maybe.existingAssets))
  )
}

async function handler(req: Request, params: Params<string>) {
  const projectId = params.id
  ensure(projectId != null, 'missing project id', Status.BAD_REQUEST)

  const user = await requireUser(req)

  const owner = params.owner
  ensure(owner != null, 'missing owner', Status.BAD_REQUEST)
  const repo = params.repo
  ensure(repo != null, 'missing repo', Status.BAD_REQUEST)
  const branch = params.branch
  ensure(branch != null, 'missing branch', Status.BAD_REQUEST)

  const body = await req.json()
  ensure(isBranchContentsRequest(body), 'invalid request', Status.BAD_REQUEST)

  const githubAuth = await getGithubAuthentication({ userId: user.user_id })
  ensure(githubAuth != null, 'unauthorized', Status.UNAUTHORIZED)

  const client = newOctokitClient(githubAuth.access_token)

  return wrapGithubAPIRequest(
    client,
    getBranchProjectContents({
      projectId: projectId,
      owner: owner,
      repo: repo,
      branch: branch,
      uploadAssets: body.uploadAssets,
      existingAssets: body.existingAssets ?? [],
    }),
  )
}
