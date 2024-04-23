import { Octokit } from '@octokit/rest'
import { json, type ActionFunctionArgs } from '@remix-run/node'
import { ALLOW } from '../handlers/validators'
import { getGithubAuthentication } from '../models/githubAuthentication.server'
import {
  isResponseWithMessageData,
  isSearchPublicRepositoriesRequest,
  toApiFailure,
  toApiSuccess,
} from '../types'
import { ensure, handle, requireUser } from '../util/api.server'
import { Status } from '../util/statusCodes'

export async function action(args: ActionFunctionArgs) {
  return handle(args, {
    POST: {
      validator: ALLOW,
      handler: handleSearchPublicRepositories,
    },
  })
}

export async function handleSearchPublicRepositories(req: Request) {
  const user = await requireUser(req)

  const githubAuth = await getGithubAuthentication({ userId: user.user_id })
  ensure(githubAuth != null, 'missing auth', Status.FORBIDDEN)

  const body = await req.json()
  ensure(isSearchPublicRepositoriesRequest(body), 'invalid request', Status.BAD_REQUEST)

  const owner = body.owner.trim()
  ensure(owner.length > 0, 'invalid owner', Status.BAD_REQUEST)
  const repo = body.repo.trim()
  ensure(repo.length > 0, 'invalid owner', Status.BAD_REQUEST)

  const octokit = new Octokit({ auth: githubAuth.access_token })

  try {
    const response = await octokit.request('GET /repos/{owner}/{repo}', {
      owner: owner,
      repo: repo,
    })
    if (response.status < 200 || response.status > 299) {
      return json(toApiFailure(`repository not found (${response.status})`), {
        status: response.status,
        headers: { 'cache-control': 'no-cache' },
      })
    }

    const { data } = response

    return toApiSuccess({
      repository: {
        avatarUrl: data.owner.avatar_url,
        defaultBranch: data.default_branch,
        description: data.description,
        fullName: data.full_name,
        isPrivate: data.private,
        name: data.name,
        permissions: data.permissions,
        updatedAt: data.updated_at,
      },
    })
  } catch (err) {
    if (isResponseWithMessageData(err)) {
      return json(toApiFailure(err.response.data.message), {
        status: err.status,
        headers: { 'cache-control': 'no-cache' },
      })
    }
    throw err
  }
}
