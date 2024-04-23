import { json } from '@remix-run/node'
import { getGithubAuthentication } from '../models/githubAuthentication.server'
import {
  isSearchPublicRepositoriesRequest,
  toApiFailure,
  toApiSuccess,
  isResponseWithMessageData,
} from '../types'
import { requireUser, ensure } from '../util/api.server'
import { newOctokitClient } from '../util/github'
import { Status } from '../util/statusCodes'

export async function handleSearchPublicRepository(req: Request) {
  const user = await requireUser(req)

  const body = await req.json()
  ensure(isSearchPublicRepositoriesRequest(body), 'invalid request', Status.BAD_REQUEST)

  const owner = body.owner.trim()
  ensure(owner.length > 0, 'invalid owner', Status.BAD_REQUEST)
  const repo = body.repo.trim()
  ensure(repo.length > 0, 'invalid repo', Status.BAD_REQUEST)

  const githubAuth = await getGithubAuthentication({ userId: user.user_id })
  ensure(githubAuth != null, 'missing auth', Status.FORBIDDEN)

  const octokit = newOctokitClient(githubAuth.access_token)

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
    return isResponseWithMessageData(err)
      ? json(toApiFailure(err.response.data.message), {
          status: err.status,
          headers: { 'cache-control': 'no-cache' },
        })
      : json(toApiFailure(`${err}`), {
          status: Status.INTERNAL_ERROR,
          headers: { 'cache-control': 'no-cache' },
        })
  }
}
