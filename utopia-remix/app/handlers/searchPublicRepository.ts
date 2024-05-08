import { getGithubAuthentication } from '../models/githubAuthentication.server'
import { isSearchPublicRepositoriesRequest } from '../types'
import { ensure, requireUser } from '../util/api.server'
import { ApiError } from '../util/errors'
import { newOctokitClient, wrapGithubAPIRequest } from '../util/github.server'
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

  return wrapGithubAPIRequest(octokit, async (client) => {
    const response = await client.request('GET /repos/{owner}/{repo}', {
      owner: owner,
      repo: repo,
    })

    if (response.status < 200 || response.status > 299) {
      throw new ApiError(`repository not found (${response.status})`, response.status)
    }

    return {
      repository: {
        avatarUrl: response.data.owner.avatar_url,
        defaultBranch: response.data.default_branch,
        description: response.data.description,
        fullName: response.data.full_name,
        isPrivate: response.data.private,
        name: response.data.name,
        permissions: response.data.permissions,
        updatedAt: response.data.updated_at,
      },
    }
  })
}
