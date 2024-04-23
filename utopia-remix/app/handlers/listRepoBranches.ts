import { type Params } from '@remix-run/react'
import { getGithubAuthentication } from '../models/githubAuthentication.server'
import { ensure, requireUser } from '../util/api.server'
import { ApiError } from '../util/errors'
import { newOctokitClient, wrapGithubAPIRequest } from '../util/github'
import { Status } from '../util/statusCodes'

export async function handleListRepoBranches(req: Request, params: Params<string>) {
  const user = await requireUser(req)

  const { owner, repo } = params
  ensure(owner != null, 'missing owner', Status.BAD_REQUEST)
  ensure(repo != null, 'missing repo', Status.BAD_REQUEST)

  const githubAuth = await getGithubAuthentication({ userId: user.user_id })
  ensure(githubAuth != null, 'missing auth', Status.FORBIDDEN)

  const octokit = newOctokitClient(githubAuth.access_token)

  return wrapGithubAPIRequest(octokit, async (client) => {
    const branches = await client.request('GET /repos/{owner}/{repo}/branches', {
      owner: owner,
      repo: repo,
    })

    if (branches.status < 200 || branches.status > 299) {
      throw new ApiError(`branches not found (${branches.status})`, branches.status)
    }

    return {
      branches: branches.data.map((branch) => ({ name: branch.name })),
    }
  })
}
