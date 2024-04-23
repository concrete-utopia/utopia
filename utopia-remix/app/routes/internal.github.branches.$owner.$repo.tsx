import type { LoaderFunctionArgs } from '@remix-run/node'
import { json } from '@remix-run/node'
import { ensure, handle, requireUser } from '../util/api.server'
import { ALLOW } from '../handlers/validators'
import { Octokit } from '@octokit/rest'
import { Status } from '../util/statusCodes'
import { getGithubAuthentication } from '../models/githubAuthentication.server'
import type { Params } from '@remix-run/react'
import { isResponseWithMessageData, toApiFailure } from '../types'

export async function loader(args: LoaderFunctionArgs) {
  return handle(args, {
    GET: {
      validator: ALLOW,
      handler: handleListRepoBranches,
    },
  })
}

export async function handleListRepoBranches(req: Request, params: Params<string>) {
  const user = await requireUser(req)

  const githubAuth = await getGithubAuthentication({ userId: user.user_id })
  ensure(githubAuth != null, 'missing auth', Status.FORBIDDEN)

  const octokit = new Octokit({ auth: githubAuth.access_token })

  const { owner, repo } = params
  ensure(owner != null, 'missing owner', Status.BAD_REQUEST)
  ensure(repo != null, 'missing repo', Status.BAD_REQUEST)

  try {
    const branches = await octokit.request('GET /repos/{owner}/{repo}/branches', {
      owner: owner,
      repo: repo,
    })
    if (branches.status < 200 || branches.status > 299) {
      return json(toApiFailure(`branches not found (${branches.status})`), {
        status: branches.status,
        headers: { 'cache-control': 'no-cache' },
      })
    }

    return {
      type: 'SUCCESS',
      branches: branches.data.map((branch) => ({ name: branch.name })),
    }
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
