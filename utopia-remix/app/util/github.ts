import { Octokit } from '@octokit/rest'
import type { RequestInterface } from '@octokit/types'
import { toApiSuccess, isResponseWithMessageData, toApiFailure } from '../types'
import { Status } from './statusCodes'
import { json } from '@remix-run/node'

export function githubRepositoryPrettyName(repo: string | null): string {
  if (repo == null) {
    return ''
  }
  const parts = repo.split(':')
  const name = parts[0]
  if (parts.length > 1) {
    const branch = parts[1]
    return `${name} (${branch})`
  }
  return name
}

export interface OctokitClient {
  request: RequestInterface
}

export function newOctokitClient(auth: string): OctokitClient {
  return new Octokit({ auth: auth })
}

export async function wrapGithubAPIRequest(
  client: OctokitClient,
  fn: (client: OctokitClient) => Promise<unknown>,
) {
  try {
    const result = await fn(client)
    return toApiSuccess(result)
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
