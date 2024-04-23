import { Octokit } from '@octokit/rest'
import type { RequestInterface } from '@octokit/types'

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
