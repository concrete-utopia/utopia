import urljoin from 'url-join'
import { UTOPIA_BACKEND } from '../../../common/env-vars'
import type { GithubRepo } from '../../../components/editor/store/editor-state'
import type { GetUsersPublicRepositoriesResponse } from './operations/load-repositories'
import type { GetGithubUserResponse } from './helpers'
import { HEADERS, MODE } from '../../../common/server'

type TypedResponse<T> = Response & { json: () => Promise<T> }

type GetRequest<T> = () => Promise<TypedResponse<T>>

export interface IGithubEndpoints {
  repositories: GetRequest<GetUsersPublicRepositoriesResponse>
  userDetails: GetRequest<GetGithubUserResponse>
}

export const GithubEndpoints2: IGithubEndpoints = {
  repositories: () =>
    fetch(urljoin(UTOPIA_BACKEND, 'github', 'user', 'repositories'), {
      method: 'GET',
      credentials: 'include',
      headers: HEADERS,
      mode: MODE,
    }),

  userDetails: () =>
    fetch(urljoin(UTOPIA_BACKEND, 'github', 'user'), {
      method: 'GET',
      credentials: 'include',
      headers: HEADERS,
      mode: MODE,
    }),
}

export const GithubEndpoints = {
  save: (projectID: string) => urljoin(UTOPIA_BACKEND, 'github', 'save', projectID),
  getBranches: ({ owner, repository }: GithubRepo) =>
    urljoin(UTOPIA_BACKEND, 'github', 'branches', owner, repository),
  branchContents: (githubRepo: GithubRepo, branchName: string) =>
    urljoin(
      UTOPIA_BACKEND,
      'github',
      'branches',
      githubRepo.owner,
      githubRepo.repository,
      'branch',
      branchName,
    ),
  asset: (githubRepo: GithubRepo, assetSha: string) =>
    urljoin(
      UTOPIA_BACKEND,
      'github',
      'branches',
      githubRepo.owner,
      githubRepo.repository,
      'asset',
      assetSha,
    ),
  updatePullRequests: (githubRepo: GithubRepo, branchName: string) =>
    urljoin(
      UTOPIA_BACKEND,
      'github',
      'branches',
      githubRepo.owner,
      githubRepo.repository,
      'branch',
      branchName,
      'pullrequest',
    ),
  authenticationStatus: () => urljoin(UTOPIA_BACKEND, 'github', 'authentication', 'status'),
  authenticationStart: () => urljoin(UTOPIA_BACKEND, 'github', 'authentication', 'start'),
} as const
