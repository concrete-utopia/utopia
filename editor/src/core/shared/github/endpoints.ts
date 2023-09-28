import urljoin from 'url-join'
import { UTOPIA_BACKEND } from '../../../common/env-vars'
import type { GithubRepo, PersistentModel } from '../../../components/editor/store/editor-state'
import type { GetUsersPublicRepositoriesResponse } from './operations/load-repositories'
import { HEADERS, MODE } from '../../../common/server'
import type { GetBranchesResponse } from './operations/list-branches'
import type { GetBranchContentResponse, GithubSaveAssetResponse } from './helpers'
import type { SaveToGithubResponse } from './operations/commit-and-push'
import type { GetBranchPullRequestResponse } from './operations/list-pull-requests-for-branch'

type TypedResponse<T> = Response & { json: () => Promise<T> }

export interface IGithubEndpoints {
  repositories: () => Promise<TypedResponse<GetUsersPublicRepositoriesResponse>>
  getBranches: (repo: GithubRepo) => Promise<TypedResponse<GetBranchesResponse>>
  branchContents: (
    githubRepo: GithubRepo,
    branchName: string,
    commitSha: string | null,
    previousCommitSha: string | null,
  ) => Promise<TypedResponse<GetBranchContentResponse>>
  save: (
    projectId: string,
    branchName: string | null,
    commitMessage: string | null,
    postBody: PersistentModel,
  ) => Promise<TypedResponse<SaveToGithubResponse>>
  updatePullRequests: (
    githubRepo: GithubRepo,
    branchName: string,
  ) => Promise<TypedResponse<GetBranchPullRequestResponse>>
  asset: (
    githubRepo: GithubRepo,
    assetSha: string,
    projectId: string,
    path: string,
  ) => Promise<TypedResponse<GithubSaveAssetResponse>>
}

function createQueryParamsString(params: Record<string, string | null | undefined>): string {
  let shouldIncludeQueryParams = false
  let paramsRecord: Record<string, string> = {}
  for (const [key, value] of Object.entries(params)) {
    if (value != null) {
      shouldIncludeQueryParams = true
      paramsRecord[key] = value
    }
  }
  if (!shouldIncludeQueryParams) {
    return ''
  }

  const searchParams = new URLSearchParams(paramsRecord)
  return `?${searchParams}`
}

export const GithubApiEndpoints: IGithubEndpoints = {
  repositories: () =>
    fetch(urljoin(UTOPIA_BACKEND, 'github', 'user', 'repositories'), {
      method: 'GET',
      credentials: 'include',
      headers: HEADERS,
      mode: MODE,
    }),

  getBranches: ({ owner, repository }: GithubRepo) =>
    fetch(urljoin(UTOPIA_BACKEND, 'github', 'branches', owner, repository), {
      method: 'GET',
      credentials: 'include',
      headers: HEADERS,
      mode: MODE,
    }),
  branchContents: (
    githubRepo: GithubRepo,
    branchName: string,
    commitSha: string | null,
    previousCommitSha: string | null,
  ) => {
    const urlToUse =
      urljoin(
        UTOPIA_BACKEND,
        'github',
        'branches',
        githubRepo.owner,
        githubRepo.repository,
        'branch',
        branchName,
      ) + createQueryParamsString({ commit_sha: commitSha, previus_commit_sha: previousCommitSha })

    return fetch(urlToUse, {
      method: 'GET',
      credentials: 'include',
      headers: HEADERS,
      mode: MODE,
    })
  },
  save: (
    projectId: string,
    branchName: string | null,
    commitMessage: string | null,
    persistentModel: PersistentModel,
  ) => {
    const urlToUse =
      urljoin(UTOPIA_BACKEND, 'github', 'save', projectId) +
      createQueryParamsString({ branch_name: branchName, commit_message: commitMessage })

    const postBody = JSON.stringify(persistentModel)

    return fetch(urlToUse, {
      method: 'POST',
      credentials: 'include',
      headers: HEADERS,
      mode: MODE,
      body: postBody,
    })
  },
  updatePullRequests: (githubRepo: GithubRepo, branchName: string) =>
    fetch(
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
      {
        method: 'GET',
        credentials: 'include',
        headers: HEADERS,
        mode: MODE,
      },
    ),
  asset: (githubRepo: GithubRepo, assetSha: string, projectId: string, path: string) => {
    const urlToUse =
      urljoin(
        UTOPIA_BACKEND,
        'github',
        'branches',
        githubRepo.owner,
        githubRepo.repository,
        'asset',
        assetSha,
      ) + createQueryParamsString({ project_id: projectId, path: path })

    return fetch(urlToUse, {
      method: 'POST',
      credentials: 'include',
      headers: HEADERS,
      mode: MODE,
    })
  },
}

export const GithubEndpoints = {
  userDetails: () => urljoin(UTOPIA_BACKEND, 'github', 'user'),
  authenticationStatus: () => urljoin(UTOPIA_BACKEND, 'github', 'authentication', 'status'),
  authenticationStart: () => urljoin(UTOPIA_BACKEND, 'github', 'authentication', 'start'),
} as const
