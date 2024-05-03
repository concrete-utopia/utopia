import { HEADERS, MODE } from '../../../../common/server'
import type { EditorAction, EditorDispatch } from '../../../../components/editor/action-types'
import {
  setGithubState,
  updateGithubData,
  updateGithubSettings,
} from '../../../../components/editor/actions/action-creators'
import { requestSearchPublicGithubRepository } from '../../../../components/editor/server'
import type { GithubOperation, GithubRepo } from '../../../../components/editor/store/editor-state'
import {
  emptyGithubSettings,
  githubRepoFullName,
} from '../../../../components/editor/store/editor-state'
import { assertNever } from '../../utils'
import { GithubEndpoints } from '../endpoints'
import type { GithubFailure, GithubOperationSource, RepositoryEntry } from '../helpers'
import { githubAPIError, githubAPIErrorFromResponse, runGithubOperation } from '../helpers'
import type { GithubOperationContext } from './github-operation-context'

export interface GetUsersPublicRepositoriesSuccess {
  type: 'SUCCESS'
  repositories: Array<RepositoryEntry>
}

export type GetUsersPublicRepositoriesResponse = GetUsersPublicRepositoriesSuccess | GithubFailure

export const getUsersPublicGithubRepositories =
  (operationContext: GithubOperationContext) =>
  async (
    dispatch: EditorDispatch,
    initiator: GithubOperationSource,
    githubRepo: GithubRepo | null,
  ): Promise<Array<EditorAction>> => {
    return runGithubOperation(
      { name: 'loadRepositories' },
      dispatch,
      initiator,
      async (operation: GithubOperation) => {
        const url = GithubEndpoints.repositories()

        const response = await operationContext.fetch(url, {
          method: 'GET',
          credentials: 'include',
          headers: HEADERS,
          mode: MODE,
        })
        if (!response.ok) {
          throw await githubAPIErrorFromResponse(operation, response)
        }

        const responseBody: GetUsersPublicRepositoriesResponse = await response.json()
        switch (responseBody.type) {
          case 'FAILURE':
            if (responseBody.failureReason.includes('Authentication')) {
              dispatch(
                [
                  updateGithubSettings(emptyGithubSettings()),
                  setGithubState({ authenticated: false }),
                ],
                'everyone',
              )
            }
            throw githubAPIError(operation, responseBody.failureReason)
          case 'SUCCESS':
            const userRepos = responseBody.repositories.filter((repo) => !repo.isPrivate)

            // if the github repository is defined and is not found in the user-owner repos, grab its details too
            const publicRepo = await searchPublicRepoIfMissingFromUserRepos(
              operationContext,
              githubRepo,
              userRepos,
            )

            return [
              updateGithubData({
                userRepositories: userRepos,
                publicRepositories: publicRepo != null ? [publicRepo] : [],
              }),
            ]
          default:
            const _exhaustiveCheck: never = responseBody
            throw githubAPIError(
              operation,
              `Unhandled response body ${JSON.stringify(responseBody)}`,
            )
        }
      },
    )
  }

async function searchPublicRepoIfMissingFromUserRepos(
  operationContext: GithubOperationContext,
  githubRepo: GithubRepo | null,
  userRepos: RepositoryEntry[],
): Promise<RepositoryEntry | null> {
  if (
    githubRepo == null ||
    userRepos.some((repo) => repo.fullName === githubRepoFullName(githubRepo))
  ) {
    return null
  }
  return getPublicRepositoryEntryOrNull(operationContext, {
    owner: githubRepo.owner,
    repo: githubRepo.repository,
  })
}

export interface SearchPublicRepositorySuccess {
  type: 'SUCCESS'
  repository: RepositoryEntry
}

export type SearchPublicRepositoryResponse = SearchPublicRepositorySuccess | GithubFailure

export const searchPublicGithubRepository =
  (operationContext: GithubOperationContext) =>
  async (
    dispatch: EditorDispatch,
    initiator: GithubOperationSource,
    params: {
      owner: string
      repo: string
    },
  ): Promise<Array<EditorAction>> => {
    return runGithubOperation(
      { name: 'searchRepository' },
      dispatch,
      initiator,
      async (operation: GithubOperation) => {
        const response = await requestSearchPublicGithubRepository(operationContext, params)
        if (!response.ok) {
          throw await githubAPIErrorFromResponse(operation, response)
        }

        const responseBody: SearchPublicRepositoryResponse = await response.json()
        switch (responseBody.type) {
          case 'FAILURE':
            if (responseBody.failureReason.includes('Authentication')) {
              dispatch(
                [
                  updateGithubSettings(emptyGithubSettings()),
                  setGithubState({ authenticated: false }),
                ],
                'everyone',
              )
            }
            throw githubAPIError(operation, responseBody.failureReason)
          case 'SUCCESS':
            return [
              updateGithubData({
                publicRepositories: [responseBody.repository],
              }),
            ]
          default:
            assertNever(responseBody)
        }
      },
    )
  }

export async function getPublicRepositoryEntryOrNull(
  operationContext: GithubOperationContext,
  params: {
    owner: string
    repo: string
  },
): Promise<RepositoryEntry | null> {
  const response = await requestSearchPublicGithubRepository(operationContext, params)
  if (!response.ok) {
    console.error(`Cannot get repository data: ${response.status}`)
    return null
  }
  try {
    const responseBody: SearchPublicRepositoryResponse = await response.json()
    switch (responseBody.type) {
      case 'FAILURE':
        console.error(`Cannot get repository data: ${responseBody.failureReason}`)
        return null
      case 'SUCCESS':
        return responseBody.repository
      default:
        assertNever(responseBody)
    }
  } catch (err) {
    console.error(`Cannot get repository data: ${err}`)
    return null
  }
}
