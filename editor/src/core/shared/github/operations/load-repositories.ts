import { HEADERS, MODE } from '../../../../common/server'
import type { EditorAction, EditorDispatch } from '../../../../components/editor/action-types'
import {
  setGithubState,
  updateGithubData,
  updateGithubSettings,
} from '../../../../components/editor/actions/action-creators'
import type { GithubOperation } from '../../../../components/editor/store/editor-state'
import { emptyGithubSettings } from '../../../../components/editor/store/editor-state'
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
            return [
              updateGithubData({
                userRepositories: responseBody.repositories.filter((repo) => !repo.isPrivate),
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
        const url = GithubEndpoints.searchRepository()

        const response = await operationContext.fetch(url, {
          method: 'POST',
          credentials: 'include',
          headers: HEADERS,
          mode: MODE,
          body: JSON.stringify({ owner: params.owner, repo: params.repo }),
        })
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
