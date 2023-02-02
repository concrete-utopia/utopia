import urljoin from 'url-join'
import { UTOPIA_BACKEND } from '../../../../common/env-vars'
import { HEADERS, MODE } from '../../../../common/server'
import { EditorAction, EditorDispatch } from '../../../../components/editor/action-types'
import {
  setGithubState,
  updateGithubData,
  updateGithubSettings,
} from '../../../../components/editor/actions/action-creators'
import {
  emptyGithubSettings,
  GithubOperation,
} from '../../../../components/editor/store/editor-state'
import {
  githubAPIError,
  githubAPIErrorFromResponse,
  GithubFailure,
  RepositoryEntry,
  runGithubOperation,
} from '../helpers'

export interface GetUsersPublicRepositoriesSuccess {
  type: 'SUCCESS'
  repositories: Array<RepositoryEntry>
}

export type GetUsersPublicRepositoriesResponse = GetUsersPublicRepositoriesSuccess | GithubFailure

export async function getUsersPublicGithubRepositories(
  dispatch: EditorDispatch,
): Promise<Array<EditorAction>> {
  return runGithubOperation(
    { name: 'loadRepositories' },
    dispatch,
    async (operation: GithubOperation) => {
      const url = urljoin(UTOPIA_BACKEND, 'github', 'user', 'repositories')

      const response = await fetch(url, {
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
              publicRepositories: responseBody.repositories.filter((repo) => !repo.isPrivate),
            }),
          ]
        default:
          const _exhaustiveCheck: never = responseBody
          throw githubAPIError(operation, `Unhandled response body ${JSON.stringify(responseBody)}`)
      }
    },
  )
}
