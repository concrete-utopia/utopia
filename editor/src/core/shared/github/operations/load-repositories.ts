import { HEADERS, MODE } from '../../../../common/server'
import type { EditorAction, EditorDispatch } from '../../../../components/editor/action-types'
import {
  setGithubState,
  updateGithubData,
  updateGithubSettings,
} from '../../../../components/editor/actions/action-creators'
import type { GithubOperation } from '../../../../components/editor/store/editor-state'
import { emptyGithubSettings } from '../../../../components/editor/store/editor-state'
import { GithubApiEndpoints } from '../endpoints'
import type { GithubFailure, RepositoryEntry } from '../helpers'
import { githubAPIError, githubAPIErrorFromResponse, runGithubOperation } from '../helpers'

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
      const response = await GithubApiEndpoints.repositories()

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
