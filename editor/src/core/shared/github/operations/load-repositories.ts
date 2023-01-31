import urljoin from 'url-join'
import { UTOPIA_BACKEND } from '../../../../common/env-vars'
import { HEADERS, MODE } from '../../../../common/server'
import { notice } from '../../../../components/common/notice'
import { EditorAction, EditorDispatch } from '../../../../components/editor/action-types'
import {
  setGithubState,
  showToast,
  updateGithubData,
  updateGithubSettings,
} from '../../../../components/editor/actions/action-creators'
import { emptyGithubSettings } from '../../../../components/editor/store/editor-state'
import { GithubFailure, RepositoryEntry, runGithubOperation } from '../helpers'

export interface GetUsersPublicRepositoriesSuccess {
  type: 'SUCCESS'
  repositories: Array<RepositoryEntry>
}

export type GetUsersPublicRepositoriesResponse = GetUsersPublicRepositoriesSuccess | GithubFailure

export async function getUsersPublicGithubRepositories(
  dispatch: EditorDispatch,
): Promise<Array<EditorAction>> {
  return runGithubOperation({ name: 'loadRepositories' }, dispatch, async () => {
    const url = urljoin(UTOPIA_BACKEND, 'github', 'user', 'repositories')

    const response = await fetch(url, {
      method: 'GET',
      credentials: 'include',
      headers: HEADERS,
      mode: MODE,
    })
    if (response.ok) {
      const responseBody: GetUsersPublicRepositoriesResponse = await response.json()
      switch (responseBody.type) {
        case 'FAILURE':
          const actions: EditorAction[] = [
            showToast(
              notice(
                `Github: Error getting a user's repositories: ${responseBody.failureReason}`,
                'ERROR',
              ),
            ),
          ]
          if (responseBody.failureReason.includes('Authentication')) {
            actions.push(
              updateGithubSettings(emptyGithubSettings()),
              setGithubState({ authenticated: false }),
            )
          }
          return actions
        case 'SUCCESS':
          return [
            updateGithubData({
              publicRepositories: responseBody.repositories.filter((repo) => !repo.isPrivate),
            }),
          ]
        default:
          const _exhaustiveCheck: never = responseBody
          throw new Error(`Github: Unhandled response body ${JSON.stringify(responseBody)}`)
      }
    } else {
      return [
        showToast(
          notice(`Github: Unexpected status returned from endpoint: ${response.status}`, 'ERROR'),
        ),
      ]
    }
  })
}
