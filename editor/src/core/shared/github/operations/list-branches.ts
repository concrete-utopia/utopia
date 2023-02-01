import urljoin from 'url-join'
import { UTOPIA_BACKEND } from '../../../../common/env-vars'
import { HEADERS, MODE } from '../../../../common/server'
import { notice } from '../../../../components/common/notice'
import { EditorAction, EditorDispatch } from '../../../../components/editor/action-types'
import { showToast, updateGithubData } from '../../../../components/editor/actions/action-creators'
import { GithubRepo } from '../../../../components/editor/store/editor-state'
import { GithubBranch, GithubFailure, runGithubOperation } from '../helpers'

export type GetBranchesResult = Array<GithubBranch>

export interface GetBranchesSuccess {
  type: 'SUCCESS'
  branches: GetBranchesResult
}

export type GetBranchesResponse = GetBranchesSuccess | GithubFailure

export async function getBranchesForGithubRepository(
  dispatch: EditorDispatch,
  githubRepo: GithubRepo,
): Promise<Array<EditorAction>> {
  return runGithubOperation({ name: 'listBranches' }, dispatch, async () => {
    const url = urljoin(
      UTOPIA_BACKEND,
      'github',
      'branches',
      githubRepo.owner,
      githubRepo.repository,
    )

    const response = await fetch(url, {
      method: 'GET',
      credentials: 'include',
      headers: HEADERS,
      mode: MODE,
    })

    if (response.ok) {
      const responseBody: GetBranchesResponse = await response.json()

      switch (responseBody.type) {
        case 'FAILURE':
          return [
            showToast(
              notice(`Error when listing branches: ${responseBody.failureReason}`, 'ERROR'),
            ),
          ]
        case 'SUCCESS':
          return [updateGithubData({ branches: responseBody.branches })]
        default:
          const _exhaustiveCheck: never = responseBody
          throw new Error(`Unhandled response body ${JSON.stringify(responseBody)}`)
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
