import urljoin from 'url-join'
import { UTOPIA_BACKEND } from '../../../../common/env-vars'
import { HEADERS, MODE } from '../../../../common/server'
import { EditorAction, EditorDispatch } from '../../../../components/editor/action-types'
import { updateGithubData } from '../../../../components/editor/actions/action-creators'
import { GithubOperation, GithubRepo } from '../../../../components/editor/store/editor-state'
import {
  githubAPIError,
  githubAPIErrorFromResponse,
  GithubBranch,
  GithubFailure,
  runGithubOperation,
} from '../helpers'

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
  return runGithubOperation(
    { name: 'listBranches' },
    dispatch,
    async (operation: GithubOperation) => {
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

      if (!response.ok) {
        throw await githubAPIErrorFromResponse(operation, response)
      }

      const responseBody: GetBranchesResponse = await response.json()

      switch (responseBody.type) {
        case 'FAILURE':
          throw githubAPIError(operation, responseBody.failureReason)
        case 'SUCCESS':
          return [updateGithubData({ branches: responseBody.branches })]
        default:
          const _exhaustiveCheck: never = responseBody
          throw githubAPIError(operation, `Unhandled response body ${JSON.stringify(responseBody)}`)
      }
    },
  )
}
