import type { EditorAction, EditorDispatch } from '../../../../components/editor/action-types'
import { updateGithubData } from '../../../../components/editor/actions/action-creators'
import type { GithubOperation, GithubRepo } from '../../../../components/editor/store/editor-state'
import type { GithubBranch, GithubFailure } from '../helpers'
import { githubAPIError, githubAPIErrorFromResponse, runGithubOperation } from '../helpers'
import type { GithubOperationContext } from './github-operation-context'

export type GetBranchesResult = Array<GithubBranch>

export interface GetBranchesSuccess {
  type: 'SUCCESS'
  branches: GetBranchesResult
}

export type GetBranchesResponse = GetBranchesSuccess | GithubFailure

export const getBranchesForGithubRepository =
  (operationContext: GithubOperationContext) =>
  async (dispatch: EditorDispatch, githubRepo: GithubRepo): Promise<Array<EditorAction>> => {
    return runGithubOperation(
      { name: 'listBranches' },
      dispatch,
      async (operation: GithubOperation) => {
        const response = await operationContext.githubEndpoints.getBranches(githubRepo)

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
            throw githubAPIError(
              operation,
              `Unhandled response body ${JSON.stringify(responseBody)}`,
            )
        }
      },
    )
  }
