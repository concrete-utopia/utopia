import { HEADERS, MODE } from '../../../../common/server'
import type { EditorAction, EditorDispatch } from '../../../../components/editor/action-types'
import { updateGithubData } from '../../../../components/editor/actions/action-creators'
import type {
  GithubOperation,
  GithubRepo,
  GithubUser,
} from '../../../../components/editor/store/editor-state'
import { GithubEndpoints } from '../endpoints'
import type { GithubBranch, GithubFailure, GithubOperationSource } from '../helpers'
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
  async (
    dispatch: EditorDispatch,
    userDetails: GithubUser | null,
    githubRepo: GithubRepo,
    initiator: GithubOperationSource,
  ): Promise<Array<EditorAction>> => {
    return runGithubOperation(
      { name: 'listBranches' },
      userDetails,
      dispatch,
      initiator,
      async (operation: GithubOperation) => {
        const url = GithubEndpoints.getBranches(githubRepo)

        const response = await operationContext.fetch(url, {
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
            throw githubAPIError(
              operation,
              `Unhandled response body ${JSON.stringify(responseBody)}`,
            )
        }
      },
    )
  }
