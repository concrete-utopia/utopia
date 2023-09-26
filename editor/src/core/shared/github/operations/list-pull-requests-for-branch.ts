import urljoin from 'url-join'
import { UTOPIA_BACKEND } from '../../../../common/env-vars'
import { HEADERS, MODE } from '../../../../common/server'
import type { EditorAction, EditorDispatch } from '../../../../components/editor/action-types'
import { updateGithubData } from '../../../../components/editor/actions/action-creators'
import type {
  GithubOperation,
  GithubRepo,
  PullRequest,
} from '../../../../components/editor/store/editor-state'
import type { GithubFailure } from '../helpers'
import { githubAPIError, githubAPIErrorFromResponse, runGithubOperation } from '../helpers'
import type { GithubOperationContext } from './github-operation-context'

export interface GetBranchPullRequestSuccess {
  type: 'SUCCESS'
  pullRequests: Array<PullRequest>
}

export type GetBranchPullRequestResponse = GetBranchPullRequestSuccess | GithubFailure

const RE_PULL_REQUEST_URL_NUMBER = /.+\/pull\/([0-9]+).*/

export function getPullRequestNumberFromUrl(url: string): number {
  try {
    return parseInt(url.replace(RE_PULL_REQUEST_URL_NUMBER, '$1'))
  } catch (err) {
    return NaN
  }
}

export const updatePullRequestsForBranch =
  (operationContext: GithubOperationContext) =>
  async (
    dispatch: EditorDispatch,
    githubRepo: GithubRepo,
    branchName: string,
  ): Promise<Array<EditorAction>> => {
    return runGithubOperation(
      {
        name: 'listPullRequestsForBranch',
        githubRepo: githubRepo,
        branchName: branchName,
      },
      dispatch,
      async (operation: GithubOperation) => {
        const url = urljoin(
          UTOPIA_BACKEND,
          'github',
          'branches',
          githubRepo.owner,
          githubRepo.repository,
          'branch',
          branchName,
          'pullrequest',
        )

        const response = await operationContext.fetch(url, {
          method: 'GET',
          credentials: 'include',
          headers: HEADERS,
          mode: MODE,
        })

        if (!response.ok) {
          throw await githubAPIErrorFromResponse(operation, response)
        }

        const responseBody: GetBranchPullRequestResponse = await response.json()

        switch (responseBody.type) {
          case 'FAILURE':
            throw githubAPIError(operation, responseBody.failureReason)
          case 'SUCCESS':
            return [
              updateGithubData({
                currentBranchPullRequests: responseBody.pullRequests.map((pr) => ({
                  ...pr,
                  number: getPullRequestNumberFromUrl(pr.htmlURL),
                })),
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
