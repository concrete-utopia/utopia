import urljoin from 'url-join'
import { UTOPIA_BACKEND } from '../../../../common/env-vars'
import { HEADERS, MODE } from '../../../../common/server'
import { notice } from '../../../../components/common/notice'
import { EditorAction, EditorDispatch } from '../../../../components/editor/action-types'
import { showToast, updateGithubData } from '../../../../components/editor/actions/action-creators'
import { GithubRepo, PullRequest } from '../../../../components/editor/store/editor-state'
import { GithubFailure, runGithubOperation } from '../helpers'

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

export async function updatePullRequestsForBranch(
  dispatch: EditorDispatch,
  githubRepo: GithubRepo,
  branchName: string,
): Promise<Array<EditorAction>> {
  return runGithubOperation(
    {
      name: 'listPullRequestsForBranch',
      githubRepo: githubRepo,
      branchName: branchName,
    },
    dispatch,
    async () => {
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

      const response = await fetch(url, {
        method: 'GET',
        credentials: 'include',
        headers: HEADERS,
        mode: MODE,
      })

      if (response.ok) {
        const responseBody: GetBranchPullRequestResponse = await response.json()

        switch (responseBody.type) {
          case 'FAILURE':
            return [
              showToast(
                notice(
                  `Error when listing pull requests for branch: ${responseBody.failureReason}`,
                  'ERROR',
                ),
              ),
            ]
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
            throw new Error(`Unhandled response body ${JSON.stringify(responseBody)}`)
        }
      } else {
        return [
          showToast(
            notice(`Github: Unexpected status returned from endpoint: ${response.status}`, 'ERROR'),
          ),
        ]
      }
    },
  )
}
