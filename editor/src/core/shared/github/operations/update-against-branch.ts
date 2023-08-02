import { notice } from '../../../../components/common/notice'
import type { EditorDispatch } from '../../../../components/editor/action-types'
import {
  showToast,
  updateAgainstGithub,
  updateBranchContents,
  updateGithubData,
} from '../../../../components/editor/actions/action-creators'
import type { GithubOperation, GithubRepo } from '../../../../components/editor/store/editor-state'
import type { GetBranchContentResponse } from '../helpers'
import {
  getBranchContentFromServer,
  githubAPIError,
  githubAPIErrorFromResponse,
  runGithubOperation,
} from '../helpers'

export async function updateProjectAgainstGithub(
  dispatch: EditorDispatch,
  githubRepo: GithubRepo,
  branchName: string,
  commitSha: string,
): Promise<void> {
  await runGithubOperation(
    { name: 'updateAgainstBranch' },
    dispatch,
    async (operation: GithubOperation) => {
      const branchLatestRequest = getBranchContentFromServer(githubRepo, branchName, null, null)
      const specificCommitRequest = getBranchContentFromServer(
        githubRepo,
        branchName,
        commitSha,
        null,
      )

      const branchLatestResponse = await branchLatestRequest
      const specificCommitResponse = await specificCommitRequest

      if (!branchLatestResponse.ok) {
        throw await githubAPIErrorFromResponse(operation, branchLatestResponse)
      }
      if (!specificCommitResponse.ok) {
        throw await githubAPIErrorFromResponse(operation, specificCommitResponse)
      }

      const branchLatestContent: GetBranchContentResponse = await branchLatestResponse.json()

      if (branchLatestContent.type === 'FAILURE') {
        throw githubAPIError(operation, branchLatestContent.failureReason)
      }
      if (branchLatestContent.branch == null) {
        throw githubAPIError(operation, `Could not find latest code for branch ${branchName}`)
      }

      const specificCommitContent: GetBranchContentResponse = await specificCommitResponse.json()
      if (specificCommitContent.type === 'FAILURE') {
        throw githubAPIError(operation, specificCommitContent.failureReason)
      }
      if (specificCommitContent.branch == null) {
        throw githubAPIError(
          operation,
          `Could not find commit ${commitSha} for branch ${branchName}`,
        )
      }

      dispatch(
        [
          updateBranchContents(branchLatestContent.branch.content),
          updateAgainstGithub(
            branchLatestContent.branch.content,
            specificCommitContent.branch.content,
            branchLatestContent.branch.originCommit,
          ),
          updateGithubData({ upstreamChanges: null }),
          showToast(
            notice(`Github: Updated the project against the branch ${branchName}.`, 'SUCCESS'),
          ),
        ],
        'everyone',
      )
      return []
    },
  )
}
