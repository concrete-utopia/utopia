import { getProjectContentsChecksums } from '../../../../components/assets'
import { notice } from '../../../../components/common/notice'
import { EditorDispatch } from '../../../../components/editor/action-types'
import {
  showToast,
  updateAgainstGithub,
  updateBranchContents,
  updateGithubChecksums,
  updateGithubData,
} from '../../../../components/editor/actions/action-creators'
import { GithubRepo } from '../../../../components/editor/store/editor-state'
import {
  getBranchContentFromServer,
  GetBranchContentResponse,
  runGithubOperation,
} from '../helpers'

export async function updateProjectAgainstGithub(
  dispatch: EditorDispatch,
  githubRepo: GithubRepo,
  branchName: string,
  commitSha: string,
): Promise<void> {
  await runGithubOperation({ name: 'updateAgainstBranch' }, dispatch, async () => {
    const branchLatestRequest = getBranchContentFromServer(githubRepo, branchName, null, null)
    const specificCommitRequest = getBranchContentFromServer(
      githubRepo,
      branchName,
      commitSha,
      null,
    )

    const branchLatestResponse = await branchLatestRequest
    const specificCommitResponse = await specificCommitRequest

    if (branchLatestResponse.ok && specificCommitResponse.ok) {
      const branchLatestContent: GetBranchContentResponse = await branchLatestResponse.json()
      const specificCommitContent: GetBranchContentResponse = await specificCommitResponse.json()

      function failWithReason(failureReason: string): void {
        dispatch(
          [showToast(notice(`Error when updating against Github: ${failureReason}`, 'ERROR'))],
          'everyone',
        )
      }

      if (branchLatestContent.type === 'SUCCESS') {
        if (branchLatestContent.branch == null) {
          failWithReason(`Could not find latest code for branch ${branchName}`)
        } else {
          if (specificCommitContent.type === 'SUCCESS') {
            if (specificCommitContent.branch == null) {
              failWithReason(`Could not find commit ${commitSha} for branch ${branchName}`)
            } else {
              dispatch(
                [
                  updateGithubChecksums(
                    getProjectContentsChecksums(branchLatestContent.branch.content, {}),
                  ),
                  updateBranchContents(branchLatestContent.branch.content),
                  updateAgainstGithub(
                    branchLatestContent.branch.content,
                    specificCommitContent.branch.content,
                    branchLatestContent.branch.originCommit,
                  ),
                  updateGithubData({ upstreamChanges: null }),
                  showToast(
                    notice(
                      `Github: Updated the project against the branch ${branchName}.`,
                      'SUCCESS',
                    ),
                  ),
                ],
                'everyone',
              )
            }
          } else {
            failWithReason(specificCommitContent.failureReason)
          }
        }
      } else {
        failWithReason(branchLatestContent.failureReason)
      }
    } else {
      const failureStatus = branchLatestResponse.ok
        ? specificCommitResponse.status
        : branchLatestResponse.status
      dispatch(
        [
          showToast(
            notice(`Github: Unexpected status returned from endpoint: ${failureStatus}`, 'ERROR'),
          ),
        ],
        'everyone',
      )
    }
    return []
  })
}
