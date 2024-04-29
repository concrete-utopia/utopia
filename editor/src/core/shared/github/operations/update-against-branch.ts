import type { UtopiaTsWorkers } from '../../../../core/workers/common/worker-types'
import type { ProjectContentTreeRoot } from '../../../../components/assets'
import { notice } from '../../../../components/common/notice'
import type { EditorDispatch } from '../../../../components/editor/action-types'
import {
  showToast,
  updateAgainstGithub,
  updateBranchContents,
  updateGithubData,
} from '../../../../components/editor/actions/action-creators'
import type {
  GithubOperation,
  GithubRepo,
  GithubUser,
} from '../../../../components/editor/store/editor-state'
import { updateProjectContentsWithParseResults } from '../../parser-projectcontents-utils'
import type { GetBranchContentResponse, GithubOperationSource } from '../helpers'
import {
  getBranchContentFromServer,
  githubAPIError,
  githubAPIErrorFromResponse,
  runGithubOperation,
} from '../helpers'
import { saveAssetsToProject } from './load-branch'
import type { GithubOperationContext } from './github-operation-context'

export const updateProjectAgainstGithub =
  (operationContext: GithubOperationContext) =>
  async (
    workers: UtopiaTsWorkers,
    userDetails: GithubUser | null,
    dispatch: EditorDispatch,
    githubRepo: GithubRepo,
    branchName: string,
    commitSha: string,
    projectID: string,
    currentProjectContents: ProjectContentTreeRoot,
    initiator: GithubOperationSource,
  ): Promise<void> => {
    await runGithubOperation(
      { name: 'updateAgainstBranch' },
      userDetails,
      dispatch,
      initiator,
      async (operation: GithubOperation) => {
        const branchLatestRequest = getBranchContentFromServer(
          githubRepo,
          branchName,
          null,
          null,
          operationContext,
        )
        const specificCommitRequest = getBranchContentFromServer(
          githubRepo,
          branchName,
          commitSha,
          null,
          operationContext,
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
        // Push any code through the parser so that the representations we end up with are in a state of `BOTH_MATCH`.
        // So that it will override any existing files that might already exist in the project when sending them to VS Code.
        // TODO: Ideally this should only do a partial parse based on what has changed.
        const parsedLatestContent = await updateProjectContentsWithParseResults(
          workers,
          branchLatestContent.branch.content,
        )

        // Save assets to the server from Github.
        await saveAssetsToProject(operationContext)(
          userDetails,
          githubRepo,
          projectID,
          branchLatestContent.branch,
          dispatch,
          currentProjectContents,
          initiator,
        )

        dispatch(
          [
            updateBranchContents(parsedLatestContent),
            updateAgainstGithub(
              parsedLatestContent,
              specificCommitContent.branch.content,
              branchLatestContent.branch.originCommit,
            ),
            updateGithubData({
              upstreamChanges: null,
              lastRefreshedCommit: branchLatestContent.branch.originCommit,
            }),
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
