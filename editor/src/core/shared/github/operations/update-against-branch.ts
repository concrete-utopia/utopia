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
import type { GithubOperation, GithubRepo } from '../../../../components/editor/store/editor-state'
import { updateProjectContentsWithParseResults } from '../../parser-projectcontents-utils'
import type { GithubOperationSource } from '../helpers'
import { getExistingAssets, githubAPIError, runGithubOperation } from '../helpers'
import { saveAssetsToProject } from './load-branch'
import type { GithubOperationContext } from './github-operation-context'
import { getBranchProjectContents } from '../../../../components/editor/server'

export const updateProjectAgainstGithub =
  (operationContext: GithubOperationContext) =>
  async (
    workers: UtopiaTsWorkers,
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
      dispatch,
      initiator,
      async (operation: GithubOperation) => {
        const existingAssets = getExistingAssets(currentProjectContents)

        const branchLatestResponse = await getBranchProjectContents(operationContext)({
          projectId: projectID,
          repo: githubRepo.repository,
          owner: githubRepo.owner,
          branch: branchName,
          specificCommitSha: null,
          previousCommitSha: null,
          existingAssets: existingAssets,
        })
        const specificCommitResponse = await getBranchProjectContents(operationContext)({
          projectId: projectID,
          repo: githubRepo.repository,
          owner: githubRepo.owner,
          branch: branchName,
          specificCommitSha: commitSha,
          previousCommitSha: null,
          existingAssets: existingAssets,
        })

        if (branchLatestResponse.type === 'FAILURE') {
          throw githubAPIError(operation, branchLatestResponse.failureReason)
        }
        if (branchLatestResponse.branch == null) {
          throw githubAPIError(operation, `Could not find latest code for branch ${branchName}`)
        }

        if (specificCommitResponse.type === 'FAILURE') {
          throw githubAPIError(operation, specificCommitResponse.failureReason)
        }
        if (specificCommitResponse.branch == null) {
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
          branchLatestResponse.branch.content,
        )

        // Save assets to the server from Github.
        await saveAssetsToProject(operationContext)(
          githubRepo,
          projectID,
          branchLatestResponse.branch,
          dispatch,
          currentProjectContents,
          initiator,
        )

        dispatch(
          [
            updateGithubData({
              upstreamChanges: null,
              lastRefreshedCommit: branchLatestResponse.branch.originCommit,
            }),
            updateBranchContents(parsedLatestContent),
            updateAgainstGithub(
              parsedLatestContent,
              specificCommitResponse.branch.content,
              branchLatestResponse.branch.originCommit,
            ),
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
