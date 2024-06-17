import type { EditorAction } from '../../../../components/editor/action-types'
import { updateBranchContents } from '../../../../components/editor/actions/action-creators'
import { getBranchProjectContents } from '../../../../components/editor/server'
import type { GithubRepo } from '../../../../components/editor/store/editor-state'
import type { GithubOperationContext } from './github-operation-context'

export const getBranchChecksums =
  (operationContext: GithubOperationContext) =>
  async (
    projectId: string,
    githubRepo: GithubRepo,
    branchName: string,
    commitSha: string,
  ): Promise<Array<EditorAction>> => {
    const specificCommitResponse = await getBranchProjectContents(operationContext)({
      projectId: projectId,
      repo: githubRepo.repository,
      owner: githubRepo.owner,
      branch: branchName,
      specificCommitSha: commitSha,
      previousCommitSha: null,
      existingAssets: [],
    })

    if (specificCommitResponse.type === 'SUCCESS' && specificCommitResponse.branch != null) {
      return [updateBranchContents(specificCommitResponse.branch.content)]
    }
    return []
  }
