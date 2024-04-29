import type { EditorAction } from '../../../../components/editor/action-types'
import { updateBranchContents } from '../../../../components/editor/actions/action-creators'
import type { GithubRepo, GithubUser } from '../../../../components/editor/store/editor-state'
import type { GetBranchContentResponse } from '../helpers'
import { getBranchContentFromServer } from '../helpers'
import type { GithubOperationContext } from './github-operation-context'

export const getBranchChecksums =
  (operationContext: GithubOperationContext) =>
  async (
    userDetails: GithubUser | null,
    githubRepo: GithubRepo,
    branchName: string,
    commitSha: string,
  ): Promise<Array<EditorAction>> => {
    const specificCommitRequest = getBranchContentFromServer(
      githubRepo,
      branchName,
      commitSha,
      null,
      operationContext,
    )

    const specificCommitResponse = await specificCommitRequest

    if (specificCommitResponse.ok) {
      const specificCommitContent: GetBranchContentResponse = await specificCommitResponse.json()
      if (specificCommitContent.type === 'SUCCESS') {
        if (specificCommitContent.branch != null) {
          return [updateBranchContents(specificCommitContent.branch.content)]
        }
      }
    }
    return []
  }
