import { HEADERS, MODE } from '../../../../common/server'
import { notice } from '../../../../components/common/notice'
import type { EditorDispatch } from '../../../../components/editor/action-types'
import {
  showToast,
  updateBranchContents,
  updateGithubSettings,
} from '../../../../components/editor/actions/action-creators'
import type {
  FileChecksums,
  GithubOperation,
  GithubRepo,
  PersistentModel,
} from '../../../../components/editor/store/editor-state'
import { projectGithubSettings } from '../../../../components/editor/store/editor-state'
import type { GithubFailure, GithubOperationSource } from '../helpers'
import {
  dispatchPromiseActions,
  getExistingAssets,
  githubAPIError,
  githubAPIErrorFromResponse,
  runGithubOperation,
} from '../helpers'
import { getBranchesForGithubRepository } from './list-branches'
import type { GithubOperationContext } from './github-operation-context'
import { GithubEndpoints } from '../endpoints'
import { getBranchProjectContents } from '../../../../components/editor/server'

export interface SaveProjectToGithubOptions {
  branchName: string | null
  commitMessage: string | null
  branchOriginChecksums: FileChecksums
}

export interface SaveToGithubSuccess {
  type: 'SUCCESS'
  branchName: string
  url: string
  newCommit: string
}

export type SaveToGithubResponse = SaveToGithubSuccess | GithubFailure

export const saveProjectToGithub =
  (operationContext: GithubOperationContext) =>
  async (
    projectID: string,
    targetRepository: GithubRepo,
    persistentModel: PersistentModel,
    dispatch: EditorDispatch,
    options: SaveProjectToGithubOptions,
    initiator: GithubOperationSource,
  ): Promise<void> => {
    await runGithubOperation(
      { name: 'commitAndPush' },
      dispatch,
      initiator,
      async (operation: GithubOperation) => {
        const { branchName, commitMessage } = options

        // If this is a straight push, load the repo before saving
        // in order to retrieve the head origin commit hash
        // and avoid a fast forward error.
        let originCommit = persistentModel.githubSettings.originCommit
        if (originCommit == null && targetRepository != null && branchName != null) {
          const getBranchResponse = await getBranchProjectContents(operationContext)({
            projectId: projectID,
            repo: targetRepository.repository,
            owner: targetRepository.owner,
            existingAssets: getExistingAssets(persistentModel.projectContents),
            branch: branchName,
            previousCommitSha: null,
            specificCommitSha: null,
          })
          if (getBranchResponse.type === 'SUCCESS' && getBranchResponse.branch != null) {
            originCommit = getBranchResponse.branch.originCommit
          }
        }

        const patchedModel: PersistentModel = {
          ...persistentModel,
          githubSettings: {
            ...persistentModel.githubSettings,
            originCommit: originCommit,
            targetRepository: targetRepository,
          },
        }

        const url = GithubEndpoints.save(projectID)

        let includeQueryParams: boolean = false
        let paramsRecord: Record<string, string> = {}
        if (branchName != null) {
          includeQueryParams = true
          paramsRecord.branch_name = branchName
        }
        if (commitMessage != null) {
          includeQueryParams = true
          paramsRecord.commit_message = commitMessage
        }
        const searchParams = new URLSearchParams(paramsRecord)
        const urlToUse = includeQueryParams ? `${url}?${searchParams}` : url

        const postBody = JSON.stringify(patchedModel)
        const response = await operationContext.fetch(urlToUse, {
          method: 'POST',
          credentials: 'include',
          headers: HEADERS,
          mode: MODE,
          body: postBody,
        })
        if (!response.ok) {
          throw await githubAPIErrorFromResponse(operation, response)
        }

        const responseBody: SaveToGithubResponse = await response.json()
        switch (responseBody.type) {
          case 'FAILURE':
            throw githubAPIError(operation, responseBody.failureReason)
          case 'SUCCESS':
            dispatch(
              [
                updateGithubSettings(
                  projectGithubSettings(
                    targetRepository,
                    responseBody.newCommit,
                    responseBody.branchName,
                    responseBody.newCommit,
                    true,
                  ),
                ),
                updateBranchContents(persistentModel.projectContents),
                showToast(notice(`Saved to branch ${responseBody.branchName}.`, 'SUCCESS')),
              ],
              'everyone',
            )

            // refresh the branches after the content was saved
            await dispatchPromiseActions(
              dispatch,
              getBranchesForGithubRepository(operationContext)(
                dispatch,
                targetRepository,
                initiator,
              ),
            )
            break
          default:
            const _exhaustiveCheck: never = responseBody
            throw githubAPIError(
              operation,
              `Unhandled response body ${JSON.stringify(responseBody)}`,
            )
        }
        return []
      },
    )
  }
