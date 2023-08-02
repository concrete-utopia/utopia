import urljoin from 'url-join'
import { UTOPIA_BACKEND } from '../../../../common/env-vars'
import { HEADERS, MODE } from '../../../../common/server'
import { getProjectContentsChecksums } from '../../../../components/assets'
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
import type { GetBranchContentResponse, GithubFailure } from '../helpers'
import {
  dispatchPromiseActions,
  getBranchContentFromServer,
  githubAPIError,
  githubAPIErrorFromResponse,
  runGithubOperation,
} from '../helpers'
import { getBranchesForGithubRepository } from './list-branches'

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

export async function saveProjectToGithub(
  projectID: string,
  targetRepository: GithubRepo,
  persistentModel: PersistentModel,
  dispatch: EditorDispatch,
  options: SaveProjectToGithubOptions,
): Promise<void> {
  await runGithubOperation(
    { name: 'commitAndPush' },
    dispatch,
    async (operation: GithubOperation) => {
      const { branchName, commitMessage } = options

      // If this is a straight push, load the repo before saving
      // in order to retrieve the head origin commit hash
      // and avoid a fast forward error.
      let originCommit = persistentModel.githubSettings.originCommit
      if (originCommit == null && targetRepository != null && branchName != null) {
        const getBranchResponse = await getBranchContentFromServer(
          targetRepository,
          branchName,
          null,
          null,
        )
        if (getBranchResponse.ok) {
          const content: GetBranchContentResponse = await getBranchResponse.json()
          if (content.type === 'SUCCESS' && content.branch != null) {
            originCommit = content.branch.originCommit
          }
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

      const url = urljoin(UTOPIA_BACKEND, 'github', 'save', projectID)

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
      const response = await fetch(urlToUse, {
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
          void dispatchPromiseActions(
            dispatch,
            getBranchesForGithubRepository(dispatch, targetRepository),
          )
          break
        default:
          const _exhaustiveCheck: never = responseBody
          throw githubAPIError(operation, `Unhandled response body ${JSON.stringify(responseBody)}`)
      }
      return []
    },
  )
}
