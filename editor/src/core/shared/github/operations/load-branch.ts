import { getProjectContentsChecksums, walkContentsTreeAsync } from '../../../../components/assets'
import { notice } from '../../../../components/common/notice'
import { EditorDispatch } from '../../../../components/editor/action-types'
import {
  showToast,
  updateBranchContents,
  updateProjectContents,
} from '../../../../components/editor/actions/action-creators'
import {
  GithubData,
  GithubOperation,
  GithubRepo,
  packageJsonFileFromProjectContents,
} from '../../../../components/editor/store/editor-state'
import { BuiltInDependencies } from '../../../es-modules/package-manager/built-in-dependencies-list'
import { refreshDependencies } from '../../dependencies'
import { RequestedNpmDependency } from '../../npm-dependency-types'
import { forceNotNull } from '../../optional-utils'
import { isTextFile } from '../../project-file-types'
import {
  BranchContent,
  connectRepo,
  getBranchContentFromServer,
  GetBranchContentResponse,
  githubAPIError,
  githubAPIErrorFromResponse,
  runGithubOperation,
  saveGithubAsset,
} from '../helpers'

async function saveAssetsToProject(
  githubRepo: GithubRepo,
  projectID: string,
  branchContent: BranchContent,
  dispatch: EditorDispatch,
): Promise<void> {
  await walkContentsTreeAsync(branchContent.content, async (fullPath, projectFile) => {
    switch (projectFile.type) {
      case 'IMAGE_FILE':
        await saveGithubAsset(
          githubRepo,
          forceNotNull('Commit sha should exist.', projectFile.gitBlobSha),
          projectID,
          fullPath,
          dispatch,
        )
        break
      case 'ASSET_FILE':
        await saveGithubAsset(
          githubRepo,
          forceNotNull('Commit sha should exist.', projectFile.gitBlobSha),
          projectID,
          fullPath,
          dispatch,
        )
        break
      default:
      // Do nothing.
    }
  })
}

export async function updateProjectWithBranchContent(
  dispatch: EditorDispatch,
  projectID: string,
  githubRepo: GithubRepo,
  branchName: string,
  resetBranches: boolean,
  currentDeps: Array<RequestedNpmDependency>,
  builtInDependencies: BuiltInDependencies,
): Promise<void> {
  await runGithubOperation(
    {
      name: 'loadBranch',
      branchName: branchName,
      githubRepo: githubRepo,
    },
    dispatch,
    async (operation: GithubOperation) => {
      const response = await getBranchContentFromServer(githubRepo, branchName, null, null)
      if (!response.ok) {
        throw githubAPIErrorFromResponse(operation, response)
      }

      const responseBody: GetBranchContentResponse = await response.json()
      switch (responseBody.type) {
        case 'FAILURE':
          throw githubAPIError(operation, responseBody.failureReason)
        case 'SUCCESS':
          if (responseBody.branch == null) {
            throw githubAPIError(operation, `Could not find branch ${branchName}`)
          }
          const newGithubData: Partial<GithubData> = {
            upstreamChanges: null,
          }
          if (resetBranches) {
            newGithubData.branches = null
          }

          // Save assets to the server from Github.
          await saveAssetsToProject(githubRepo, projectID, responseBody.branch, dispatch)

          const packageJson = packageJsonFileFromProjectContents(responseBody.branch.content)
          if (packageJson != null && isTextFile(packageJson)) {
            await refreshDependencies(
              dispatch,
              packageJson.fileContents.code,
              currentDeps,
              builtInDependencies,
              {},
            )
          }

          dispatch(
            [
              ...connectRepo(
                resetBranches,
                githubRepo,
                responseBody.branch.originCommit,
                branchName,
                true,
              ),
              updateProjectContents(responseBody.branch.content),
              updateBranchContents(responseBody.branch.content),
              showToast(
                notice(
                  `Github: Updated the project with the content from ${branchName}`,
                  'SUCCESS',
                ),
              ),
            ],
            'everyone',
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
