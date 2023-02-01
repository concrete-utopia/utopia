import { getProjectContentsChecksums } from '../../../../components/assets'
import { notice } from '../../../../components/common/notice'
import { EditorDispatch } from '../../../../components/editor/action-types'
import {
  showToast,
  updateBranchContents,
  updateGithubChecksums,
  updateProjectContents,
} from '../../../../components/editor/actions/action-creators'
import {
  GithubData,
  GithubRepo,
  packageJsonFileFromProjectContents,
} from '../../../../components/editor/store/editor-state'
import { BuiltInDependencies } from '../../../es-modules/package-manager/built-in-dependencies-list'
import { refreshDependencies } from '../../dependencies'
import { RequestedNpmDependency } from '../../npm-dependency-types'
import { isTextFile } from '../../project-file-types'
import {
  connectRepo,
  getBranchContentFromServer,
  GetBranchContentResponse,
  runGithubOperation,
} from '../helpers'

export async function updateProjectWithBranchContent(
  dispatch: EditorDispatch,
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
    async () => {
      const response = await getBranchContentFromServer(githubRepo, branchName, null, null)
      if (response.ok) {
        const responseBody: GetBranchContentResponse = await response.json()
        switch (responseBody.type) {
          case 'FAILURE':
            dispatch(
              [showToast(notice(`Error saving to Github: ${responseBody.failureReason}`, 'ERROR'))],
              'everyone',
            )
            break
          case 'SUCCESS':
            if (responseBody.branch == null) {
              dispatch(
                [showToast(notice(`Github: Could not find branch ${branchName}.`, 'ERROR'))],
                'everyone',
              )
            } else {
              const newGithubData: Partial<GithubData> = {
                upstreamChanges: null,
              }
              if (resetBranches) {
                newGithubData.branches = null
              }

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
                  updateGithubChecksums(
                    getProjectContentsChecksums(responseBody.branch.content, {}),
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
            }
            break
          default:
            const _exhaustiveCheck: never = responseBody
            throw new Error(`Github: Unhandled response body ${JSON.stringify(responseBody)}`)
        }
      } else {
        dispatch(
          [
            showToast(
              notice(
                `Github: Unexpected status returned from endpoint: ${response.status}`,
                'ERROR',
              ),
            ),
          ],
          'everyone',
        )
      }
      return []
    },
  )
}
