import type { UtopiaTsWorkers } from '../../../../core/workers/common/worker-types'
import type { ProjectContentTreeRoot } from '../../../../components/assets'
import { getProjectFileByFilePath } from '../../../../components/assets'
import {
  packageJsonFileFromProjectContents,
  walkContentsTreeAsync,
} from '../../../../components/assets'
import { notice } from '../../../../components/common/notice'
import type { EditorDispatch } from '../../../../components/editor/action-types'
import {
  extractPropertyControlsFromDescriptorFiles,
  showToast,
  truncateHistory,
  updateBranchContents,
  updateProjectContents,
} from '../../../../components/editor/actions/action-creators'
import type {
  GithubData,
  GithubOperation,
  GithubRepo,
  GithubUser,
} from '../../../../components/editor/store/editor-state'
import type { BuiltInDependencies } from '../../../es-modules/package-manager/built-in-dependencies-list'
import { refreshDependencies } from '../../dependencies'
import type { RequestedNpmDependency } from '../../npm-dependency-types'
import { forceNotNull } from '../../optional-utils'
import { isTextFile } from '../../project-file-types'
import type { BranchContent, GetBranchContentResponse, GithubOperationSource } from '../helpers'
import {
  connectRepo,
  getBranchContentFromServer,
  githubAPIError,
  githubAPIErrorFromResponse,
  runGithubOperation,
  saveGithubAsset,
} from '../helpers'
import { updateProjectContentsWithParseResults } from '../../parser-projectcontents-utils'
import type { GithubOperationContext } from './github-operation-context'
import { createStoryboardFileIfNecessary } from '../../../../components/editor/actions/actions'
import { getAllComponentDescriptorFilePaths } from '../../../property-controls/property-controls-local'

export const saveAssetsToProject =
  (operationContext: GithubOperationContext) =>
  async (
    githubRepo: GithubRepo,
    projectID: string,
    branchContent: BranchContent,
    dispatch: EditorDispatch,
    currentProjectContents: ProjectContentTreeRoot,
    initiator: GithubOperationSource,
  ): Promise<void> => {
    await walkContentsTreeAsync(branchContent.content, async (fullPath, projectFile) => {
      const alreadyExistingFile = getProjectFileByFilePath(currentProjectContents, fullPath)
      // Only for these two types of project file (easing the typechecking of the subsequent check).
      if (projectFile.type === 'IMAGE_FILE' || projectFile.type === 'ASSET_FILE') {
        // Pre-requisites (only one needs to apply):
        // 1. There's no already existing file with this filename.
        // 2. The file type has changed.
        // 3. The file has the same type, but the SHA is different.
        if (
          alreadyExistingFile == null ||
          alreadyExistingFile.type !== projectFile.type ||
          (alreadyExistingFile.type === projectFile.type &&
            alreadyExistingFile.gitBlobSha !== projectFile.gitBlobSha)
        ) {
          switch (projectFile.type) {
            case 'IMAGE_FILE':
              await saveGithubAsset(
                githubRepo,
                forceNotNull('Commit sha should exist.', projectFile.gitBlobSha),
                projectID,
                fullPath,
                dispatch,
                operationContext,
                initiator,
              )
              break
            case 'ASSET_FILE':
              await saveGithubAsset(
                githubRepo,
                forceNotNull('Commit sha should exist.', projectFile.gitBlobSha),
                projectID,
                fullPath,
                dispatch,
                operationContext,
                initiator,
              )
              break
            default:
            // Do nothing.
          }
        }
      }
    })
  }

export const updateProjectWithBranchContent =
  (operationContext: GithubOperationContext) =>
  async (
    workers: UtopiaTsWorkers,
    dispatch: EditorDispatch,
    projectID: string,
    githubRepo: GithubRepo,
    branchName: string | null,
    resetBranches: boolean,
    currentDeps: Array<RequestedNpmDependency>,
    builtInDependencies: BuiltInDependencies,
    currentProjectContents: ProjectContentTreeRoot,
    initiator: GithubOperationSource,
  ): Promise<void> => {
    await runGithubOperation(
      {
        name: 'loadBranch',
        branchName: branchName,
        githubRepo: githubRepo,
      },
      dispatch,
      initiator,
      async (operation: GithubOperation) => {
        const response = await getBranchContentFromServer(
          githubRepo,
          branchName,
          null,
          null,
          operationContext,
        )
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

            // Push any code through the parser so that the representations we end up with are in a state of `BOTH_MATCH`.
            // So that it will override any existing files that might already exist in the project when sending them to VS Code.
            const parsedProjectContents = createStoryboardFileIfNecessary(
              await updateProjectContentsWithParseResults(workers, responseBody.branch.content),
              'create-placeholder',
            )

            // Save assets to the server from Github.
            await saveAssetsToProject(operationContext)(
              githubRepo,
              projectID,
              responseBody.branch,
              dispatch,
              currentProjectContents,
              initiator,
            )

            // Update the editor with everything so that if anything else fails past this point
            // there's no loss of data from the user's perspective.
            dispatch(
              [
                ...connectRepo(
                  resetBranches,
                  githubRepo,
                  responseBody.branch.originCommit,
                  branchName,
                  true,
                ),
                updateProjectContents(parsedProjectContents),
                updateBranchContents(parsedProjectContents),
                truncateHistory(),
              ],
              'everyone',
            )

            const componentDescriptorFiles =
              getAllComponentDescriptorFilePaths(parsedProjectContents)

            // If there's a package.json file, then attempt to load the dependencies for it.
            let dependenciesPromise: Promise<void> = Promise.resolve()
            const packageJson = packageJsonFileFromProjectContents(parsedProjectContents)
            if (packageJson != null && isTextFile(packageJson)) {
              dependenciesPromise = refreshDependencies(
                dispatch,
                packageJson.fileContents.code,
                currentDeps,
                builtInDependencies,
                {},
              ).then(() => {})
            }

            // When the dependencies update has gone through, then indicate that the project was imported.
            await dependenciesPromise
              .catch(() => {
                dispatch(
                  [
                    showToast(
                      notice(
                        `Github: There was an error when attempting to update the dependencies.`,
                        'ERROR',
                      ),
                    ),
                  ],
                  'everyone',
                )
              })
              .finally(() => {
                dispatch(
                  [
                    extractPropertyControlsFromDescriptorFiles(componentDescriptorFiles),
                    showToast(
                      notice(
                        `Github: Updated the project with the content from ${branchName}`,
                        'SUCCESS',
                      ),
                    ),
                  ],
                  'everyone',
                )
              })

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
