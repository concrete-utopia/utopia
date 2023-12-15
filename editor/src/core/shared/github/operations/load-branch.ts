import type { UtopiaTsWorkers } from '../../../../core/workers/common/worker-types'
import type { ProjectContentTreeRoot } from '../../../../components/assets'
import { getProjectFileByFilePath } from '../../../../components/assets'
import {
  packageJsonFileFromProjectContents,
  walkContentsTreeAsync,
} from '../../../../components/assets'
import { notice } from '../../../../components/common/notice'
import type {
  AddToast,
  EditorDispatch,
  UpdateGithubOperations,
} from '../../../../components/editor/action-types'
import {
  showToast,
  truncateHistory,
  updateBranchContents,
  updateProjectContents,
} from '../../../../components/editor/actions/action-creators'
import type {
  GithubData,
  GithubOperation,
  GithubRepo,
} from '../../../../components/editor/store/editor-state'
import type { BuiltInDependencies } from '../../../es-modules/package-manager/built-in-dependencies-list'
import { refreshDependencies } from '../../dependencies'
import type { RequestedNpmDependency } from '../../npm-dependency-types'
import { forceNotNull } from '../../optional-utils'
import { isTextFile } from '../../project-file-types'
import type { BranchContent, GetBranchContentResponse } from '../helpers'
import {
  connectRepo,
  getBranchContentFromServer,
  githubAPIError,
  githubAPIErrorFromResponse,
  runGithubOperation,
  runGithubOperation2,
  saveGithubAsset,
} from '../helpers'
import { updateProjectContentsWithParseResults } from '../../parser-projectcontents-utils'
import type { GithubOperationContext } from './github-operation-context'
import { createStoryboardFileIfNecessary } from '../../../../components/editor/actions/actions'

export const saveAssetsToProject =
  (operationContext: GithubOperationContext) =>
  async (
    githubRepo: GithubRepo,
    projectID: string,
    branchContent: BranchContent,
    dispatch: EditorDispatch,
    currentProjectContents: ProjectContentTreeRoot,
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
              )
              break
            case 'ASSET_FILE':
              // it seems like this is just plain duplicated code from case 'IMAGE_FILE', I should merge the two cases
              await saveGithubAsset(
                githubRepo,
                forceNotNull('Commit sha should exist.', projectFile.gitBlobSha),
                projectID,
                fullPath,
                dispatch,
                operationContext,
              )
              break
            default:
            // Do nothing.
          }
        }
      }
    })
  }

// 1. create and refactor loadBranchAndSaveAssets
// 2. remove the dispatch dependency from loadBranchAndSaveAssets, create callbacks instead for setting GithubOperations and Error toasts
// 3. make loadBranchAndSaveAssets return Promise<parsedProjectContents>
// 4. refactor refreshDependencies into a similarly dispatchless function
// 5. call refreshDependencies from Editor.tsx, get the project contents, and use those to fire storedState.persistence.createNew
// 6. once the project is loaded and saved, kick off refreshDependencies
// 7. revisit what UI we show until the project is loaded

export const updateProjectWithBranchContent =
  (operationContext: GithubOperationContext) =>
  async (
    workers: UtopiaTsWorkers,
    dispatch: EditorDispatch,
    projectID: string,
    githubRepo: GithubRepo,
    branchName: string,
    resetBranches: boolean,
    currentDeps: Array<RequestedNpmDependency>,
    builtInDependencies: BuiltInDependencies,
    currentProjectContents: ProjectContentTreeRoot,
  ): Promise<void> => {
    const loadBranchResult = await loadBranchFromGithub(
      branchName,
      githubRepo,
      (action) => dispatch([action]),
      operationContext,
    )

    if (loadBranchResult == null) {
      return // throw error? nah
    }

    const parseAndUploadAssetsResult = await parseDownloadedProject(
      branchName,
      githubRepo,
      (action) => dispatch([action]),
      loadBranchResult,
      resetBranches,
      workers,
    )

    if (parseAndUploadAssetsResult == null) {
      return
    }

    // Save assets to the server from Github.
    await saveAssetsToProject(operationContext)(
      githubRepo,
      projectID,
      parseAndUploadAssetsResult.branch,
      dispatch,
      currentProjectContents,
    )

    // Update the editor with everything so that if anything else fails past this point
    // there's no loss of data from the user's perspective.
    dispatch(
      [
        ...connectRepo(
          resetBranches,
          githubRepo,
          parseAndUploadAssetsResult.branch.originCommit,
          branchName,
          true,
        ),
        updateProjectContents(parseAndUploadAssetsResult.parsedProjectContents),
        updateBranchContents(parseAndUploadAssetsResult.parsedProjectContents),
        truncateHistory(),
      ],
      'everyone',
    )

    await runGithubOperation2(
      {
        name: 'loadBranch',
        branchName: branchName,
        githubRepo: githubRepo,
      },
      (action) => dispatch([action]),
      async (operation: GithubOperation) => {
        // If there's a package.json file, then attempt to load the dependencies for it.
        let dependenciesPromise: Promise<void> = Promise.resolve()
        const packageJson = packageJsonFileFromProjectContents(
          parseAndUploadAssetsResult.parsedProjectContents,
        )
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
          // hmmm I think this should be a .then here, because we don't want to show Success if the promise rejected
          .finally(() => {
            dispatch(
              [
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
      },
    )
  }

async function loadBranchFromGithub(
  branchName: string,
  githubRepo: GithubRepo,
  onUpdate: (update: UpdateGithubOperations | AddToast) => void,
  operationContext: GithubOperationContext,
) {
  return runGithubOperation2(
    {
      name: 'loadBranch',
      branchName: branchName,
      githubRepo: githubRepo,
    },
    onUpdate,
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

      return response
    },
  )
}

async function parseDownloadedProject(
  branchName: string,
  githubRepo: GithubRepo,
  onUpdate: (update: UpdateGithubOperations | AddToast) => void,
  loadBranchResult: Response,
  resetBranches: boolean,
  workers: UtopiaTsWorkers,
) {
  return runGithubOperation2(
    {
      name: 'loadBranch',
      branchName: branchName,
      githubRepo: githubRepo,
    },
    onUpdate,
    async (operation: GithubOperation) => {
      const responseBody: GetBranchContentResponse = await loadBranchResult.json()
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
          )

          return {
            parsedProjectContents: parsedProjectContents,
            branch: responseBody.branch,
          }

        default:
          const _exhaustiveCheck: never = responseBody
          throw githubAPIError(operation, `Unhandled response body ${JSON.stringify(responseBody)}`)
      }
    },
  )
}
