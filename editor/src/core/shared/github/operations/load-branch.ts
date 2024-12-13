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
  updateImportStatus,
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
import type {
  BranchContent,
  GetBranchContentResponse,
  GetBranchContentSuccess,
  GithubOperationSource,
} from '../helpers'
import {
  connectRepo,
  getExistingAssets,
  githubAPIError,
  runGithubOperation,
  saveGithubAsset,
} from '../helpers'
import type { GithubOperationContext } from './github-operation-context'
import { getAllComponentDescriptorFilePaths } from '../../../property-controls/property-controls-local'
import { GithubOperations } from '.'
import { assertNever } from '../../utils'
import { updateProjectContentsWithParseResults } from '../../parser-projectcontents-utils'
import {
  notifyOperationCriticalError,
  notifyOperationFinished,
  notifyOperationStarted,
  pauseImport,
  startImportProcess,
} from '../../import/import-operation-service'
import {
  RequirementResolutionResult,
  resetRequirementsResolutions,
  startPostParseValidation,
  startPreParseValidation,
} from '../../import/project-health-check/utopia-requirements-service'
import {
  checkAndFixUtopiaRequirementsParsed,
  checkAndFixUtopiaRequirementsPreParsed,
} from '../../import/project-health-check/check-utopia-requirements'
import { ImportOperationResult } from '../../import/import-operation-types'
import { isFeatureEnabled } from '../../../../utils/feature-switches'

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
    let promises: Promise<void>[] = []
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
              promises.push(
                saveGithubAsset(
                  githubRepo,
                  forceNotNull('Commit sha should exist.', projectFile.gitBlobSha),
                  projectID,
                  fullPath,
                  dispatch,
                  operationContext,
                  initiator,
                ),
              )
              break
            case 'ASSET_FILE':
              promises.push(
                saveGithubAsset(
                  githubRepo,
                  forceNotNull('Commit sha should exist.', projectFile.gitBlobSha),
                  projectID,
                  fullPath,
                  dispatch,
                  operationContext,
                  initiator,
                ),
              )
              break
            default:
            // Do nothing.
          }
        }
      }
    })
    await Promise.all(promises)
  }

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
    initiator: GithubOperationSource,
  ): Promise<void> => {
    startImportProcess(dispatch)
    notifyOperationStarted(dispatch, {
      type: 'loadBranch',
      branchName: branchName,
      githubRepo: githubRepo,
    })
    await runGithubOperation(
      {
        name: 'loadBranch',
        branchName: branchName,
        githubRepo: githubRepo,
      },
      dispatch,
      initiator,
      async (operation: GithubOperation) => {
        const responseBody = await GithubOperations.getBranchProjectContents({
          projectId: projectID,
          owner: githubRepo.owner,
          repo: githubRepo.repository,
          branch: branchName,
          existingAssets: getExistingAssets(currentProjectContents),
          previousCommitSha: null,
          specificCommitSha: null,
        })

        switch (responseBody.type) {
          case 'FAILURE':
            if (isFeatureEnabled('Import Wizard')) {
              notifyOperationCriticalError(dispatch, { type: 'loadBranch' })
            }
            throw githubAPIError(operation, responseBody.failureReason)
          case 'SUCCESS':
            if (responseBody.branch == null) {
              if (isFeatureEnabled('Import Wizard')) {
                notifyOperationCriticalError(dispatch, { type: 'loadBranch' })
              }
              throw githubAPIError(operation, `Could not find branch ${branchName}`)
            }
            const newGithubData: Partial<GithubData> = {
              upstreamChanges: null,
            }
            if (resetBranches) {
              newGithubData.branches = null
            }
            notifyOperationFinished(dispatch, { type: 'loadBranch' }, ImportOperationResult.Success)

            // pre parse validation
            resetRequirementsResolutions(dispatch)
            startPreParseValidation(dispatch)
            const {
              fixedProjectContents: projectContents,
              result: preParsedRequirementResolutionResult,
            } = checkAndFixUtopiaRequirementsPreParsed(dispatch, responseBody.branch.content)
            if (preParsedRequirementResolutionResult === RequirementResolutionResult.Critical) {
              // wait for the user to resume the import if they choose to
              await pauseImport(dispatch)
            }

            // parse files
            notifyOperationStarted(dispatch, { type: 'parseFiles' })
            // Push any code through the parser so that the representations we end up with are in a state of `BOTH_MATCH`.
            // So that it will override any existing files that might already exist in the project when sending them to VS Code.
            const parseResults = await updateProjectContentsWithParseResults(
              workers,
              projectContents,
            )
            notifyOperationFinished(dispatch, { type: 'parseFiles' }, ImportOperationResult.Success)

            // post parse validation
            startPostParseValidation(dispatch)
            const { fixedProjectContents, result: postParsedRequirementResolutionResult } =
              checkAndFixUtopiaRequirementsParsed(dispatch, parseResults)
            if (postParsedRequirementResolutionResult === RequirementResolutionResult.Critical) {
              // wait for the user to resume the import if they choose to
              await pauseImport(dispatch)
            }

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
                updateProjectContents(fixedProjectContents),
                updateBranchContents(fixedProjectContents),
                truncateHistory(),
              ],
              'everyone',
            )

            const componentDescriptorFiles =
              getAllComponentDescriptorFilePaths(fixedProjectContents)

            // If there's a package.json file, then attempt to load the dependencies for it.
            let dependenciesPromise: Promise<void> = Promise.resolve()
            const packageJson = packageJsonFileFromProjectContents(fixedProjectContents)
            if (packageJson != null && isTextFile(packageJson)) {
              notifyOperationStarted(dispatch, { type: 'refreshDependencies' })
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
                if (isFeatureEnabled('Import Wizard')) {
                  notifyOperationFinished(
                    dispatch,
                    { type: 'refreshDependencies' },
                    ImportOperationResult.Error,
                  )
                } else {
                  showToast(
                    notice(
                      `Github: There was an error when attempting to update the dependencies.`,
                      'ERROR',
                    ),
                  )
                }
              })
              .finally(() => {
                dispatch(
                  [
                    extractPropertyControlsFromDescriptorFiles(componentDescriptorFiles),
                    isFeatureEnabled('Import Wizard')
                      ? updateImportStatus({ status: 'done' })
                      : showToast(
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
            assertNever(responseBody)
        }
        return []
      },
    )
  }
