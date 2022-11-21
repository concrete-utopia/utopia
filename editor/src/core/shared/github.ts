import { mergeDiff3 } from 'node-diff3'
import { createSelector } from 'reselect'
import urljoin from 'url-join'
import { UTOPIA_BACKEND } from '../../common/env-vars'
import { HEADERS, MODE } from '../../common/server'
import {
  addFileToProjectContents,
  deriveGithubFileChanges,
  getContentsTreeFileFromString,
  getProjectContentsChecksums,
  getProjectFileFromContents,
  isProjectContentDirectory,
  isProjectContentFile,
  projectContentDirectory,
  projectContentFile,
  ProjectContentsTree,
  ProjectContentTreeRoot,
} from '../../components/assets'
import { notice } from '../../components/common/notice'
import { EditorAction, EditorDispatch } from '../../components/editor/action-types'
import {
  deleteFile,
  setGithubState,
  removeFileConflict,
  showToast,
  updateAgainstGithub,
  updateBranchContents,
  updateFile,
  updateGithubChecksums,
  updateGithubData,
  updateGithubOperations,
  updateGithubSettings,
  updateProjectContents,
} from '../../components/editor/actions/action-creators'
import {
  EditorStorePatched,
  emptyGithubData,
  emptyGithubSettings,
  GithubChecksums,
  GithubData,
  GithubOperation,
  GithubRepo,
  GithubUser,
  packageJsonFileFromProjectContents,
  PersistentModel,
  projectGithubSettings,
  PullRequest,
} from '../../components/editor/store/editor-state'
import { BuiltInDependencies } from '../es-modules/package-manager/built-in-dependencies-list'
import { refreshDependencies } from './dependencies'
import { RequestedNpmDependency } from './npm-dependency-types'
import { propOrNull } from './object-utils'
import {
  isTextFile,
  ProjectFile,
  RevisionsState,
  textFile,
  textFileContents,
  unparsed,
} from './project-file-types'
import { emptySet } from './set-utils'
import { trimUpToAndIncluding } from './string-utils'
import { arrayEquals } from './utils'

export function parseGithubProjectString(maybeProject: string): GithubRepo | null {
  const withoutGithubPrefix = trimUpToAndIncluding('github.com/', maybeProject)

  const repoParts = withoutGithubPrefix.split('/')
  const owner = repoParts[0] ?? ''
  const repo = repoParts[1] ?? ''

  if (owner === '' || repo === '') {
    return null
  } else {
    return {
      owner: owner,
      repository: repo,
    }
  }
}

export interface SaveToGithubSuccess {
  type: 'SUCCESS'
  branchName: string
  url: string
  newCommit: string
}

export interface GithubFailure {
  type: 'FAILURE'
  failureReason: string
}

export type SaveToGithubResponse = SaveToGithubSuccess | GithubFailure

export interface GithubBranch {
  name: string
  new?: boolean
}

export type GetBranchesResult = Array<GithubBranch>

export interface GetBranchesSuccess {
  type: 'SUCCESS'
  branches: GetBranchesResult
}

export type GetBranchesResponse = GetBranchesSuccess | GithubFailure

export interface BranchContent {
  content: ProjectContentTreeRoot
  originCommit: string
}

export interface GetBranchContentSuccess {
  type: 'SUCCESS'
  branch: BranchContent | null
}

export type GetBranchContentResponse = GetBranchContentSuccess | GithubFailure

export interface RepositoryEntryPermissions {
  admin: boolean
  push: boolean
  pull: boolean
}

export function repositoryEntryPermissions(
  admin: boolean,
  push: boolean,
  pull: boolean,
): RepositoryEntryPermissions {
  return {
    admin: admin,
    push: push,
    pull: pull,
  }
}

export interface RepositoryEntry {
  fullName: string
  name: string
  avatarUrl: string | null
  isPrivate: boolean
  description: string | null
  updatedAt: string | null
  defaultBranch: string
  permissions: RepositoryEntryPermissions
}

export function repositoryEntry(
  avatarUrl: string | null,
  isPrivate: boolean,
  fullName: string,
  name: string,
  description: string | null,
  updatedAt: string | null,
  defaultBranch: string,
  permissions: RepositoryEntryPermissions,
): RepositoryEntry {
  return {
    avatarUrl,
    isPrivate,
    fullName,
    name,
    description,
    updatedAt,
    defaultBranch,
    permissions,
  }
}

export interface GetUsersPublicRepositoriesSuccess {
  type: 'SUCCESS'
  repositories: Array<RepositoryEntry>
}

export type GetUsersPublicRepositoriesResponse = GetUsersPublicRepositoriesSuccess | GithubFailure

export interface GetBranchPullRequestSuccess {
  type: 'SUCCESS'
  pullRequests: Array<PullRequest>
}

export type GetBranchPullRequestResponse = GetBranchPullRequestSuccess | GithubFailure

export interface GetGithubUserSuccess {
  type: 'SUCCESS'
  user: GithubUser
}

export type GetGithubUserResponse = GetGithubUserSuccess | GithubFailure

export interface SaveProjectToGithubOptions {
  branchName: string | null
  commitMessage: string | null
}

export async function saveProjectToGithub(
  projectID: string,
  persistentModel: PersistentModel,
  dispatch: EditorDispatch,
  options: SaveProjectToGithubOptions,
): Promise<void> {
  const { branchName, commitMessage } = options
  const operation: GithubOperation = { name: 'commish' }

  dispatch([updateGithubOperations(operation, 'add')], 'everyone')

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

  const postBody = JSON.stringify(persistentModel)
  const response = await fetch(urlToUse, {
    method: 'POST',
    credentials: 'include',
    headers: HEADERS,
    mode: MODE,
    body: postBody,
  })
  if (response.ok) {
    const responseBody: SaveToGithubResponse = await response.json()
    switch (responseBody.type) {
      case 'FAILURE':
        dispatch(
          [
            showToast(
              notice(`Error when saving to Github: ${responseBody.failureReason}`, 'ERROR'),
            ),
          ],
          'everyone',
        )
        break
      case 'SUCCESS':
        dispatch(
          [
            updateGithubChecksums(getProjectContentsChecksums(persistentModel.projectContents)),
            updateGithubSettings(
              projectGithubSettings(
                persistentModel.githubSettings.targetRepository,
                responseBody.newCommit,
                responseBody.branchName,
                responseBody.newCommit,
                true,
              ),
            ),
            updateBranchContents(persistentModel.projectContents),
            showToast(notice(`Saved to branch ${responseBody.branchName}.`, 'INFO')),
          ],
          'everyone',
        )

        // refresh the branches after the content was saved
        if (persistentModel.githubSettings.targetRepository) {
          void getBranchesForGithubRepository(
            dispatch,
            persistentModel.githubSettings.targetRepository,
          )
        }
        break
      default:
        const _exhaustiveCheck: never = responseBody
        throw new Error(`Unhandled response body ${JSON.stringify(responseBody)}`)
    }
  } else {
    dispatch(
      [showToast(notice(`Unexpected status returned from endpoint: ${response.status}`, 'ERROR'))],
      'everyone',
    )
  }
  dispatch([updateGithubOperations(operation, 'remove')], 'everyone')
}

export async function getBranchesForGithubRepository(
  dispatch: EditorDispatch,
  githubRepo: GithubRepo,
): Promise<void> {
  const operation: GithubOperation = { name: 'listBranches' }

  dispatch([updateGithubOperations(operation, 'add')], 'everyone')

  const url = urljoin(UTOPIA_BACKEND, 'github', 'branches', githubRepo.owner, githubRepo.repository)

  const response = await fetch(url, {
    method: 'GET',
    credentials: 'include',
    headers: HEADERS,
    mode: MODE,
  })

  if (response.ok) {
    const responseBody: GetBranchesResponse = await response.json()

    switch (responseBody.type) {
      case 'FAILURE':
        dispatch(
          [
            showToast(
              notice(`Error when listing branches: ${responseBody.failureReason}`, 'ERROR'),
            ),
          ],
          'everyone',
        )
        break
      case 'SUCCESS':
        dispatch([updateGithubData({ branches: responseBody.branches })], 'everyone')
        break
      default:
        const _exhaustiveCheck: never = responseBody
        throw new Error(`Unhandled response body ${JSON.stringify(responseBody)}`)
    }
  } else {
    dispatch(
      [showToast(notice(`Unexpected status returned from endpoint: ${response.status}`, 'ERROR'))],
      'everyone',
    )
  }

  dispatch([updateGithubOperations(operation, 'remove')], 'everyone')
}

const RE_PULL_REQUEST_URL_NUMBER = /.+\/pull\/([0-9]+).*/

export function getPullRequestNumberFromUrl(url: string): number {
  try {
    return parseInt(url.replace(RE_PULL_REQUEST_URL_NUMBER, '$1'))
  } catch (err) {
    return NaN
  }
}

export async function updatePullRequestsForBranch(
  dispatch: EditorDispatch,
  githubRepo: GithubRepo,
  branchName: string,
): Promise<void> {
  const operation: GithubOperation = {
    name: 'listPullRequestsForBranch',
    githubRepo: githubRepo,
    branchName: branchName,
  }

  dispatch([updateGithubOperations(operation, 'add')], 'everyone')

  const url = urljoin(
    UTOPIA_BACKEND,
    'github',
    'branches',
    githubRepo.owner,
    githubRepo.repository,
    'branch',
    branchName,
    'pullrequest',
  )

  const response = await fetch(url, {
    method: 'GET',
    credentials: 'include',
    headers: HEADERS,
    mode: MODE,
  })

  if (response.ok) {
    const responseBody: GetBranchPullRequestResponse = await response.json()

    switch (responseBody.type) {
      case 'FAILURE':
        dispatch(
          [
            showToast(
              notice(
                `Error when listing pull requests for branch: ${responseBody.failureReason}`,
                'ERROR',
              ),
            ),
          ],
          'everyone',
        )
        break
      case 'SUCCESS':
        dispatch(
          [
            updateGithubData({
              currentBranchPullRequests: responseBody.pullRequests.map((pr) => ({
                ...pr,
                number: getPullRequestNumberFromUrl(pr.htmlURL),
              })),
            }),
          ],
          'everyone',
        )
        break
      default:
        const _exhaustiveCheck: never = responseBody
        throw new Error(`Unhandled response body ${JSON.stringify(responseBody)}`)
    }
  } else {
    dispatch(
      [showToast(notice(`Unexpected status returned from endpoint: ${response.status}`, 'ERROR'))],
      'everyone',
    )
  }

  dispatch([updateGithubOperations(operation, 'remove')], 'everyone')
}

export async function updateProjectAgainstGithub(
  dispatch: EditorDispatch,
  githubRepo: GithubRepo,
  branchName: string,
  commitSha: string,
): Promise<void> {
  const operation: GithubOperation = {
    name: 'updateAgainstBranch',
  }

  dispatch([updateGithubOperations(operation, 'add')], 'everyone')

  const branchLatestRequest = getBranchContentFromServer(githubRepo, branchName, null)
  const specificCommitRequest = getBranchContentFromServer(githubRepo, branchName, commitSha)

  const branchLatestResponse = await branchLatestRequest
  const specificCommitResponse = await specificCommitRequest

  if (branchLatestResponse.ok && specificCommitResponse.ok) {
    const branchLatestContent: GetBranchContentResponse = await branchLatestResponse.json()
    const specificCommitContent: GetBranchContentResponse = await specificCommitResponse.json()

    function failWithReason(failureReason: string): void {
      dispatch(
        [showToast(notice(`Error when updating against Github: ${failureReason}`, 'ERROR'))],
        'everyone',
      )
    }

    if (branchLatestContent.type === 'SUCCESS') {
      if (branchLatestContent.branch == null) {
        failWithReason(`Could not find latest code for branch ${branchName}`)
      } else {
        if (specificCommitContent.type === 'SUCCESS') {
          if (specificCommitContent.branch == null) {
            failWithReason(`Could not find commit ${commitSha} for branch ${branchName}`)
          } else {
            dispatch(
              [
                updateGithubChecksums(
                  getProjectContentsChecksums(branchLatestContent.branch.content),
                ),
                updateBranchContents(branchLatestContent.branch.content),
                updateAgainstGithub(
                  branchLatestContent.branch.content,
                  specificCommitContent.branch.content,
                  branchLatestContent.branch.originCommit,
                ),
                updateGithubData({ upstreamChanges: null }),
                showToast(
                  notice(`Updated the project against the branch ${branchName}.`, 'SUCCESS'),
                ),
              ],
              'everyone',
            )
          }
        } else {
          failWithReason(specificCommitContent.failureReason)
        }
      }
    } else {
      failWithReason(branchLatestContent.failureReason)
    }
  } else {
    const failureStatus = branchLatestResponse.ok
      ? specificCommitResponse.status
      : branchLatestResponse.status
    dispatch(
      [showToast(notice(`Unexpected status returned from endpoint: ${failureStatus}`, 'ERROR'))],
      'everyone',
    )
  }

  dispatch([updateGithubOperations(operation, 'remove')], 'everyone')
}

export function connectRepo(
  resetBranches: boolean,
  githubRepo: GithubRepo,
  originCommit: string | null,
  branchName: string | null,
): Array<EditorAction> {
  const newGithubData: Partial<GithubData> = {
    upstreamChanges: null,
  }
  if (resetBranches) {
    newGithubData.branches = []
  }
  return [
    updateGithubSettings(
      projectGithubSettings(githubRepo, originCommit, branchName, originCommit, false),
    ),
    updateGithubData(newGithubData),
  ]
}

export async function updateProjectWithBranchContent(
  dispatch: EditorDispatch,
  githubRepo: GithubRepo,
  branchName: string,
  resetBranches: boolean,
  currentDeps: Array<RequestedNpmDependency>,
  builtInDependencies: BuiltInDependencies,
): Promise<void> {
  const operation: GithubOperation = {
    name: 'loadBranch',
    branchName: branchName,
    githubRepo: githubRepo,
  }

  dispatch([updateGithubOperations(operation, 'add')], 'everyone')

  const response = await getBranchContentFromServer(githubRepo, branchName, null)

  if (response.ok) {
    const responseBody: GetBranchContentResponse = await response.json()

    switch (responseBody.type) {
      case 'FAILURE':
        dispatch(
          [
            showToast(
              notice(`Error when saving to Github: ${responseBody.failureReason}`, 'ERROR'),
            ),
          ],
          'everyone',
        )
        break
      case 'SUCCESS':
        if (responseBody.branch == null) {
          dispatch([showToast(notice(`Could not find branch ${branchName}.`, 'ERROR'))], 'everyone')
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
              ),
              updateGithubChecksums(getProjectContentsChecksums(responseBody.branch.content)),
              updateProjectContents(responseBody.branch.content),
              updateBranchContents(responseBody.branch.content),
              showToast(
                notice(`Updated the project with the content from ${branchName}`, 'SUCCESS'),
              ),
            ],
            'everyone',
          )
        }
        break
      default:
        const _exhaustiveCheck: never = responseBody
        throw new Error(`Unhandled response body ${JSON.stringify(responseBody)}`)
    }
  } else {
    dispatch(
      [showToast(notice(`Unexpected status returned from endpoint: ${response.status}`, 'ERROR'))],
      'everyone',
    )
  }

  dispatch([updateGithubOperations(operation, 'remove')], 'everyone')
}

async function getBranchContentFromServer(
  githubRepo: GithubRepo,
  branchName: string,
  commitSha: string | null,
): Promise<Response> {
  const url = urljoin(
    UTOPIA_BACKEND,
    'github',
    'branches',
    githubRepo.owner,
    githubRepo.repository,
    'branch',
    branchName,
  )
  let includeQueryParams: boolean = false
  let paramsRecord: Record<string, string> = {}
  if (commitSha != null) {
    includeQueryParams = true
    paramsRecord.commit_sha = commitSha
  }
  const searchParams = new URLSearchParams(paramsRecord)
  const urlToUse = includeQueryParams ? `${url}?${searchParams}` : url

  return fetch(urlToUse, {
    method: 'GET',
    credentials: 'include',
    headers: HEADERS,
    mode: MODE,
  })
}

export async function getUserDetailsFromServer(): Promise<GithubUser> {
  const url = urljoin(UTOPIA_BACKEND, 'github', 'user')

  const response = await fetch(url, {
    method: 'GET',
    credentials: 'include',
    headers: HEADERS,
    mode: MODE,
  })

  if (response.ok) {
    const responseBody: GetGithubUserResponse = await response.json()
    switch (responseBody.type) {
      case 'FAILURE':
        throw new Error(
          `Error when attempting to retrieve the user details: ${responseBody.failureReason}`,
        )
      case 'SUCCESS':
        return responseBody.user
      default:
        const _exhaustiveCheck: never = responseBody
        throw new Error(`Unhandled response body ${JSON.stringify(responseBody)}`)
    }
  } else {
    throw new Error(`Unexpected status returned from user details endpoint: ${response.status}`)
  }
}

// Designed to wrap around a function that checks for a valid Github authentication.
export async function updateUserDetailsWhenAuthenticated(
  dispatch: EditorDispatch,
  authenticationCheck: Promise<boolean>,
): Promise<boolean> {
  const authenticationResult = await authenticationCheck
  if (authenticationResult) {
    await getUserDetailsFromServer()
      .then((userDetails) => {
        dispatch([updateGithubData({ githubUserDetails: userDetails })], 'everyone')
      })
      .catch((error) => {
        console.error(`Error while attempting to retrieve Github user details: ${error}`)
      })
  }
  return authenticationResult
}

export async function getUsersPublicGithubRepositories(dispatch: EditorDispatch): Promise<void> {
  const operation: GithubOperation = { name: 'loadRepositories' }

  dispatch([updateGithubOperations(operation, 'add')], 'everyone')

  const url = urljoin(UTOPIA_BACKEND, 'github', 'user', 'repositories')

  const response = await fetch(url, {
    method: 'GET',
    credentials: 'include',
    headers: HEADERS,
    mode: MODE,
  })
  if (response.ok) {
    const responseBody: GetUsersPublicRepositoriesResponse = await response.json()
    switch (responseBody.type) {
      case 'FAILURE':
        const actions: EditorAction[] = [
          showToast(
            notice(
              `Error when getting a user's repositories: ${responseBody.failureReason}`,
              'ERROR',
            ),
          ),
        ]
        if (responseBody.failureReason.includes('Authentication')) {
          actions.push(
            updateGithubSettings(emptyGithubSettings()),
            setGithubState({ authenticated: false }),
          )
        }
        dispatch(actions, 'everyone')
        break
      case 'SUCCESS':
        dispatch(
          [
            updateGithubData({
              publicRepositories: responseBody.repositories.filter((repo) => !repo.isPrivate),
            }),
          ],
          'everyone',
        )
        break
      default:
        const _exhaustiveCheck: never = responseBody
        throw new Error(`Unhandled response body ${JSON.stringify(responseBody)}`)
    }
  } else {
    dispatch(
      [showToast(notice(`Unexpected status returned from endpoint: ${response.status}`, 'ERROR'))],
      'everyone',
    )
  }

  dispatch([updateGithubOperations(operation, 'remove')], 'everyone')
}

export const githubFileChangesSelector = createSelector(
  (store: EditorStorePatched) => store.editor.projectContents,
  (store) => store.userState.githubState.authenticated,
  (store) => store.editor.githubChecksums,
  (store) => store.editor.githubData.treeConflicts,
  (
    projectContents,
    githubAuthenticated,
    githubChecksums,
    treeConflicts,
  ): GithubFileChanges | null => {
    if (!githubAuthenticated) {
      return null
    }
    const checksums = getProjectContentsChecksums(projectContents)
    return deriveGithubFileChanges(checksums, githubChecksums, treeConflicts)
  },
)

export type GithubFileStatus = 'modified' | 'deleted' | 'untracked' | 'conflicted'

export interface GithubFileChanges {
  untracked: Array<string>
  modified: Array<string>
  deleted: Array<string>
  conflicted: Array<string>
}

export function emptyGithubFileChanges(): GithubFileChanges {
  return {
    untracked: [],
    modified: [],
    deleted: [],
    conflicted: [],
  }
}

export function getGithubFileChangesCount(changes: GithubFileChanges | null): number {
  if (changes == null) {
    return 0
  }
  return (
    changes.untracked.length +
    changes.modified.length +
    changes.deleted.length +
    changes.conflicted.length
  )
}

export function githubFileChangesEquals(
  a: GithubFileChanges | null,
  b: GithubFileChanges | null,
): boolean {
  if (a == null && b == null) {
    return true
  }
  if (a == null || b == null) {
    return false
  }
  return (
    arrayEquals(a.untracked, b.untracked) &&
    arrayEquals(a.modified, b.modified) &&
    arrayEquals(a.deleted, b.deleted) &&
    arrayEquals(a.conflicted, b.conflicted)
  )
}

export type GithubFileChangesListItem = {
  status: GithubFileStatus
  filename: string
}

export function githubFileChangesToList(
  changes: GithubFileChanges | null,
): Array<GithubFileChangesListItem> {
  if (changes == null) {
    return []
  }

  const toItem = (status: GithubFileStatus, files: Array<string>) =>
    files.map((d) => ({ status: status, filename: d }))

  const sortByFilename = (a: GithubFileChangesListItem, b: GithubFileChangesListItem) =>
    a.filename.localeCompare(b.filename)

  return [
    ...toItem('untracked', changes.untracked),
    ...toItem('modified', changes.modified),
    ...toItem('deleted', changes.deleted),
    ...toItem('conflicted', changes.conflicted),
  ].sort(sortByFilename)
}

export function revertAllGithubFiles(
  branchContents: ProjectContentTreeRoot | null,
): Array<EditorAction> {
  let actions: Array<EditorAction> = []
  if (branchContents != null) {
    actions.push(updateProjectContents(branchContents))
  }
  return actions
}

export function revertGithubFile(
  status: GithubFileStatus,
  filename: string,
  projectContents: ProjectContentTreeRoot,
  branchContents: ProjectContentTreeRoot | null,
): Array<EditorAction> {
  let actions: Array<EditorAction> = []
  if (branchContents != null) {
    switch (status) {
      case 'untracked':
        actions.push(deleteFile(filename))
        break
      case 'deleted':
      case 'modified':
        const previousFile = getContentsTreeFileFromString(branchContents, filename)
        if (previousFile != null) {
          const newTree = addFileToProjectContents(projectContents, filename, previousFile)
          actions.push(updateProjectContents(newTree))
        }
        break
    }
  }
  return actions
}

export interface DifferingTypesConflict {
  type: 'DIFFERING_TYPES'
  currentContents: ProjectContentsTree
  originContents: ProjectContentsTree | null
  branchContents: ProjectContentsTree
}

export function differingTypesConflict(
  currentContents: ProjectContentsTree,
  originContents: ProjectContentsTree | null,
  branchContents: ProjectContentsTree,
): DifferingTypesConflict {
  return {
    type: 'DIFFERING_TYPES',
    currentContents: currentContents,
    originContents: originContents,
    branchContents: branchContents,
  }
}

export interface CurrentChangedBranchDeleted {
  type: 'CURRENT_CHANGED_BRANCH_DELETED'
  currentContents: ProjectContentsTree
  originContents: ProjectContentsTree
}

export function currentChangedBranchDeleted(
  currentContents: ProjectContentsTree,
  originContents: ProjectContentsTree,
): CurrentChangedBranchDeleted {
  return {
    type: 'CURRENT_CHANGED_BRANCH_DELETED',
    currentContents: currentContents,
    originContents: originContents,
  }
}

export interface CurrentDeletedBranchChanged {
  type: 'CURRENT_DELETED_BRANCH_CHANGED'
  originContents: ProjectContentsTree
  branchContents: ProjectContentsTree
}

export function currentDeletedBranchChanged(
  originContents: ProjectContentsTree,
  branchContents: ProjectContentsTree,
): CurrentDeletedBranchChanged {
  return {
    type: 'CURRENT_DELETED_BRANCH_CHANGED',
    originContents: originContents,
    branchContents: branchContents,
  }
}

export type Conflict =
  | DifferingTypesConflict
  | CurrentChangedBranchDeleted
  | CurrentDeletedBranchChanged

export type TreeConflicts = { [path: string]: Conflict }

export interface WithTreeConflicts<T> {
  value: T
  treeConflicts: TreeConflicts
}

export function withTreeConflicts<T>(value: T, treeConflicts: TreeConflicts): WithTreeConflicts<T> {
  return {
    value: value,
    treeConflicts: treeConflicts,
  }
}

export function mergeProjectContents(
  currentTime: number,
  currentTree: ProjectContentTreeRoot,
  originTree: ProjectContentTreeRoot,
  branchTree: ProjectContentTreeRoot,
): WithTreeConflicts<ProjectContentTreeRoot> {
  let keys: Set<string> = emptySet()
  Object.keys(currentTree).forEach(keys.add, keys)
  Object.keys(originTree).forEach(keys.add, keys)
  Object.keys(branchTree).forEach(keys.add, keys)

  let result: ProjectContentTreeRoot = {}
  let treeConflicts: TreeConflicts = {}
  for (const key of keys) {
    const currentContents = propOrNull(key, currentTree)
    const originContents = propOrNull(key, originTree)
    const branchContents = propOrNull(key, branchTree)
    const fullPath =
      currentContents?.fullPath ?? originContents?.fullPath ?? branchContents?.fullPath
    if (fullPath == null) {
      throw new Error(`Invalid state of the elements being null reached.`)
    } else {
      const combinedElement = mergeProjectContentsTree(
        currentTime,
        fullPath,
        currentContents,
        originContents,
        branchContents,
      )
      treeConflicts = {
        ...treeConflicts,
        ...combinedElement.treeConflicts,
      }
      if (combinedElement.value != null) {
        result[key] = combinedElement.value
      }
    }
  }

  return withTreeConflicts(result, treeConflicts)
}

export function projectFileContentOrTypeChanged(first: ProjectFile, second: ProjectFile): boolean {
  if (first.type === second.type) {
    if (first.type === 'TEXT_FILE' && second.type === 'TEXT_FILE') {
      return first.fileContents.code !== second.fileContents.code
    } else if (first.type === 'IMAGE_FILE' && second.type === 'IMAGE_FILE') {
      return first.hash !== second.hash
    } else if (
      first.type === 'ASSET_FILE' &&
      second.type === 'ASSET_FILE' &&
      first.gitBlobSha !== null &&
      second.gitBlobSha != null
    ) {
      return first.gitBlobSha !== second.gitBlobSha
    } else if (first.type === 'DIRECTORY' && second.type === 'DIRECTORY') {
      return false
    } else {
      return false
    }
  } else {
    return true
  }
}

export function projectContentsTreeContentOrTypeChange(
  first: ProjectContentsTree | null,
  second: ProjectContentsTree | null,
): boolean {
  if (first?.type === 'PROJECT_CONTENT_FILE' && second?.type === 'PROJECT_CONTENT_FILE') {
    return projectFileContentOrTypeChanged(first.content, second.content)
  } else if (
    first?.type === 'PROJECT_CONTENT_DIRECTORY' &&
    second?.type === 'PROJECT_CONTENT_DIRECTORY'
  ) {
    return projectFileContentOrTypeChanged(first.directory, second.directory)
  } else {
    return first?.type !== second?.type
  }
}

export function checkForTreeConflicts(
  fullPath: string,
  currentContents: ProjectContentsTree | null,
  originContents: ProjectContentsTree | null,
  branchContents: ProjectContentsTree | null,
): WithTreeConflicts<ProjectContentsTree | null> {
  // Check what changes have been made against the origin and potentially between Utopia and the branch.
  const currentContentsToOriginChanged = projectContentsTreeContentOrTypeChange(
    originContents,
    currentContents,
  )
  const branchContentsToOriginChanged = projectContentsTreeContentOrTypeChange(
    originContents,
    branchContents,
  )
  const branchContentsToCurrentChanged = projectContentsTreeContentOrTypeChange(
    currentContents,
    branchContents,
  )

  if (currentContentsToOriginChanged && !branchContentsToOriginChanged) {
    // Utopia has changed from the origin, but the branch has not.
    return withTreeConflicts(currentContents, {})
  } else if (!currentContentsToOriginChanged && branchContentsToOriginChanged) {
    // The branch has changed from the origin, but Utopia has not.
    return withTreeConflicts(branchContents, {})
    // Neither Utopia nor the branch have changed from the origin.
  } else if (!currentContentsToOriginChanged && !branchContentsToOriginChanged) {
    return withTreeConflicts(originContents, {})
  } else if (
    currentContentsToOriginChanged &&
    branchContentsToOriginChanged &&
    !branchContentsToCurrentChanged
  ) {
    // Utopia has changed from the origin, the branch has changed from the origin but Utopia and the branch are compatible values.
    return withTreeConflicts(currentContents, {})
  } else if (
    currentContentsToOriginChanged &&
    branchContentsToOriginChanged &&
    branchContentsToCurrentChanged
  ) {
    // Utopia has changed from the origin, the branch has changed from the origin and Utopia and the branch are incompatible.
    if (currentContents == null) {
      if (originContents == null) {
        // Origin and Utopia project lack an entry, so go with whatever the branch contains.
        return withTreeConflicts(branchContents, {})
      } else {
        if (branchContents == null) {
          // Both Utopia and the branch have deleted this.
          return withTreeConflicts(branchContents, {})
        } else {
          // Utopia deleted this, but it changed in the branch.
          return withTreeConflicts(currentContents, {
            [fullPath]: currentDeletedBranchChanged(originContents, branchContents),
          })
        }
      }
    } else {
      if (originContents == null) {
        if (branchContents == null) {
          // Created in Utopia, didn't exist before.
          return withTreeConflicts(currentContents, {})
        } else {
          // Didn't exist previously, created in both the branch and Utopia.
          return withTreeConflicts(currentContents, {
            [fullPath]: differingTypesConflict(currentContents, originContents, branchContents),
          })
        }
      } else {
        if (branchContents == null) {
          // Deleted on the branch, but changed in Utopia.
          return withTreeConflicts(currentContents, {
            [fullPath]: currentChangedBranchDeleted(currentContents, originContents),
          })
        } else {
          // Existed previously, but changed incompatibly in both Utopia and the branch.
          return withTreeConflicts(currentContents, {
            [fullPath]: differingTypesConflict(currentContents, originContents, branchContents),
          })
        }
      }
    }
  } else {
    throw new Error(`Unhandled case reached.`)
  }
}

/*
 * Indication of the change flow that is expected:
 *       /-->current
 *      /
 * origin
 *      \
 *       \-->branch
 */
export function mergeProjectContentsTree(
  currentTime: number,
  fullPath: string,
  currentContents: ProjectContentsTree | null,
  originContents: ProjectContentsTree | null,
  branchContents: ProjectContentsTree | null,
): WithTreeConflicts<ProjectContentsTree | null> {
  if (
    isProjectContentFile(currentContents) &&
    isTextFile(currentContents.content) &&
    isProjectContentFile(originContents) &&
    isTextFile(originContents.content) &&
    isProjectContentFile(branchContents) &&
    isTextFile(branchContents.content)
  ) {
    // At this location in all 3 places there is a text file.
    const currentCode = currentContents.content.fileContents.code
    const originCode = originContents.content.fileContents.code
    const branchCode = branchContents.content.fileContents.code
    if (currentCode === originCode && branchCode === originCode) {
      // Code is the same in all 3 places, skip any merging activity.
      return withTreeConflicts(originContents, {})
    } else {
      // There's a code change between the 3 places, perform a merge of the changes.
      const mergedResult = mergeDiff3(currentCode, originCode, branchCode, {
        label: { a: 'Your Changes', o: 'Original', b: 'Branch Changes' },
        stringSeparator: /\r?\n/,
      }).result.join('\n')
      const updatedTextFile = textFile(
        textFileContents(mergedResult, unparsed, RevisionsState.CodeAhead),
        null,
        null,
        currentTime,
      )

      return withTreeConflicts(projectContentFile(fullPath, updatedTextFile), {})
    }
  } else if (
    isProjectContentDirectory(currentContents) &&
    isProjectContentDirectory(originContents) &&
    isProjectContentDirectory(branchContents)
  ) {
    // All 3 branches are directories.
    const mergedResult = mergeProjectContents(
      currentTime,
      currentContents.children,
      originContents.children,
      branchContents.children,
    )
    return withTreeConflicts(
      projectContentDirectory(fullPath, currentContents.directory, mergedResult.value),
      mergedResult.treeConflicts,
    )
  } else {
    return checkForTreeConflicts(fullPath, currentContents, originContents, branchContents)
  }
}

export async function refreshGithubData(
  dispatch: EditorDispatch,
  githubAuthenticated: boolean,
  githubRepo: GithubRepo | null,
  branchName: string | null,
  branchChecksums: GithubChecksums | null,
  githubUserDetails: GithubUser | null,
): Promise<void> {
  if (githubAuthenticated) {
    if (githubUserDetails === null) {
      void getUserDetailsFromServer().then((r) =>
        dispatch([updateGithubData({ githubUserDetails: r })]),
      )
    }
    void getUsersPublicGithubRepositories(dispatch)
    if (githubRepo != null) {
      let upstreamChangesSuccess = false
      void getBranchesForGithubRepository(dispatch, githubRepo)
      if (branchName != null && branchChecksums != null) {
        void updatePullRequestsForBranch(dispatch, githubRepo, branchName)
        const branchContentResponse = await getBranchContentFromServer(githubRepo, branchName, null)
        if (branchContentResponse.ok) {
          const branchLatestContent: GetBranchContentResponse = await branchContentResponse.json()
          if (branchLatestContent.type === 'SUCCESS' && branchLatestContent.branch != null) {
            upstreamChangesSuccess = true
            const upstreamChecksums = getProjectContentsChecksums(
              branchLatestContent.branch.content,
            )
            const upstreamChanges = deriveGithubFileChanges(branchChecksums, upstreamChecksums, {})
            dispatch([updateGithubData({ upstreamChanges: upstreamChanges })], 'everyone')
          }
        }
      }
      if (!upstreamChangesSuccess) {
        dispatch([updateGithubData({ upstreamChanges: null })], 'everyone')
      }
    } else {
      dispatch([updateGithubData({ branches: null })], 'everyone')
    }
  } else {
    dispatch([updateGithubData(emptyGithubData())], 'everyone')
  }
}

export function disconnectGithubProjectActions(): EditorAction[] {
  return [
    updateGithubData(emptyGithubData()),
    updateGithubChecksums({}),
    updateBranchContents(null),
    updateGithubSettings(emptyGithubSettings()),
  ]
}

interface GithubSaveAssetResponseSuccess {
  type: 'SUCCESS'
}

type GithubSaveAssetResponse = GithubSaveAssetResponseSuccess | GithubFailure

export async function saveGithubAsset(
  githubRepo: GithubRepo,
  assetSha: string,
  projectID: string,
  path: string,
): Promise<void> {
  const url = urljoin(
    UTOPIA_BACKEND,
    'github',
    'branches',
    githubRepo.owner,
    githubRepo.repository,
    'asset',
    assetSha,
  )

  const paramsRecord: Record<string, string> = {
    project_id: projectID,
    path: path,
  }
  const searchParams = new URLSearchParams(paramsRecord)
  const urlToUse = `${url}?${searchParams}`

  const response = await fetch(urlToUse, {
    method: 'POST',
    credentials: 'include',
    headers: HEADERS,
    mode: MODE,
  })
  if (response.ok) {
    const responseBody: GithubSaveAssetResponse = await response.json()
    switch (responseBody.type) {
      case 'FAILURE':
        throw new Error(`Failed to save asset ${responseBody.failureReason}`)
      case 'SUCCESS':
        return Promise.resolve()
      default:
        const _exhaustiveCheck: never = responseBody
        throw new Error(`Unhandled response body ${JSON.stringify(responseBody)}`)
    }
  } else {
    throw new Error(`Failed to save asset due to status code ${response.status}.`)
  }
}

export async function resolveConflict(
  githubRepo: GithubRepo,
  projectID: string,
  path: string,
  conflict: Conflict,
  whichChange: 'utopia' | 'branch',
  dispatch: EditorDispatch,
): Promise<void> {
  async function updateFileInProjectContents(file: ProjectContentsTree): Promise<void> {
    // Update the file in the server first before putting it into the project contents.
    const projectFile = getProjectFileFromContents(file)
    let gitBlobSha: string | undefined = undefined
    switch (projectFile.type) {
      case 'IMAGE_FILE':
      case 'ASSET_FILE':
        gitBlobSha = projectFile.gitBlobSha
        break
      default:
    }
    let saveAssetPromise: Promise<void>
    if (gitBlobSha == null) {
      saveAssetPromise = Promise.resolve()
    } else {
      saveAssetPromise = saveGithubAsset(githubRepo, gitBlobSha, projectID, path)
    }
    void saveAssetPromise.then(() => {
      dispatch([updateFile(path, projectFile, true), removeFileConflict(path)], 'everyone')
    })
  }

  async function removeFromContents(): Promise<void> {
    return new Promise((resolve) => {
      dispatch([deleteFile(path), removeFileConflict(path)], 'everyone')
      resolve()
    })
  }

  // Handle the varying cases here.
  switch (conflict.type) {
    case 'DIFFERING_TYPES':
      switch (whichChange) {
        case 'utopia':
          return updateFileInProjectContents(conflict.currentContents)
        case 'branch':
          return updateFileInProjectContents(conflict.branchContents)
        default:
          const _exhaustiveCheck: never = whichChange
          throw new Error(`Unhandled change ${JSON.stringify(whichChange)}`)
      }
      break
    case 'CURRENT_DELETED_BRANCH_CHANGED':
      switch (whichChange) {
        case 'utopia':
          return removeFromContents()
        case 'branch':
          return updateFileInProjectContents(conflict.branchContents)
        default:
          const _exhaustiveCheck: never = whichChange
          throw new Error(`Unhandled change ${JSON.stringify(whichChange)}`)
      }
      break
    case 'CURRENT_CHANGED_BRANCH_DELETED':
      switch (whichChange) {
        case 'utopia':
          return updateFileInProjectContents(conflict.currentContents)
        case 'branch':
          return removeFromContents()
        default:
          const _exhaustiveCheck: never = whichChange
          throw new Error(`Unhandled change ${JSON.stringify(whichChange)}`)
      }
      break
    default:
      const _exhaustiveCheck: never = conflict
      throw new Error(`Unhandled conflict ${JSON.stringify(conflict)}`)
  }
}
