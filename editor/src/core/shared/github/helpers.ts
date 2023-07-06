import { mergeDiff3 } from 'node-diff3'
import { createSelector } from 'reselect'
import urljoin from 'url-join'
import { UTOPIA_BACKEND } from '../../../common/env-vars'
import { HEADERS, MODE } from '../../../common/server'
import type { ProjectContentsTree, ProjectContentTreeRoot } from '../../../components/assets'
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
} from '../../../components/assets'
import { notice } from '../../../components/common/notice'
import type { EditorAction, EditorDispatch } from '../../../components/editor/action-types'
import {
  deleteFile,
  removeFileConflict,
  showToast,
  updateBranchContents,
  updateFile,
  updateGithubData,
  updateGithubOperations,
  updateGithubSettings,
  updateProjectContents,
} from '../../../components/editor/actions/action-creators'
import type {
  EditorStorePatched,
  FileChecksumsWithFile,
  GithubData,
  GithubOperation,
  GithubRepo,
  GithubUser,
} from '../../../components/editor/store/editor-state'
import {
  emptyGithubData,
  emptyGithubSettings,
  FileChecksums,
  githubOperationPrettyName,
  projectGithubSettings,
} from '../../../components/editor/store/editor-state'
import type { UtopiaStoreAPI } from '../../../components/editor/store/store-hook'
import { Substores, useEditorState } from '../../../components/editor/store/store-hook'
import { propOrNull } from '../object-utils'
import type { ProjectFile } from '../project-file-types'
import {
  isTextFile,
  RevisionsState,
  textFile,
  textFileContents,
  unparsed,
} from '../project-file-types'
import { emptySet } from '../set-utils'
import { trimUpToAndIncluding } from '../string-utils'
import { arrayEqualsByReference } from '../utils'
import { getBranchChecksums } from './operations/get-branch-checksums'
import { getBranchesForGithubRepository } from './operations/list-branches'
import { updatePullRequestsForBranch } from './operations/list-pull-requests-for-branch'
import { getUsersPublicGithubRepositories } from './operations/load-repositories'
import { updateProjectAgainstGithub } from './operations/update-against-branch'

export function dispatchPromiseActions(
  dispatch: EditorDispatch,
  promise: Promise<Array<EditorAction>>,
): Promise<void> {
  return promise.then((actions) => dispatch(actions, 'everyone'))
}

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

export interface GithubBranch {
  name: string
  new?: boolean
}

export interface GithubFailure {
  type: 'FAILURE'
  failureReason: string
}

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

export interface GetGithubUserSuccess {
  type: 'SUCCESS'
  user: GithubUser
}

export type GetGithubUserResponse = GetGithubUserSuccess | GithubFailure

export type GithubOperationLogic = (operation: GithubOperation) => Promise<Array<EditorAction>>

export async function runGithubOperation(
  operation: GithubOperation,
  dispatch: EditorDispatch,
  logic: GithubOperationLogic,
): Promise<Array<EditorAction>> {
  let result: Array<EditorAction> = []

  const opName = githubOperationPrettyName(operation)
  try {
    dispatch([updateGithubOperations(operation, 'add')], 'everyone')
    result = await logic(operation)
  } catch (error: any) {
    dispatch(
      [showToast(notice(`${opName} failed. See the console for more information.`, 'ERROR'))],
      'everyone',
    )
    console.error(`[GitHub] operation "${opName}" failed:`, error)
    throw error
  } finally {
    dispatch([updateGithubOperations(operation, 'remove')], 'everyone')
  }

  return result
}

export interface GithubAPIError {
  operation: GithubOperation
  status: string
  data?: string
}

export function githubAPIError(
  operation: GithubOperation,
  status: string,
  message?: string,
): GithubAPIError {
  return {
    operation: operation,
    status: status,
    data: message,
  }
}

export async function githubAPIErrorFromResponse(
  operation: GithubOperation,
  response: Response,
): Promise<GithubAPIError> {
  async function getText() {
    try {
      return await response.text()
    } catch (e) {
      return null
    }
  }
  return {
    operation: operation,
    status: `${response.statusText} (${response.status})`,
    data: (await getText()) ?? undefined,
  }
}

export function connectRepo(
  resetBranches: boolean,
  githubRepo: GithubRepo,
  originCommit: string | null,
  branchName: string | null,
  branchLoaded: boolean,
): Array<EditorAction> {
  const newGithubData: Partial<GithubData> = {
    upstreamChanges: null,
  }
  if (resetBranches) {
    newGithubData.branches = []
  }
  return [
    updateGithubSettings(
      projectGithubSettings(githubRepo, originCommit, branchName, originCommit, branchLoaded),
    ),
    updateGithubData(newGithubData),
  ]
}

export async function getBranchContentFromServer(
  githubRepo: GithubRepo,
  branchName: string,
  commitSha: string | null,
  previousCommitSha: string | null,
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
  if (previousCommitSha != null) {
    includeQueryParams = true
    paramsRecord.previous_commit_sha = previousCommitSha
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

export async function getUserDetailsFromServer(): Promise<Array<EditorAction>> {
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
        return [updateGithubData({ githubUserDetails: responseBody.user })]
      default:
        const _exhaustiveCheck: never = responseBody
        throw new Error(`Unhandled response body ${JSON.stringify(responseBody)}`)
    }
  } else {
    throw new Error(
      `Github: Unexpected status returned from user details endpoint: ${response.status}`,
    )
  }
}

// Designed to wrap around a function that checks for a valid Github authentication.
export async function updateUserDetailsWhenAuthenticated(
  dispatch: EditorDispatch,
  authenticationCheck: Promise<boolean>,
): Promise<boolean> {
  const authenticationResult = await authenticationCheck
  if (authenticationResult) {
    await dispatchPromiseActions(dispatch, getUserDetailsFromServer()).catch((error) => {
      console.error(`Error while attempting to retrieve Github user details: ${error}`)
    })
  }
  return authenticationResult
}

const githubFileChangesSelector = createSelector(
  (store: EditorStorePatched) => store.userState.githubState.authenticated,
  (store: EditorStorePatched) => store.derived.branchOriginContentsChecksums,
  (store: EditorStorePatched) => store.editor.githubData.treeConflicts,
  (store: EditorStorePatched) => store.derived.projectContentsChecksums,
  (
    githubAuthenticated,
    branchOriginContentsChecksums,
    treeConflicts,
    projectContentsChecksums,
  ): GithubFileChanges | null => {
    if (!githubAuthenticated) {
      return null
    }
    return deriveGithubFileChanges(
      branchOriginContentsChecksums,
      projectContentsChecksums,
      treeConflicts,
    )
  },
)

export function useGithubFileChanges(): GithubFileChanges | null {
  const storeType = useEditorState(
    Substores.restOfStore,
    (store) => store.storeName,
    'useGithubFileChanges storeName',
  )
  if (storeType !== 'low-priority-store') {
    throw new Error('useGithubFileChanges hook must only be used inside the low-priority-store!')
  }
  return useEditorState(Substores.fullStore, githubFileChangesSelector, 'useGithubFileChanges')
}

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
    arrayEqualsByReference(a.untracked, b.untracked) &&
    arrayEqualsByReference(a.modified, b.modified) &&
    arrayEqualsByReference(a.deleted, b.deleted) &&
    arrayEqualsByReference(a.conflicted, b.conflicted)
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
      const latestVersion = Math.max(
        currentContents.content.versionNumber,
        originContents.content.versionNumber,
        branchContents.content.versionNumber,
      )
      const updatedTextFile = textFile(
        textFileContents(mergedResult, unparsed, RevisionsState.CodeAhead),
        null,
        null,
        latestVersion + 1,
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

const GITHUB_REFRESH_INTERVAL_MILLISECONDS = 30_000

let githubPollingTimeoutId: number | undefined = undefined

export function startGithubPolling(utopiaStoreAPI: UtopiaStoreAPI, dispatch: EditorDispatch): void {
  if (githubPollingTimeoutId != null) {
    window.clearTimeout(githubPollingTimeoutId)
  }
  function pollGithub(): void {
    try {
      const currentState = utopiaStoreAPI.getState()
      const githubAuthenticated = currentState.userState.githubState.authenticated
      const githubRepo = currentState.editor.githubSettings.targetRepository
      const branchName = currentState.editor.githubSettings.branchName
      const branchOriginContentsChecksums = currentState.derived.branchOriginContentsChecksums
      const githubUserDetails = currentState.editor.githubData.githubUserDetails
      const lastRefreshedCommit = currentState.editor.githubData.lastRefreshedCommit
      const originCommitSha = currentState.editor.githubSettings.originCommit
      void refreshGithubData(
        dispatch,
        githubAuthenticated,
        githubRepo,
        branchName,
        branchOriginContentsChecksums,
        githubUserDetails,
        lastRefreshedCommit,
        originCommitSha,
      )
    } finally {
      // Trigger another one to run Xms _after_ this has finished.
      githubPollingTimeoutId = window.setTimeout(pollGithub, GITHUB_REFRESH_INTERVAL_MILLISECONDS)
    }
  }

  // Trigger a poll initially.
  githubPollingTimeoutId = window.setTimeout(pollGithub, 0)
}

export async function refreshGithubData(
  dispatch: EditorDispatch,
  githubAuthenticated: boolean,
  githubRepo: GithubRepo | null,
  branchName: string | null,
  branchOriginChecksums: FileChecksumsWithFile | null,
  githubUserDetails: GithubUser | null,
  previousCommitSha: string | null,
  originCommitSha: string | null,
): Promise<void> {
  // Collect actions which are the results of the various requests,
  // but not those that show which Github operations are running.
  const promises: Array<Promise<Array<EditorAction>>> = []
  if (githubAuthenticated) {
    if (githubUserDetails === null) {
      promises.push(getUserDetailsFromServer())
    }
    promises.push(getUsersPublicGithubRepositories(dispatch))
    if (githubRepo != null) {
      promises.push(getBranchesForGithubRepository(dispatch, githubRepo))
      promises.push(
        updateUpstreamChanges(branchName, branchOriginChecksums, githubRepo, previousCommitSha),
      )
      if (branchName != null) {
        if (branchOriginChecksums == null) {
          if (originCommitSha != null) {
            promises.push(getBranchChecksums(githubRepo, branchName, originCommitSha))
          }
        } else {
          promises.push(updatePullRequestsForBranch(dispatch, githubRepo, branchName))
        }
      }
    } else {
      promises.push(Promise.resolve([updateGithubData({ branches: null })]))
    }
  } else {
    promises.push(Promise.resolve([updateGithubData(emptyGithubData())]))
  }

  // Resolve all the promises.
  await Promise.allSettled(promises).then((results) => {
    let actions: Array<EditorAction> = []
    for (const result of results) {
      switch (result.status) {
        case 'rejected':
          console.error(`Error while polling Github: ${result.reason}`)
          break
        case 'fulfilled':
          actions.push(...result.value)
          break
      }
    }

    // Dispatch all the actions from all the polling functions.
    dispatch(actions, 'everyone')
  })
}

async function updateUpstreamChanges(
  branchName: string | null,
  branchOriginChecksums: FileChecksumsWithFile | null,
  githubRepo: GithubRepo,
  previousCommitSha: string | null,
): Promise<Array<EditorAction>> {
  const actions: Array<EditorAction> = []
  let upstreamChangesSuccess = false
  if (branchName != null && branchOriginChecksums != null) {
    const branchContentResponse = await getBranchContentFromServer(
      githubRepo,
      branchName,
      null,
      previousCommitSha,
    )
    if (branchContentResponse.ok) {
      const branchLatestContent: GetBranchContentResponse = await branchContentResponse.json()
      if (branchLatestContent.type === 'SUCCESS' && branchLatestContent.branch != null) {
        upstreamChangesSuccess = true
        const branchLatestChecksums = getProjectContentsChecksums(
          branchLatestContent.branch.content,
          {},
        )
        const upstreamChanges = deriveGithubFileChanges(
          branchOriginChecksums,
          branchLatestChecksums,
          {},
        )
        actions.push(
          updateGithubData({
            upstreamChanges: upstreamChanges,
            lastRefreshedCommit: branchLatestContent.branch.originCommit,
          }),
        )
      }
    } else if (branchContentResponse.status === 304) {
      // Not modified status means that the branch has the same commit SHA.
      upstreamChangesSuccess = true
    }
  }
  if (!upstreamChangesSuccess) {
    actions.push(updateGithubData({ upstreamChanges: null }))
  }
  return actions
}

export function disconnectGithubProjectActions(): EditorAction[] {
  return [
    updateGithubData(emptyGithubData()),
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
  dispatch: EditorDispatch,
): Promise<void> {
  await runGithubOperation(
    { name: 'saveAsset', path: path },
    dispatch,
    async (operation: GithubOperation) => {
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
      if (!response.ok) {
        throw githubAPIErrorFromResponse(operation, response)
      }

      const responseBody: GithubSaveAssetResponse = await response.json()
      switch (responseBody.type) {
        case 'FAILURE':
          throw githubAPIError(operation, responseBody.failureReason)
        case 'SUCCESS':
          await Promise.resolve()
          break
        default:
          const _exhaustiveCheck: never = responseBody
          throw githubAPIError(operation, `Unhandled response body ${JSON.stringify(responseBody)}`)
      }
      return []
    },
  )
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
      saveAssetPromise = saveGithubAsset(githubRepo, gitBlobSha, projectID, path, dispatch)
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
