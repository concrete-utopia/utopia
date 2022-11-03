import { createSelector } from 'reselect'
import urljoin from 'url-join'
import { UTOPIA_BACKEND } from '../../common/env-vars'
import { HEADERS, MODE } from '../../common/server'
import {
  addFileToProjectContents,
  deriveGithubFileChanges,
  getContentsTreeFileFromString,
  getProjectContentsChecksums,
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
  showToast,
  updateAgainstGithub,
  updateBranchContents,
  updateGithubChecksums,
  updateGithubOperations,
  updateGithubSettings,
  updateProjectContents,
} from '../../components/editor/actions/action-creators'
import {
  EditorStorePatched,
  GithubOperation,
  GithubRepo,
  PersistentModel,
  projectGithubSettings,
} from '../../components/editor/store/editor-state'
import { propOrNull } from './object-utils'
import { forceNotNull } from './optional-utils'
import { emptySet } from './set-utils'
import { trimUpToAndIncluding } from './string-utils'
import { arrayEquals } from './utils'
import { merge, mergeDiff3 } from 'node-diff3'
import { isTextFile, RevisionsState, textFile, textFileContents } from './project-file-types'

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

export interface GetBranchesBranch {
  name: string
}

export type GetBranchesResult = Array<GetBranchesBranch>

export interface GetBranchesSuccess {
  type: 'SUCCESS'
  branches: GetBranchesResult
}

export type GetBranchesResponse = GetBranchesSuccess | GithubFailure

export interface GetBranchContentSuccess {
  type: 'SUCCESS'
  content: ProjectContentTreeRoot
  originCommit: string
}

export type GetBranchContentResponse = GetBranchContentSuccess | GithubFailure

export interface RepositoryEntryPermissions {
  admin: boolean
  push: boolean
  pull: boolean
}

export interface RepositoryEntry {
  fullName: string
  avatarUrl: string | null
  private: boolean
  description: string | null
  name: string | null
  updatedAt: string | null
  defaultBranch: string | null
  permissions: RepositoryEntryPermissions
}

export interface GetUsersPublicRepositoriesSuccess {
  type: 'SUCCESS'
  repositories: Array<RepositoryEntry>
}

export type GetUsersPublicRepositoriesResponse = GetUsersPublicRepositoriesSuccess | GithubFailure

export async function saveProjectToGithub(
  projectID: string,
  persistentModel: PersistentModel,
  dispatch: EditorDispatch,
): Promise<void> {
  const operation: GithubOperation = { name: 'commish' }

  dispatch([updateGithubOperations(operation, 'add')], 'everyone')

  const url = urljoin(UTOPIA_BACKEND, 'github', 'save', projectID)

  const postBody = JSON.stringify(persistentModel)
  const response = await fetch(url, {
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
              ),
            ),
            updateBranchContents(persistentModel.projectContents),
            showToast(notice(`Saved to branch ${responseBody.branchName}.`, 'INFO')),
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

export async function getBranchesForGithubRepository(
  dispatch: EditorDispatch,
  githubRepo: GithubRepo,
): Promise<GetBranchesResponse> {
  const operation: GithubOperation = { name: 'listBranches' }

  dispatch([updateGithubOperations(operation, 'add')], 'everyone')

  const url = urljoin(UTOPIA_BACKEND, 'github', 'branches', githubRepo.owner, githubRepo.repository)

  const response = await fetch(url, {
    method: 'GET',
    credentials: 'include',
    headers: HEADERS,
    mode: MODE,
  })

  dispatch([updateGithubOperations(operation, 'remove')], 'everyone')

  if (response.ok) {
    const responseBody: GetBranchesResponse = await response.json()
    return responseBody
  } else {
    return {
      type: 'FAILURE',
      failureReason: 'Server error.',
    }
  }
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
      if (specificCommitContent.type === 'SUCCESS') {
        dispatch(
          [
            updateAgainstGithub(
              branchLatestContent.content,
              specificCommitContent.content,
              branchLatestContent.originCommit,
            ),
          ],
          'everyone',
        )
      } else {
        failWithReason(specificCommitContent.failureReason)
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

export async function updateProjectWithBranchContent(
  dispatch: EditorDispatch,
  githubRepo: GithubRepo,
  branchName: string,
  commitSha: string | null,
): Promise<void> {
  const operation: GithubOperation = {
    name: 'loadBranch',
    branchName: branchName,
    githubRepo: githubRepo,
  }

  dispatch([updateGithubOperations(operation, 'add')], 'everyone')

  const response = await getBranchContentFromServer(githubRepo, branchName, commitSha)

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
        dispatch(
          [
            updateGithubChecksums(getProjectContentsChecksums(responseBody.content)),
            updateProjectContents(responseBody.content),
            updateBranchContents(responseBody.content),
            updateGithubSettings(
              projectGithubSettings(githubRepo, responseBody.originCommit, branchName),
            ),
            showToast(notice(`Updated the project with the content from ${branchName}`, 'SUCCESS')),
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

export async function getUsersPublicGithubRepositories(
  dispatch: EditorDispatch,
  callback: (repositories: Array<RepositoryEntry>) => void,
): Promise<void> {
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
        dispatch(
          [
            showToast(
              notice(
                `Error when getting a user's repositories: ${responseBody.failureReason}`,
                'ERROR',
              ),
            ),
          ],
          'everyone',
        )
        break
      case 'SUCCESS':
        callback(responseBody.repositories)
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
  (projectContents, githubAuthenticated, githubChecksums): GithubFileChanges | null => {
    if (!githubAuthenticated) {
      return null
    }
    const checksums = getProjectContentsChecksums(projectContents)
    return deriveGithubFileChanges(checksums, githubChecksums)
  },
)

export type GithubFileStatus = 'modified' | 'deleted' | 'untracked' | 'conflict'

export interface GithubFileChanges {
  untracked: Array<string>
  modified: Array<string>
  deleted: Array<string>
}

export function getGithubFileChangesCount(changes: GithubFileChanges | null): number {
  if (changes == null) {
    return 0
  }
  return changes.untracked.length + changes.modified.length + changes.deleted.length
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
    arrayEquals(a.deleted, b.deleted)
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

export function mergeProjectContents(
  currentTree: ProjectContentTreeRoot,
  originTree: ProjectContentTreeRoot,
  branchTree: ProjectContentTreeRoot,
): ProjectContentTreeRoot {
  let keys: Set<string> = emptySet()
  Object.keys(currentTree).forEach(keys.add, keys)
  Object.keys(originTree).forEach(keys.add, keys)
  Object.keys(branchTree).forEach(keys.add, keys)

  let result: ProjectContentTreeRoot = {}
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
      if (combinedElement != null) {
        result[key] = combinedElement
      }
    }
  }

  return result
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
): ProjectContentsTree | null {
  if (originContents == null && branchContents == null) {
    // Origin and branch lack an entry, so go with whatever the Utopia project contains.
    return currentContents
  } else if (originContents == null && currentContents == null) {
    // Origin and Utopia project lack an entry, so go with whatever the branch contains.
    return branchContents
  } else if (
    isProjectContentFile(currentContents) &&
    isTextFile(currentContents.content) &&
    isProjectContentFile(originContents) &&
    isTextFile(originContents.content) &&
    isProjectContentFile(branchContents) &&
    isTextFile(branchContents.content)
  ) {
    // All 3 branches are a file.
    const mergedResult = mergeDiff3(
      currentContents.content.fileContents.code,
      originContents.content.fileContents.code,
      branchContents.content.fileContents.code,
      {
        label: { a: 'Your Changes', o: 'Original', b: 'Branch Changes' },
        stringSeparator: /[\r\n]+/,
      },
    ).result.join('\n')
    const updatedTextFile = textFile(
      textFileContents(
        mergedResult,
        originContents.content.fileContents.parsed,
        RevisionsState.CodeAhead,
      ),
      originContents.content.lastSavedContents,
      originContents.content.lastParseSuccess,
      Date.now(),
    )

    return projectContentFile(fullPath, updatedTextFile)
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
    return projectContentDirectory(fullPath, currentContents.directory, mergedResult)
  } else {
    // More cases in the future needed for tree conflicts at the least.
    // For now go with whatever the editor has.
    return currentContents
  }
}
