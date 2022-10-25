import { UTOPIA_BACKEND } from '../../common/env-vars'
import urljoin from 'url-join'
import {
  GithubOperation,
  GithubRepo,
  PersistentModel,
  projectGithubSettings,
} from '../../components/editor/store/editor-state'
import { trimUpToAndIncluding } from './string-utils'
import { HEADERS, MODE } from '../../common/server'
import { EditorDispatch } from '../../components/editor/action-types'
import { notice } from '../../components/common/notice'
import {
  showToast,
  updateGithubSettings,
  updateProjectContents,
  updateGithubOperations,
} from '../../components/editor/actions/action-creators'
import { ProjectContentTreeRoot } from '../../components/assets'

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

export interface RepositoryEntry {
  fullName: string
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
            updateGithubSettings(
              projectGithubSettings(
                persistentModel.githubSettings.targetRepository,
                responseBody.newCommit,
              ),
            ),
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

export async function getBranchContent(
  dispatch: EditorDispatch,
  githubRepo: GithubRepo,
  projectID: string,
  branchName: string,
): Promise<void> {
  const operation: GithubOperation = { name: 'loadBranch', branchName: branchName }

  dispatch([updateGithubOperations(operation, 'add')], 'everyone')

  const url = urljoin(
    UTOPIA_BACKEND,
    'github',
    'branches',
    githubRepo.owner,
    githubRepo.repository,
    branchName,
  )
  const searchParams = new URLSearchParams({
    project_id: projectID,
  })

  const response = await fetch(`${url}?${searchParams}`, {
    method: 'POST',
    credentials: 'include',
    headers: HEADERS,
    mode: MODE,
  })

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
            updateProjectContents(responseBody.content),
            updateGithubSettings(projectGithubSettings(githubRepo, responseBody.originCommit)),
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

export async function getUsersPublicGithubRepositories(
  dispatch: EditorDispatch,
  callback: (repositories: Array<RepositoryEntry>) => void,
): Promise<void> {
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
}
