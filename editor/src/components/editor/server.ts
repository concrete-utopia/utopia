import { isBackendBFF, UTOPIA_BACKEND, UTOPIA_BACKEND_BASE_URL } from '../../common/env-vars'
import {
  assetURL,
  getLoginState,
  HEADERS,
  MODE,
  newProjectURL,
  projectURL,
  thumbnailURL,
  userConfigURL,
} from '../../common/server'
import type {
  GithubRepo,
  PersistentModel,
  UserConfiguration,
  UserPermissions,
} from './store/editor-state'
import { emptyUserConfiguration, emptyUserPermissions } from './store/editor-state'
import type { LoginState } from '../../uuiui-deps'
import urljoin from 'url-join'
import JSZip from 'jszip'
import type { AssetFileWithFileName } from '../assets'
import { gitBlobChecksumFromBuffer } from '../assets'
import { isLoginLost } from '../../common/user'
import { notice } from '../common/notice'
import type { EditorDispatch } from './action-types'
import { isLoggedIn } from './action-types'
import {
  setLoginState,
  showToast,
  removeToast,
  setUserConfiguration,
  setGithubState,
} from './actions/action-creators'
import type { GetBranchContentResponse } from '../../core/shared/github/helpers'
import {
  githubAPIErrorDataFromResponse,
  updateUserDetailsWhenAuthenticated,
} from '../../core/shared/github/helpers'
import { GithubAuth } from '../../utils/github-auth'
import type { User } from '../../../liveblocks.config'
import { liveblocksClient } from '../../../liveblocks.config'
import type { Collaborator } from '../../core/shared/multiplayer'
import type { LiveObject } from '@liveblocks/client'
import { projectIdToRoomId } from '../../utils/room-id'
import { assertNever } from '../../core/shared/utils'
import { checkOnlineState } from './online-status'
import type { GithubOperationContext } from '../../core/shared/github/operations/github-operation-context'
import { GithubEndpoints } from '../../core/shared/github/endpoints'

export { fetchProjectList, fetchShowcaseProjects, getLoginState } from '../../common/server'

export const PROJECT_ID_ENDPOINT = UTOPIA_BACKEND + 'projectid/'

interface CreateProjectResponse {
  id: string
}

export interface SaveProjectResponse {
  id: string
  ownerId: string
}

interface ProjectLoaded {
  type: 'ProjectLoaded'
  id: string
  ownerId: string
  title: string
  createdAt: string
  modifiedAt: string
  content: PersistentModel
}

interface ProjectUnchanged {
  type: 'ProjectUnchanged'
  id: string
}

interface ProjectNotFound {
  type: 'ProjectNotFound'
}

interface ProjectNotAuthorized {
  type: 'ProjectNotAuthorized'
}

export type LoadProjectResponse =
  | ProjectLoaded
  | ProjectUnchanged
  | ProjectNotFound
  | ProjectNotAuthorized

interface SaveAssetResponse {
  id: string
}

interface SaveProjectRequest {
  name: string | null
  content: PersistentModel | null
}

interface CreateProjectRequest {
  name: string | null
  content: PersistentModel | null
  accessLevel: string | null
}

interface RequestFailure {
  type: 'FAILURE'
  statusCode: number
  errorMessage: string
}

interface RequestSuccess<T> {
  type: 'SUCCESS'
  value: T
}

function requestFailure(statusCode: number, errorMessage: string): RequestFailure {
  return {
    type: 'FAILURE',
    statusCode: statusCode,
    errorMessage: errorMessage,
  }
}

function requestSuccess<T>(value: T): RequestSuccess<T> {
  return {
    type: 'SUCCESS',
    value: value,
  }
}

export type ServerResponse<T> = RequestSuccess<T> | RequestFailure

export function isRequestFailure<T>(response: ServerResponse<T>): response is RequestFailure {
  return response.type === 'FAILURE'
}

export function isRequestSuccess<T>(response: ServerResponse<T>): response is RequestSuccess<T> {
  return response.type === 'SUCCESS'
}

export async function createNewProjectID(): Promise<string> {
  // POSTs the persistent model as JSON body. receives a project ID
  const response = await fetch(PROJECT_ID_ENDPOINT, {
    method: 'POST',
    credentials: 'include',
    headers: HEADERS,
    mode: MODE,
  })
  if (response.ok) {
    const result: CreateProjectResponse = await response.json()
    return result.id
  } else {
    // FIXME Client should show an error if server requests fail
    throw new Error(
      `Create new project request failed (${response.status}): ${response.statusText}`,
    )
  }
}

export async function createNewProject(
  persistentModel: PersistentModel | null,
  name: string,
  accessLevel: string | null,
): Promise<SaveProjectResponse> {
  // PUTs the persistent model as JSON body.
  const url = newProjectURL()
  const bodyValue: CreateProjectRequest = {
    name: name,
    content: persistentModel,
    accessLevel: accessLevel ?? '',
  }
  const postBody = JSON.stringify(bodyValue)
  const response = await fetch(url, {
    method: 'PUT',
    credentials: 'include',
    body: postBody,
    headers: HEADERS,
    mode: MODE,
  })
  if (response.ok) {
    return response.json()
  } else {
    // FIXME Client should show an error if server requests fail
    throw new Error(`New project creation failed (${response.status}): ${response.statusText}`)
  }
}

export async function updateSavedProject(
  projectId: string,
  persistentModel: PersistentModel | null,
  name: string,
): Promise<SaveProjectResponse> {
  // PUTs the persistent model as JSON body.
  const url = projectURL(projectId)
  const bodyValue: SaveProjectRequest = {
    name: name,
    content: persistentModel,
  }
  const postBody = JSON.stringify(bodyValue)
  const response = await fetch(url, {
    method: 'PUT',
    credentials: 'include',
    body: postBody,
    headers: HEADERS,
    mode: MODE,
  })
  if (response.ok) {
    return response.json()
  } else {
    // FIXME Client should show an error if server requests fail
    throw new Error(`Save project request failed (${response.status}): ${response.statusText}`)
  }
}

export async function loadProject(
  projectId: string,
  lastSavedTS: string | null = null,
): Promise<LoadProjectResponse> {
  // GETs the persistent model as a JSON body
  const baseUrl = projectURL(projectId)
  const url = lastSavedTS == null ? baseUrl : `${baseUrl}?last_saved=${lastSavedTS}`
  const response = await fetch(url, {
    method: 'GET',
    credentials: 'include',
    headers: HEADERS,
    mode: MODE,
  })
  if (response.ok) {
    return response.json()
  } else if (response.status === 404) {
    return { type: 'ProjectNotFound' }
  } else if (response.status === 403) {
    return { type: 'ProjectNotAuthorized' }
  } else {
    // FIXME Client should show an error if server requests fail
    throw new Error(`server responded with ${response.status} ${response.statusText}`)
  }
}

export async function updateAssetFileName(
  projectId: string,
  oldFileName: string,
  newFileName: string,
): Promise<void> {
  const baseUrl = assetURL(projectId, newFileName)
  const url = `${baseUrl}?old_file_name=${oldFileName}`
  const response = await fetch(url, {
    method: 'PUT',
    credentials: 'include',
    headers: HEADERS,
    mode: MODE,
  })
  if (response.ok) {
    return
  } else {
    throw new Error(
      `Update asset file name request failed (${response.status}): ${response.statusText}`,
    )
  }
}

export async function deleteAssetFile(projectId: string, fileName: string): Promise<void> {
  const url = assetURL(projectId, fileName)
  const response = await fetch(url, {
    method: 'DELETE',
    credentials: 'include',
    headers: HEADERS,
    mode: MODE,
  })
  if (response.ok) {
    return
  } else {
    throw new Error(`Delete asset request failed (${response.status}): ${response.statusText}`)
  }
}

function getMimeStrippedBase64(base64: string): string {
  const splitBase64 = base64.split(',')
  switch (splitBase64.length) {
    case 1:
      // No mime prefix.
      return base64
    case 2:
      // Mime prefix.
      return splitBase64[1]
    default:
      throw new Error('Invalid Base64 content for asset.')
  }
}

async function saveAssetRequest(
  projectId: string,
  fileType: string,
  base64: string,
  fileName: string,
): Promise<string> {
  const mimeStrippedBase64 = getMimeStrippedBase64(base64)
  const asset = Buffer.from(mimeStrippedBase64, 'base64')
  const url = assetURL(projectId, fileName)
  const response = await fetch(url, {
    method: 'POST',
    credentials: 'include',
    headers: {
      'Content-Type': fileType,
    },
    body: asset,
  })
  if (response.ok) {
    return gitBlobChecksumFromBuffer(asset)
  } else {
    throw new Error(`Save asset request failed (${response.status}): ${response.statusText}`)
  }
}

export async function saveAsset(
  projectId: string,
  fileType: string,
  base64: string,
  imageId: string,
): Promise<string | null> {
  try {
    return await saveAssetRequest(projectId, fileType, base64, imageId)
  } catch (e) {
    // FIXME Client should show an error if server requests fail
    console.error(e)
    return null
  }
}

export interface AssetToSave {
  fileType: string
  base64: string
  fileName: string
}

export function assetToSave(fileType: string, base64: string, fileName: string): AssetToSave {
  return {
    fileType: fileType,
    base64: base64,
    fileName: fileName,
  }
}

export async function saveAssets(
  projectId: string,
  assets: Array<AssetToSave>,
): Promise<Array<string | null>> {
  const promises = assets.map((asset) =>
    saveAsset(projectId, asset.fileType, asset.base64, asset.fileName),
  )
  return Promise.all(promises)
}

export async function saveThumbnail(thumbnail: Buffer, projectId: string): Promise<void> {
  const url = thumbnailURL(projectId)
  const response = await fetch(url, {
    method: 'POST',
    credentials: 'include',
    headers: {
      'Content-Type': 'image/png',
    },
    body: thumbnail,
  })
  if (response.ok) {
    return
  } else {
    // FIXME Client should show an error if server requests fail
    console.error(`Save thumbnail request failed (${response.status}): ${response.statusText}`)
    return
  }
}

export async function getUserPermissions(
  loginState: LoginState,
  projectId: string | null,
): Promise<UserPermissions> {
  if (!isBackendBFF()) {
    return emptyUserPermissions(true)
  }
  switch (loginState.type) {
    case 'LOGGED_IN':
      if (projectId == null) {
        return emptyUserPermissions(false)
      }
      const permissionsUrl = UTOPIA_BACKEND_BASE_URL + `internal/projects/${projectId}/permissions`
      const response = await fetch(permissionsUrl, {
        method: 'GET',
        credentials: 'include',
        headers: HEADERS,
        mode: MODE,
      })
      if (response.ok) {
        return response.json()
      } else {
        // FIXME Client should show an error if server requests fail
        throw new Error(`server responded with ${response.status} ${response.statusText}`)
      }
    case 'LOGIN_NOT_YET_KNOWN':
    case 'NOT_LOGGED_IN':
    case 'LOGIN_LOST':
    case 'OFFLINE_STATE':
    case 'COOKIES_OR_LOCALFORAGE_UNAVAILABLE':
      return emptyUserPermissions(false)
    default:
      assertNever(loginState)
  }
}

export async function getUserConfiguration(loginState: LoginState): Promise<UserConfiguration> {
  switch (loginState.type) {
    case 'LOGGED_IN':
      const url = userConfigURL()
      const response = await fetch(url, {
        method: 'GET',
        credentials: 'include',
        headers: HEADERS,
        mode: MODE,
      })
      if (response.ok) {
        return response.json()
      } else {
        // FIXME Client should show an error if server requests fail
        throw new Error(`server responded with ${response.status} ${response.statusText}`)
      }
    case 'LOGIN_NOT_YET_KNOWN':
    case 'NOT_LOGGED_IN':
    case 'LOGIN_LOST':
    case 'OFFLINE_STATE':
    case 'COOKIES_OR_LOCALFORAGE_UNAVAILABLE':
      return emptyUserConfiguration()
    default:
      const _exhaustiveCheck: never = loginState
      throw new Error(`Unknown login state.`)
  }
}

export async function saveUserConfiguration(userConfig: UserConfiguration): Promise<void> {
  const url = userConfigURL()
  const response = await fetch(url, {
    method: 'POST',
    credentials: 'include',
    headers: HEADERS,
    mode: MODE,
    body: JSON.stringify(userConfig),
  })
  if (response.ok) {
    return
  } else {
    // FIXME Client should show an error if server requests fail
    throw new Error(`server responded with ${response.status} ${response.statusText}`)
  }
}

export async function downloadGithubRepo(
  owner: string,
  repo: string,
): Promise<ServerResponse<JSZip>> {
  const url = urljoin(UTOPIA_BACKEND, 'github', 'import', owner, repo)
  const response = await fetch(url, {
    method: 'GET',
    credentials: 'include',
    mode: MODE,
  })
  if (response.ok) {
    const buffer = await response.arrayBuffer()
    const zipFile = await JSZip.loadAsync(buffer)
    return requestSuccess(zipFile)
  } else {
    return requestFailure(
      response.status,
      `Download github repo request failed: ${response.statusText}`,
    )
  }
}

const loginLostNoticeID: string = 'login-lost-notice'

export function startPollingLoginState(
  dispatch: EditorDispatch,
  initialLoginState: LoginState,
): void {
  let previousLoginState: LoginState = initialLoginState
  setInterval(async () => {
    const isOnline = await checkOnlineState()
    if (isOnline) {
      const loginState = await getLoginState('no-cache')
      if (previousLoginState.type !== loginState.type) {
        if (isLoggedIn(loginState)) {
          // Fetch the user configuration
          void getUserConfiguration(loginState).then((userConfig) =>
            dispatch([setUserConfiguration(userConfig)]),
          )

          // Fetch the github auth status
          void updateUserDetailsWhenAuthenticated(
            dispatch,
            GithubAuth.isAuthenticatedWithGithub(loginState),
          ).then((authenticatedWithGithub) =>
            dispatch([
              setGithubState({
                authenticated: authenticatedWithGithub,
              }),
            ]),
          )

          if (isLoginLost(previousLoginState)) {
            // Login was lost and subsequently regained so remove the persistent toast.
            dispatch([removeToast(loginLostNoticeID)])
          }
        }

        dispatch([setLoginState(loginState)])

        if (isLoginLost(loginState)) {
          dispatch([
            showToast(
              notice(
                `You have been logged out. You can continue working, but your work won't be saved until you log in again.`,
                'ERROR',
                true,
                loginLostNoticeID,
              ),
            ),
          ])
        }
      }
      previousLoginState = loginState
    }
  }, 5000)
}

async function extractBase64FromBlob(blob: Blob): Promise<string> {
  return new Promise((resolve, reject) => {
    const reader = new FileReader()

    reader.onload = async () => {
      resolve(reader.result as string)
    }
    reader.onerror = (error) => {
      reject(error)
    }
    reader.readAsDataURL(blob)
  })
}

async function downloadAssetFromProject(
  projectId: string,
  fileWithName: AssetFileWithFileName,
): Promise<AssetFileWithFileName> {
  if (fileWithName.file.base64 != undefined) {
    return fileWithName
  } else {
    if (window.top == null) {
      throw new Error(`Failed downloading asset: window.top is null`)
    }
    const baseUrl = window.top.location.origin
    const assetUrl = urljoin(baseUrl, 'p', projectId, fileWithName.fileName)
    const assetResponse = await fetch(assetUrl, {
      method: 'GET',
      credentials: 'include',
    })
    if (assetResponse.ok) {
      const blob = await assetResponse.blob()
      const base64 = await extractBase64FromBlob(blob)

      return {
        ...fileWithName,
        file: {
          ...fileWithName.file,
          base64: base64,
        },
      }
    } else {
      console.error(
        `Failed to retrieve asset ${fileWithName.fileName} (${assetResponse.status}): ${assetResponse.statusText}`,
      )
      return fileWithName
    }
  }
}

export async function downloadAssetsFromProject(
  projectId: string | null,
  allProjectAssets: Array<AssetFileWithFileName>,
): Promise<Array<AssetFileWithFileName>> {
  if (projectId == null) {
    return allProjectAssets
  } else {
    const allPromises = allProjectAssets.map((asset) => downloadAssetFromProject(projectId, asset))
    return Promise.all(allPromises)
  }
}

export async function updateCollaborators(projectId: string) {
  if (!isBackendBFF()) {
    return
  }
  const response = await fetch(
    UTOPIA_BACKEND_BASE_URL + `internal/projects/${projectId}/collaborators`,
    {
      method: 'POST',
      credentials: 'include',
      mode: MODE,
    },
  )
  if (!response.ok) {
    throw new Error(`Update collaborators failed (${response.status}): ${response.statusText}`)
  }
}

export async function getCollaborators(projectId: string): Promise<Collaborator[]> {
  if (!isBackendBFF()) {
    return getCollaboratorsFromLiveblocks(projectId)
  }

  const response = await fetch(
    UTOPIA_BACKEND_BASE_URL + `internal/projects/${projectId}/collaborators`,
    {
      method: 'GET',
      credentials: 'include',
      mode: MODE,
    },
  )
  if (response.ok) {
    const result: Collaborator[] = await response.json()
    return result
  } else {
    throw new Error(`Load collaborators failed (${response.status}): ${response.statusText}`)
  }
}

// TODO remove this once the BFF is on
async function getCollaboratorsFromLiveblocks(projectId: string): Promise<Collaborator[]> {
  const room = liveblocksClient.getRoom(projectIdToRoomId(projectId))
  if (room == null) {
    return []
  }
  const storage = await room.getStorage()
  const collabs = storage.root.get('collaborators') as LiveObject<{ [userId: string]: User }>
  if (collabs == null) {
    return []
  }
  return Object.values(collabs.toObject()).map((u) => u.toObject())
}

export async function requestProjectAccess(projectId: string): Promise<void> {
  if (!isBackendBFF()) {
    return
  }
  const response = await fetch(`/internal/projects/${projectId}/access/request`, {
    method: 'POST',
    credentials: 'include',
    mode: MODE,
  })
  if (!response.ok) {
    throw new Error(`Request project access failed (${response.status}): ${response.statusText}`)
  }
}

export async function updateGithubRepository(
  projectId: string,
  githubRepository: (GithubRepo & { branch: string | null }) | null,
): Promise<void> {
  if (!isBackendBFF()) {
    return
  }
  const url = urljoin(`/internal/projects/${projectId}/github/repository/update`)
  const response = await fetch(url, {
    method: 'POST',
    credentials: 'include',
    headers: HEADERS,
    mode: MODE,
    body: JSON.stringify({ githubRepository: githubRepository }),
  })
  if (!response.ok) {
    throw new Error(`Update Github repository failed (${response.status}): ${response.statusText}`)
  }
}

export async function requestSearchPublicGithubRepository(
  operationContext: GithubOperationContext,
  params: {
    owner: string
    repo: string
  },
): Promise<Response> {
  const url = GithubEndpoints.searchRepository()

  return operationContext.fetch(url, {
    method: 'POST',
    credentials: 'include',
    headers: HEADERS,
    mode: MODE,
    body: JSON.stringify({ owner: params.owner, repo: params.repo }),
  })
}

export type ExistingAsset = {
  gitBlobSha?: string
  path: string
  type: string
}

type GetBranchProjectContentsRequest = {
  type: 'GET_BRANCH_PROJECT_CONTENTS_REQUEST'
  existingAssets: ExistingAsset[] | null
  uploadAssets: boolean
}

function getBranchProjectContentsRequest(
  params: Omit<GetBranchProjectContentsRequest, 'type'>,
): GetBranchProjectContentsRequest {
  return {
    type: 'GET_BRANCH_PROJECT_CONTENTS_REQUEST',
    ...params,
  }
}

export function getBranchProjectContents(operationContext: GithubOperationContext) {
  return async function (params: {
    projectId: string
    owner: string
    repo: string
    branch: string
    existingAssets: ExistingAsset[]
  }): Promise<GetBranchContentResponse> {
    const url = GithubEndpoints.getBranchProjectContents({
      projectId: params.projectId,
      owner: params.owner,
      repo: params.repo,
      branch: params.branch,
    })
    const response = await operationContext.fetch(url, {
      method: 'POST',
      credentials: 'include',
      headers: HEADERS,
      mode: MODE,
      body: JSON.stringify(
        getBranchProjectContentsRequest({
          existingAssets: params.existingAssets,
          uploadAssets: true,
        }),
      ),
    })
    if (!response.ok) {
      const reason = await githubAPIErrorDataFromResponse(response)
      return {
        type: 'FAILURE',
        failureReason: reason ?? `Github operation failed: ${response.status}`,
      }
    }
    return response.json()
  }
}
