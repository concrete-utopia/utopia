import { UTOPIA_BACKEND } from '../../common/env-vars'
import {
  assetURL,
  getLoginState,
  HEADERS,
  MODE,
  projectURL,
  thumbnailURL,
  userConfigURL,
} from '../../common/server'
import { imageFile, isImageFile } from '../../core/model/project-file-utils'
import { ImageFile } from '../../core/shared/project-file-types'
import { PersistentModel, UserConfiguration, emptyUserConfiguration } from './store/editor-state'
import { LoginState } from '../../uuiui-deps'
import urljoin = require('url-join')
import * as JSZip from 'jszip'
import { addFileToProjectContents, walkContentsTree } from '../assets'
import type { EditorDispatch } from './action-types'
import { setLoginState, showToast } from './actions/action-creators'
import { isLoginLost } from '../../common/user'
import { notice } from '../common/notice'

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

type LoadProjectResponse = ProjectLoaded | ProjectUnchanged | ProjectNotFound

interface SaveAssetResponse {
  id: string
}

interface SaveProjectRequest {
  name: string | null
  content: PersistentModel | null
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

async function saveAssetRequest(
  projectId: string,
  fileType: string,
  base64: string,
  fileName: string,
): Promise<void> {
  const asset = Buffer.from(base64, 'base64')
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
    return
  } else {
    throw new Error(`Save asset request failed (${response.status}): ${response.statusText}`)
  }
}

export async function saveAsset(
  projectId: string,
  fileType: string,
  base64: string,
  imageId: string,
): Promise<void> {
  try {
    return saveAssetRequest(projectId, fileType, base64, imageId)
  } catch (e) {
    // FIXME Client should show an error if server requests fail
    console.error(e)
    return
  }
}

interface AssetToSave {
  fileType: string
  base64: string
  fileName: string
}

export async function saveAssets(projectId: string, assets: Array<AssetToSave>): Promise<void> {
  const promises = assets.map((asset) =>
    saveAsset(projectId, asset.fileType, asset.base64, asset.fileName),
  )
  await Promise.all(promises)
  return
}

export async function saveImagesFromProject(
  projectId: string,
  model: PersistentModel,
): Promise<PersistentModel> {
  let promises: Array<Promise<{ contentId: string; projectContent: ImageFile }>> = []

  walkContentsTree(model.projectContents, (fullPath, projectContent) => {
    if (
      isImageFile(projectContent) &&
      projectContent.base64 != null &&
      projectContent.imageType != null
    ) {
      try {
        promises.push(
          saveAssetRequest(
            projectId,
            projectContent.imageType,
            projectContent.base64,
            fullPath,
          ).then(() => {
            return { contentId: fullPath, projectContent: projectContent }
          }),
        )
      } catch (e) {
        // FIXME Client should show an error if server requests fail
        console.error(e)
      }
    }
  })

  return Promise.all(promises).then((updatedFiles) => {
    const updatedProjectContents = updatedFiles.reduce(
      (workingProjectContents, { contentId, projectContent }) => {
        // Scrub the image type and base64
        const updatedFile = imageFile(
          undefined,
          undefined,
          projectContent.width,
          projectContent.height,
          projectContent.hash,
        )
        return addFileToProjectContents(workingProjectContents, contentId, updatedFile)
      },
      model.projectContents,
    )

    return {
      ...model,
      projectContents: updatedProjectContents,
    }
  })
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
    case 'NOT_LOGGED_IN':
    case 'LOGIN_LOST':
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
  const url = urljoin(UTOPIA_BACKEND, 'github', owner, repo)
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

export function startPollingLoginState(
  dispatch: EditorDispatch,
  initialLoginState: LoginState,
): void {
  let previousLoginState: LoginState = initialLoginState
  setInterval(async () => {
    const loginState = await getLoginState('no-cache')
    if (previousLoginState.type !== loginState.type) {
      dispatch([setLoginState(loginState)])
      if (isLoginLost(loginState)) {
        dispatch([showToast(notice(`You have been logged out.`, 'ERROR', true))])
      }
    }
    previousLoginState = loginState
  }, 5000)
}
