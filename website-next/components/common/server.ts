import * as localforage from 'localforage'
import { UTOPIA_BACKEND, THUMBNAIL_ENDPOINT, ASSET_ENDPOINT, BASE_URL } from './env-vars'
import { ProjectListing } from './persistence'
import {
  cookiesOrLocalForageUnavailable,
  isLoggedIn,
  isLoginLost,
  loginLost,
  LoginState,
  offlineState,
} from './user'
// Stupid style of import because the website and editor are different
// and so there's no style of import which works with both projects.
const urljoin = require('url-join')

export const PROJECT_ENDPOINT = UTOPIA_BACKEND + 'project/'
export const PROJECT_EDITOR = BASE_URL + 'project'

// if we want to enable CORS, we need the server to be able to answer to preflight OPTION requests with the proper Allow-Access headers
// until then, this is keeping us safe from attempting a CORS request that results in cryptic error messages
export const MODE = 'same-origin'

export const HEADERS = {
  Accept: 'application/json',
  'Content-Type': 'application/json',
}

export interface ProjectOwnerResponse {
  isOwner: boolean
}

export type ProjectOwnerState = ProjectOwnerResponse | 'unowned'

export interface ServerProjectListing {
  id: string
  ownerName: string | null
  ownerPicture: string | null
  title: string
  description: string | null
  createdAt: string
  modifiedAt: string
}

export interface FetchProjectListResponse {
  projects: Array<ServerProjectListing>
}

export function assetURL(projectId: string, fileName: string): string {
  return urljoin(ASSET_ENDPOINT, projectId, fileName)
}

export function projectURL(projectId: string): string {
  return urljoin(PROJECT_ENDPOINT, projectId)
}

export function projectEditorURL(projectId: string): string {
  return urljoin(PROJECT_EDITOR, projectId)
}

export function thumbnailURL(projectId: string): string {
  return urljoin(THUMBNAIL_ENDPOINT, projectId)
}

export function imagePathWithoutHashURL(projectId: string, fileName: string): string {
  return urljoin(BASE_URL, 'project', projectId, fileName)
}

export function imagePathURL(fileName: string): string {
  return urljoin('./', fileName)
}

export function userConfigURL(): string {
  return urljoin(UTOPIA_BACKEND, 'user', 'config')
}

let CachedLoginStatePromise: Promise<LoginState> | null = null

export async function getLoginState(useCache: 'cache' | 'no-cache'): Promise<LoginState> {
  if (useCache === 'cache' && CachedLoginStatePromise != null) {
    return CachedLoginStatePromise
  } else {
    const promise = createGetLoginStatePromise(CachedLoginStatePromise)
    CachedLoginStatePromise = promise
    return promise
  }
}

let localForageAvailableResult: boolean | null = null

export async function isLocalForageAvailable(): Promise<boolean> {
  if (localForageAvailableResult == null) {
    const result = await localforage
      .keys()
      .then(() => true)
      .catch(() => false)
    localForageAvailableResult = result
    return result
  } else {
    return localForageAvailableResult
  }
}

async function areCookiesAndLocalForageAvailable(): Promise<boolean> {
  const localForageAvailable = await isLocalForageAvailable()
  // eslint-disable-next-line no-restricted-globals
  return localForageAvailable && navigator.cookieEnabled
}

async function createGetLoginStatePromise(
  previousLogin: Promise<LoginState> | null,
): Promise<LoginState> {
  try {
    const cookiesAndLocalForageAvailable = await areCookiesAndLocalForageAvailable()
    if (cookiesAndLocalForageAvailable) {
      const url = UTOPIA_BACKEND + 'user'
      const response = await fetch(url, {
        method: 'GET',
        credentials: 'include',
        headers: HEADERS,
        mode: MODE,
      })
      if (response.ok) {
        const result = await response.json()
        const previousLoginState = previousLogin == null ? null : await previousLogin
        return getLoginStateFromResponse(result, previousLoginState)
      } else {
        console.error(`Fetch user details failed (${response.status}): ${response.statusText}`)
        return loginLost
      }
    } else {
      return cookiesOrLocalForageUnavailable
    }
  } catch (e) {
    console.error(`Fetch user details failed: ${e}`)
    return offlineState
  }
}

function getLoginStateFromResponse(
  response: any,
  previousLoginState: LoginState | null,
): LoginState {
  if (
    !isLoggedIn(response) &&
    (isLoggedIn(previousLoginState) || isLoginLost(previousLoginState))
  ) {
    // if we used to be logged in but we are not anymore, return a LoginLost
    return loginLost
  } else {
    return response
  }
}

export async function checkProjectOwnership(projectId: string): Promise<ProjectOwnerState> {
  const url = `${projectURL(projectId)}/owner`
  const response = await fetch(url, {
    method: 'GET',
    credentials: 'include',
    headers: HEADERS,
    mode: MODE,
  })
  if (response.ok) {
    return response.json()
  } else if (response.status === 404) {
    return 'unowned'
  } else {
    // FIXME Client should show an error if server requests fail
    console.error(`server responded with ${response.status} ${response.statusText}`)
    return {
      isOwner: false,
    }
  }
}

function serverProjectListingToProjectListing(
  serverProjectListing: ServerProjectListing,
): ProjectListing {
  return {
    ...serverProjectListing,
    thumbnail: thumbnailURL(serverProjectListing.id),
  }
}

function serverProjectsListToProjectsList(
  serverProjectList: Array<ServerProjectListing>,
): Array<ProjectListing> {
  return serverProjectList.map(serverProjectListingToProjectListing)
}

export async function fetchProjectList(): Promise<Array<ProjectListing>> {
  // GETs the list of all project IDs for a user
  const url = UTOPIA_BACKEND + 'projects/'
  const response = await fetch(url, {
    method: 'GET',
    credentials: 'include',
    headers: HEADERS,
    mode: MODE,
  })
  if (response.ok) {
    const responseBody: FetchProjectListResponse = await response.json()
    return serverProjectsListToProjectsList(responseBody.projects.reverse())
  } else {
    // FIXME Client should show an error if server requests fail
    console.error(`Fetch project list failed (${response.status}): ${response.statusText}`)
    return []
  }
}

export async function fetchShowcaseProjects(): Promise<Array<ProjectListing>> {
  // GETs the list of all project IDs for a user
  const url = UTOPIA_BACKEND + 'showcase/'
  const response = await fetch(url, {
    method: 'GET',
    credentials: 'include',
    headers: HEADERS,
    mode: MODE,
  })
  if (response.ok) {
    const responseBody: FetchProjectListResponse = await response.json()
    return serverProjectsListToProjectsList(responseBody.projects)
  } else {
    // FIXME Client should show an error if server requests fail
    console.error(`Fetch showcase projects failed (${response.status}): ${response.statusText}`)
    return []
  }
}

export async function fetchProjectMetadata(projectId: string): Promise<ProjectListing | null> {
  // GETs the metadata for a given project ID
  const url = urljoin(UTOPIA_BACKEND, 'project', projectId, 'metadata')
  const response = await fetch(url, {
    method: 'GET',
    credentials: 'include',
    headers: HEADERS,
    mode: MODE,
  })
  if (response.ok) {
    const responseBody: ServerProjectListing = await response.json()
    return serverProjectListingToProjectListing(responseBody)
  } else {
    return null
  }
}

export async function deleteProjectFromServer(projectId: string): Promise<void> {
  const url = projectURL(projectId)
  const response = await fetch(url, {
    method: 'DELETE',
    credentials: 'include',
    headers: HEADERS,
    mode: MODE,
  })
  if (response.ok) {
    return
  } else {
    console.error(
      `Failed to delete project ${projectId} (${response.status}): ${response.statusText}`,
    )
  }
}
