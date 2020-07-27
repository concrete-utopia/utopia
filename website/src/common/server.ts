import { UTOPIA_BACKEND, THUMBNAIL_ENDPOINT, ASSET_ENDPOINT, BASE_URL } from './env-vars'
import { ProjectListing } from './persistence'
import { LoginState, notLoggedIn } from './user'
// Stupid style of import because the website and editor are different
// and so there's no style of import which works with both projects.
const urljoin = require('url-join')

export const PROJECT_ENDPOINT = UTOPIA_BACKEND + 'project/'

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

export function thumbnailURL(projectId: string): string {
  return urljoin(THUMBNAIL_ENDPOINT, projectId)
}

export function imagePathWithoutHashURL(projectId: string, fileName: string): string {
  return urljoin(BASE_URL, 'project', projectId, fileName)
}

export function imagePathURL(fileName: string): string {
  return urljoin('./', fileName)
}

let CachedLoginStatePromise: Promise<LoginState> | null = null

export async function getLoginState(): Promise<LoginState> {
  if (CachedLoginStatePromise != null) {
    return CachedLoginStatePromise
  } else {
    const promise = createGetLoginStatePromise()
    CachedLoginStatePromise = promise
    return promise
  }
}

async function createGetLoginStatePromise(): Promise<LoginState> {
  const url = UTOPIA_BACKEND + 'user'
  const response = await fetch(url, {
    method: 'GET',
    credentials: 'include',
    headers: HEADERS,
    mode: MODE,
  })
  if (response.ok) {
    const result = await response.json()
    return result
  } else {
    // FIXME Client should show an error if server requests fail
    console.error(`Fetch user details failed (${response.status}): ${response.statusText}`)
    return notLoggedIn
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

function serverProjectListToProjectList(
  serverProjectList: Array<ServerProjectListing>,
): Array<ProjectListing> {
  return serverProjectList.map((listing) => {
    return {
      ...listing,
      thumbnail: thumbnailURL(listing.id),
    }
  })
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
    return serverProjectListToProjectList(responseBody.projects.reverse())
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
    return serverProjectListToProjectList(responseBody.projects)
  } else {
    // FIXME Client should show an error if server requests fail
    console.error(`Fetch showcase projects failed (${response.status}): ${response.statusText}`)
    return []
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
