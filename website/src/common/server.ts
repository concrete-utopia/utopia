import { UTOPIA_BACKEND, THUMBNAIL_ENDPOINT, ASSET_ENDPOINT, BASE_URL } from './env-vars'
import type { ProjectListing } from './persistence'
import { LoginState, notLoggedIn } from './user'
// Stupid style of import because the website and editor are different
// and so there's no style of import which works with both projects.
import urljoin from 'url-join'

const PROJECT_ENDPOINT = (productionConfig: boolean) =>
  UTOPIA_BACKEND(productionConfig) + 'project/'

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
  title: string
  createdAt: string
  modifiedAt: string
}

export interface FetchProjectListResponse {
  projects: Array<ServerProjectListing>
}

export function assetURL(productionConfig: boolean, projectId: string, fileName: string): string {
  return urljoin(ASSET_ENDPOINT(productionConfig), projectId, fileName)
}

export function projectURL(productionConfig: boolean, projectId: string): string {
  return urljoin(PROJECT_ENDPOINT(productionConfig), projectId)
}

export function thumbnailURL(productionConfig: boolean, projectId: string): string {
  return urljoin(THUMBNAIL_ENDPOINT(productionConfig), projectId)
}

export function imagePathWithoutHashURL(
  productionConfig: boolean,
  projectId: string,
  fileName: string,
): string {
  return urljoin(BASE_URL(productionConfig), 'project', projectId, fileName)
}

export function imagePathURL(fileName: string): string {
  return urljoin('./', fileName)
}

let CachedLoginStatePromise: Promise<LoginState> | null = null

export async function getLoginState(productionConfig: boolean): Promise<LoginState> {
  if (CachedLoginStatePromise != null) {
    return CachedLoginStatePromise
  } else {
    const promise = createGetLoginStatePromise(productionConfig)
    CachedLoginStatePromise = promise
    return promise
  }
}

async function createGetLoginStatePromise(productionConfig: boolean): Promise<LoginState> {
  const url = UTOPIA_BACKEND(productionConfig) + 'user'
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

export async function checkProjectOwnership(
  productionConfig: boolean,
  projectId: string,
): Promise<ProjectOwnerState> {
  const url = `${projectURL(productionConfig, projectId)}/owner`
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
  productionConfig: boolean,
  serverProjectList: Array<ServerProjectListing>,
): Array<ProjectListing> {
  return serverProjectList.map((listing) => {
    return {
      ...listing,
      thumbnail: thumbnailURL(productionConfig, listing.id),
    }
  })
}

export async function fetchProjectList(productionConfig: boolean): Promise<Array<ProjectListing>> {
  // GETs the list of all project IDs for a user
  const url = UTOPIA_BACKEND(productionConfig) + 'projects/'
  const response = await fetch(url, {
    method: 'GET',
    credentials: 'include',
    headers: HEADERS,
    mode: MODE,
  })
  if (response.ok) {
    const responseBody: FetchProjectListResponse = await response.json()
    return serverProjectListToProjectList(productionConfig, responseBody.projects.reverse())
  } else {
    // FIXME Client should show an error if server requests fail
    console.error(`Fetch project list failed (${response.status}): ${response.statusText}`)
    return []
  }
}

export async function fetchShowcaseProjects(
  productionConfig: boolean,
): Promise<Array<ProjectListing>> {
  // GETs the list of all project IDs for a user
  const url = UTOPIA_BACKEND(productionConfig) + 'showcase/'
  const response = await fetch(url, {
    method: 'GET',
    credentials: 'include',
    headers: HEADERS,
    mode: MODE,
  })
  if (response.ok) {
    const responseBody: FetchProjectListResponse = await response.json()
    return serverProjectListToProjectList(productionConfig, responseBody.projects)
  } else {
    // FIXME Client should show an error if server requests fail
    console.error(`Fetch showcase projects failed (${response.status}): ${response.statusText}`)
    return []
  }
}

export async function deleteProjectFromServer(
  productionConfig: boolean,
  projectId: string,
): Promise<void> {
  const url = projectURL(productionConfig, projectId)
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
