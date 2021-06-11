import * as localforage from 'localforage'
import { deleteProjectFromServer } from './server'

export interface ProjectListing {
  id: string
  ownerName: string | null
  ownerPicture: string | null
  title: string
  description: string | null
  createdAt: string
  modifiedAt: string
  thumbnail: string
}

export const LOCAL_PROJECT_PREFIX = 'local-project-'

interface LocalProject {
  model: any // from the website's perspective we don't give a shit about this
  createdAt: string
  lastModified: string
  thumbnail: string
}

export function localProjectKey(projectId: string): string {
  return `${LOCAL_PROJECT_PREFIX}${projectId}`
}

export async function fetchLocalProject(projectId: string): Promise<LocalProject | null> {
  return localforage.getItem<LocalProject | null>(localProjectKey(projectId))
}

export async function fetchProjectListFromLocalStorage(): Promise<Array<ProjectListing>> {
  const allLocalKeys = await localforage.keys()
  const allLocalProjectKeys = allLocalKeys.filter((key) => key.startsWith(LOCAL_PROJECT_PREFIX))

  const projectListingPromises: Array<Promise<ProjectListing>> = allLocalProjectKeys.map(
    async (key) => {
      const projectId = key.split(LOCAL_PROJECT_PREFIX)[1]
      const localProject = await fetchLocalProject(projectId)

      if (localProject == null) {
        throw new Error(`Local project ${projectId} could not be loaded.`)
      } else {
        return {
          id: projectId,
          ownerName: null,
          ownerPicture: null,
          title: '',
          description: null,
          createdAt: localProject.createdAt,
          modifiedAt: localProject.lastModified,
          thumbnail: localProject.thumbnail,
        }
      }
    },
  )

  return Promise.all(projectListingPromises)
}

const AUTHED_REDIRECT_URL_KEY: string = 'authed-redirect-url'

export async function setRedirectUrl(redirectUrl: string): Promise<string> {
  return localforage.setItem(AUTHED_REDIRECT_URL_KEY, redirectUrl)
}

export async function getAndClearRedirectUrl(): Promise<string> {
  const redirectUrl = await localforage.getItem<string | null>(AUTHED_REDIRECT_URL_KEY)
  await localforage.removeItem(AUTHED_REDIRECT_URL_KEY)
  return redirectUrl == null ? '/projects' : redirectUrl
}

export async function deleteProject(projectId: string): Promise<void> {
  const localProject = await fetchLocalProject(projectId)
  if (localProject != null) {
    return localforage.removeItem(localProjectKey(projectId))
  } else {
    return deleteProjectFromServer(projectId)
  }
}
