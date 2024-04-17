import * as React from 'react'
import type { ProjectListing } from '../../../common/persistence'
import { fetchProjectMetadata } from '../../../common/server'
import type { EditorDispatch } from '../action-types'
import { updateProjectServerState } from '../actions/action-creators'
import { checkProjectOwned, projectIsStoredLocally } from '../persistence/persistence-backend'
import type { ProjectOwnership } from '../persistence/generic/persistence-types'
import { CollaborationEndpoints } from '../collaborative-endpoints'
import { checkOnlineState } from '../online-status'

export interface ProjectMetadataFromServer {
  title: string
  ownerName: string | null
  ownerPicture: string | null
  hasPendingRequests?: boolean
}

export function projectMetadataFromServer(
  title: string,
  ownerName: string | null,
  ownerPicture: string | null,
  hasPendingRequests?: boolean,
): ProjectMetadataFromServer {
  return {
    title: title,
    ownerName: ownerName,
    ownerPicture: ownerPicture,
    hasPendingRequests: hasPendingRequests,
  }
}

export type IsMyProject = 'yes' | 'no' | 'unknown'

export interface ProjectServerState {
  isMyProject: IsMyProject
  ownerId: string | null
  projectData: ProjectMetadataFromServer | null
  forkedFromProjectData: ProjectMetadataFromServer | null
  currentlyHolderOfTheBaton: boolean
}

export function projectServerState(
  isMyProject: ProjectServerState['isMyProject'],
  ownerId: string | null,
  projectData: ProjectMetadataFromServer | null,
  forkedFromProjectData: ProjectMetadataFromServer | null,
  currentlyHolderOfTheBaton: boolean,
): ProjectServerState {
  return {
    isMyProject: isMyProject,
    ownerId: ownerId,
    projectData: projectData,
    forkedFromProjectData: forkedFromProjectData,
    currentlyHolderOfTheBaton: currentlyHolderOfTheBaton,
  }
}

export function emptyProjectServerState(): ProjectServerState {
  return projectServerState('unknown', null, null, null, false)
}

export const ProjectServerStateContext = React.createContext<ProjectServerState>(
  emptyProjectServerState(),
)

function projectListingToProjectMetadataFromServer(
  projectListing: ProjectListing | null,
): ProjectMetadataFromServer | null {
  if (projectListing == null) {
    return null
  } else {
    return {
      title: projectListing.title,
      ownerName: projectListing.ownerName ?? null,
      ownerPicture: projectListing.ownerPicture ?? null,
      hasPendingRequests: projectListing.hasPendingRequests,
    }
  }
}

export async function getProjectServerState(
  loggedIn: boolean,
  dispatch: EditorDispatch,
  projectId: string | null,
  forkedFromProjectId: string | null,
): Promise<ProjectServerState | null> {
  // If the editor is offline, then skip this lookup for the moment.
  const isOnline = await checkOnlineState()
  if (!isOnline) {
    return null
  }
  const existsLocally = projectId == null ? true : await projectIsStoredLocally(projectId)
  const projectListing =
    projectId == null || existsLocally ? null : await fetchProjectMetadata(projectId)
  const forkedFromProjectListing =
    forkedFromProjectId == null || existsLocally
      ? null
      : await fetchProjectMetadata(forkedFromProjectId)

  async function getOwnership(): Promise<ProjectOwnership> {
    if (projectId == null) {
      return { isOwner: true, ownerId: null }
    }
    const { isOwner, ownerId } = await checkProjectOwned(loggedIn, projectId)
    return { isOwner, ownerId }
  }
  const ownership = await getOwnership()

  async function getHolderOfTheBaton(): Promise<boolean> {
    if (!loggedIn) {
      return false
    } else if (ownership.isOwner) {
      return CollaborationEndpoints.claimControlOverProject(projectId)
        .then((result) => {
          return result ?? ownership.isOwner
        })
        .catch(() => {
          CollaborationEndpoints.displayControlErrorToast(
            dispatch,
            'Error while attempting to claim control over this project.',
          )
          return false
        })
    } else {
      return false
    }
  }

  const holderOfTheBaton = await getHolderOfTheBaton()

  return {
    isMyProject: ownership.isOwner ? 'yes' : 'no',
    ownerId: ownership.ownerId,
    projectData: projectListingToProjectMetadataFromServer(projectListing),
    forkedFromProjectData: projectListingToProjectMetadataFromServer(forkedFromProjectListing),
    currentlyHolderOfTheBaton: holderOfTheBaton,
  }
}

type SuccessOrFailure = 'success' | 'failure'

let updateProjectServerStateInStoreRunCount: number = 0

export function getUpdateProjectServerStateInStoreRunCount(): number {
  return updateProjectServerStateInStoreRunCount
}

export async function updateProjectServerStateInStore(
  loggedIn: boolean,
  projectId: string | null,
  forkedFromProjectId: string | null,
  dispatch: EditorDispatch,
): Promise<SuccessOrFailure> {
  return getProjectServerState(loggedIn, dispatch, projectId, forkedFromProjectId)
    .then<SuccessOrFailure, SuccessOrFailure>((serverState) => {
      if (serverState != null) {
        dispatch([updateProjectServerState(serverState)], 'everyone')
      }
      return 'success'
    })
    .catch<SuccessOrFailure>((error) => {
      console.error('Error while updating server state.', error)
      return 'failure'
    })
    .finally(() => {
      updateProjectServerStateInStoreRunCount++
    })
}

export interface ProjectServerStateUpdaterProps {
  projectId: string | null
  forkedFromProjectId: string | null
  loggedIn: boolean
  dispatch: EditorDispatch
}

let serverStateWatcherInstance: number | null = null
const baseWatcherIntervalTime: number = 10 * 1000
let currentWatcherIntervalMultiplier: number = 1

function restartServerStateWatcher(
  loggedIn: boolean,
  projectId: string | null,
  forkedFromProjectId: string | null,
  dispatch: EditorDispatch,
): void {
  void updateProjectServerStateInStore(loggedIn, projectId, forkedFromProjectId, dispatch)
  // Reset the multiplier if triggered from outside of `restartWatcherInterval`.
  currentWatcherIntervalMultiplier = 1

  function restartWatcherInterval(): void {
    if (serverStateWatcherInstance != null) {
      window.clearInterval(serverStateWatcherInstance)
    }
    serverStateWatcherInstance = window.setInterval(() => {
      void updateProjectServerStateInStore(loggedIn, projectId, forkedFromProjectId, dispatch).then(
        (result) => {
          // If there's a failure, then double the multiplier and recreate the interval.
          if (result === 'failure') {
            if (currentWatcherIntervalMultiplier < 10) {
              currentWatcherIntervalMultiplier = currentWatcherIntervalMultiplier * 2
              restartWatcherInterval()
            }
          }

          // If this call succeeds, reset the multiplier and recreate the interval if the multiplier
          // has changed.
          if (result === 'success') {
            if (currentWatcherIntervalMultiplier !== 1) {
              currentWatcherIntervalMultiplier = 1
              restartWatcherInterval()
            }
          }
        },
      )
    }, baseWatcherIntervalTime * currentWatcherIntervalMultiplier)
  }
  restartWatcherInterval()
}

export const ProjectServerStateUpdater = React.memo(
  (props: React.PropsWithChildren<ProjectServerStateUpdaterProps>) => {
    const { projectId, forkedFromProjectId, dispatch, loggedIn, children } = props
    React.useEffect(() => {
      restartServerStateWatcher(loggedIn, projectId, forkedFromProjectId, dispatch)
    }, [dispatch, projectId, forkedFromProjectId, loggedIn])
    return <>{children}</>
  },
)
