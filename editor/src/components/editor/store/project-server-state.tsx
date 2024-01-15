import * as React from 'react'
import type { ProjectListing } from '../../../common/persistence'
import { IS_TEST_ENVIRONMENT } from '../../../common/env-vars'
import { fetchProjectMetadata } from '../../../common/server'
import type { EditorDispatch } from '../action-types'
import { updateProjectServerState } from '../actions/action-creators'
import { checkProjectOwned } from '../persistence/persistence-backend'
import type { ProjectOwnership } from '../persistence/generic/persistence-types'
import { isFeatureEnabled } from '../../../utils/feature-switches'
import { CollaborationEndpoints } from '../collaborative-endpoints'

export interface ProjectMetadataFromServer {
  title: string
  ownerName: string | null
  ownerPicture: string | null
}

export function projectMetadataFromServer(
  title: string,
  ownerName: string | null,
  ownerPicture: string | null,
): ProjectMetadataFromServer {
  return {
    title: title,
    ownerName: ownerName,
    ownerPicture: ownerPicture,
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
    }
  }
}

export async function getProjectServerState(
  dispatch: EditorDispatch,
  projectId: string | null,
  forkedFromProjectId: string | null,
): Promise<ProjectServerState> {
  if (IS_TEST_ENVIRONMENT) {
    return {
      ...emptyProjectServerState(),
      isMyProject: 'yes',
      currentlyHolderOfTheBaton: true,
    }
  } else {
    const projectListing = projectId == null ? null : await fetchProjectMetadata(projectId)
    const forkedFromProjectListing =
      forkedFromProjectId == null ? null : await fetchProjectMetadata(forkedFromProjectId)

    async function getOwnership(): Promise<ProjectOwnership> {
      if (projectId == null) {
        return { isOwner: true, ownerId: null }
      }
      const { isOwner, ownerId } = await checkProjectOwned(projectId)
      return { isOwner, ownerId }
    }
    const ownership = await getOwnership()

    const holderOfTheBaton = ownership.isOwner
      ? await CollaborationEndpoints.claimControlOverProject(projectId)
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
      : false

    return {
      isMyProject: ownership.isOwner ? 'yes' : 'no',
      ownerId: ownership.ownerId,
      projectData: projectListingToProjectMetadataFromServer(projectListing),
      forkedFromProjectData: projectListingToProjectMetadataFromServer(forkedFromProjectListing),
      currentlyHolderOfTheBaton: holderOfTheBaton,
    }
  }
}

type SuccessOrFailure = 'success' | 'failure'

export async function updateProjectServerStateInStore(
  projectId: string | null,
  forkedFromProjectId: string | null,
  dispatch: EditorDispatch,
): Promise<SuccessOrFailure> {
  return getProjectServerState(dispatch, projectId, forkedFromProjectId)
    .then<SuccessOrFailure, SuccessOrFailure>((serverState) => {
      dispatch([updateProjectServerState(serverState)], 'everyone')
      return 'success'
    })
    .catch((error) => {
      console.error('Error while updating server state.', error)
      return 'failure'
    })
}

export interface ProjectServerStateUpdaterProps {
  projectId: string | null
  forkedFromProjectId: string | null
  dispatch: EditorDispatch
}

let serverStateWatcherInstance: number | null = null
const baseWatcherIntervalTime: number = 10 * 1000
let currentWatcherIntervalMultiplier: number = 1

function restartServerStateWatcher(
  projectId: string | null,
  forkedFromProjectId: string | null,
  dispatch: EditorDispatch,
): void {
  if (isFeatureEnabled('Baton Passing For Control')) {
    void updateProjectServerStateInStore(projectId, forkedFromProjectId, dispatch)
    // Reset the multiplier if triggered from outside of `restartWatcherInterval`.
    currentWatcherIntervalMultiplier = 1

    function restartWatcherInterval(): void {
      if (serverStateWatcherInstance != null) {
        window.clearInterval(serverStateWatcherInstance)
      }
      serverStateWatcherInstance = window.setInterval(() => {
        void updateProjectServerStateInStore(projectId, forkedFromProjectId, dispatch).then(
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
  } else {
    void updateProjectServerStateInStore(projectId, forkedFromProjectId, dispatch)
  }
}

export const ProjectServerStateUpdater = React.memo(
  (props: React.PropsWithChildren<ProjectServerStateUpdaterProps>) => {
    const { projectId, forkedFromProjectId, dispatch, children } = props
    React.useEffect(() => {
      restartServerStateWatcher(projectId, forkedFromProjectId, dispatch)
    }, [dispatch, projectId, forkedFromProjectId])
    return <>{children}</>
  },
)
