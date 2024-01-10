import * as React from 'react'
import type { ProjectListing } from '../../../common/persistence'
import { IS_TEST_ENVIRONMENT } from '../../../common/env-vars'
import { fetchProjectMetadata } from '../../../common/server'
import type { EditorDispatch } from '../action-types'
import { updateProjectServerState } from '../actions/action-creators'
import { checkProjectOwned } from '../persistence/persistence-backend'
import type { ProjectOwnership } from '../persistence/generic/persistence-types'
import { isFeatureEnabled } from '../../../utils/feature-switches'
import { claimControlOverProject, displayControlErrorToast } from './collaborative-editing'

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
      ? await claimControlOverProject(projectId)
          .then((result) => {
            return result ?? ownership.isOwner
          })
          .catch(() => {
            displayControlErrorToast(
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

export function updateProjectServerStateInStore(
  projectId: string | null,
  forkedFromProjectId: string | null,
  dispatch: EditorDispatch,
) {
  void getProjectServerState(dispatch, projectId, forkedFromProjectId)
    .then((serverState) => {
      dispatch([updateProjectServerState(serverState)], 'everyone')
    })
    .catch((error) => {
      console.error('Error while updating server state.', error)
    })
}

export interface ProjectServerStateUpdaterProps {
  projectId: string | null
  forkedFromProjectId: string | null
  dispatch: EditorDispatch
}

let serverStateWatcherInstance: number | null = null
function restartServerStateWatcher(
  projectId: string | null,
  forkedFromProjectId: string | null,
  dispatch: EditorDispatch,
): void {
  if (isFeatureEnabled('Baton Passing For Control')) {
    if (serverStateWatcherInstance != null) {
      window.clearInterval(serverStateWatcherInstance)
    }
    updateProjectServerStateInStore(projectId, forkedFromProjectId, dispatch)
    serverStateWatcherInstance = window.setInterval(
      () => updateProjectServerStateInStore(projectId, forkedFromProjectId, dispatch),
      10 * 1000,
    )
  } else {
    updateProjectServerStateInStore(projectId, forkedFromProjectId, dispatch)
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
