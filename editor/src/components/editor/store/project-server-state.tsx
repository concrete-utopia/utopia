import * as React from 'react'
import type { ProjectListing } from '../../../common/persistence'
import { IS_TEST_ENVIRONMENT } from '../../../common/env-vars'
import { fetchProjectMetadata } from '../../../common/server'
import type { EditorDispatch } from '../action-types'
import { updateProjectServerState } from '../actions/action-creators'
import { checkProjectOwned } from '../persistence/persistence-backend'

export interface ProjectMetadataFromServer {
  title: string
  ownerName: string | null
  ownerPicture: string | null
  shared: boolean
}

export function projectMetadataFromServer(
  title: string,
  ownerName: string | null,
  ownerPicture: string | null,
  shared: boolean,
): ProjectMetadataFromServer {
  return {
    title: title,
    ownerName: ownerName,
    ownerPicture: ownerPicture,
    shared: shared,
  }
}

export interface ProjectServerState {
  isMyProject: 'yes' | 'no' | 'unknown'
  projectData: ProjectMetadataFromServer | null
  forkedFromProjectData: ProjectMetadataFromServer | null
}

export function projectServerState(
  isMyProject: ProjectServerState['isMyProject'],
  projectData: ProjectMetadataFromServer | null,
  forkedFromProjectData: ProjectMetadataFromServer | null,
): ProjectServerState {
  return {
    isMyProject: isMyProject,
    projectData: projectData,
    forkedFromProjectData: forkedFromProjectData,
  }
}

export function emptyProjectServerState(): ProjectServerState {
  return projectServerState('unknown', null, null)
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
      shared: projectListing.shared,
    }
  }
}

export async function getProjectServerState(
  projectId: string | null,
  forkedFromProjectId: string | null,
): Promise<ProjectServerState> {
  if (IS_TEST_ENVIRONMENT) {
    return emptyProjectServerState()
  } else {
    const projectListing = projectId == null ? null : await fetchProjectMetadata(projectId)
    const forkedFromProjectListing =
      forkedFromProjectId == null ? null : await fetchProjectMetadata(forkedFromProjectId)
    const isMyProject =
      projectId == null
        ? 'yes'
        : await checkProjectOwned(projectId).then((isMyProjectFromServer) => {
            return isMyProjectFromServer ? 'yes' : 'no'
          })
    return {
      isMyProject: isMyProject,
      projectData: projectListingToProjectMetadataFromServer(projectListing),
      forkedFromProjectData: projectListingToProjectMetadataFromServer(forkedFromProjectListing),
    }
  }
}

export interface ProjectServerStateUpdaterProps {
  projectId: string | null
  forkedFromProjectId: string | null
  dispatch: EditorDispatch
}

export const ProjectServerStateUpdater = React.memo(
  (props: React.PropsWithChildren<ProjectServerStateUpdaterProps>) => {
    const { projectId, forkedFromProjectId, dispatch, children } = props
    React.useEffect(() => {
      void getProjectServerState(projectId, forkedFromProjectId)
        .then((serverState) => {
          dispatch([updateProjectServerState(serverState)], 'everyone')
        })
        .catch((error) => {
          console.error('Error while updating server state.', error)
        })
    }, [dispatch, projectId, forkedFromProjectId])
    return <>{children}</>
  },
)
