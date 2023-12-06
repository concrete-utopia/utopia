import * as React from 'react'
import type { ProjectListing } from '../../../common/persistence'
import { IS_TEST_ENVIRONMENT } from '../../../common/env-vars'
import { fetchProjectMetadata } from '../../../common/server'
import type { EditorDispatch } from '../action-types'
import { updateProjectServerState } from '../actions/action-creators'
import { checkProjectOwned } from '../persistence/persistence-backend'
import type { ProjectOwnership } from '../persistence/generic/persistence-types'

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
}

export function projectServerState(
  isMyProject: ProjectServerState['isMyProject'],
  ownerId: string | null,
  projectData: ProjectMetadataFromServer | null,
  forkedFromProjectData: ProjectMetadataFromServer | null,
): ProjectServerState {
  return {
    isMyProject: isMyProject,
    ownerId: ownerId,
    projectData: projectData,
    forkedFromProjectData: forkedFromProjectData,
  }
}

export function emptyProjectServerState(): ProjectServerState {
  return projectServerState('unknown', null, null, null)
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
  projectId: string | null,
  forkedFromProjectId: string | null,
): Promise<ProjectServerState> {
  if (IS_TEST_ENVIRONMENT) {
    return emptyProjectServerState()
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

    return {
      isMyProject: ownership.isOwner ? 'yes' : 'no',
      ownerId: ownership.ownerId,
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

export function isProjectViewerFromState(state: ProjectServerState): boolean {
  return isProjectViewer(state.isMyProject)
}

export function isProjectViewer(isMyProject: IsMyProject): boolean {
  return isMyProject === 'no'
}
