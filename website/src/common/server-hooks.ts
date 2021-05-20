import * as React from 'react'
import { isLocal } from '../components/editor/persistence'
import { reduxDevtoolsLogMessage } from '../core/shared/redux-devtools'
import { checkProjectOwnership, fetchProjectMetadata } from './server'

type ProjectMetadata = {
  title: string | null
  ownerName: string | null
  ownerPicture: string | null
}

export function useGetProjectMetadata(projectId: string | null): ProjectMetadata {
  const previousProjectIdRef = React.useRef<string | null>(null)
  const [userData, setUserData] = React.useState<ProjectMetadata>({
    title: null,
    ownerName: null,
    ownerPicture: null,
  })
  if (previousProjectIdRef.current !== projectId) {
    previousProjectIdRef.current = projectId
    if (projectId == null) {
      setUserData({
        title: null,
        ownerName: null,
        ownerPicture: null,
      })
    } else {
      fetchProjectMetadata(projectId).then((projectListing) => {
        // safeguard against an old Fetch arriving for an outdated projectId
        if (previousProjectIdRef.current === projectId) {
          setUserData({
            title: projectListing?.title ?? null,
            ownerName: projectListing?.ownerName ?? null,
            ownerPicture: projectListing?.ownerPicture ?? null,
          })
        }
      })
    }
  }

  return userData
}

export function useIsMyProject(projectId: string | null): boolean {
  const previousProjectIdRef = React.useRef<string | null>(null)
  const [myProject, setMyProject] = React.useState<boolean>(false)

  if (previousProjectIdRef.current !== projectId) {
    previousProjectIdRef.current = projectId
    if (projectId == null) {
      setMyProject(true)
    } else {
      checkProjectOwnership(projectId).then((ownershipResult) => {
        const isMyProject = isLocal() || ownershipResult === 'unowned' || ownershipResult.isOwner
        reduxDevtoolsLogMessage('useIsMyProject called', {
          isLocal: isLocal(),
          ownershipResult: ownershipResult,
        })
        setMyProject(isMyProject)
      })
    }
  }

  return myProject
}
