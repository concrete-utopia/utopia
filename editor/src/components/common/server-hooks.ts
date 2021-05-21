import * as React from 'react'
import { isLocal } from '../editor/persistence'
import { reduxDevtoolsLogMessage } from '../../core/shared/redux-devtools'
import { checkProjectOwnership, fetchProjectMetadata } from '../../common/server'

type ProjectMetadata = {
  title: string
  ownerName: string | null
  ownerPicture: string | null
}

export function useGetProjectMetadata(projectId: string | null): ProjectMetadata | null {
  const previousProjectIdRef = React.useRef<string | null>(null)
  const [userData, setUserData] = React.useState<ProjectMetadata | null>(null)
  if (previousProjectIdRef.current !== projectId) {
    previousProjectIdRef.current = projectId
    if (projectId == null) {
      setUserData(null)
    } else {
      fetchProjectMetadata(projectId).then((projectListing) => {
        // safeguard against an old Fetch arriving for an outdated projectId
        if (previousProjectIdRef.current === projectId) {
          if (projectListing != null) {
            setUserData({
              title: projectListing.title,
              ownerName: projectListing.ownerName ?? null,
              ownerPicture: projectListing.ownerPicture ?? null,
            })
          } else {
            setUserData(null)
          }
        }
      })
    }
  }

  return userData
}

export function useIsMyProject(projectId: string | null): 'yes' | 'no' | 'unknown' {
  const previousProjectIdRef = React.useRef<string | null | undefined>(undefined)
  const [myProject, setMyProject] = React.useState<'yes' | 'no' | 'unknown'>('unknown')

  if (previousProjectIdRef.current !== projectId) {
    previousProjectIdRef.current = projectId
    if (projectId == null) {
      setMyProject('yes')
    } else {
      checkProjectOwnership(projectId).then((ownershipResult) => {
        const isMyProject = isLocal() || ownershipResult === 'unowned' || ownershipResult.isOwner
        reduxDevtoolsLogMessage('useIsMyProject called', {
          isLocal: isLocal(),
          ownershipResult: ownershipResult,
        })
        setMyProject(isMyProject ? 'yes' : 'no')
      })
    }
  }

  return myProject
}
