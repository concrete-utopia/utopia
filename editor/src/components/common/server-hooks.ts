import React from 'react'
import { checkProjectOwned } from '../editor/persistence/persistence-backend'
import { reduxDevtoolsLogMessage } from '../../core/shared/redux-devtools'
import { fetchProjectMetadata } from '../../common/server'
import { IS_TEST_ENVIRONMENT } from '../../common/env-vars'

type ProjectMetadata = {
  title: string
  ownerName: string | null
  ownerPicture: string | null
}

export function useGetProjectMetadata(projectId: string | null): ProjectMetadata | null {
  const previousProjectIdRef = React.useRef<string | null>(null)
  const [userData, setUserData] = React.useState<ProjectMetadata | null>(null)

  // When this is run in a test environment, the real logic of this function will fail
  // because the `fetch` function could not be found and if it did it would have to call
  // a server to get a response.
  if (IS_TEST_ENVIRONMENT) {
    return null
  } else {
    if (previousProjectIdRef.current !== projectId) {
      previousProjectIdRef.current = projectId
      if (projectId == null) {
        setUserData(null)
      } else {
        void fetchProjectMetadata(projectId).then((projectListing) => {
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
}

export function useIsMyProject(projectId: string | null): 'yes' | 'no' | 'unknown' {
  const previousProjectIdRef = React.useRef<string | null | undefined>(undefined)
  const [myProject, setMyProject] = React.useState<'yes' | 'no' | 'unknown'>('unknown')

  // When this is run in a test environment, the real logic of this function will fail
  // because the `fetch` function could not be found and if it did it would have to call
  // a server to get a response.
  if (IS_TEST_ENVIRONMENT) {
    return 'unknown'
  } else {
    if (previousProjectIdRef.current !== projectId) {
      previousProjectIdRef.current = projectId
      if (projectId == null) {
        setMyProject('yes')
      } else {
        void checkProjectOwned(projectId).then((isMyProject) => {
          reduxDevtoolsLogMessage('useIsMyProject called', {
            isMyProject: isMyProject,
          })
          setMyProject(isMyProject ? 'yes' : 'no')
        })
      }
    }

    return myProject
  }
}
