import React from 'react'
import { useProjectsStore } from '../stores/projectsStore'
import type { ProjectAccessRequestWithUserDetails } from '../types'
import { isProjectAccessRequestWithUserDetailsArray } from '../types'

/**
 * 1. set the sharing project id to the given project id
 * 2. fetch access requests asynchronously and set them in the contextual store
 */
export function useOpenShareDialog(projectId: string) {
  const setSharingProjectId = useProjectsStore((store) => store.setSharingProjectId)
  const fetchAccessRequests = useFetchAccessRequests()

  return React.useCallback(() => {
    setSharingProjectId(projectId)
    fetchAccessRequests(projectId)
  }, [fetchAccessRequests, setSharingProjectId, projectId])
}

function useFetchAccessRequests() {
  const setSharingProjectAccessRequests = useProjectsStore(
    (store) => store.setSharingProjectAccessRequests,
  )

  return React.useCallback(
    async (projectId: string) => {
      setSharingProjectAccessRequests({ state: 'loading', requests: [] })
      let requests: ProjectAccessRequestWithUserDetails[] = []
      try {
        const resp = await fetch(`/internal/projects/${projectId}/access/requests`, {
          method: 'GET',
          credentials: 'include',
        })
        const data = await resp.json()
        if (isProjectAccessRequestWithUserDetailsArray(data)) {
          requests = data
        }
      } finally {
        setSharingProjectAccessRequests({ state: 'ready', requests: requests })
      }
    },
    [setSharingProjectAccessRequests],
  )
}
