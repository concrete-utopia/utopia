import React from 'react'
import { useProjectsStore } from '../store'
import type { ProjectAccessRequestWithUserDetails } from '../types'
import { isProjectAccessRequestWithUserDetailsArray } from '../types'

/**
 * 1. set the sharing project id to the given project's id
 * 2. fetch access requests asynchronously and set them in the contextual store
 */
export function useOpenShareDialog(projectId: string) {
  const setSharingProjectId = useProjectsStore((store) => store.setSharingProjectId)
  const setSharingProjectAccessRequests = useProjectsStore(
    (store) => store.setSharingProjectAccessRequests,
  )

  const fetchAccessRequests = React.useCallback(async () => {
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
  }, [setSharingProjectAccessRequests, projectId])

  return React.useCallback(() => {
    setSharingProjectId(projectId)
    fetchAccessRequests()
  }, [fetchAccessRequests, setSharingProjectId, projectId])
}
