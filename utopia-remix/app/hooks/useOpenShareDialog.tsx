import React from 'react'
import type { ProjectAccessRequestWithUserDetails } from '../types'
import { isProjectAccessRequestWithUserDetailsArray, type ProjectListing } from '../types'
import { useFetcher } from '@remix-run/react'
import { useProjectsStore } from '../store'
import { useFetcherData } from './useFetcherData'

/**
 * 1. set the sharing project id to the given project's id
 * 2. fetch access requests asynchronously and set them in the contextual store
 */
export function useOpenShareDialog(project: ProjectListing) {
  const setSharingProjectId = useProjectsStore((store) => store.setSharingProjectId)

  const accessRequestsFetcher = useFetcher()
  const setSharingProjectAccessRequests = useProjectsStore(
    (store) => store.setSharingProjectAccessRequests,
  )

  const fetchAccessRequests = React.useCallback(() => {
    if (project == null) {
      return
    }
    setSharingProjectAccessRequests({ state: 'loading', requests: [] })
    const action = `/internal/projects/${project.proj_id}/access/requests`
    accessRequestsFetcher.submit({}, { method: 'GET', action: action })
  }, [accessRequestsFetcher, project, setSharingProjectAccessRequests])

  const updateAccessRequests = React.useCallback(
    (data: ProjectAccessRequestWithUserDetails[]) => {
      setSharingProjectAccessRequests({ state: 'ready', requests: data })
    },
    [setSharingProjectAccessRequests],
  )

  useFetcherData(
    accessRequestsFetcher,
    isProjectAccessRequestWithUserDetailsArray,
    updateAccessRequests,
  )

  return React.useCallback(() => {
    setSharingProjectId(project.proj_id)
    fetchAccessRequests()
  }, [setSharingProjectId, fetchAccessRequests, project])
}
