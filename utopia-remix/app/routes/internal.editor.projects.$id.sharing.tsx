import type { LoaderFunctionArgs } from '@remix-run/node'
import { json, useFetcher, useLoaderData, useRevalidator } from '@remix-run/react'
import React from 'react'
import { SharingDialogContent } from '../components/sharingDialog'
import { getProjectSharingDetails } from '../models/project.server'
import { ProjectsContext, createProjectsStore, useProjectsStore } from '../stores/projectsStore'
import type { ProjectAccessRequestWithUserDetails, ProjectSharingDetails } from '../types'
import { AccessLevel, asAccessLevel } from '../types'
import { ensure, requireUser } from '../util/api.server'
import { Status } from '../util/statusCodes'
import type { UserDetails } from 'prisma-client'

const RevalidateInterval = 15_000 //ms

export async function loader(args: LoaderFunctionArgs) {
  const projectId = args.params.id
  ensure(projectId != null, 'invalid project id', Status.BAD_REQUEST)

  const user = await requireUser(args.request)
  ensure(user != null, 'unauthorized', Status.UNAUTHORIZED)

  // Note: at the moment only the owner of the project can open the share dialog, but that's fine until we
  // introduce full write-enabled, non-owner roles support.
  const sharingDetails = await getProjectSharingDetails({
    projectId: projectId,
    userId: user.user_id,
  })
  ensure(sharingDetails != null, 'project not found', Status.NOT_FOUND)

  return json(
    {
      user: user,
      project: sharingDetails,
    },
    { headers: { 'cache-control': 'no-cache' } },
  )
}

type LoaderData = {
  user: UserDetails
  project: ProjectSharingDetails
}

const EditorProjectSharingPage = React.memo(() => {
  const data = useLoaderData<typeof loader>() as unknown as LoaderData

  const store = React.useRef(
    createProjectsStore({
      myUser: data.user,
      sharingProjectId: data.project.proj_id,
    }),
  ).current

  return (
    <ProjectsContext.Provider value={store}>
      <EditorProjectSharingPageInner />
    </ProjectsContext.Provider>
  )
})
EditorProjectSharingPage.displayName = 'EditorProjectSharingPage'

export default EditorProjectSharingPage

const EditorProjectSharingPageInner = React.memo(() => {
  const data = useLoaderData<typeof loader>() as unknown as LoaderData

  useRevalidateWithInterval(RevalidateInterval)
  useUpdateSharingRequests(data.project.ProjectAccessRequest)

  const [accessLevel, setAccessLevel] = React.useState(
    asAccessLevel(data.project.ProjectAccess?.access_level) ?? AccessLevel.PRIVATE,
  )

  const changeAccessFetcher = useFetcher()

  const changeProjectAccessLevel = React.useCallback(
    (newAccessLevel: AccessLevel) => {
      if (data.project == null) {
        return
      }
      setAccessLevel(newAccessLevel)
      changeAccessFetcher.submit(
        { accessLevel: newAccessLevel.toString() },
        { method: 'POST', action: `/internal/projects/${data.project.proj_id}/access` },
      )
    },
    [changeAccessFetcher, data.project],
  )

  return (
    <SharingDialogContent
      project={data.project}
      accessRequestsState='ready'
      accessLevel={accessLevel}
      changeProjectAccessLevel={changeProjectAccessLevel}
      asDialog={false}
    />
  )
})
EditorProjectSharingPage.displayName = 'EditorProjectSharingPage'

// Revalidate the loader at fixed intervals of the given timeout milliseconds.
function useRevalidateWithInterval(timeout: number) {
  const revalidator = useRevalidator()

  React.useEffect(() => {
    const interval = window.setInterval(revalidator.revalidate, timeout)

    return function () {
      window.clearInterval(interval)
    }
  }, [revalidator, timeout])
}

function getAccessRequestsKey(reqs: ProjectAccessRequestWithUserDetails[]): string {
  return reqs.map((r) => `${r.id}:${r.status}`).join(',')
}

// Update the store's sharing requests data if they changed after a refresh.
function useUpdateSharingRequests(initialRequests: ProjectAccessRequestWithUserDetails[]) {
  const [lastRequestsKey, setLastRequestsKey] = React.useState<string | null>(null)

  const setSharingProjectAccessRequests = useProjectsStore(
    (store) => store.setSharingProjectAccessRequests,
  )

  React.useEffect(() => {
    const newKey = getAccessRequestsKey(initialRequests)
    if (lastRequestsKey !== newKey) {
      setLastRequestsKey(newKey)
      setSharingProjectAccessRequests({
        state: 'ready',
        requests: initialRequests,
      })
    }
  }, [initialRequests, setSharingProjectAccessRequests, lastRequestsKey])
}
