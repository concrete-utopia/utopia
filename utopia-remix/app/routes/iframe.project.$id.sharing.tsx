import type { LoaderFunctionArgs } from '@remix-run/node'
import { json, useFetcher, useLoaderData } from '@remix-run/react'
import type { UserDetails } from 'prisma-client'
import React from 'react'
import { SharingDialogContent } from '../components/sharingDialog'
import { getProjectSharingDetails } from '../models/project.server'
import type { SharingProjectAccessRequests } from '../stores/projectsStore'
import { ProjectsContext, createProjectsStore } from '../stores/projectsStore'
import { AccessLevel, asAccessLevel, type ProjectSharingDetails } from '../types'
import { ensure, requireUser } from '../util/api.server'
import { Status } from '../util/statusCodes'

export async function loader(args: LoaderFunctionArgs) {
  const projectId = args.params.id
  ensure(projectId != null, 'invalid project id', Status.BAD_REQUEST)
  const user = await requireUser(args.request)
  ensure(user != null, 'unauthorized', Status.UNAUTHORIZED)
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

const SharingIframe = React.memo(() => {
  const data = useLoaderData<typeof loader>() as unknown as {
    user: UserDetails
    project: ProjectSharingDetails
  }

  const accessRequests: SharingProjectAccessRequests = React.useMemo(() => {
    return { state: 'ready', requests: data.project.ProjectAccessRequest }
  }, [data.project.ProjectAccessRequest])

  const store = React.useRef(
    createProjectsStore({
      myUser: data.user,
      sharingProjectId: data.project.proj_id,
      sharingProjectAccessRequests: accessRequests,
    }),
  ).current

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
    <ProjectsContext.Provider value={store}>
      <SharingDialogContent
        project={data.project}
        accessRequestsState={accessRequests.state}
        accessLevel={accessLevel}
        changeProjectAccessLevel={changeProjectAccessLevel}
        asDialog={false}
      />
    </ProjectsContext.Provider>
  )
})
SharingIframe.displayName = 'SharingIframe'

export default SharingIframe
