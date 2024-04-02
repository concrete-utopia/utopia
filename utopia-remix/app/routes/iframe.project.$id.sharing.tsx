import type { LoaderFunctionArgs } from '@remix-run/node'
import { useLoaderData } from '@remix-run/react'
import type { UserDetails } from 'prisma-client'
import React from 'react'
import { SharingDialogContent } from '../components/sharingDialog'
import { getProjectSharingDetails } from '../models/project.server'
import { ProjectsContext, createProjectsStore } from '../stores/projectsStore'
import type { ProjectAccessRequestWithUserDetails } from '../types'
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

  return {
    user: user,
    project: sharingDetails.project,
    accessRequests: sharingDetails.accessRequests,
  }
}

const SharingIframe = React.memo(() => {
  const data = useLoaderData<typeof loader>() as unknown as {
    user: UserDetails
    project: ProjectSharingDetails
    accessRequests: ProjectAccessRequestWithUserDetails[]
  }

  const store = React.useRef(
    createProjectsStore({
      myUser: data.user,
      sharingProjectId: data.project.proj_id,
      sharingProjectAccessRequests: { state: 'ready', requests: data.accessRequests },
    }),
  ).current

  const changeProjectAccess = React.useCallback(() => {
    // TODO
  }, [])

  return (
    <ProjectsContext.Provider value={store}>
      <div style={{ height: 450, width: 450, boxSizing: 'border-box' }}>
        <SharingDialogContent
          project={data.project}
          accessLevel={
            asAccessLevel(data.project.ProjectAccess?.access_level) ?? AccessLevel.PRIVATE
          }
          accessRequests={{ state: 'ready', requests: data.accessRequests }}
          changeProjectAccessLevel={changeProjectAccess}
          asDialog={false}
        />
      </div>
    </ProjectsContext.Provider>
  )
})
SharingIframe.displayName = 'SharingIframe'

export default SharingIframe
