import { listProjects } from '../models/project.server'
import { getManyUserDetails } from '../models/userDetails.server'
import type { ListProjectsResponseV1 } from '../types'
import { requireUser, ensure } from '../util/api.server'
import { Status } from '../util/statusCodes'

export async function handleListProjects(req: Request): Promise<ListProjectsResponseV1> {
  const user = await requireUser(req)

  const projects = await listProjects({ ownerId: user.user_id })

  const userIds = new Set(projects.map((p) => p.owner_id))
  const userDetails = await getManyUserDetails(Array.from(userIds))

  const projectsWithOwnerData = projects.map((p) => {
    const owner = userDetails.find((details) => details.user_id === p.owner_id)
    ensure(owner != null, 'owner not found', Status.NOT_FOUND)

    return {
      id: p.proj_id,
      ownerName: owner?.name ?? null,
      ownerPicture: owner?.picture ?? null,
      title: p.title,
      description: null,
      createdAt: p.created_at.toISOString(),
      modifiedAt: p.modified_at.toISOString(),
    }
  })

  return {
    projects: projectsWithOwnerData,
  }
}
