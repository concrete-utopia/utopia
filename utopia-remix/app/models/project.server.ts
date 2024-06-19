import { prisma } from '../db.server'
import type {
  CollaboratorsByProject,
  GithubRepository,
  ProjectListing,
  ProjectMetadataForEditor,
  ProjectSharingDetails,
} from '../types'
import {
  AccessLevel,
  AccessRequestStatus,
  asAccessLevel,
  userToCollaborator,
  githubRepositoryStringOrNull,
  type ProjectWithoutContentFromDB,
} from '../types'
import { ensure } from '../util/api.server'
import { Status } from '../util/statusCodes'

const selectProjectWithoutContent: Record<keyof ProjectWithoutContentFromDB, true> = {
  id: true,
  proj_id: true,
  owner_id: true,
  title: true,
  created_at: true,
  modified_at: true,
  deleted: true,
  ProjectAccess: true,
  github_repository: true,
}

export async function listProjects(params: { ownerId: string }): Promise<ProjectListing[]> {
  const projects = await prisma.project.findMany({
    select: selectProjectWithoutContent,
    where: {
      owner_id: params.ownerId,
      OR: [{ deleted: null }, { deleted: false }],
    },
    orderBy: { modified_at: 'desc' },
  })

  const pendingRequests = await prisma.projectAccessRequest.findMany({
    where: {
      project_id: {
        in: projects
          .filter((p) => p.ProjectAccess?.access_level === AccessLevel.COLLABORATIVE)
          .map((p) => p.proj_id),
      },
      status: AccessRequestStatus.PENDING,
    },
    select: { project_id: true },
  })

  return projects.map((project) => {
    return {
      ...project,
      hasPendingRequests: pendingRequests.some((r) => r.project_id === project.proj_id),
    }
  })
}

export async function getProjectOwnership(
  params: { id: string },
  config: { includeDeleted: boolean } = { includeDeleted: false },
): Promise<{ ownerId: string; accessLevel: AccessLevel } | null> {
  let whereOptions: {
    proj_id: string
    OR?: { deleted: boolean | null }[]
  } = {
    proj_id: params.id,
  }
  // if we're not including deleted projects, we need to add a condition to the query
  if (!config.includeDeleted) {
    whereOptions.OR = [{ deleted: null }, { deleted: false }]
  }
  // find the project and return the owner_id
  const project = await prisma.project.findFirst({
    select: { owner_id: true, ProjectAccess: true },
    where: whereOptions,
  })
  if (project == null) {
    return null
  }
  const accessLevel = asAccessLevel(project.ProjectAccess?.access_level) ?? AccessLevel.PRIVATE // not ideal that projects don't have always an access level

  return {
    ownerId: project.owner_id,
    accessLevel: accessLevel,
  }
}

export async function getProjectSharingDetails(params: {
  projectId: string
  userId: string
}): Promise<ProjectSharingDetails | null> {
  return prisma.project.findUnique({
    where: {
      proj_id: params.projectId,
      owner_id: params.userId,
    },
    select: {
      proj_id: true,
      ProjectAccess: true,
      ProjectAccessRequest: { include: { User: true } },
    },
  })
}

export async function renameProject(params: {
  id: string
  userId: string
  title: string
}): Promise<ProjectListing> {
  return prisma.project.update({
    where: {
      proj_id: params.id,
      owner_id: params.userId,
      OR: [{ deleted: null }, { deleted: false }],
    },
    data: { title: params.title },
    select: selectProjectWithoutContent,
  })
}

export async function softDeleteProject(params: { id: string; userId: string }): Promise<void> {
  await prisma.project.update({
    where: {
      proj_id: params.id,
      owner_id: params.userId,
      OR: [{ deleted: null }, { deleted: false }],
    },
    data: { deleted: true },
  })
}

export async function restoreDeletedProject(params: { id: string; userId: string }): Promise<void> {
  await prisma.project.update({
    where: {
      proj_id: params.id,
      owner_id: params.userId,
      deleted: true,
    },
    data: { deleted: null },
  })
}

export async function listDeletedProjects(params: { ownerId: string }): Promise<ProjectListing[]> {
  return await prisma.project.findMany({
    select: selectProjectWithoutContent,
    where: {
      owner_id: params.ownerId,
      deleted: true,
    },
    orderBy: { modified_at: 'desc' },
  })
}

export async function hardDeleteProject(params: { id: string; userId: string }): Promise<void> {
  await prisma.project.delete({
    where: {
      proj_id: params.id,
      owner_id: params.userId,
      deleted: true,
    },
  })
}

export async function hardDeleteAllProjects(params: { userId: string }): Promise<void> {
  await prisma.project.deleteMany({
    where: {
      owner_id: params.userId,
      deleted: true,
    },
  })
}

export async function getProjectAccessLevel(params: { projectId: string }): Promise<AccessLevel> {
  const projectAccess = await prisma.projectAccess.findUnique({
    where: { project_id: params.projectId },
    select: { access_level: true },
  })
  ensure(projectAccess != null, 'Project not found', Status.NOT_FOUND)
  const accessLevel = asAccessLevel(projectAccess.access_level)
  ensure(accessLevel != null, 'Invalid access level', Status.INTERNAL_ERROR)
  return accessLevel
}

export async function listSharedWithMeProjectsAndCollaborators(params: {
  userId: string
}): Promise<{
  projects: ProjectListing[]
  collaborators: CollaboratorsByProject
}> {
  // 1. grab the projects for which the user is a collaborator, are not deleted, are
  // collaborative, and the user is not the owner.
  const projectsAndCollaborators = await prisma.projectCollaborator.findMany({
    where: {
      user_id: params.userId, // the user is a collaborator
      Project: {
        OR: [{ deleted: null }, { deleted: false }], // project is not deleted
        owner_id: { not: params.userId }, // the owner is not the user
        ProjectAccess: { access_level: AccessLevel.COLLABORATIVE }, // the project is collaborative
      },
    },
    select: {
      Project: {
        select: {
          ...selectProjectWithoutContent,
          ProjectCollaborator: {
            select: { User: { select: { user_id: true, name: true, picture: true } } },
          },
        },
      },
    },
    orderBy: { Project: { modified_at: 'desc' } },
  })
  const projects = projectsAndCollaborators.map(({ Project }) => Project)

  // 2. grab the owner details in case some are missing from the collaborators
  const owners = await prisma.userDetails.findMany({
    where: { user_id: { in: projects.map((p) => p.owner_id) } },
  })

  // 3. build the collaborators map
  let collaboratorsByProject: CollaboratorsByProject = {}
  for (const project of projects) {
    const projectId = project.proj_id

    collaboratorsByProject[projectId] = project.ProjectCollaborator.map(({ User }) =>
      userToCollaborator(User),
    )

    // the owner of a project should always appear in the list of collaborators, so
    // make sure to append it if it's missing
    const collaboratorsIncludeOwner = collaboratorsByProject[projectId].some(
      (collaborator) => collaborator.id === project.owner_id,
    )
    if (!collaboratorsIncludeOwner) {
      const owner = owners.find(({ user_id }) => user_id === project.owner_id)
      if (owner != null) {
        collaboratorsByProject[projectId].push(userToCollaborator(owner))
      }
    }
  }

  // 4. return both projects and collabs
  return {
    projects: projects,
    collaborators: collaboratorsByProject,
  }
}

export async function updateGithubRepository(params: {
  projectId: string
  userId: string
  repository: GithubRepository | null
}) {
  return prisma.project.update({
    where: {
      owner_id: params.userId,
      proj_id: params.projectId,
    },
    data: {
      github_repository: githubRepositoryStringOrNull(params.repository),
      modified_at: new Date(),
    },
  })
}

export async function getProjectExtraMetadataForEditor(params: {
  projectId: string
  userId: string
}): Promise<ProjectMetadataForEditor | null> {
  const project = await prisma.project.findUnique({
    where: {
      proj_id: params.projectId,
      owner_id: params.userId,
    },
    select: {
      ProjectAccessRequest: true,
    },
  })
  if (project == null) {
    return null
  }

  return {
    hasPendingRequests: project.ProjectAccessRequest.some(
      (r) => r.status === AccessRequestStatus.PENDING,
    ),
  }
}
