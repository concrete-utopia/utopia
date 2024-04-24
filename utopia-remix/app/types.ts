import type { ProjectAccessRequest, UserDetails } from 'prisma-client'
import { Prisma } from 'prisma-client'
import { ensure } from './util/api.server'
import { assertNever } from './util/assertNever'
import { Status } from './util/statusCodes'

const fullProjectFromDB = Prisma.validator<Prisma.ProjectDefaultArgs>()({
  include: {
    ProjectAccess: true,
  },
})

type FullProjectFromDB = Prisma.ProjectGetPayload<typeof fullProjectFromDB>

export type ProjectWithoutContentFromDB = Omit<FullProjectFromDB, 'content'>

export type ProjectListing = ProjectWithoutContentFromDB & {
  hasPendingRequests?: boolean
}

export type ProjectSharingDetails = Pick<ProjectListing, 'proj_id' | 'ProjectAccess'> & {
  ProjectAccessRequest: ProjectAccessRequestWithUserDetails[]
}

// Legacy response
export interface ProjectListingV1 {
  id: string
  ownerName: string | null
  ownerPicture: string | null
  title: string
  description: string | null
  createdAt: string
  modifiedAt: string
}

export type ListProjectsResponseV1 = {
  projects: ProjectListingV1[]
}

export interface Collaborator {
  id: string
  name: string | null
  avatar: string | null
}

export type CollaboratorsByProject = { [projectId: string]: Collaborator[] }

export function userToCollaborator(user: UserDetails): Collaborator {
  return {
    id: user.user_id,
    name: user.name,
    avatar: user.picture,
  }
}

export const AccessLevel = {
  PRIVATE: 0,
  PUBLIC: 1,
  WITH_LINK: 2,
  COLLABORATIVE: 3,
} as const

export type AccessLevel = (typeof AccessLevel)[keyof typeof AccessLevel]

export function asAccessLevel(accessLevel: number | undefined | null): AccessLevel | null {
  switch (accessLevel) {
    case AccessLevel.PRIVATE:
      return AccessLevel.PRIVATE
    case AccessLevel.PUBLIC:
      return AccessLevel.PUBLIC
    case AccessLevel.WITH_LINK:
      return AccessLevel.WITH_LINK
    case AccessLevel.COLLABORATIVE:
      return AccessLevel.COLLABORATIVE
    default:
      return null
  }
}

export const UserProjectPermission = {
  CAN_VIEW_PROJECT: 0,
  CAN_FORK_PROJECT: 1,
  CAN_PLAY_PROJECT: 2,
  CAN_EDIT_PROJECT: 3,
  CAN_COMMENT_PROJECT: 4,
  CAN_SHOW_PRESENCE: 5,
  CAN_REQUEST_ACCESS: 6,
  CAN_SEE_LIVE_CHANGES: 7,
  CAN_MANAGE_PROJECT: 8,
} as const

export type UserProjectPermission =
  (typeof UserProjectPermission)[keyof typeof UserProjectPermission]

export const UserProjectRole = {
  VIEWER: 0,
  COLLABORATOR: 1,
  EDITOR: 2,
  ADMIN: 3,
} as const

export type UserProjectRole = (typeof UserProjectRole)[keyof typeof UserProjectRole]

export function asUserProjectRole(role: number | undefined | null): UserProjectRole | null {
  switch (role) {
    case UserProjectRole.VIEWER:
      return UserProjectRole.VIEWER
    case UserProjectRole.COLLABORATOR:
      return UserProjectRole.COLLABORATOR
    case UserProjectRole.EDITOR:
      return UserProjectRole.EDITOR
    case UserProjectRole.ADMIN:
      return UserProjectRole.ADMIN
    default:
      return null
  }
}
interface BaseOperation {
  projectId: string
}

function baseOperation(projectId: string): BaseOperation {
  return {
    projectId: projectId,
  }
}

type OperationRename = BaseOperation & {
  type: 'rename'
  newTitle: string
}

export function operationRename(projectId: string, newTitle: string): OperationRename {
  return {
    type: 'rename',
    ...baseOperation(projectId),
    newTitle: newTitle,
  }
}

type OperationDelete = BaseOperation & {
  type: 'delete'
}

export function operationDelete(projectId: string): OperationDelete {
  return { type: 'delete', ...baseOperation(projectId) }
}

type OperationDestroy = BaseOperation & {
  type: 'destroy'
}

export function operationDestroy(projectId: string): OperationDestroy {
  return { type: 'destroy', ...baseOperation(projectId) }
}

type OperationRestore = BaseOperation & {
  type: 'restore'
}

export function operationRestore(projectId: string): OperationRestore {
  return { type: 'restore', ...baseOperation(projectId) }
}

type OperationChangeAccess = BaseOperation & {
  type: 'changeAccess'
  newAccessLevel: AccessLevel
}

export function operationChangeAccess(
  projectId: string,
  newAccessLevel: AccessLevel,
): OperationChangeAccess {
  return { type: 'changeAccess', ...baseOperation(projectId), newAccessLevel: newAccessLevel }
}

export type UpdateAccessRequestAction = 'approve' | 'reject' | 'destroy'

type OperationUpdateAccessRequest = BaseOperation & {
  type: 'updateAccessRequest'
  tokenId: string
  action: UpdateAccessRequestAction
}

export function operationUpdateAccessRequest(
  projectId: string,
  tokenId: string,
  action: UpdateAccessRequestAction,
): OperationUpdateAccessRequest {
  return {
    type: 'updateAccessRequest',
    ...baseOperation(projectId),
    tokenId: tokenId,
    action: action,
  }
}

export type Operation =
  | OperationRename
  | OperationDelete
  | OperationDestroy
  | OperationRestore
  | OperationChangeAccess
  | OperationUpdateAccessRequest

export type OperationType =
  | 'rename'
  | 'delete'
  | 'destroy'
  | 'restore'
  | 'changeAccess'
  | 'updateAccessRequest'

export function areBaseOperationsEquivalent(a: Operation, b: Operation): boolean {
  return a.projectId === b.projectId && a.type === b.type
}

export function getOperationDescription(op: Operation, project: ProjectListing): string {
  switch (op.type) {
    case 'delete':
      return `Deleting project ${project.title}`
    case 'destroy':
      return `Destroying project ${project.title}`
    case 'rename':
      return `Renaming project ${project.title} to ${op.newTitle}`
    case 'restore':
      return `Restoring project ${project.title}`
    case 'changeAccess':
      return `Changing access level of project ${project.title}`
    case 'updateAccessRequest':
      switch (op.action) {
        case 'approve':
          return `Granting access request to project ${project.title}`
        case 'reject':
          return `Rejecting access request to project ${project.title}`
        case 'destroy':
          return `Deleting access request to project ${project.title}`
        default:
          assertNever(op.action)
      }
      break // required for typecheck
    default:
      assertNever(op)
  }
}

export enum AccessRequestStatus {
  PENDING,
  APPROVED,
  REJECTED,
}

export function mustAccessRequestStatus(n: number): AccessRequestStatus {
  const maybe = n as AccessRequestStatus
  switch (maybe) {
    case AccessRequestStatus.PENDING:
      return AccessRequestStatus.PENDING
    case AccessRequestStatus.APPROVED:
      return AccessRequestStatus.APPROVED
    case AccessRequestStatus.REJECTED:
      return AccessRequestStatus.REJECTED
    default:
      assertNever(maybe)
  }
}

export type ProjectAccessRequestWithUserDetails = ProjectAccessRequest & {
  User: UserDetails | null
}

export function isProjectAccessRequestWithUserDetails(
  u: unknown,
): u is ProjectAccessRequestWithUserDetails {
  const maybe = u as ProjectAccessRequestWithUserDetails
  return (
    u != null &&
    typeof u === 'object' &&
    maybe.id != null &&
    maybe.status != null &&
    maybe.user_id != null &&
    maybe.project_id != null
  )
}

export function isProjectAccessRequestWithUserDetailsArray(
  u: unknown,
): u is ProjectAccessRequestWithUserDetails[] {
  const maybe = u as ProjectAccessRequestWithUserDetails[]
  return (
    u != null &&
    typeof u === 'object' &&
    Array.isArray(u) &&
    maybe.every(isProjectAccessRequestWithUserDetails)
  )
}

export type GithubRepository = {
  owner: string
  repository: string
  branch: string | null
}

export interface UpdateGithubRepositoryRequestBody {
  githubRepository: GithubRepository | null
}

export function isUpdateGithubRepositoryBody(u: unknown): u is UpdateGithubRepositoryRequestBody {
  const maybe = u as UpdateGithubRepositoryRequestBody
  return u != null && typeof u === 'object' && maybe.githubRepository !== undefined
}

// Github-specific constraints
export const MaxGithubOwnerLength = 39 // https://docs.github.com/en/enterprise-cloud@latest/admin/identity-and-access-management/iam-configuration-reference/username-considerations-for-external-authentication
export const MaxGithubRepositoryLength = 100 // https://github.com/dead-claudia/github-limits
export const MaxGithubBranchNameLength = 255 // https://stackoverflow.com/questions/24014361/max-length-of-git-branch-name

export function githubRepositoryStringOrNull(repo: GithubRepository | null): string | null {
  if (repo == null) {
    return null
  }

  const owner = repo.owner.trim().slice(0, MaxGithubOwnerLength)
  ensure(owner.length > 0, 'invalid github owner', Status.BAD_REQUEST)

  const repository = repo.repository.trim().slice(0, MaxGithubRepositoryLength)
  ensure(repository.length > 0, 'invalid github repository', Status.BAD_REQUEST)

  const branch = repo.branch == null ? null : repo.branch.trim().slice(0, MaxGithubBranchNameLength)
  ensure(branch == null || branch.length > 0, 'invalid github branch', Status.BAD_REQUEST)

  return branch == null ? `${owner}/${repository}` : `${owner}/${repository}:${branch}`
}

export interface ProjectMetadataV1 {
  id: string
  title: string
}

export function isProjectMetadataV1(u: unknown): u is ProjectMetadataV1 {
  const maybe = u as ProjectMetadataV1
  return u != null && typeof u === 'object' && maybe.id != null && maybe.id != null
}

export type ProjectMetadataForEditor = {
  hasPendingRequests: boolean
}

export type SearchPublicRepositoriesRequest = {
  owner: string
  repo: string
}

export function isSearchPublicRepositoriesRequest(
  u: unknown,
): u is SearchPublicRepositoriesRequest {
  const maybe = u as SearchPublicRepositoriesRequest
  return u != null && typeof u === 'object' && maybe.owner != null && maybe.repo != null
}

type ApiSuccess<T> = T & { type: 'SUCCESS' }

export function toApiSuccess<T>(data: T): ApiSuccess<T> {
  return {
    type: 'SUCCESS',
    ...data,
  }
}

type ApiFailure = {
  type: 'FAILURE'
  failureReason: string
}

export function toApiFailure(reason: string): ApiFailure {
  return {
    type: 'FAILURE',
    failureReason: reason,
  }
}

interface WithMessageData {
  status: number
  response: {
    data: { message: string }
  }
}

export function isResponseWithMessageData(u: unknown): u is WithMessageData {
  const maybe = u as WithMessageData
  return (
    u != null &&
    typeof u === 'object' &&
    maybe.response != null &&
    maybe.response.data != null &&
    maybe.response.data.message != null &&
    maybe.status != null
  )
}
