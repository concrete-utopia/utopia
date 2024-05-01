import type { UtopiaPrismaClient } from './db.server'
import type { AccessLevel } from './types'
import { SESSION_COOKIE_NAME } from './util/api.server'

export async function wait(ms: number) {
  return new Promise((res) => setTimeout(res, ms))
}

export async function createTestUser(
  client: UtopiaPrismaClient,
  params: { id: string; name?: string },
) {
  await client.userDetails.create({
    data: {
      user_id: params.id,
      email: `${params.id}@example.com`,
      name: params.name ?? params.id,
    },
  })
}

export async function createTestProject(
  client: UtopiaPrismaClient,
  params: {
    id: string
    ownerId: string
    title?: string
    content?: string
    createdAt?: Date
    modifiedAt?: Date
    deleted?: boolean
    accessLevel?: AccessLevel
  },
) {
  const now = new Date()
  await client.projectID.create({
    data: { proj_id: params.id },
  })
  await client.project.create({
    data: {
      proj_id: params.id,
      title: params.title ?? params.id,
      owner_id: params.ownerId,
      created_at: params.createdAt ?? now,
      modified_at: params.modifiedAt ?? now,
      content: Buffer.from(params.content ?? 'test'),
      deleted: params.deleted ?? null,
    },
  })
  if (params.accessLevel != null) {
    await client.projectAccess.create({
      data: { project_id: params.id, access_level: params.accessLevel, modified_at: new Date() },
    })
  }
}

export async function createTestSession(
  client: UtopiaPrismaClient,
  params: {
    key: string
    userId: string
  },
) {
  const now = new Date()
  await client.persistentSession.create({
    data: {
      key: params.key,
      session_json: {
        userID: params.userId,
      },
      created_at: now,
      accessed_at: now,
      session: Buffer.from(''),
    },
  })
}

export async function createTestProjectCollaborator(
  client: UtopiaPrismaClient,
  params: { projectId: string; userId: string },
) {
  await client.projectCollaborator.create({
    data: { project_id: params.projectId, user_id: params.userId },
  })
}

export async function createTestProjectAccess(
  client: UtopiaPrismaClient,
  params: { projectId: string; accessLevel: AccessLevel },
) {
  await client.projectAccess.create({
    data: {
      project_id: params.projectId,
      access_level: params.accessLevel,
      modified_at: new Date(),
    },
  })
}

export async function createTestProjectAccessRequest(
  client: UtopiaPrismaClient,
  params: { projectId: string; userId: string; status: AccessLevel; token: string },
) {
  await client.projectAccessRequest.create({
    data: {
      project_id: params.projectId,
      user_id: params.userId,
      status: params.status,
      token: params.token,
    },
  })
}

interface DeletableModel {
  /* eslint-disable-next-line no-empty-pattern */
  deleteMany: ({}) => Promise<any>
}

export async function truncateTables(models: DeletableModel[]) {
  for (const model of models) {
    await model.deleteMany({})
  }
}

export function newTestRequest(params?: {
  path?: string
  search?: Record<string, string>
  method?: string
  headers?: { [key: string]: string }
  authCookie?: string
  formData?: FormData
  body?: string
}): Request {
  const path = (params?.path ?? '').replace(/^\/+/, '')
  const url = new URL(`http://localhost:8000/${path}`)
  if (params?.search != null) {
    for (const key of Object.keys(params.search)) {
      url.searchParams.set(key, params.search[key])
    }
  }
  const req = new Request(url, {
    method: params?.method,
    body: params?.formData ?? params?.body,
  })

  if (params?.headers != null) {
    for (const key of Object.keys(params.headers)) {
      req.headers.set(key, params.headers[key])
    }
  }

  if (params?.authCookie != null) {
    req.headers.set('cookie', `${SESSION_COOKIE_NAME}=${params.authCookie}`)
  }

  return req
}

export function newFormData(data: { [key: string]: string }): FormData {
  const formData = new FormData()
  for (const key of Object.keys(data)) {
    formData.append(key, data[key])
  }
  return formData
}

export async function createTestGithubAuth(
  client: UtopiaPrismaClient,
  params: {
    token: string
    userId: string
  },
) {
  await client.githubAuthentication.create({
    data: {
      access_token: params.token,
      user_id: params.userId,
    },
  })
}
