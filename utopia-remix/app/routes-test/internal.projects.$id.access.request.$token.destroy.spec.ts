import type { Params } from '@remix-run/react'
import { prisma } from '../db.server'
import { action } from '../routes/internal.projects.$id.access.request.$token.destroy'
import {
  createTestProject,
  createTestProjectAccessRequest,
  createTestSession,
  createTestUser,
  newTestRequest,
  truncateTables,
} from '../test-util'
import { AccessRequestStatus } from '../types'
import type { ApiResponse } from '../util/api.server'
import { Status } from '../util/statusCodes'

describe('handleDestroyAccessRequest', () => {
  afterEach(async () => {
    await truncateTables([
      prisma.projectID,
      prisma.projectAccessRequest,
      prisma.projectAccess,
      prisma.persistentSession,
      prisma.project,
      prisma.userDetails,
    ])
  })

  beforeEach(async () => {
    await createTestUser(prisma, { id: 'bob' })
    await createTestUser(prisma, { id: 'alice' })
    await createTestUser(prisma, { id: 'carol' })
    await createTestSession(prisma, { userId: 'bob', key: 'the-key' })
    await createTestProject(prisma, { id: 'one', ownerId: 'bob' })
    await createTestProject(prisma, { id: 'two', ownerId: 'alice' })
    await createTestProjectAccessRequest(prisma, {
      projectId: 'one',
      userId: 'alice',
      status: AccessRequestStatus.PENDING,
      token: 'alice-token',
    })
    await createTestProjectAccessRequest(prisma, {
      projectId: 'one',
      userId: 'carol',
      status: AccessRequestStatus.PENDING,
      token: 'carol-token',
    })
  })

  it('requires a project id', async () => {
    const got = await getActionResult(newTestRequest({ method: 'POST' }), {})
    expect(got).toEqual({
      message: 'Invalid project id',
      status: Status.BAD_REQUEST,
      error: 'Error',
    })
  })

  it('requires an existing project', async () => {
    const got = await getActionResult(
      newTestRequest({ method: 'POST', authCookie: 'alice-token' }),
      {
        id: 'WRONG',
      },
    )
    expect(got).toEqual({
      message: 'Project not found',
      status: Status.NOT_FOUND,
      error: 'Error',
    })
  })

  it('requires a valid auth cookie', async () => {
    const got = await getActionResult(newTestRequest({ method: 'POST' }), { id: 'one' })
    expect(got).toEqual({
      message: 'Project not found',
      status: Status.NOT_FOUND,
      error: 'Error',
    })
  })

  it('requires an accessible project', async () => {
    const got = await getActionResult(newTestRequest({ method: 'POST', authCookie: 'the-key' }), {
      id: 'two',
    })
    expect(got).toEqual({
      message: 'Project not found',
      status: Status.NOT_FOUND,
      error: 'Error',
    })
  })

  it('requires a request token', async () => {
    const got = await getActionResult(newTestRequest({ method: 'POST', authCookie: 'the-key' }), {
      id: 'one',
    })
    expect(got).toEqual({
      message: 'invalid token',
      status: Status.BAD_REQUEST,
      error: 'Error',
    })
  })

  it('destroys the access request', async () => {
    const got = await getActionResult(newTestRequest({ method: 'POST', authCookie: 'the-key' }), {
      id: 'one',
      token: 'alice-token',
    })
    expect(got).toEqual({})

    const reqs = await prisma.projectAccessRequest.findMany({ where: { project_id: 'one' } })
    expect(reqs.length).toBe(1)
    expect(reqs[0].user_id).toBe('carol')
  })
})

// TODO it would be good to make this a separate reausable helper that supports both actions and loaders
async function getActionResult(req: Request, params: Params<string>) {
  const response = await (action({
    request: req,
    params: params,
    context: {},
  }) as Promise<ApiResponse<Record<string, never>>>)
  return await response.json()
}
