import { Status } from '../util/statusCodes'
import { loader } from '../routes/v1.project.$id.metadata'
import type { Params } from '@remix-run/react'
import {
  createTestProject,
  createTestProjectAccessRequest,
  createTestSession,
  createTestUser,
  newTestRequest,
  truncateTables,
} from '../test-util'
import type { ApiResponse } from '../util/api.server'
import { prisma } from '../db.server'
import { AccessRequestStatus } from '../types'
import * as proxyUtil from '../util/proxy.server'

describe('v1.project.$id.metadata', () => {
  let proxyMock: jest.SpyInstance
  afterEach(async () => {
    await truncateTables([
      prisma.projectID,
      prisma.projectAccessRequest,
      prisma.projectAccess,
      prisma.persistentSession,
      prisma.project,
      prisma.userDetails,
    ])
    proxyMock.mockRestore()
  })

  beforeEach(async () => {
    await createTestUser(prisma, { id: 'bob' })
    await createTestUser(prisma, { id: 'alice' })
    await createTestProject(prisma, { id: 'one', ownerId: 'bob' })
    await createTestProject(prisma, { id: 'two', ownerId: 'bob' })
    await createTestProjectAccessRequest(prisma, {
      projectId: 'one',
      userId: 'alice',
      status: AccessRequestStatus.PENDING,
      token: 't1',
    })
    await createTestSession(prisma, { userId: 'bob', key: 'bob-key' })

    proxyMock = jest.spyOn(proxyUtil, 'proxy')
  })

  it('requires a valid project id', async () => {
    const got = await getLoaderResult(newTestRequest({ method: 'GET' }), {})
    expect(got).toEqual({
      message: 'Invalid project id',
      status: Status.BAD_REQUEST,
      error: 'Error',
    })
  })

  it('requires an existing project', async () => {
    const got = await getLoaderResult(newTestRequest({ method: 'GET' }), { id: 'WRONG' })
    expect(got).toEqual({
      message: 'Project not found',
      status: Status.NOT_FOUND,
      error: 'Error',
    })
  })

  it('requires a valid user', async () => {
    const got = await getLoaderResult(newTestRequest({ method: 'GET' }), { id: 'one' })
    expect(got).toEqual({
      message: 'Project not found',
      status: Status.NOT_FOUND,
      error: 'Error',
    })
  })

  it("returns the original response if it's not metadata", async () => {
    proxyMock.mockResolvedValue('hey there')
    const got = await getLoaderResult(
      newTestRequest({
        method: 'GET',
        authCookie: 'bob-key',
      }),
      { id: 'one' },
    )
    expect(got).toEqual('hey there')
  })

  it('enriches the metadata with the extras', async () => {
    proxyMock.mockResolvedValue({ id: 'one', title: 'project one' })
    let got = await getLoaderResult(
      newTestRequest({
        method: 'GET',
        authCookie: 'bob-key',
      }),
      { id: 'one' },
    )
    expect(got).toEqual({ id: 'one', title: 'project one', hasPendingRequests: true })

    proxyMock.mockResolvedValue({ id: 'two', title: 'project two' })
    got = await getLoaderResult(
      newTestRequest({
        method: 'GET',
        authCookie: 'bob-key',
      }),
      { id: 'two' },
    )
    expect(got).toEqual({ id: 'two', title: 'project two', hasPendingRequests: false })
  })
})

async function getLoaderResult(req: Request, params: Params<string>) {
  const response = await (loader({
    request: req,
    params: params,
    context: {},
  }) as Promise<ApiResponse<Record<string, never>>>)
  return await response.json()
}
