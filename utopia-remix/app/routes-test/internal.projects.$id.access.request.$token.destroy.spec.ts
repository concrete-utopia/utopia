import { prisma } from '../db.server'
import { handleDestroyAccessRequest } from '../routes/internal.projects.$id.access.request.$token.destroy'
import {
  createTestProject,
  createTestProjectAccessRequest,
  createTestSession,
  createTestUser,
  newTestRequest,
  truncateTables,
} from '../test-util'
import { AccessRequestStatus } from '../types'

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

  it('requires a user', async () => {
    const fn = async () => handleDestroyAccessRequest(newTestRequest(), {})
    await expect(fn).rejects.toThrow('missing session cookie')
  })

  it('requires a project id', async () => {
    const fn = async () => handleDestroyAccessRequest(newTestRequest({ authCookie: 'the-key' }), {})
    await expect(fn).rejects.toThrow('invalid project id')
  })

  it('requires a token', async () => {
    const fn = async () =>
      handleDestroyAccessRequest(newTestRequest({ authCookie: 'the-key' }), { id: 'one' })
    await expect(fn).rejects.toThrow('invalid token')
  })

  it('destroys the access request', async () => {
    await handleDestroyAccessRequest(newTestRequest({ authCookie: 'the-key' }), {
      id: 'one',
      token: 'alice-token',
    })
    const reqs = await prisma.projectAccessRequest.findMany({ where: { project_id: 'one' } })
    expect(reqs.length).toBe(1)
    expect(reqs[0].user_id).toBe('carol')
  })
})
