import { prisma } from '../db.server'
import { handleDestroyAllProjects } from '../routes/internal.projects.destroy'
import {
  createTestProject,
  createTestSession,
  createTestUser,
  newTestRequest,
  truncateTables,
} from '../test-util'
import { ApiError } from '../util/errors'

describe('handleDestroyAllProjects', () => {
  afterEach(async () => {
    await truncateTables([
      prisma.userDetails,
      prisma.persistentSession,
      prisma.project,
      prisma.projectID,
    ])
  })

  beforeEach(async () => {
    await createTestUser(prisma, { id: 'bob' })
    await createTestUser(prisma, { id: 'alice' })
    await createTestSession(prisma, { key: 'the-key', userId: 'bob' })
    await createTestProject(prisma, { id: 'one', ownerId: 'bob' })
    await createTestProject(prisma, { id: 'two', ownerId: 'bob', deleted: true })
    await createTestProject(prisma, { id: 'three', ownerId: 'alice', deleted: true })
    await createTestProject(prisma, { id: 'four', ownerId: 'bob' })
    await createTestProject(prisma, { id: 'five', ownerId: 'bob' })
    await createTestProject(prisma, { id: 'six', ownerId: 'bob', deleted: true })
    await createTestProject(prisma, { id: 'seven', ownerId: 'alice' })
    await createTestProject(prisma, { id: 'eight', ownerId: 'alice', deleted: true })
  })

  it('requires a user', async () => {
    const fn = async () =>
      handleDestroyAllProjects(newTestRequest({ method: 'POST', authCookie: 'wrong-key' }), {})
    await expect(fn).rejects.toThrow(ApiError)
    await expect(fn).rejects.toThrow('unauthorized')
  })
  it('hard-deletes all soft-deleted projects owned by the user', async () => {
    const fn = async () => {
      const req = newTestRequest({ method: 'POST', authCookie: 'the-key' })
      return handleDestroyAllProjects(req, {})
    }

    await fn()
    const bobProjects = await prisma.project.findMany({
      where: { owner_id: 'bob' },
      orderBy: { id: 'asc' },
    })
    expect(bobProjects.map((p) => p.proj_id)).toEqual(['one', 'four', 'five'])
    const aliceProjects = await prisma.project.findMany({
      where: { owner_id: 'alice' },
      orderBy: { id: 'asc' },
    })
    expect(aliceProjects.map((p) => p.proj_id)).toEqual(['three', 'seven', 'eight'])
  })
})
