import { prisma } from '../db.server'
import { handleListDeletedProjects } from '../routes/internal.projects.deleted'
import {
  createTestProject,
  createTestSession,
  createTestUser,
  newTestRequest,
  truncateTables,
} from '../test-util'
import { ApiError } from '../util/errors'

describe('handleDeleteProject', () => {
  afterEach(async () => {
    await truncateTables([
      prisma.userDetails,
      prisma.persistentSession,
      prisma.project,
      prisma.projectID,
    ])
  })

  beforeEach(async () => {
    await createTestUser(prisma, { id: 'foo' })
    await createTestUser(prisma, { id: 'bar' })
    await createTestSession(prisma, { key: 'the-key', userId: 'foo' })
    await createTestProject(prisma, { id: 'one', ownerId: 'foo', title: 'project-one' })
    await createTestProject(prisma, {
      id: 'two',
      ownerId: 'foo',
      title: 'project-two',
      deleted: true,
    })
    await createTestProject(prisma, { id: 'three', ownerId: 'bar', title: 'project-three' })
    await createTestProject(prisma, {
      id: 'four',
      ownerId: 'foo',
      title: 'project-four',
      deleted: true,
    })
    await createTestProject(prisma, {
      id: 'five',
      ownerId: 'foo',
      title: 'project-five',
      deleted: true,
    })
  })

  it('requires a user', async () => {
    const fn = async () =>
      handleListDeletedProjects(newTestRequest({ method: 'POST', authCookie: 'wrong-key' }), {})
    await expect(fn).rejects.toThrow(ApiError)
    await expect(fn).rejects.toThrow('unauthorized')
  })
  it('returns deleted projects', async () => {
    const fn = async () => {
      const req = newTestRequest({ method: 'POST', authCookie: 'the-key' })
      return handleListDeletedProjects(req, {})
    }

    const got = await fn()
    expect(got.projects).toHaveLength(3)
    expect(got.projects.map((p) => p.proj_id)).toEqual(['five', 'four', 'two'])
  })
})
