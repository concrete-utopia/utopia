import { prisma } from '../db.server'
import { handleDestroyProject } from '../routes/internal.projects.$id.destroy'
import {
  createTestProject,
  createTestSession,
  createTestUser,
  newTestRequest,
  truncateTables,
} from '../test-util'
import { ApiError } from '../util/errors'

describe('handleDestroyProject', () => {
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
    await createTestProject(prisma, {
      id: 'one',
      ownerId: 'foo',
      title: 'project-one',
    })
    await createTestProject(prisma, {
      id: 'two',
      ownerId: 'foo',
      title: 'project-two',
      deleted: true,
    })
    await createTestProject(prisma, {
      id: 'three',
      ownerId: 'bar',
      title: 'project-three',
      deleted: true,
    })
  })

  it('requires a user', async () => {
    const fn = async () =>
      handleDestroyProject(newTestRequest({ method: 'POST', authCookie: 'wrong-key' }), {})
    await expect(fn).rejects.toThrow(ApiError)
    await expect(fn).rejects.toThrow('unauthorized')
  })
  it('requires a valid id', async () => {
    const fn = async () =>
      handleDestroyProject(newTestRequest({ method: 'POST', authCookie: 'the-key' }), {})
    await expect(fn).rejects.toThrow(ApiError)
    await expect(fn).rejects.toThrow('id is null')
  })
  it('requires a valid project', async () => {
    const fn = async () => {
      const req = newTestRequest({ method: 'POST', authCookie: 'the-key' })
      return handleDestroyProject(req, { id: 'doesnt-exist' })
    }

    await expect(fn).rejects.toThrow('Record to delete does not exist')
  })
  it('requires ownership of the project', async () => {
    const fn = async () => {
      const req = newTestRequest({ method: 'POST', authCookie: 'the-key' })
      return handleDestroyProject(req, { id: 'three' })
    }

    await expect(fn).rejects.toThrow('Record to delete does not exist')
  })
  it('requires soft-deletion of the project', async () => {
    const fn = async () => {
      const req = newTestRequest({ method: 'POST', authCookie: 'the-key' })
      return handleDestroyProject(req, { id: 'one' })
    }

    await expect(fn).rejects.toThrow('Record to delete does not exist')
  })
  it('hard-deletes the project', async () => {
    const fn = async () => {
      const req = newTestRequest({ method: 'POST', authCookie: 'the-key' })
      return handleDestroyProject(req, { id: 'two' })
    }

    await fn()
    const got = await prisma.project.count({ where: { proj_id: 'two' } })
    expect(got).toEqual(0)
  })
})
